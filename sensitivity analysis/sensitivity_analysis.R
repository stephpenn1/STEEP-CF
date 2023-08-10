# Basic sensitivity analysis
library(dplyr)
library(FME)
library(tidyr)
library(ggplot2)
library(relaimpo)

# Millenial setup

# Define constants
NUM_YEARS <- 1
NUM_RUNS <- 1000

# Read in input data - Described in Table A1 of Abramoff et al. (2021)
## forc_st: soil temperature in degrees Celcius
## forc_sw: volumetric soil moisture in mm3/mm3
## forc_npp: daily plant inputs in gC/m2/day
inputdata <- read.table("./input/model input/globalaverage.txt")
names(inputdata) <- c("forc_st","forc_sw","forc_npp")

# Read in parameters - Described in Table A1 of Abramoff et al. (2021)
parameters.file <- read.table("./input/model input/soilpara_in_fit.txt")
parameters <- as.list(parameters.file$V2)
names(parameters) <- parameters.file$V1

# Read in functions
source("./functions/run_functions.R") # R script that contains calls to run model
source("./functions/derivs_V2_MM.R") # The official version of Millennial V2

# Create a runlist to vary select parameters
generate_params <- function(run_numbers){
  nruns <- length(run_numbers)
  tibble(
    run_number = run_numbers,
    param_pH = rnorm(n = nruns, mean = 5.3, sd = 0.53), # pH, adjusted to average of site values
    param_bulkd = rnorm(n = nruns, mean = 1000, sd = 100), # bulk density in kg soil m-3
    param_pc = rnorm(n = nruns, mean = 0.86, sd = 0.086), # slope of mineral C - clay relationship from Georgiou et al. in review
    param_claysilt = rnorm(n = nruns, mean = 80, sd = 8)  #clay and silt content in %
  )
}

# Generate unique runs
runs <- c(1:NUM_RUNS)
runlist <- as.list(generate_params(runs))

# Function to create vectors for all the existing fixed parameters
params_fixed <- list()
for(p in names(parameters)){
  params_fixed[[p]] <- rep_len(parameters[[p]], length.out = length(runs))
}

# Combine the two lists
runlist_final <- c(runlist, params_fixed)

# Run the model
output <- list()
for(r in 1:length(runs)){
  # To access parameter values:
  # First, we want to drop the run number, so we pass in runlist_final[-1] to
  # remove the first element of the list.
  # Then, we need to access the list of parameter values for each run.
  # We can extract this from the list by looping through each run number and
  # pulling out that column from each list element. i.e., the first column
  # corresponds to run 1, second column to run 2, etc.
  params <- lapply(runlist_final[-1], `[`, r)

  # Run Millenial
  output[[r]] <- as.data.frame(Run_Model(inputdata,
                           derivs_V2_MM,
                           params,
                           num.years=NUM_YEARS,
                           # This can be adjusted to run a steady state
                           state=c(POM = 1, LMWC = 1, AGG = 1, MIC = 1, MAOM=1, CO2=0)))

  # Re-assign run number column now that the model has run
  output[[r]]$run_number <- r

}

# Combine data
model_output <- bind_rows(output) %>% as_tibble()

# Plot just the model outputs as a ribbon plot
# Clean up data
ribbon_plot_data <- model_output %>%
  pivot_longer(cols = c(POM, LMWC, AGG, MIC, MAOM, CO2),
               names_to = "metric",
               values_to = "values") %>%
  group_by(time, metric) %>%
  summarize(sd = sd(values),
            min = min(values),
            max = max(values),
            median = median(values))

# Plot
ribbon_plot <- ribbon_plot_data %>%
  ggplot(aes(x = time, fill = metric)) +
  geom_ribbon(aes(ymin = min, ymax = max),  alpha = 0.5) +
  geom_line(aes(y = median, color = metric), linewidth = 1) +
  facet_wrap(~metric, scales = "free_y") +
  scale_color_brewer(palette = "BrBG") +
  scale_fill_brewer(palette = "BrBG") +
  labs(x = "Time (days)",
       y = "Units?") +
  theme_bw() ; ribbon_plot

ggsave(plot = ribbon_plot, "./figures/model_outputs.jpg", height = 6, width = 9, units = "in")

# Calculate relative importance
# Define function
calc_relimp <- function(x) {

    t <- x$time # record time...
    x$time <- NULL # and drop time column
    run <- x$run_number[1] # record run number
    x$run_number <- NULL # drop run number column
    n <- x$name # record parameter name
    x$name <- NULL # and drop

    # Fit model. The formula notation means "CO2 as a
    # function of all other terms" (which is why we dropped year)
    lin <- lm(param ~ ., data = x)

    # Calculate relative importance metrics and extract 'lmg' results
    # 'lmg' is the R^2 contribution averaged over orderings among regressors;
    # should sum to one because we're using relative importances (rela=TRUE)
    # If the function can't solve, return a try error and continue running
    relimp <- try(calc.relimp(lin, type = "lmg", rela = TRUE)@lmg)
    if(class(relimp) == "try-error") {
      message(t, " calc.relimp error")
      return(NULL)
    }

    # Return a data frame: time, parameter, relative importance
    tibble(time = unique(t),
           output = names(relimp),
           value = relimp,
           source = n[1])
}

# Create a dataframe to pass to relative importance function
importance_test <- model_output %>%
  left_join(bind_rows(runlist), by = "run_number")

# Loop through each row of the data (except for the first day, where
# outputs = 1 and 0 - note this can be changed in the "run model" function)
# We want to look at all of the outputs, define here to loop through
out_params <- c("POM", "LMWC", "AGG", "MIC", "MAOM", "CO2")

# Create a list to store results
out <- list()
for(p in out_params){
  # Isolate one output at a time and get only the parameters we vary
  importance_test_filter <- importance_test %>%
    dplyr::select(c(run_number, time, p, names(runlist[2:5]))) %>%
    # Rename the param column to the output variable of interest...
    rename(param = p) %>%
    # ...and create a new column to preserve its name
    mutate(name = p)
  # Split the data up by day to calculate variance for each timestep
  imp <- split(importance_test_filter, list(importance_test_filter$time))
  out[[p]] <- lapply(imp[-1], FUN = calc_relimp) %>% bind_rows()
}

# Plot relative importance
relaimpo_plot <- bind_rows(out) %>%
  ggplot(aes(x = time, y = value, fill = output)) +
  geom_area() +
  facet_wrap(~source) +
  labs(x = "Timestep",
       y = "Relative importance for total CO2",
       fill = "Parameter") +
  # Adjust the zoom as needed - need to zoom out with longer runs
  coord_cartesian(ylim = c(0.65, 1.0)) +
  scale_fill_manual(values = pals::brewer.set2(n = 5)) +
  theme_bw() ; relaimpo_plot

ggsave(plot = relaimpo_plot, "./figures/relative_importance.jpg", height = 6, width = 9, units = "in")

# Plot relative importance vs soil moisture
soil_moisture <- inputdata %>%
  dplyr::select(moisture = forc_sw) %>%
  mutate(time = 1:365,
         sm_group = cut(moisture, 25)) %>%
  group_by(sm_group) %>%
  mutate(mean_sm_group = mean(moisture)) %>%
  left_join(bind_rows(out), by = c("time")) %>%
  filter(time != 1)

# Plot
soil_importance_plot <- soil_moisture %>%
  ggplot(aes(mean_sm_group, value, fill = output)) +
  geom_area() +
  facet_wrap(~source) +
  labs(x = "Soil moisture",
       y = "Relative importance for total CO2",
       fill = "Parameter") +
  coord_cartesian(ylim = c(0.95, 1.0)) +
  scale_fill_manual(values = pals::brewer.set2(n = 5)) +
  theme_bw() ; soil_importance_plot

ggsave(plot = soil_importance_plot, "./figures/soil_moisture_relaimpo.jpg",
       height = 6, width = 9, units = "in")
