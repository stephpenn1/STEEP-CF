# Script to process 2022 Teros soil VWC, EC, and Temp
# Code adopted from Peter Regier

# STEEP-CF | Created 11/2023
# Stephanie Pennington | stephanie.pennington@pnnl.gov

# ---------------- Set up ----------------

# Load packages
require(pacman)
p_load(tidyverse,
       janitor,
       parsedate,
       lubridate,
       cowplot,
       purrr,
       furrr,
       plotly,
       googledrive,
       googlesheets4)

Sys.setenv(TZ='EST')

# List files
files <- list.files(path = "tempest_data", pattern = "PNNL_1", full.names = T) #filter for only Control plot

# Define function to read in teros data and format
read_teros <- function(filename){

  message("processing", filename)

  read_delim(filename, col_names = T, skip = 1) %>%
    slice(3:n()) %>%
    clean_names() %>%
    gather(channel, value, -timestamp, -record, -statname) %>%
    mutate(datetime_raw = parsedate::parse_date(timestamp, default_tz = "EST")) %>%
    #mutate(datetime = parsedate::parse_date(timestamp)) %>%
    separate(statname, into = c("inst", "data_logger_id"), sep = "_") %>%
    select(-inst, -timestamp) %>%
    mutate(channel = str_replace(channel, "teros_", "")) %>%
    separate(channel, into = c("terosdata_table_channel", "variable"), sep = "_") %>%
    mutate(variable = as.integer(gsub(")", "", variable, fixed = TRUE)),
           # Give them sensible names
           variable = case_when(variable == 1 ~ "vwc",
                                variable == 2 ~ "tsoil",
                                variable == 3 ~ "ec"),
           value = as.numeric(value)) %>%
    filter(variable != "ec")
}

# ---------------- Load data ----------------

lapply(files, read_teros) %>% bind_rows() -> t_raw

# ---------------- Make hourly averages ----------------

t_raw %>%
  mutate(datetime_raw = with_tz(datetime_raw, tzone = "EST"),
         Date = as_date(datetime_raw),
         #transform VWC to unitless
         value = case_when(variable == "vwc" ~ (0.0003879 * value) - 0.6956,
                           TRUE ~ value)) %>%
  group_by(variable, Date) %>%
  summarise(Value = mean(value, na.rm = TRUE)) %>%
  filter(Date >= "2022-01-01", Date <= "2022-12-31") %>%
  mutate(DOY = yday(Date)) %>%
  rename(Variable = variable) -> teros_2022

# ---------------- Graph to check ----------------

ggplot(teros_2022, aes(x = DOY, y = Value)) +
  geom_line() +
  facet_wrap(~variable, scales = "free") +
  theme_light()

