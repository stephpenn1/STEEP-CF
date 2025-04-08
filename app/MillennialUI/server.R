
library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {

  forcing_file <- reactive({

    req(input$forc_file)

    ext <- tools::file_ext(input$forc_file$name)
    switch(ext,
           csv = vroom::vroom(input$forc_file$datapath, delim = ","),
           validate("Invalid file; Please upload a .csv file")
    )

    read_csv(input$forc_file$datapath)
    })

  observeEvent(input$run,
               ignoreInit = TRUE, {
                 print("yay")

                 forcing <- forcing_file()

                 #Define site-level parameters
                 parameters$param_pH <- input$ph
                 parameters$param_bulkd <- input$bulkd
                 parameters$param_pc <- input$pc
                 parameters$param_claysilt <- input$claysilt

                 Run_Model(forcing,
                           derivs_V2_MM,
                           parameters,
                           num.years = input$years,
                           state=c(POM = 1, LMWC = 1, AGG = 1, MIC = 1, MAOM=1, CO2=0)) ->> model_output

                 output$model_graph <- renderPlotly({

                   model_output %>%
                     as_tibble() %>%
                     mutate(across(is.numeric, as.double)) %>%
                     mutate(CO2_emissions = c(NA, diff(CO2))) %>% #calculate co2 emission rate
                     dplyr::select(-CO2) %>%
                     pivot_longer(cols = POM:CO2_emissions, names_to = "pool", values_to = "value") %>%
                     ggplot(aes(x = time, y = value, group = pool)) +
                     geom_line(color = "#1E3C6E", linewidth = 0.7) +
                     facet_wrap(~pool, scales = "free_y") +
                     labs(x = "Time (days)", y = "Value") + theme_minimal()
                 })
               }
  )

  output$forcing_plot <- renderPlot({

    forcing_file() %>%
      pivot_longer(cols = !DOY) %>%
      ggplot(aes(x = DOY, y = value)) +
      geom_line(color = "#35978F", linewidth = 0.7) +
      facet_wrap(~name, scales = "free", labeller = labeller(name = as_labeller(force_names, label_parsed))) +
      labs(x = "Day of Year", y = "") +
      theme_minimal(base_size = 24)

  })

  output$sim_years <- renderText({input$years})


}
