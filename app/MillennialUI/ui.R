
# Define UI for application that draws a histogram
page_sidebar(
  title = "MillennialUI",
  theme = bs_theme(bootswatch = "sandstone", version = version_default()),

  sidebar = sidebar(
    accordion(
      accordion_panel(
        "Forcings", icon = bsicons::bs_icon("file-bar-graph"),
        fileInput(
          inputId = "forc_file",
          label = "Forcing dataset",
          multiple = FALSE,
          buttonLabel = "Upload",
          placeholder = "No file selected",
          accept = ".csv"
        )
      ),
      accordion_panel(
        "Parameters", icon = bsicons::bs_icon("sliders"),
        numericInput(inputId = "ph", label = "pH", value = 7),
        numericInput(inputId = "bulkd", label = "Bulk Density", value = 1000),
        numericInput(inputId = "pc", label = "PC", value = 0.86),
        numericInput(inputId = "claysilt", label = "ClaySilt", value = 80),
        numericInput(inputId = "years", label = "Simulation Years", value = 1, min = 1, max = 50)
      )
    ),
    actionBttn(
      inputId = "run",
      label = "Run Millennial",
      style = "unite",
      color = "primary"
    )
  ),
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Gallons distributed",
      value = '240,000',
      showcase = bsicons::bs_icon("droplet"),
      theme = "text-success"
#      theme = value_box_theme(bg = "#e6f2fd", fg = "#0B538E")
    ),
    value_box(
      title = "Simulation Years",
      value = textOutput("sim_years", container = h2),
      showcase = bsicons::bs_icon("calendar3"),
      theme = "text-success")
  ),
  layout_columns(
    col_widths = c(12, 12),
    row_heights = c(3, 4),
    card(
      full_screen = TRUE,
      card_header("Output"),
      plotlyOutput("model_graph")
    )
  ),
  layout_columns(
    col_widths = c(12, 12),
    row_heights = c(3, 4),
    card(
      full_screen = TRUE,
      card_header("Forcings"),
      plotOutput("forcing_plot")
    )
  )
)
