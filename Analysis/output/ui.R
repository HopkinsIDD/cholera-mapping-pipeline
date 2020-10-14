library(Cairo)
ui <- fluidPage(
  titlePanel("Cholera Mapping Output Viewer"),
  helpText(
    "A shiny app for viewing the output of the cholera mapping pipeline"
  ),
  sidebarPanel(
    uiOutput("rates_log"),
    uiOutput("cases_log"),
    uiOutput("uniform_scale"),
    uiOutput("break_times"),
    uiOutput("break_chains"),
    tabsetPanel(
      id = "sidebar",
      tabPanel(
        title = "Data",
        p("One or models have been run on the following data"),
        br(),
        br(),
        uiOutput("data_stubs")
      ),
      tabPanel(
        title = "Models",
        p("The following models have been run on the selected data"),
        br(),
        br(),
        uiOutput("model_stubs")
      ),
      tabPanel(
        title = "Iterations",
        p("At least one model has been run for the following number of iterations"),
        br(),
        br(),
        uiOutput("iterations_stubs")
      )
    )
  ),
  mainPanel(
    tabsetPanel(
      id = "main",
      tabPanel(
        title = "Raw Input",
        tabsetPanel(
          id = "Views",
          tabPanel(
            title = "Case Counts",
            plotOutput("raw_cases")
          ),
          tabPanel(
            title = "Case Counts Rescaled by Area",
            plotOutput("raw_area_adjusted_cases")
          ),
          tabPanel(
            title = "Number of Observations",
            plotOutput("raw_observations")
          ),
          tabPanel(
            title = "Polygon Level Variance",
            plotOutput("raw_variance")
          ),
          tabPanel(
            title = "Population",
            plotOutput("raster_population")
          ),
          tabPanel(
            title = "Covariates",
            plotOutput("raster_covariates")
          )
        )
      ),
      tabPanel(
        title = "Raw Output",
        tabsetPanel(
          id = "output",
          tabPanel(
            title = "Modeled Cases",
            plotOutput("modeled_cases")
          ),
          tabPanel(
            title = "Modeled Rates",
            plotOutput("modeled_rates")
          )
        )
      ),
      tabPanel(
        title = "Observation Error",
        tabsetPanel(
          id = "observation-error",
          tabPanel(
            title = "Scatter Plot",
            uiOutput("model_fidelity")
          )
        )
      )
    )
  )
)
