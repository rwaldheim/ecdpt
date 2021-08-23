# Defines the modal in which the cell masses can be exported from Excel
graphModal <- modalDialog(
  {
    fluidPage(
      style = "font-size:15pt;",
      tags$head(tags$style(".modal-dialog{min-width:60%}")),
      fluidRow(align = "center", graphInfoTable),
    )
  },
  title = "Graph Types",
  easyClose = TRUE
)

graphbuilder <- modalDialog(
  {
    fluidPage(
      useShinyjs(),
      useShinyalert(),
      tags$head(tags$style(".modal-dialog{width:80%}")),
      tags$head(tags$style(".modal-body{ min-height:1000px}")),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            headerPanel("Graph Options"),
          ),
          fluidRow(
            style = "padding:5%; border: 1px solid black;",
            strong("Basis for Plot Types"), tags$br(),
            helpText("Between Analysis requires a second dataset to be imported"), tags$br(),
            radioButtons("perType", NULL, choices = c("Within Analysis", "Between Analyses"), inline = TRUE),
            hidden(fileInput("compAnalysis", "Data to Compare"))
          ),
          fluidRow(
            style = "padding:5%; margin:5%;",
            radioButtons("typeGraph", "Graph Type:", choices = c(
              "dQdV Graphs", "Voltage Profiles", "Voltage vs. Time",
              "Charge Voltage", "Discharge Voltage",
              "Average Voltage", "Delta Voltage", "Discharge Capacity", "Charge Capacity"
            ), inline = FALSE),
            radioButtons("plotStyle", "Plot Style:", choiceNames = c("Point", "Line", "Both"), choiceValues = c("p", "l", "o"), inline = TRUE),
            checkboxGroupInput("cells", "Cell to Analyze:", choices = 1, inline = FALSE),
            selectInput("renderCycles", "Cycles of Interest:", choices = 1, multiple = TRUE),
          ),
          fluidRow(
            strong("Click to show coordinates:"), tags$br(), tags$br(),
            "X: ",
            textOutput("hoverCoordx", inline = TRUE), tags$br(),
            "Y: ",
            textOutput("hoverCoordy", inline = TRUE),
            style = "border: 1px solid black; padding: 5%; margin: 5%;"
          ),
          fluidRow(
            textInput("fileName", "Name of graph file:"),
            actionButton("saveGraph", "Save Graph", width = "100%", class = "btn-primary"),
            style = "border: 4px double black; padding: 5%; margin: 5%;"
          ),
        ),
        mainPanel(
          fluidRow(
            plotOutput("outputPlot", height = "800px", click = "plot_click"),
            style = "padding: 5%;",
          ),
          fluidRow(
            h3("Graph Formatting"),
            helpText("*If left blank, they will be calculated using the min and max of the data to be plotted."),
            column(
              2,
              numericInput("xMin", "X Min", value = NULL),
              sliderInput("textSize", "Text Size", min = 0.1, max = 5, value = 1, ticks = FALSE),
            ),
            column(
              2,
              numericInput("xMax", "X Max", value = NULL),
              sliderInput("pointSize", "Point/Line Size", min = 0.1, max = 5, value = 1, ticks = FALSE),
            ),
            column(
              2,
              numericInput("yMin", "Y Min", value = NULL),
            ),
            column(
              2,
              numericInput("yMax", "Y Max", value = NULL),
            ),
            column(
              4,
              textInput("originalData", "Original Data Name", value = "Original Data"),
              textInput("compareData", "Comparison Data Name", value = "Comparison Data"),
            ),
            style = "border: 1px dashed black; padding: 2%;",
          )
        )
      )
    )
  },
  size = "l",
  title = "Post-Processing Graph Builder"
)
