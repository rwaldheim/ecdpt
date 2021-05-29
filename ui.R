library(shiny)

ui <- shinyUI({
  fluidPage(
  
  # Utilizing javascript within Shiny allows for addd features such as enable/disable of inputs
  useShinyjs(),
  
  # Shinyalert is a package that makes interactive"pop-ups" (modals) easy to generate
  useShinyalert(),
  
  setBackgroundColor(
    color = c("ghostwhite", "lightgrey"),
    gradient = "linear",
    direction = "bottom",
    shinydashboard = FALSE
  ),
  
  fluidRow(headerPanel("Electrochemical Data Processing Tool (EcDPT)")),
  
  fluidRow(div(style = "color: red; font-size: 20pt;", "!!! Pay Attention. Controls Have Moved !!!"), align = "center"),
  
  # This first column is where most user inputs are, with the exception of the directory name
  column(4, align = "center",
         
         # This generates the optional block in which the user can import a previous R environment
         fluidRow(
           strong("Start Here"), tags$br(),
           "Current Cell Group: ", textOutput("currDir", inline = TRUE), tags$br(),
           actionButton("chooseDir", "Cell Group Location*", class = "btn-secondary", style = "width:80%; margin:5%; font-size:100%"), tags$br(),
           helpText("The analysis will create a folder within the selected folder."),
           style = "border: 1px solid black; padding: 5%; margin:5%"
         ),
         
         fluidRow(
           fileInput("rerun", "Optional: Import Previous R Environment", multiple = FALSE, accept = ".RData"),
           actionButton("load", "Load"),
           style = "border: 1px dashed black; margin: 5%; padding: 5%"
         ),
         
         # These are the"optional" parameters that need to be filled out if select graphs are selected
         fluidRow(
           strong("Optional Parameters"), tags$br(),
           "Parameters responsible for certain graphs.", tags$br(), tags$br(),
           
           # Used for dishcharge areal capacity graphs
           numericInput("area","Limiting Electrode Area (cm^2)", 2.74, min = 0),
           
           # Used for C-Rate calculations
           # numericInput("perActive","Active Loading of Limiting Electrode (wt%)", 96, min = 0, max = 100),
           # numericInput("capActive","Capacity of Limiting Active Material (mAh/g)", 155, min = 0, max = 100),
           style ="border: 1px dashed black; padding: 5%; margin:5%"
         ),
  ),
  
  # The second column is where selection of graphs and further features are selected
  column(4, align ="left", 
         fluidRow(
           # Presents options for graphs to be generated
           "Choose graphs to be generated:",
           actionButton("whatGraph","What's this?", class ="btn-link"),
           checkboxGroupInput("gGraphs", NULL, choices = c("Discharge Capacity","Discharge Areal Capacity",
                                                           "Total Discharge Capacity","Average Voltage","Delta Voltage","Capacity Loss"), inline = FALSE),
           "Choose graphs to animate:",
           checkboxGroupInput("gAnim", NULL, choices = c("dQdV Plots", "Voltage Profiles"), inline = FALSE),
           "Advanced Analysis",
           #radioButtons("advCalc", NULL, choices = c("No", "Yes"), inline = TRUE),
           #helpText(HTML("Advanced Analysis includes:<ul><li>C-Rate Calculations</li><li>Capacity Fade per Rate</li><li>Origin Export</ul>")),
           style ="margin: 5%; border: 1px solid black; padding: 5%"
         ),
  ), 
  
  # The final column is where all the"action" items are, aka clicking any of these buttons will trigger a process
  column(4, align ="center",
         fluidRow(
           selectInput("dirName", "Analysis Name*", c("Formation", "RateCap", "Constant Current", "CC-CV"), selected = "RateCap"), tags$br(),
           strong("Files to be Analyzed*"), tags$br(),
           "Import all Arbin files of interest.", tags$br(), tags$br(),
           fileInput("files", NULL, multiple = TRUE),
           actionButton("submit", "Begin Analysis", class = 'btn-success', style = "width:80%; height:100px; margin:5%; font-size:100%"),
           style = "border: 4px double black; padding: 5%; margin:5%"
         ),
         
         # This final block enables a button after data becomes available, which trigger the modal to build custom graphs
         fluidRow(
           strong("Custom Graph Builder"), tags$br(),
           "Customize Graphs Once Data is Available",
           disabled(actionButton("graphBuilder","Launch", width = '80%', class ="btn-primary", style ="height:50px; margin:5%; font-size:100%")), tags$br(),
           style ="border: 1px solid black; padding: 5%; margin:5%"
         ),
         
         fluidRow(
           strong("Optional: Import Active Material Masses from Excel"), tags$br(),
           "Running Analysis without Masses Will Render Raw Capacities (Ah)",
           textAreaInput("masses", NULL, height = "100px", resize = "vertical"),
           helpText("Enter the masses separated by a new line and in the order they appear in the data table below."),
           actionButton("excelImport", "Import"),
           style = "border: 1px dashed black; padding: 5%; margin:5%"
         ),
  ),
  
  # This renders the summary datatable at the bottom of the interface once data is imported
  fluidRow(
    dataTableOutput("channels")
  )
)
}
)