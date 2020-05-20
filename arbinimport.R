# ######
#
# Welcome to the Battery Analyzer Utility!
#   
# This script aims to make it quick and efficient to analyze data exported by an Arbin battery cycler
#
# ######
 
# ######
# 
# These are all the required packages to aid in several of the processes, ranging from data analysis to plotting
# 
# ######

require(readxl)
require(dplyr)
require(shiny)
require(tcltk)
require(DT)
require(shinyjs)
require(shinyalert)
require(pracma)
require(purrr)
require(zoo)
require(plotrix)
require(tools)
require(shinyWidgets)

require(gifski)

# This line tests if the current R environment is interactive, RStudio makes an interactive environment by default
if (interactive()) {
  
  # ######
  # 
  # All the global variables within the script, aka variables that need to be accessed by more than one
  # function or session
  #
  #"Reactive Values" are ones that need to be readily changed, such as user inputs and variables to be displayed
  # 
  # ######
  data <- reactiveValues(data = data.frame())
  final <- data.frame()
  dirLocation <- reactiveVal("")
  numCycles <- data.frame()
  dQdVData <- data.frame()
  total <- data.frame()
  cycle_facts <- data.frame()
  tmp_data <- data.frame()
  dirName <<- ""
  tmp_cycles <- vector()
  titleLabel <-""
  xlabel <-""
  ylabel <-""
  addParams <- FALSE
  catMetric <<- vector()
  legTitle <<-""
  sheetName <<-""
  bounds <<- vector()

  # ######
  # 
  # This is the UI function for Shiny, it defines how the layout of what the user sees
  # 
  # ######
  ui <- fluidPage(
    
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
    
    fluidRow(headerPanel("Battery Analyzer Utility")),
    
    # This first column is where most user inputs are, with the exception of the directory name
    column(4,
           
      # This generates the optional block in which the user can import a previous R environment
      fluidRow(
        strong("Files to be Analyzed*"), tags$br(),
        "Import all Arbin files of interest.", tags$br(), tags$br(),
        fileInput("files", NULL, multiple = TRUE),
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
             style ="margin: 5%; border: 1px solid black; padding: 5%"
           ),
           
           fluidRow(
             imageOutput("birlaLogo")
           ),
    ), 
    
    # The final column is where all the"action" items are, aka clicking any of these buttons will trigger a process
    column(4, align ="center",
      fluidRow(
        textInput("dirName", "Analysis Name*"), tags$br(),
        "Current Location: ", textOutput("currDir", inline = TRUE), tags$br(),
        actionButton("chooseDir", "Change Output Location*", class = "btn-secondary", style = "width:80%; margin:5%; font-size:100%"), tags$br(),
        helpText("The analysis will create a folder within the selected folder."),
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
  
  # ######
  # 
  # This is the server functon of Shiny. It defines all the"processing" of the data that the user initiated through the interface
  # 
  # ######
  server <- function(input, output, session) {
    
    # This sets the maximum file size Shiny will import, the default of 5Mb is not large enough to handle Arbin files
    options(shiny.maxRequestSize=50*1024^2)
    
    split_path <- function(x) if (dirname(x)==x) x else c(basename(x),split_path(dirname(x)))
    
    # Defines the modal in which the cell masses can be exported from Excel
    graphModal <- modalDialog({
      fluidPage(style ="font-size:15pt;",
        tags$head(tags$style(".modal-dialog{min-width:60%}")),

        fluidRow(align ="center",
          HTML('
            <style type="text/css">
            .tg  {border-collapse:collapse;border-spacing:0;}
            .tg td{font-family:Arial, sans-serif;font-size:14px;padding:10px 5px;border-style:solid;border-width:1px;overflow:hidden;word-break:normal;border-color:black;}
            .tg th{font-family:Arial, sans-serif;font-size:14px;font-weight:normal;padding:10px 5px;border-style:solid;border-width:1px;overflow:hidden;word-break:normal;border-color:black;}
            .tg .tg-gfnm{background-color:#efefef;border-color:#000000;text-align:center;vertical-align:middle}
            .tg .tg-i0p4{font-weight:bold;background-color:#ecf4ff;border-color:#000000;text-align:center;vertical-align:middle}
            .tg .tg-3fas{background-color:#efefef;border-color:#000000;text-align:left;vertical-align:middle}
            .tg .tg-o3hj{background-color:#ecf4ff;border-color:#000000;text-align:center;vertical-align:middle}
            .tg .tg-xwyw{border-color:#000000;text-align:center;vertical-align:middle}
            .tg .tg-0a7q{border-color:#000000;text-align:left;vertical-align:middle}
            </style>
            <table class="tg">
              <tr>
                <th class="tg-i0p4">Graph</th>
                <th class="tg-i0p4">X Axis</th>
                <th class="tg-i0p4">Y Axis</th>
                <th class="tg-o3hj"><span style="font-weight:bold">Plot Frequency</span><br></th>
                <th class="tg-i0p4">Description</th>
              </tr>
              <tr>
                <td class="tg-xwyw">dQdV Graph</td>
                <td class="tg-xwyw">Voltage (V)</td>
                <td class="tg-xwyw">dQdV (Ah/V)</td>
                <td class="tg-xwyw">per cycle</td>
                <td class="tg-0a7q">The differential capacity plot for each cycle<br></td>
              </tr>
              <tr>
                <td class="tg-gfnm">Voltage Profile</td>
                <td class="tg-gfnm">Continuous Capacity (mAh/g or Ah)</td>
                <td class="tg-gfnm">Voltage (V)</td>
                <td class="tg-gfnm">per cycle</td>
                <td class="tg-3fas">Voltage vs. capacity plot for each cycle. Units depend if the masses are specified.</td>
              </tr>
              <tr>
                <td class="tg-xwyw">Voltage vs. Time</td>
                <td class="tg-xwyw">Time (min)</td>
                <td class="tg-xwyw">Voltage (V)</td>
                <td class="tg-xwyw">per cycle</td>
                <td class="tg-0a7q">The voltage as a function of time, including all steps</td>
              </tr>
              <tr>
                <td class="tg-gfnm">Discharge Capacity</td>
                <td class="tg-gfnm">Cycle</td>
                <td class="tg-gfnm">Discharge Capacity (mAh/g or Ah)</td>
                <td class="tg-gfnm">per cell</td>
                <td class="tg-3fas">Discharge capacity for each <span style="font-weight:bold">individual cell </span>per cycle. Coulombic efficiency is also plotted on a secondary axis. Units depend if the masses are specified.</td>
              </tr>
              <tr>
                <td class="tg-xwyw">Discharge Areal Capacity</td>
                <td class="tg-xwyw">Cycle</td>
                <td class="tg-xwyw">Discharge Capacity (Ah/cm<sup>2</sup>)</td>
                <td class="tg-xwyw">per cell</td>
                <td class="tg-0a7q">Discharge areal capacity for each <span style="font-weight:bold">individual cell </span>per cycle. Coulombic efficiency is also plotted on a secondary axis.</td>
              </tr>
              <tr>
                <td class="tg-gfnm">Total Discharge Capacity</td>
                <td class="tg-gfnm">Cycle</td>
                <td class="tg-gfnm">Discharge Capacity (mAh/g or Ah)</td>
                <td class="tg-gfnm">per analysis</td>
                <td class="tg-3fas">Discharge capacity summarized for <span style="font-weight:bold">all cells</span> in the analysis. Coulombic efficiency is also plotted on a secondary axis. Mean is plotted as a point with error bars presenting the standard error between the cells. Units depend if the masses are specified.</td>
              </tr>
              <tr>
                <td class="tg-xwyw">Average Voltage</td>
                <td class="tg-xwyw">Cycle</td>
                <td class="tg-xwyw">Voltage (V)</td>
                <td class="tg-xwyw">per cell</td>
                <td class="tg-0a7q">The average voltage vs capacity for each cycle. The charge voltage (V<sub>charge</sub>) and discharge voltage (V<sub>discharge</sub>) were calculated using the average value theorem. The average voltage is then (V<sub>charge</sub> + V<sub>discharge</sub>)/2. Charge and discharge voltages are plotted alongside the average.</td>
              </tr>
              <tr>
                <td class="tg-gfnm">Delta Voltage</td>
                <td class="tg-gfnm">Cycle</td>
                <td class="tg-gfnm">Voltage (V)</td>
                <td class="tg-gfnm">per cell</td>
                <td class="tg-3fas">The delta voltage vs capacity for each cycle. The charge voltage (V<sub>charge</sub>) and discharge voltage (V<sub>discharge</sub>) were calculated using the average value theorem. The delta voltage is then V<sub>charge</sub> - V<sub>discharge</sub>). Charge and discharge voltages are plotted alongside the average.</td>
              </tr>
              <tr>
                <td class="tg-xwyw">Capacity Loss</td>
                <td class="tg-xwyw">Cycle</td>
                <td class="tg-xwyw">Capacity (mAh/g or Ah)</td>
                <td class="tg-xwyw">per cell</td>
                <td class="tg-0a7q">The discharge capacity minus the charge capacity for each cycle. Units depend if the masses are specified.</td>
              </tr>
            </table>
          ')
        ),
      )}, title ="Graph Types", easyClose = TRUE)
    
    # Renders the GIF image for the excelModal
    output$importGIF <- renderImage({
      list(src ="excelImport.gif", width ="70%")
    }, deleteFile = FALSE)
    
    # Ensures the files imported for analysis are Excel files
    observeEvent(input$files, {
      validFile <- FALSE
      
      for (file in input$files) {
        if (file_ext(file) =="xlsx" | file_ext(file) =="xls") {
          validFile <- TRUE
        }
      }
      
      if (validFile) {
        renderTable()
      } else {
        shinyalert("That isn't right...","Please upload an Excel file.","error")
      }
    })
    
    observeEvent(input$chooseDir, {
      dirLocation(tk_choose.dir())
      
      if (!is.na(dirLocation())) {
        output$currDir <- renderText({paste(split_path(dirLocation())[1], "(", split_path(dirLocation())[2], ")")})
      }
    })
    
    output$birlaLogo <- renderImage({
      list(src = "birlaLogo.png", style="display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE)

    graphbuilder <- modalDialog({
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
            
            fluidRow(style ="padding:5%; margin:5%;",
              radioButtons("typeGraph","Graph Type:", choices = c("dQdV Graphs","Voltage Profiles", "Voltage vs. Time", 
                                                                  "Charge Voltage", "Discharge Voltage", 
                                                                  "Average Voltage", "Delta Voltage", "Discharge Capacity", "Charge Capacity" ), inline = FALSE),
              radioButtons("plotStyle","Plot Style:", choiceNames = c("Point","Line","Both"), choiceValues = c("p","l","o"),  inline = TRUE),
              checkboxGroupInput("cells","Cell to Analyze:", choices = 1, inline = FALSE),
              hidden(checkboxGroupInput("cellsMulti","Cells to Analyze:", choices = 1, inline = FALSE)),
              selectInput("renderCycles","Cycles of Interest:", choices = 1, multiple = TRUE),
            ),
            
            fluidRow(
              strong("Click to show coordinates:"), tags$br(), tags$br(),
              "X: ",
              textOutput("hoverCoordx", inline = TRUE), tags$br(),
              "Y: ",
              textOutput("hoverCoordy", inline = TRUE),
              style ="border: 1px solid black; padding: 5%; margin: 5%;"
            ),
             
            fluidRow(
             textInput("fileName","Name of graph file:"),
             actionButton("saveGraph","Save Graph", width = '100%', class = 'btn-primary'),
             style ="border: 4px double black; padding: 5%; margin: 5%;"
            ),
          ),
          
          mainPanel(
                 fluidRow(
                  plotOutput("outputPlot", height ="800px", click = "plot_click"),
                  style = "padding: 5%;",
                 ),
                 fluidRow(
                   h3("Graph Formatting"),
                   helpText("*If left blank, they will be calculated using the min and max of the data to be plotted."),
                   column(2,
                   numericInput("xMin", "X Min", value = NULL),
                   sliderInput("textSize", "Text Size", min = 0.1, max = 5, value = 1, ticks = FALSE),
                   ),
                   column(2,
                   numericInput("xMax", "X Max", value = NULL),
                   sliderInput("pointSize", "Point/Line Size", min = 0.1, max = 5, value = 1, ticks = FALSE),
                   ),
                   column(2, 
                   numericInput("yMin", "Y Min", value = NULL),
                   ),
                   column(2,
                   numericInput("yMax", "Y Max", value = NULL),
                   ),
                   style ="border: 1px dashed black; padding: 2%;",
                 )
          )
        )
      )
    }, size ="l", title ="Post-Processing Graph Builder")
    
    # Method for importing the previous R environment
    observeEvent(input$load, {
      load(input$rerun$datapath[[1]])
      
      validFile <- FALSE
      
      if (file_ext(input$rerun$datapath) =="RData") {
        validFile <- TRUE
      }
      
      if (validFile) {
        data <<- filter(data, grepl('Channel', sheet))
        dirLocation(dirLocation())
        numCycles <<- numCycles
        dQdVData <<- dQdVData
        total <<- total
        cycle_facts <<- cycle_facts
        
        output$channels <- renderDataTable(data, editable = TRUE, options=list(columnDefs = list(list(visible=FALSE, targets=c(4)))), 
                                           colnames = c("File","Sheet","Mass (g)","Filepath","Limiting Electrode Area (cm^2)","Active Material Loading (wt%)", 
                                                       "Active Mateial Capacity (mAh/g)"))
        
        enable("graphBuilder")
      } else {
        shinyalert("That isn't right...","Please upload an RData file.","error")
      }
    })
    
    # After the validation of the Arbin files, they macros (file name, and sheets) are taken and rendered in to a datatable
    renderTable <- function() {
      output$channels <- renderDataTable({
        files <- input$files
        
        if (is.null(files)) {
          return(NULL)
        }
        
        file_sheet <- data.frame()
        for (i in 1:nrow(files)) {
          sheets <- excel_sheets(files[i, 4])
          file_sheet <- rbind(file_sheet, data.frame(name = rep(files[["name"]][i], length(sheets)),"sheet" = sheets,"Mass" = rep(0, length(sheets)),
                                                     datapath = rep(files[["datapath"]][i], length(sheets)), area = rep(input$area, length(sheets))))
        }
        
        data <<- filter(file_sheet, grepl('Channel', sheet) & !grepl('Chart', sheet))
        
        data
  
      }, editable = FALSE, options=list(columnDefs = list(list(visible=FALSE, targets=c(4)))), 
      colnames = c("File","Sheet","Mass (g)","Filepath","Limiting Electrode Area (cm^2)"))
    }
    
    observeEvent(input$whatGraph, {
      showModal(graphModal)
    })
    
    # Data validation the masses imported from Excel, if valid they are placed into the datatable
    observeEvent(input$excelImport, {
      if (length(names(data)) <= 1) {
        shinyalert("Uh oh!","You need to import cells first!","error")
        removeModal()
      } else {
        tryCatch({
          masses <- lapply(strsplit(strRep(input$masses, "\n", ","), ",", fixed = TRUE), as.double)
          names(masses)[names(masses) =="V1"] <-"Mass"
          data$Mass <<- masses[[1]]
        }, error = function(cond) {
          print(cond)
          shinyalert("Something isn't right...","The number of masses imported did not match the amount of cells present or the text contained some special characters. Please try again.","error")
          removeModal()
        }, finally = {
          proxy = dataTableProxy("channels")
          replaceData(proxy, data)
          
          renderDataTable(data)
          
          removeModal()
        })
      }
    })
    
    # After some data validation, the main analysis is run on click of the"Run Analysis" button
    observeEvent(input$submit, {
      if (length(names(data)) <= 1) {
        shinyalert("Uh oh!", "You need to import cells first!", "error")
      } else if (is.na(dirLocation()) | dirLocation() == "") {
        shinyalert("Uh oh!", "You need to enter a directory name first!", "error")
      } else if (input$dirName == "") {
        shinyalert("Uh oh!", "You need to enter an analysis name first!", "error")
      } else {
        runscript()
      }
    })
    
    # This function responsible for analysis of the 
    runscript <- function() {
      
      # Sets up a progress bar in which to estimate how long the execution of the code will take
      progress <- Progress$new(session, min = 0, max = nrow(data))
      progress$set(message ="Plugging and chugging...\n", detail ="Starting up...")
      
      # Closes all graphics devices that may be lingering (prevents an excess from opening and slowing down the analysis)
      while (dev.cur() != 1) {
        dev.off()
      }
      
      # Resets the variables for the graph builder so new results are concatenated to old ones
      numCycles <<- data.frame()
      dQdVData <<- data.frame()
      total <<- data.frame()
      cycle_facts <<- data.frame()
      
      # Disable all input fields to prevent errors occurring from changing values
      disable("files")
      disable("lowV")
      disable("highV")
      disable("dirLocation()")
      disable("submit")
      disable("excelImport")
      disable("gGraphs")
      disable("peakFit")
      disable("area")
      disable("perActive")
      disable("capActive")
      
      # Creates the directory in which all data will be stored
      dir.create(paste(dirLocation(), input$dirName, sep = "/"))
      
      # Defines the equation for standard error of a vector
      se <- function(x) {sd(x) / sqrt(length(x))}
      
      # Update the status once all set-up functions are complete
      progress$set(detail ="Starting first cell...")
      
      # ######
      # 
      # The bulk of the analysis occurs within the loop. Each iteratin of the loop corresponds to a cell.
      # 
      # ######
      for (row in 1:nrow(data)) {
        
        # ######
        # 
        # This is where all code that should be executed on a"per cell" basis, to prepare for analysis
        # 
        # ######
        
        # Import the excel sheet corresponding to cell of interest
        tmp_excel <- read_excel(toString(data$datapath[row]), toString(data$sheet[row]))
        
        # Create an nested directory for all the data and, if applicable, then further folders for graphs of interest
        dir.create(paste(dirLocation(), "/",  input$dirName, data$sheet[row], sep ="/"))
        if (is.element("dQdV Graphs", input$gGraphs)) dir.create(paste(dirLocation(), input$dirName, data$sheet[row],"dQdV Plots", sep ="/"))
        if (is.element("Voltage Profiles", input$gGraphs)) dir.create(paste(dirLocation(), input$dirName, data$sheet[row],"Voltage Profiles", sep ="/"))
        if (is.element("Voltage vs. Time", input$gGraphs)) dir.create(paste(dirLocation(), input$dirName, data$sheet[row],"Voltage v Time", sep ="/"))
        if (input$peakFit =="fit") dir.create(paste(dirLocation(), input$dirName, data$sheet[row],"dQdV Peak Fitting", sep ="/"))
        
        # Check if masses have been imported, if they have not then all future calculations will be done on a raw capacity basis
        if (sum(data$Mass) != 0) {
          ylabel <-"Capacity (mAh/g)"
          
          tmp_excel$Q.d <- as.numeric(tmp_excel$`Discharge_Capacity(Ah)` * (1000 / data$Mass[row]))
          tmp_excel$Q.c <- as.numeric(tmp_excel$`Charge_Capacity(Ah)`* (1000 / data$Mass[row]))
          
          tmp_excel$CC <- tmp_excel$Q.d - tmp_excel$Q.c
          tmp_excel$CE <- (tmp_excel$Q.d / tmp_excel$Q.c) * 100
        } else {
          ylabel <-"Capacity (Ah)"
          
          tmp_excel$CC <- tmp_excel$`Discharge_Capacity(Ah)` - tmp_excel$`Charge_Capacity(Ah)`
          tmp_excel$CE <- (tmp_excel$`Discharge_Capacity(Ah)` / tmp_excel$`Charge_Capacity(Ah)`) * 100
        }
        tmp_excel$Cell <- row
        tmp_excel$CE[is.infinite(tmp_excel$CE)|is.nan(tmp_excel$CE)|tmp_excel$CE > 200] <- 0;
        
        # ######
        # 
        # This loop iterates through each cycle of the cell.
        # 
        # ######
        cycles <- split(tmp_excel, tmp_excel$Cycle_Index)
        prev_c <- 0
        ch_dch <- FALSE
        prev <- TRUE
        dchV <- 0
        chV <- 0
        i <- 1
        for (cycle in cycles) {
          
          # ######
          # 
          # Within each cycle, take out the individual steps. These include the charge, discharge, and others.
          # 
          # ######
          
          progress$set(detail = paste("Analyzing cell", row,", cycle", i))
          
          steps <- split(cycle, cycle$Step_Index)
          for (step in steps) {
            
            # ######
            # 
            # Isolation of the charge and discharge cycles. The algorithm is as follows:
            #   
            # If the change in voltage for the step is greater than 0.5V, it is a charge or dicharge cycle. Then, the direction (sign) of the current determines
            # if it is charge or discharge (positive current = charge step, negative current is a discharge cycle).
            # 
            # ######
            if (abs(tail(step$'Voltage(V)',1) - step$'Voltage(V)'[[1]]) > 0.5) {
              
              # ######
              # 
              # All code that should be executed for every charge/discharge cycles should be written here.
              # 
              # ######
              ch_dch <- TRUE
              if (step$'Current(A)'[[1]] > 0) {
                chV <- (1 / (tail(step$`Charge_Capacity(Ah)`,1) - step$`Charge_Capacity(Ah)`[[1]])) * trapz(step$`Charge_Capacity(Ah)`, step$`Voltage(V)`)
                dQCdV <- diff(step$`Charge_Capacity(Ah)`)/diff(step$`Voltage(V)`)
                dQdVData <<- rbind(dQdVData, data.frame(cycle=rep(i, length(dQCdV)+1), cell = rep(row, length(dQCdV)+1), c_d=rep(0, length(dQCdV)+1), voltage=step$`Voltage(V)`, dQdV=c(0, dQCdV), F_L=rep(0,length(dQCdV)+1)))
              } else {
                dchV <- (1 / (tail(step$`Discharge_Capacity(Ah)`,1) - step$`Discharge_Capacity(Ah)`[[1]])) * trapz(step$`Discharge_Capacity(Ah)`, step$`Voltage(V)`)
                dQDdV <- diff(step$`Discharge_Capacity(Ah)`)/diff(step$`Voltage(V)`)
                
                if (abs(prev_c - step$`Current(A)`[[1]]) > 0.0005) {
                  dQdVData <<- rbind(dQdVData, data.frame(cycle=rep(i, length(dQDdV)+1), cell = rep(row, length(dQDdV)+1), c_d=rep(1, length(dQDdV)+1), voltage=step$`Voltage(V)`, dQdV=c(0, dQDdV), F_L=rep(1,length(dQDdV)+1)))
                  prev_c = step$`Current(A)`[[1]]
                } else {
                  dQdVData <<- rbind(dQdVData, data.frame(cycle=rep(i, length(dQDdV)+1), cell = rep(row, length(dQDdV)+1), c_d=rep(1, length(dQDdV)+1), voltage=step$`Voltage(V)`, dQdV=c(0, dQDdV), F_L=rep(0, length(dQDdV)+1)))
                }
              }
            }
          }
            
          # ######
          # 
          # Code meant to be run on data"per cycle" should be written here
          # 
          # ######
          
          dQdVData <<- dQdVData[is.finite(dQdVData$voltage),]
          dQdVData <<- dQdVData[is.finite(dQdVData$dQdV),]
          
          if (sum(data$Mass) != 0) {
            DCap <- tail(cycle$Q.d, 1)
            CCap <- tail(cycle$Q.c, 1)
          } else {
            DCap <- tail(cycle$`Discharge_Capacity(Ah)`, 1)
            CCap <- tail(cycle$`Charge_Capacity(Ah)`, 1)
          }
          
          # Record charge and dischatge voltage, then calculate the delta and average voltage
          cycle_facts <<- rbind(cycle_facts, data.frame(cycle=i, cell=row, chV=chV, dchV=dchV, avgV=(dchV + chV) / 2, dV=chV-dchV, DCap = DCap, CCap = CCap, CE = (DCap / CCap) * 100, lostCap = CCap - DCap))
          
          i <- i + 1
          ch_dch <- FALSE
        }
        
        # ######
        # 
        # Code meant to be run on data"per cell" should be written here
        # 
        # ######
        
        cell_data <- cycle_facts[cycle_facts$cell == row,]
        
        # Discharge capacity plotting, with coulombic efficiency being plotted alongside
        if (is.element("Discharge Capacity", input$gGraphs)) {
          png(paste(dirLocation(), "/",  input$dirName,"/", data$sheet[row],"/", data$name[row], data$sheet[row]," Discharge Capacity Plot.png", sep =""))
          eol <- cell_data$`DCap`[[1]] * 0.8
          plot(cell_data$cycle, cell_data$DCap, type ="p", main=paste("Discharge Capacity for",  input$dirName), xlab=NA, ylab=paste("Discharge",  ylabel), mai=c(1,1,1,1))
          abline(h=eol, lty ="dotted")
          par(new = T)
          plot(cell_data$cycle, cell_data$CE, type ="p", axes=F, col ="red", ylab=NA, xlab="Cycle", ylim = c(0, 105))
          mtext(side = 4, line = 3,"Coulombic Efficiency (%)", col = "red")
          axis(side = 4, col ="red", col.axis = "red")
          dev.off()
        }
        
        # Discharge areal capacity plotting, with coulombic efficiency being plotted alongside
        if (is.element("Discharge Areal Capacity", input$gGraphs)) {
          png(paste(dirLocation(), "/",  input$dirName,"/", data$sheet[row],"/", data$name[row], data$sheet[row]," Discharge Areal Capacity Plot.png", sep =""))
          new_par <- old_par <- par("mar")
          new_par[4] <- old_par[2]
          par(mar = new_par)
          if (sum(data$Mass) != 0) {
            eol <- ((cell_data$DCap[[1]] * data$Mass[row,]) / data$area[row]) * 0.8
            plot(cell_data$cycle, ((cell_data$DCap * data$Mass[row,]) / data$area[row]), type ="p", main=paste("Discharge Areal Capacity for",  input$dirName), xlab=NA, ylab="Discharge Capacity (mAh/cm^2)", mai = c(1,1,1,2))
          } else {
            eol <- ((cell_data$DCap[[1]] * 1000) / data$area[row]) * 0.8
            plot(cell_data$cycle, ((cell_data$DCap * 1000) / data$area[row]), type ="p", main=paste("Discharge Areal Capacity for",  input$dirName), xlab=NA, ylab="Discharge Capacity (mAh/cm^2)", mai = c(1,1,1,2))
          }
          abline(h=eol, lty ="dotted")
          par(new = T)
          plot(cell_data$cycle, cell_data$CE, type ="p", axes=F, col ="red", ylab=NA, xlab="Cycle", ylim = c(0, 105))
          mtext(side = 4, line = 3,"Coulombic Efficiency (%)", col = "red")
          axis(side = 4, col ="red", col.axis = "red")
          dev.off()        
        }

        
        # Average voltage plotting
        if (is.element("Average Voltage", input$gGraphs)) {
          png(paste(dirLocation(), "/",  input$dirName,"/", data$sheet[row],"/", data$name[row], data$sheet[row]," Average Voltage Plot.png", sep =""))
          plot(cell_data$cycle, cell_data$chV, col="blue", main=paste("Average Voltage Plot for",  input$dirname, data$sheet[row]), xlab="Cycle", ylab="Voltage (V)", ylim=c(min(cell_data[,2:4]), max(cell_data[,2:4])))
          points(cell_data$cycle, cell_data$dchV, col="red", main=paste("Average Voltage Plot for",  input$dirName, data$sheet[row]), xlab="Cycle", ylab="Voltage (V)")
          points(cell_data$cycle, cell_data$avgV, col="black", main=paste("Average Voltage Plot for",  input$dirName, data$sheet[row]), xlab="Cycle", ylab="Voltage (V)")
          legend("bottomright", c("Charge Voltage","Discharge Voltage","Average Voltage"), col=c("blue","red","black"), pch=19)
          dev.off()
        }
        
        # Delta voltage plotting
        if (is.element("Delta Voltage", input$gGraphs)) {
          png(paste(dirLocation(), "/",  input$dirName,"/", data$sheet[row],"/", data$name[row], data$sheet[row]," Delta Voltage Plot.png", sep =""))
          plot(cell_data$cycle, cell_data$dV, main=paste("Delta Voltage Plot for",  input$dirName, data$sheet[row]), xlab="Cycle", ylab="Voltage (V)", ylim =c(0, 0.5))
          dev.off()
        }
        
        # Capacity Loss plotting
        if (is.element("Capacity Loss", input$gGraphs)) {
          png(paste(dirLocation(), "/",  input$dirName,"/", data$sheet[row],"/", data$name[row], data$sheet[row]," Capacity Loss Plot.png", sep =""))
          plot(cell_data$cycle, cell_data$lostCap, main=paste("Capacity Loss Plot for",  input$dirName, data$sheet[row]), xlab="Cycle", ylab= ylabel, ylim = c(mean(cell_data$lostCap) + (2* sd(cell_data$lostCap)), mean(cell_data$lostCap) - (1.5* sd(cell_data$lostCap))))
          abline(h=median(cell_data$lostCap), lty="dotted")
          dev.off()
        }
        
        if (is.element("dQdV Plots", input$gAnim)) {
          dQdVplot <- function(){
            tmp_data <- dQdVData[dQdVData$cell == row,]
            first_cycle <- dQdVData[dQdVData$cell == row & dQdVData$cycle == 2,]
            datalist <- split(tmp_data, tmp_data$cycle)
            lapply(datalist, function(plotData){
              p <- plot(plotData$voltage, plotData$dQdV, main=paste("dQdV Plot for",  input$dirName, data$sheet[row], "Cycle", plotData$cycle[[1]]), xlab="Voltage (V)", ylab= "dQdV (Ah/V)", 
                        xlim = c(min(tmp_data$voltage), max(tmp_data$voltage)), ylim = c(min(tmp_data$dQdV), max(tmp_data$dQdV))) + 
                   points(first_cycle$voltage, first_cycle$dQdV, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
            })
          }
          save_gif(dQdVplot(), paste(dirLocation(), input$dirName, data$sheet[row], "dQdV Animation.gif", sep = "/"), delay = 0.2)
        }
        
        if (is.element("Voltage Profiles", input$gAnim)) {
          vpPlot <- function(){
            first_cycle <- tmp_excel[tmp_excel$`Cycle_Index` == 2,]
            datalist <- split(tmp_excel, tmp_excel$`Cycle_Index`)
            lapply(datalist, function(plotData){
              p <- plot(plotData$CC, plotData$`Voltage(V)`, main=paste("Voltage Profile for",  input$dirName, data$sheet[row], "Cycle", plotData$`Cycle_Index`[[1]]), xlab=ylabel, ylab= "Voltage (V)", 
                        xlim = c(min(tmp_excel$CC), max(tmp_excel$CC)), ylim = c(min(tmp_excel$`Voltage(V)`), max(tmp_excel$`Voltage(V)`))) + 
                points(first_cycle$CC, first_cycle$`Voltage(V)`, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
            })
          }
          save_gif(vpPlot(), paste(dirLocation(), input$dirName, data$sheet[row], "Voltage Profile Animation.gif", sep = "/"), delay = 0.2)
        }
        
        # Save all data within the cell's directory
        write.csv(tmp_excel, file = paste(dirLocation(), "/",  input$dirName,"/", data$sheet[row],"/", data$sheet[row],".csv", sep =""))
        
        # Append summation data to the larger datasets to be worked with later
        final <- rbind(final, tmp_excel)
        numCycles <<- rbind(numCycles, data.frame(sheet=data$sheet[row], cycles=nrow(cell_data)))
        
        # Update progress bar
        progress$set(value = row, detail = paste("Finished", row," of", nrow(data)," cells."))
      }
      
      # ######
      # 
      # Code meant to be run on all data of all cells should be written here.
      # 
      # ######
      
      # With iterations complete, final calculations are being worked
      progress$set(detail ="Wrapping up...")
      
      # Get the last status of each cycle for each cell (namely capacity)
      DCap <- cycle_facts[c("cycle","DCap")] %>% group_by(cycle) %>% summarise_each(mean)
      CE <- cycle_facts[c("cycle","CE")] %>% group_by(cycle) %>% summarise_each(mean)
      capSEs <- cycle_facts[c("cycle","DCap")] %>% group_by(cycle) %>% summarise_each(se)
      ceSEs <- cycle_facts[c("cycle","CE")] %>% group_by(cycle) %>% summarise_each(se)
      stats <- cbind(cycle = capSEs$cycle, DCap = DCap$DCap, CE = CE$CE, capSE = capSEs$DCap, ceSE = ceSEs$CE)
      
      # Send all the data to a global variable to be used elsewhere
      total <<- final
      
      tryCatch({
        # Total dishcharge capacity plotting
        if (is.element("Total Discharge Capacity", input$gGraphs)) {
          png(paste(dirLocation(),"/", data$name[row],"Total Discharge Capacity Plot.png", sep =""))
          eol <- stats$`DCap`[[1]] * 0.8
          plot(stats$cycle, stats$DCap, type ="p", main=paste("Discharge Capacity for",  input$dirName), xlab=NA, ylab=paste("Discharge", ylabel), mai=c(1,1,1,1))
          arrows(stats$cycle, stats$DCap - stats$capSE, stats$cycle, stats$DCap + stats$capSE, length=0.05, angle=90, code=3)
          abline(h=eol, lty ="dotted")
          par(new = T)
          plot(stats$cycle, stats$CE, type ="p", axes=F, col ="red", ylab=NA, xlab="Cycle", ylim = c(0, 105))
          arrows(stats$cycle, stats$CE - stats$ceSE, stats$cycle, stats$CE + stats$ceSE, length=0.05, angle=90, code=3, col ="red")
          axis(side = 4, col ="red")
          mtext(side = 4, line = 2,"Coulombic Efficiency (%)")
          dev.off()
        }
      }, error = function(cond) {
        print(cond)
      })
      
      # Save total data and stats
      write.csv(stats, file = paste(dirLocation(), "/",  input$dirName,"/", basename(dirLocation())," Summary.csv", sep =""))
      write.csv(final, file = paste(dirLocation(), "/",  input$dirName,"/", basename(dirLocation())," Total.csv", sep =""))
      write.csv(dQdVData, file = paste(dirLocation(), "/",  input$dirName,"/", basename(dirLocation())," dQdV Data.csv", sep =""))
      write.csv(cycle_facts, file = paste(dirLocation(), "/",  input$dirName,"/", basename(dirLocation())," Cycle Facts.csv", sep =""))
      
      # If a histor directory does not exist, create it. Save all the data revelant to plotting to a RData file.
      if (!dir.exists(paste(dirLocation(), "history", sep = "/"))) {
        dir.create(paste(dirLocation(), "history", sep = "/"))
      }
      
      dirName <<- input$dirName
      
      save(dirLocation, dirName, data, dQdVData, total, cycle_facts, numCycles, file = paste(dirLocation(), "/history/", input$dirName, ".RData", sep = ""))
      
      # Modal for completed analysis
      shinyalert("Analysis Complete!", paste("All your data are now in ", dirLocation(), "/", input$dirName, sep = ""), 
                 type ="success",
                 )
      
      # Finish progress bar
      progress$set(value = nrow(data))
      
      # Re-enable all input fields, including graph builder
      enable("files")
      enable("lowV")
      enable("highV")
      enable("dirLocation()")
      enable("submit")
      enable("excelImport")
      enable("gGraphs")
      enable("peakFit")
      enable("area")
      enable("perActive")
      enable("capActive")
      enable("graphBuilder")
      
      # Close progress bar
      progress$close()
    }
    
    # Enable/Disable input field based on desired grph selection
    observeEvent(input$gGraphs, {
      disable("area")
      disable("perActive")
      disable("capActive")

      choices <- c("dQdV Graphs","Voltage Profiles","Voltage vs. Time","Discharge Capacity","Discharge Areal Capacity",
                  "Total Discharge Capacity","Average Voltage","Delta Voltage")

      if (is.element("Discharge Areal Capacity", input$gGraphs)) {
        enable("area")
      }
    })
    
    # ######
    # 
    # Graph Builder Processing
    # 
    # ######
    output$outputPlot <- renderPlot({
      
      tmp_data <<- data.frame()
      normalTime <<- data.frame()
      
      bounds <<- c(input$xMin, input$xMax, input$yMin, input$yMax)
      
      # Define function to normalize Voltage vs. Time plots
      normalizeTime <- function(x) {
        return(x - x[[1]])
      }
      
      # ######
      # 
      # Switch statements defining the bulk of the processing, depending on the desired graph
      # 
      # ######
      #if (input$perType =="Cycle Analysis") {
      sheetName <<- input$cells
      
      # Get the indicies in which the desired cells are in the data frame containing the number of cycles
      cellIndex <- match(input$cells, numCycles$sheet)
      
      switch(input$typeGraph,
            "dQdV Graphs" = {
               tmp_data <<- data.frame(x=dQdVData[dQdVData$cell %in% cellIndex,]$voltage, y=dQdVData[dQdVData$cell %in% cellIndex,]$dQdV, cycle=dQdVData[dQdVData$cell %in% cellIndex,]$cycle, cell=dQdVData[dQdVData$cell %in% cellIndex,]$cell)
               tmp_data <<- tmp_data[tmp_data$cycle == sort(as.numeric(input$renderCycles)),]
               
               titleLabel <<-"dQdV Plot"
               xlabel <<-"Voltage (V)"
               ylabel <<-"dQdV (mAh/V)"
             },
            "Voltage Profiles" = {
               tmp_data <<- data.frame(x=(-1) * total[total$Cell %in% cellIndex,]$CC, y=total[total$Cell %in% cellIndex,]$`Voltage(V)`, cycle=total[total$Cell %in% cellIndex,]$`Cycle_Index`, cell=total[total$Cell %in% cellIndex,]$Cell)
               tmp_data <<- tmp_data[tmp_data$cycle == sort(as.numeric(input$renderCycles)),]
               
               titleLabel <<-"Voltage Profile"
               if (sum(data$Mass) != 0) {
                 xlabel <-"Continuous Capacity (mAh/g)"
               } else {
                 xlabel <-"Continuous Capacity (Ah)"
               }
               ylabel <<-"Voltage (V)"
             },
            "Voltage vs. Time" = {
               tmp_data <<- data.frame(x=(total[total$Cell %in% cellIndex,]$`Test_Time(s)` / 60), y=total[total$Cell %in% cellIndex,]$`Voltage(V)`, cycle=total[total$Cell %in% cellIndex,]$`Cycle_Index`, cell=total[total$Cell %in% cellIndex,]$Cell)
               tmp_data <<- tmp_data[tmp_data$cycle %in% input$renderCycles,]
               
               x <- 0
               
               for (cell in cellIndex) {
                 normalTime <<- c(normalTime, t(aggregate(tmp_data[tmp_data$cell == cell,]$x, by=list(tmp_data[tmp_data$cell == cell,]$cycle), normalizeTime)[,2]))
               }
               
               tmp_data <<- data.frame(x=unlist(normalTime), y=tmp_data$y, cycle=tmp_data$cycle, cell=tmp_data$cell)
               tmp_data <<- tmp_data[tmp_data$y >= 0.01,]
               
               titleLabel <<-"Voltge vs. Time Plot"
               xlabel <<-"Time (min)"
               ylabel <<-"Voltage (V)"
             },
            "Charge Voltage" = {
              tmp_data <<- data.frame(x=cycle_facts[cycle_facts$cell %in% cellIndex,]$cycle, y=cycle_facts[cycle_facts$cell %in% cellIndex,]$chV, cycle=cycle_facts[cycle_facts$cell %in% cellIndex,]$cycle, cell = cycle_facts[cycle_facts$cell %in% cellIndex,]$cell)

              titleLabel <<- "Charge Voltage Plot "
              xlabel <<- "Cycle"
              ylabel <<- "Voltage (V)"
            },
            "Discharge Voltage" = {
              tmp_data <<- data.frame(x=cycle_facts[cycle_facts$cell %in% cellIndex,]$cycle, y=cycle_facts[cycle_facts$cell %in% cellIndex,]$dchV, cell=cycle_facts[cycle_facts$cell %in% cellIndex,]$cell, cycle=cycle_facts[cycle_facts$cell %in% cellIndex,]$cycle)
              
              titleLabel <<- "Discharge Voltage Plot "
              xlabel <<- "Cycle"
              ylabel <<- "Voltage (V)"
            },
            "Average Voltage" = {
              tmp_data <<- data.frame(x=cycle_facts[cycle_facts$cell %in% cellIndex,]$cycle, y=cycle_facts[cycle_facts$cell %in% cellIndex,]$avgV, cell=cycle_facts[cycle_facts$cell %in% cellIndex,]$cell, cycle=cycle_facts[cycle_facts$cell %in% cellIndex,]$cycle)
              
              titleLabel <<- "Average Voltage Plot "
              xlabel <<- "Cycle"
              ylabel <<- "Voltage (V)"
            },
            "Delta Voltage" = {
              tmp_data <<- data.frame(x=cycle_facts[cycle_facts$cell %in% cellIndex,]$cycle, y=cycle_facts[cycle_facts$cell %in% cellIndex,]$dV, cell=cycle_facts[cycle_facts$cell %in% cellIndex,]$cell, cycle=cycle_facts[cycle_facts$cell %in% cellIndex,]$cycle)

              titleLabel <<- "Delta Voltage Plot "
              xlabel <<- "Cycle"
              ylabel <<- "Voltage (V)"
            },
            "Discharge Capacity" = {
              tmp_data <<- data.frame(x=cycle_facts[cycle_facts$cell %in% cellIndex,]$cycle, y=cycle_facts[cycle_facts$cell %in% cellIndex,]$DCap, cell=cycle_facts[cycle_facts$cell %in% cellIndex,]$cell, cycle=cycle_facts[cycle_facts$cell %in% cellIndex,]$cycle)
              
              titleLabel <<- "Discharge Capacity Plot "
              xlabel <<- "Cycle"
              if (sum(data$Mass) != 0) {
                ylabel <<- "Discharge Capacity (mAh/g)"
              } else {
                ylabel <<- "Discharge Capacity (Ah)"
              }
            },
            "Charge Capacity" = {
              tmp_data <<- data.frame(x=cycle_facts[cycle_facts$cell %in% cellIndex,]$cycle, y=cycle_facts[cycle_facts$cell %in% cellIndex,]$CCap, cell=cycle_facts[cycle_facts$cell %in% cellIndex,]$cell, cycle=cycle_facts[cycle_facts$cell %in% cellIndex,]$cycle)
              
              titleLabel <<- "Charge Capacity Plot "
              xlabel <<- "Cycle"
              if (sum(data$Mass) != 0) {
                ylabel <<- "Charge Capacity (mAh/g)"
              } else {
                ylabel <<- "Charge Capacity (Ah)"
              }
            }
             
      )
      
      tmp_data <<- tmp_data[is.finite(tmp_data$x),]
      tmp_data <<- tmp_data[is.finite(tmp_data$y),]
      tmp_data <<- tmp_data[is.finite(tmp_data$cycle),]
      tmp_data <<- tmp_data[is.finite(tmp_data$cell),]
      
      tmp_data$color <<- sapply(tmp_data$cycle, function(x) {match(x, input$renderCycles, nomatch = 1)})
      tmp_data$symbol <<- sapply(tmp_data$cell, function(x) {match(x, cellIndex)})
      
      if (any(sapply(bounds, is.na))) {
        if (is.na(bounds[1])) bounds[1] <<- min(tmp_data$x)
        if (is.na(bounds[2])) bounds[2] <<- max(tmp_data$x)
        if (is.na(bounds[3])) bounds[3] <<- min(tmp_data$y)
        if (is.na(bounds[4])) bounds[4] <<- max(tmp_data$y)
      }
      
      tryCatch({
        if (input$plotStyle =="o" | input$plotStyle =="p") {
          par(mar=c(5.1, 6.1, 4.1, 2.1))
          plot(tmp_data$x, tmp_data$y, type = input$plotStyle, col = tmp_data$color, pch = tmp_data$symbol, main=titleLabel, xlim = c(bounds[1], bounds[2]), ylim = c(bounds[3], bounds[4]),  xlab=xlabel, ylab=ylabel, cex = input$pointSize, cex.axis = input$textSize, cex.lab = input$textSize, cex.main = input$textSize)
          legend("bottomright", legend = c(sort(as.numeric(input$renderCycles)), input$cells), col = c(unique(tmp_data$color), rep("black", length(input$cells))), pch = c(rep(19, length(unique(tmp_data$color))), 1:length(input$cells)), title ="Cycle", ncol=2)
        } else if (input$plotStyle =="l") {
          newLine <- subset(tmp_data, tmp_data$color == 1 & tmp_data$symbol == 1)
          plot(newLine$x, newLine$y, type ="l", col = newLine$color, lty = newLine$symbol, main=titleLabel, xlim = c(bounds[1], bounds[2]), ylim = c(bounds[3], bounds[4]),  xlab=xlabel, ylab=ylabel, lwd = input$pointSize, cex.axis = input$textSize, cex.lab = input$textSize, cex.main = input$textSize)
          
          for (i in 1:length(input$renderCycles)) {
            for (n in 1:length(cellIndex)) {
              newLine <- subset(tmp_data, tmp_data$color == i & tmp_data$symbol == n)
              lines(newLine$x, newLine$y, col = newLine$color, lty = newLine$symbol, lwd = input$pointSize)
            }
          }
          
          legend("bottomright", legend = c(sort(as.numeric(input$renderCycles)), input$cells), col = c(unique(tmp_data$color), rep("black", length(input$cells))), lty = c(rep(19, length(unique(tmp_data$color))), 1:length(input$cells)), title ="Cycle", ncol=2)
        }
      },
      error=function(cond) {
        if (length(input$cells) != 0) {
          text(0.5, 0.5, labels ="This graph requires you to select a cycle!", cex = 2)
        } else {
          text(0.5, 0.5, labels ="You don messed up A-aron!\n (no data to plot)", cex = 2)
        }
        print(cond)
        return(NA)
      })
    })
    
    # Method for handling changes in cell selection
    observeEvent(input$cells, {
      tmp_cycles <<- input$renderCycles
      updateSelectInput(session,"renderCycles", choices = 1:numCycles[numCycles$sheet == input$cells,]$cycles, selected = tmp_cycles)
    })
    
    # Error handling for graphBuilder and then showing modal
    observeEvent(input$graphBuilder, {
      if(dim(numCycles)[1] == 0 | dim(dQdVData)[1] == 0 | dim(cycle_facts)[1] == 0 | dim(total) == 0) {
        shinyalert("No Data!","Please run the analysis first or load a previous environment.","error")
      } else {
        updateCheckboxGroupInput(session,"cells", choices = data$sheet)
        showModal(graphbuilder)
      }
    })
    
    observeEvent(input$plot_click, {
      output$hoverCoordx <- renderText({input$plot_click$x})
      output$hoverCoordy <- renderText({input$plot_click$y})
    })
    
    # Method for saving graph generated by graphBuilder
    observeEvent(input$saveGraph, {
      png(paste(input$fileName,".png"))
      
      if (input$plotStyle =="o" | input$plotStyle =="p") {
        plot(tmp_data$x, tmp_data$y, type = input$plotStyle, col = tmp_data$color, pch = tmp_data$symbol, main=titleLabel, xlim = c(bounds[1], bounds[2]), ylim = c(bounds[3], bounds[4]),  xlab=xlabel, ylab=ylabel, cex = input$pointSize, cex.axis = input$textSize, cex.lab = input$textSize, cex.main = input$textSize)
        legend("bottomright", legend = c(sort(as.numeric(input$renderCycles)), input$cells), col = c(unique(tmp_data$color), rep("black", length(input$cells))), pch = c(rep(19, length(unique(tmp_data$color))), 1:length(input$cells)), title ="Cycle", ncol=2)
      } else if (input$plotStyle =="l") {
        newLine <- subset(tmp_data, tmp_data$color == 1 & tmp_data$symbol == 1)
        plot(newLine$x, newLine$y, type ="l", col = newLine$color, lty = newLine$symbol, main=titleLabel, xlim = c(bounds[1], bounds[2]), ylim = c(bounds[3], bounds[4]),  xlab=xlabel, ylab=ylabel, lwd = input$pointSize, cex.axis = input$textSize, cex.lab = input$textSize, cex.main = input$textSize)
        
        for (i in 1:length(input$renderCycles)) {
          for (n in 1:length(cellIndex)) {
            newLine <- subset(tmp_data, tmp_data$color == i & tmp_data$symbol == n)
            lines(newLine$x, newLine$y, col = newLine$color, lty = newLine$symbol, lwd = input$pointSize)
          }
        }
        
        legend("bottomright", legend = c(sort(as.numeric(input$renderCycles)), input$cells), col = c(unique(tmp_data$color), rep("black", length(input$cells))), lty = c(rep(19, length(unique(tmp_data$color))), 1:length(input$cells)), title ="Cycle", ncol=2)
      }
      
      dev.off()
      
      shinyalert("Success!", paste("Plot saved in working directory:\n", getwd()),"success")
    })
  }
}
shinyApp(ui, server)