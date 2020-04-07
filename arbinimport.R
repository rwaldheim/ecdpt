require(readxl)
require(dplyr)
require(shiny)
require(DT)
require(shinyjs)
require(shinyalert)
require(pracma)
require(purrr)
require(zoo)
require(plotrix)

if (interactive()) {
  
  data <- reactiveValues(data = data.frame())
  final <- data.frame()
  lowV <- reactiveVal(0)
  highV <- reactiveVal(0)
  dirLocation <- reactiveVal("")
  numCycles <- data.frame()
  dQdVData <- data.frame()
  total <- data.frame()
  cycle_facts <- data.frame()
  tmp_data <- data.frame()
  data_pull <- data.frame()
  tmp_cycles <- vector()
  titleLabel <- ""
  xlabel <- ""
  ylabel <- ""
  graphColor <- function() {}

  ui <- fluidPage(
    useShinyjs(),
    useShinyalert(),
    
    fluidRow(headerPanel("Arbin Import")),
    
    column(4,
      fluidRow(
        fileInput("rerun", "Optional: Import Previous R Environment", multiple = FALSE, accept = ".RData"),
        actionButton("load", "Load"),
        style = "border: 1px dashed black; margin: 5%; padding: 5%"
      ),
      
      fluidRow(
        fileInput("files", "Select All files of A single Set", multiple = TRUE),
        numericInput("lowV", "Lower Voltage (V)", 2.8, min = 0, max = 5),
        numericInput("highV", "Upper Voltage (V)", 4.25, min = 0, max = 5),
        numericInput("area", "Limiting Electrode Area (cm^2)", 2.74, min = 0),
        numericInput( "perActive","Active Loading of Limiting Electrode (wt%)", 96, min = 0, max = 100),
        numericInput( "capActive","Capacity of Limiting Active Material (mAh/g)", 155, min = 0, max = 100),
        style = "border: 1px solid black; padding: 5%; margin:5%"
      ),
    ),
    
    column(4, align = "left", 
           fluidRow(
             checkboxGroupInput("gGraphs", "Choose Graphs to Generate:", choices = c("dQdV Graphs", "Voltage Profiles", "Voltage vs. Time", "Discharge Capacity", "Discharge Areal Capacity",
                                                                                     "Total Discharge Capacity", "Average Voltage", "Delta Voltage"), inline = FALSE),
             radioButtons("peakFit", "Do Peak Fitting on  dQdV Graphs? (BETA)", choices = c("No" = "noGenGraphs", "Yes" = "fit"), inline = TRUE),
             style = "margin: 5%; border: 1px solid black; padding: 5%"
           ),
    ),
    
    column(4, align = "center",
      fluidRow(
        strong("Optional: Running Analysis Without Masses Will Have Raw Capacities Used (Ah)"),
        actionButton("excelImport", "Import Masses from Excel", width = '80%', class = "btn-secondary", style = "padding:5%; height:50px; margin:5%; font-size:100%"),
        style = "border: 1px dashed black; padding: 5%; margin:5%"
      ),
      
      fluidRow(
        textInput("dirLocation", "Enter Directory Name"),
        actionButton("submit", "Begin Analysis", class = 'btn-success', style = "padding:4px; width:80%; height:100px; margin:5%, font-size:100%"),
        style = "border: 4px double black; padding: 5%; margin:5%"
      ),
      
      fluidRow(
        strong("Customize Graphs Once Data is Available"),
        disabled(actionButton("graphBuilder", "Launch Graph Builder", width = '80%', class = "btn-primary", style = "padding: 5%; height:50px; margin:5%; font-size:100%")),
        style = "border: 1px solid black; padding: 5%; margin:5%"
      ),
    ),
    
    fluidRow(
      dataTableOutput("channels")
    )
  )
  
  server <- function(input, output, session) {
    options(shiny.maxRequestSize=30*1024^2)
    
    excelModal <- modalDialog("Copy masses from Excel now.", footer = actionButton("excelMasses", "Import"))
    
    graphbuilder <- modalDialog({
      fluidPage(
        useShinyjs(),
        useShinyalert(),
        
        tags$head(tags$style(".modal-dialog{width:80%}")),
        tags$head(tags$style(".modal-body{ min-height:1000px}")),
        
        sidebarLayout(
        
          sidebarPanel(
             fluidRow(
               radioButtons("typeGraph", "Graph Type:", choices = c("dQdV Graphs", "Voltage Profiles", "Voltage vs. Time"), inline = FALSE)
             ),
             
             fluidRow(
               radioButtons("plotStyle", "Plot Style:", choiceNames = c("Point", "Line", "Both"), choiceValues = c("p", "l", "o"),  inline = TRUE)
             ),
             
             fluidRow(
               radioButtons("cells", "Cell to Analyze:", choices = 1, inline = FALSE)
             ),
             
             fluidRow(
               selectInput("renderCycles", "Cycles of Interest:", choices = 1, multiple = TRUE)
             ),
             
             fluidRow(
               textInput("fileName", "Name of graph file:"),
               actionButton("saveGraph", "Save Graph", width = '100%', class = 'btn-primary'),
               style = "border: 4px double red;"
             ),
          ),
          
          mainPanel(
                 plotOutput("outputPlot", height = "800px")
          )
        )
      )
    }, size = "l", title = "Post-Processing Graph Builder")
    
    observeEvent(input$load, {
      load(paste("history/", input$rerun[[1]], sep = ""))
      
      data <<- filter(data, grepl('Channel', sheet))
      numCycles <<- numCycles
      dQdVData <<- dQdVData
      total <<- total
      cycle_facts <<- cycle_facts
      
      output$channels <- renderDataTable(data, editable = TRUE, options=list(columnDefs = list(list(visible=FALSE, targets=c(4)))), 
                                         colnames = c("File", "Sheet", "Mass (g)", "Filepath", "Limiting Electrode Area (cm^2)", "Active Material Loading (wt%)", 
                                                      "Active Mateial Capacity (mAh/g)", "Lower Voltage (V)", "Upper Voltage (V)"))
      
      enable("graphBuilder")
    })
    
    output$channels <- renderDataTable({
      files <- input$files
      
      if (is.null(files)) {
        return(NULL)
      }
      
      file_sheet <- data.frame()
      for (i in 1:nrow(files)) {
        sheets <- excel_sheets(files[i, 4])
        file_sheet <- rbind(file_sheet, data.frame(name = rep(files[["name"]][i], length(sheets)), "sheet" = sheets, "Mass" = rep(0, length(sheets)),
                                                   datapath = rep(files[["datapath"]][i], length(sheets)), area = rep(input$area, length(sheets)),
                                                   perActive = rep(input$perActive, length(sheets)), capActive = rep(input$capActive, length(sheets)),
                                                   lowV = rep(input$lowV, length(sheets)), highV = rep(input$highV, length(sheets))))
      }
      
      data <<- filter(file_sheet, grepl('Channel', sheet))
      
      data

    }, editable = TRUE, options=list(columnDefs = list(list(visible=FALSE, targets=c(4)))), 
    colnames = c("File", "Sheet", "Mass (g)", "Filepath", "Limiting Electrode Area (cm^2)", "Active Material Loading (wt%)", 
                 "Active Mateial Capacity (mAh/g)", "Lower Voltage (V)", "Upper Voltage (V)"))
    
    observeEvent(input$excelImport, {
      showModal(excelModal)
    })
    
    observeEvent(input$excelMasses, {
      masses <- read.table("clipboard",sep="\t")
      names(masses)[names(masses) == "V1"] <- "Mass"
      data$Mass <<- masses
      
      proxy = dataTableProxy("channels")
      replaceData(proxy, data)
      
      renderDataTable(data)
      
      removeModal()
    })
    
    observeEvent(input$submit, {
      
      if (!grepl("OneDrive", getwd())) {
        shinyReturn <- ""
        shinyalert("Uh Oh!", "You do not appear to be in a shared directory!", type = "error",
             showConfirmButton = TRUE, confirmButtonText = "Ignore", showCancelButton = TRUE,
             cancelButtonText = "Abort",
             callbackR = function(x) {
               if (x) {
                 runscript()
               }
             })
      } else {
        runscript()
      }
    })
    
    runscript <- function() {
      graphics.off()
      
      numCycles <<- data.frame()
      dQdVData <<- data.frame()
      total <<- data.frame()
      cycle_facts <<- data.frame()
      
      disable("files")
      disable("lowV")
      disable("highV")
      disable("dirLocation")
      disable("submit")
      disable("excelImport")
      disable("gGraphs")
      disable("peakFit")
      disable("area")
      disable("perActive")
      disable("capActive")

      dir.create(input$dirLocation)
      
      se <- function(x) {sd(x) / sqrt(length(x))}
      
      argmax <- function(cycle, w, span) {
          n <- length(cycle$y)
          y.smooth <- loess(cycle$y ~ cycle$x, span=span)$fitted
          y.max <- rollapply(zoo(y.smooth), 2*w+1, max, align="center")
          x.max <- rollapply(zoo(cycle$x), 2*w+1, median, align="center")
          delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
          i.max <- which(delta <= 0) + w
          list(x=cycle$x[i.max], i=i.max, y.hat=y.smooth)
      }
      
      loading <- TRUE
      
      withProgress(message = "Computing...", {
      
        for (row in 1:nrow(data)) {
          tmp_excel <- read_excel(toString(data$datapath[row]), toString(data$sheet[row]))
          
          dir.create(paste(input$dirLocation, data$sheet[row], sep = "/"))
          if (is.element("dQdV Graphs", input$gGraphs)) dir.create(paste(input$dirLocation, data$sheet[row], "dQdV Plots", sep = "/"))
          if (is.element("Voltage Profiles", input$gGraphs)) dir.create(paste(input$dirLocation, data$sheet[row], "Voltage Profiles", sep = "/"))
          if (is.element("Voltage vs. Time", input$gGraphs)) dir.create(paste(input$dirLocation, data$sheet[row], "Voltage v Time", sep = "/"))
          if (input$peakFit == "fit") dir.create(paste(input$dirLocation, data$sheet[row], "dQdV Peak Fitting", sep = "/"))
          
          if (sum(data$Mass) != 0) {
            ylabel <- "Discharge Capacity (mAh/g)"
            
            tmp_excel$Q.d <- as.numeric(tmp_excel$`Discharge_Capacity(Ah)` * (1000 / data$Mass[[1]][row]))
            tmp_excel$Q.c <- as.numeric(tmp_excel$`Charge_Capacity(Ah)`* (1000 / data$Mass[[1]][row]))
            
            tmp_excel$CC <- tmp_excel$Q.d - tmp_excel$Q.c
            tmp_excel$CE <- (tmp_excel$Q.d / tmp_excel$Q.c) * 100
          } else {
            ylabel <- "Discharge Capacity (Ah)"
            
            tmp_excel$CC <- tmp_excel$`Discharge_Capacity(Ah)` - tmp_excel$`Charge_Capacity(Ah)`
            tmp_excel$CE <- (tmp_excel$`Discharge_Capacity(Ah)` / tmp_excel$`Charge_Capacity(Ah)`) * 100
          }
          tmp_excel$Cell <- row
          tmp_excel$CE[is.infinite(tmp_excel$CE)|is.nan(tmp_excel$CE)|tmp_excel$CE > 200] <- 0;
          
          cycles <- split(tmp_excel, tmp_excel$Cycle_Index)
          prev_c <- 0
          ch_dch <- FALSE
          prev <- TRUE
          dchV <- 0
          chV <- 0
          i <- 1
          for (cycle in cycles) {
            steps <- split(cycle, cycle$Step_Index)
            for (step in steps) {
              if (abs(tail(step$'Voltage(V)',1) - step$'Voltage(V)'[[1]]) > 0.5) {
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
            
            
            
            if (ch_dch) {
              if (is.element("dQdV Graphs", input$gGraphs)) {
                png(paste(input$dirLocation, "/", data$sheet[row], "/", "dQdV Plots/", data$name[row], data$sheet[row], "Cycle ", toString(i)," dQdV Plot.png", sep = ""))
                plot(dQdVData[dQdVData$cycle == i,]$voltage, dQdVData[dQdVData$cycle == i,]$dQdV, main=paste("dQdV Plot for ",  input$dirLocation, data$sheet[row], "Cycle ", toString(i)), xlab="Voltage (V)", ylab="dQdV (mAh/V)")
                dev.off()
              }
                
              if (is.element("Voltage Profiles", input$gGraphs)) {
                png(paste(input$dirLocation, "/", data$sheet[row], "/", "Voltage Profiles/", data$name[row], data$sheet[row], "Cycle ", toString(i)," Voltage Profile Plot.png", sep = ""))
                if (sum(data$Mass) != 0) {
                  plot(tmp_excel[tmp_excel$`Cycle_Index` == i,]$`Q.d`, tmp_excel[tmp_excel$`Cycle_Index` == i,]$`Voltage(V)`, type="l", main=paste("Voltage Profile for ",  input$dirLocation, data$sheet[row]), xlab= ylabel, ylab="Voltage (V)")
                } else {
                  plot(tmp_excel[tmp_excel$`Cycle_Index` == i,]$`Discharge_Capacity(Ah)`, tmp_excel[tmp_excel$`Cycle_Index` == i,]$`Voltage(V)`, type="l", main=paste("Voltage Profile for ",  input$dirLocation, data$sheet[row]), xlab=ylabel, ylab="Voltage (V)")
                }
                dev.off()
              }
              
              if (input$peakFit == "fit") {
                w = 20
                span = 0.05
                
                tryCatch({
                  png(paste(input$dirLocation, "/", data$sheet[row], "/", "dQdV Peak Fitting/", data$name[row], data$sheet[row], "Cycle ", toString(i)," dQdV Plot.png", sep = ""))
                  plot(dQdVData[dQdVData$cycle == i,]$voltage, dQdVData[dQdVData$cycle == i,]$dQdV, main=paste("dQdV Plot for ",  input$dirLocation, data$sheet[row], "Cycle ", toString(i)), xlab="Voltage (V)", ylab="dQdV (mAh/V)")
                  chargeCycle <- data.frame(x=dQdVData[dQdVData$cycle == i & dQdVData$c_d == 0,]$voltage, y=dQdVData[dQdVData$cycle == i & dQdVData$c_d == 0,]$dQdV)
                  dischargeCycle <- data.frame(x=dQdVData[dQdVData$cycle == i & dQdVData$c_d == 1,]$voltage, y=dQdVData[dQdVData$cycle == i & dQdVData$c_d == 1,]$dQdV)
                  cPeaks <- argmax(chargeCycle, w, span)
                  dPeaks <- argmax(abs(dischargeCycle), w, span)
                  abline(v=c(cPeaks$x, dPeaks$x))
                  text(c(cPeaks$x, dPeaks$x) + 0.01, rep(0,length(c(cPeaks$x, dPeaks$x))), labels = round(c(cPeaks$x, dPeaks$x),2), srt = 90)
                  dev.off()
                }, 
                error=function(cond) {
                  graphics.off()
                  return(NA)
                }
                )
              }
            }
            
            if (is.element("Voltage vs. Time", input$gGraphs)) {
              png(paste(input$dirLocation, "/", data$sheet[row], "/", "Voltage v Time/", data$name[row], data$sheet[row], "Cycle ", toString(i)," Voltage Profile Plot.png", sep = ""))
              plot((tmp_excel[tmp_excel$`Cycle_Index` == i,]$`Test_Time(s)` - tmp_excel[tmp_excel$`Cycle_Index` == i,]$`Test_Time(s)`[[1]]) / 60, tmp_excel[tmp_excel$`Cycle_Index` == i,]$`Voltage(V)`, type="l", main=paste("Voltage vs. Time for ",  input$dirLocation, data$sheet[row]), xlab="Time (min)", ylab="Voltage (V)")
              dev.off()
            }
            
            avgV <- (dchV + chV) / 2
            cycle_facts <<- rbind(cycle_facts, data.frame(cycle=i, cell=row, chV=chV, dchV=dchV, avgV=avgV, dV=chV-dchV))
            i <- i + 1
            ch_dch <- FALSE
          }
          
          if (sum(data$Mass) != 0) {
            meanDCap <- aggregate(tmp_excel$`Q.d`, by=list(tmp_excel$`Cycle_Index`), last)
            meanCE <- aggregate(tmp_excel$CE, by=list(tmp_excel$`Cycle_Index`), last)
          } else {
            meanDCap <- aggregate(tmp_excel$`Discharge_Capacity(Ah)`, by=list(tmp_excel$`Cycle_Index`), last)
            meanCE <- aggregate(tmp_excel$CE, by=list(tmp_excel$`Cycle_Index`), last)
          }
          
          if (is.element("Discharge Capacity", input$gGraphs)) {
            png(paste(input$dirLocation, "/", data$sheet[row], "/", data$name[row], data$sheet[row]," Discharge Capacity Plot.png", sep = ""))
            eol <- meanDCap[1,2] * 0.8
            twoord.plot(meanDCap[,1], meanDCap[,2], meanCE[,1], meanCE[,2], type="p", main=paste("Discharge Capacity for ",  input$dirLocation, data$sheet[row]), xlab="Cycle",ylab = ylabel)
            abline(h=eol, lty = "dotted")
            dev.off()
          }
           
          if (is.element("Discharge Areal Capacity", input$gGraphs)) {
            png(paste(input$dirLocation, "/", data$sheet[row], "/", data$name[row], data$sheet[row]," Discharge Areal Capacity Plot.png", sep = ""))
            eol <- ((meanDCap[1,2] * 1000) / data$area[row]) * 0.8
            plot(meanDCap[,1], ((meanDCap[,2] * 1000) / data$area[row]), main=paste("Discharge Capacity for ",  input$dirLocation, data$sheet[row]), xlab="Cycle", ylab="Discharge Capacity (mAh/cm^2)")
            abline(h=eol, lty = "dotted")
            dev.off()
          }

         if (is.element("Average Voltage", input$gGraphs)) {
            png(paste(input$dirLocation, "/", data$sheet[row], "/", data$name[row], data$sheet[row]," Average Voltage Plot.png", sep = ""))
            plot(cycle_facts$cycle, cycle_facts$chV, col="blue", main=paste("Average Voltage Plot for ",  input$dirLocation, data$sheet[row]), xlab="Cycle", ylab="Voltage (V)", ylim=c(min(cycle_facts[,2:4]), max(cycle_facts[,2:4])))
            points(cycle_facts$cycle, cycle_facts$dchV, col="red", main=paste("Average Voltage Plot for ",  input$dirLocation, data$sheet[row]), xlab="Cycle", ylab="Voltage (V)")
            points(cycle_facts$cycle, cycle_facts$avgV, col="black", main=paste("Average Voltage Plot for ",  input$dirLocation, data$sheet[row]), xlab="Cycle", ylab="Voltage (V)")
            legend("top", c("Charge Voltage", "Discharge Voltage", "Average Voltage"), col=c("blue", "red", "black"), pch=19)
            dev.off()
         }
          
         if (is.element("Delta Voltage", input$gGraphs)) {
            png(paste(input$dirLocation, "/", data$sheet[row], "/", data$name[row], data$sheet[row]," Delta Voltage Plot.png", sep = ""))
            plot(cycle_facts$cycle, cycle_facts$dV, main=paste("Delta Voltage Plot for ",  input$dirLocation, data$sheet[row]), xlab="Cycle", ylab="Voltage (V)", ylim =c(0, 0.5))
            dev.off()
         }
          
          write.csv(tmp_excel, file = paste(input$dirLocation, "/", data$sheet[row], "/", data$sheet[row], ".csv", sep = ""))
          write.csv(dQdVData, file = paste(input$dirLocation, "/", data$sheet[row], "/", data$sheet[row], " dQdV Data.csv", sep = ""))
          write.csv(cycle_facts, file = paste(input$dirLocation, "/", data$sheet[row], "/", data$sheet[row], " Charge-Discharge Voltages.csv", sep = ""))
          
          final <- rbind(final, tmp_excel)
          numCycles <<- rbind(numCycles, data.frame(sheet=data$sheet[row], cycles=nrow(cycle_facts[cycle_facts$cell == row,])))
          
          incProgress(row/(nrow(data)+ 1))
        }
        
        stats <- final %>% group_by(Cell, Cycle_Index) %>% summarise_each(last)
        
        total <<- final
        
        if (sum(data$Mass) != 0) {
          totalDCap <- aggregate(stats$Q.d, list(stats$`Cycle_Index`), mean)
          totalDCapSE <- aggregate(stats$Q.d, list(stats$`Cycle_Index`), se)
        } else {
          totalDCap <- aggregate(stats$`Discharge_Capacity(Ah)`, list(stats$`Cycle_Index`), mean)
          totalDCapSE <- aggregate(stats$`Discharge_Capacity(Ah)`, list(stats$`Cycle_Index`), se)
        }
        totalCE <- aggregate(stats$CE, list(stats$`Cycle_Index`), mean)
        totalCESE <- aggregate(stats$CE, list(stats$`Cycle_Index`), se)

        if (is.element("Total Discharge Capacity", input$gGraphs)) {
          png(paste(getwd(),"/", input$dirLocation, "/", data$name[row], "Total Discharge Capacity Plot.png", sep = ""))
          eol <- totalDCap[1,2] * 0.8
          plot(totalDCap[,1], totalDCap[,2], type = "p", main=paste("Discharge Capacity for ",  input$dirLocation), xlab=NA, ylab=ylabel, mai=c(1,1,1,1))
          arrows(totalDCap[,1], totalDCap[,2] - totalDCapSE[,2], totalDCap[,1], totalDCap[,2] + totalDCapSE[,2], length=0.05, angle=90, code=3)
          par(new = T)
          plot(totalDCap[,1], totalCE[,2], type = "p", axes=F, col = "red", ylab=NA, xlab="Cycle")
          axis(side = 4)
          mtext(side = 4, line = 2, "Coulombic Efficiency (%)")
          abline(h=eol, lty = "dotted")
          dev.off()
        }
        
        write.csv(stats, file = paste(getwd(),"/", input$dirLocation, "/", data$name[row], " Summary.csv", sep = ""))
        write.csv(final, file = paste(getwd(),"/", input$dirLocation, "/", data$name[row], " Total.csv", sep = ""))
        
        if (!dir.exists("history/")) {
          dir.create("history/")
        } 
        save(data, dQdVData, total, cycle_facts, numCycles, file = paste("history/", input$dirLocation, ".RData"))
        
        shinyalert("Analysis Complete!", paste("All your data are now in ", input$dirLocation), 
                   type = "success",
                   showCancelButton = TRUE,
                   cancelButtonText = "Exit",
                   showConfirmButton = TRUE,
                   confirmButtonText = "Graph Builder",
                   callbackR = function(x) {
                     if (x) {
                       updateRadioButtons(session, "cells", choices = data$sheet)
                       showModal(graphbuilder)
                     }
                   })
        
        incProgress((nrow(data)+ 1)/(nrow(data)+ 1))
        
        enable("files")
        enable("lowV")
        enable("highV")
        enable("dirLocation")
        enable("submit")
        enable("excelImport")
        enable("gGraphs")
        enable("peakFit")
        enable("area")
        enable("perActive")
        enable("capActive")
        enable("graphBuilder")
      })
    }
    
    output$outputPlot <- renderPlot({
      sheetName <- input$cells
      
      normalizeTime <- function(x) {
        return(x - x[[1]])
      }
      
      cellIndex <- match(input$cells, numCycles$sheet)
      
      switch(input$typeGraph,
             "dQdV Graphs" = {
               tmp_data <<- data.frame(x=dQdVData[dQdVData$cell == cellIndex,]$voltage, y=dQdVData[dQdVData$cell == cellIndex,]$dQdV, cycle=dQdVData[dQdVData$cell == cellIndex,]$cycle)
               
               titleLabel <<- "dQdV Plot "
               xlabel <<- "Voltage (V)"
               ylabel <<- "dQdV (mAh/V)"
             },
             "Voltage Profiles" = {
               if (sum(data$Mass) != 0) {
                 tmp_data <<- data.frame(x=total[total$Cell == cellIndex,]$Q.d, y=total[total$Cell == cellIndex,]$`Voltage(V)`, cycle=total[total$Cell == cellIndex,]$`Cycle_Index`)
                 tmp_data <<- rbind(tmp_data, data.frame(x=total[total$Cell == cellIndex,]$Q.c, y=total[total$Cell == cellIndex,]$`Voltage(V)`, cycle=total[total$Cell == cellIndex,]$`Cycle_Index`))
                 xlabel <<- "Capacity (mAh/g)"
               } else {
                 tmp_data <<- data.frame(x=total[total$Cell == cellIndex,]$`Discharge_Capacity(Ah)`, y=total[total$Cell == cellIndex,]$`Voltage(V)`, cycle=total[total$Cell == cellIndex,]$`Cycle_Index`)
                 tmp_data <<- rbind(tmp_data, data.frame(x=total[total$Cell == cellIndex,]$`Charge_Capacity(Ah)`, y=total[total$Cell == cellIndex,]$`Voltage(V)`, cycle=total[total$Cell == cellIndex,]$`Cycle_Index`))
                 xlabel <<- "Capacity (Ah)"
               }
               
               titleLabel <<- "Voltage Profile "
               ylabel <<- "Voltage (V)"
             },
             "Voltage vs. Time" = {
               tmp_data <<- data.frame()
               data_pull <<- data.frame(x=(total[total$Cell == cellIndex,]$`Test_Time(s)` / 60), y=total[total$Cell == cellIndex,]$`Voltage(V)`, cycle=total[total$Cell == cellIndex,]$`Cycle_Index`)
               
               normalTime <<- aggregate(data_pull$x, by=list(data_pull$cycle), normalizeTime)
               for (cycle in 1:nrow(normalTime)) {
                 tmp_data <<- rbind(tmp_data, data.frame(x=normalTime$x[[cycle]],y=data_pull[data_pull$cycle == cycle,]$y, cycle=rep(cycle, length(normalTime$x[[cycle]]))))
               }
               
               tmp_data <<- tmp_data[tmp_data$y >= 0.01,]
               
               titleLabel <<- "Voltge vs. Time Plot "
               xlabel <<- "Time (min)"
               ylabel <<- "Voltage (V)"
             }, 
      )
      
      tmp_data <<- tmp_data[is.finite(tmp_data$x),]
      tmp_data <<- tmp_data[is.finite(tmp_data$y),]
      tmp_data <<- tmp_data[is.finite(tmp_data$cycle),]
      
      tryCatch({
        tmp_data <<- tmp_data[tmp_data$cycle == sort(as.numeric(input$renderCycles)),]
        
        graphColors <<- colorRamp(c("red", "blue"))
        tmp_data$color <<- sapply(tmp_data$cycle, function(x) {match(x, input$renderCycles)})
        
        if (input$plotStyle == "o" | input$plotStyle == "p") {
          plot(tmp_data$x, tmp_data$y, type = input$plotStyle, col = tmp_data$color, main=paste(titleLabel, "for ",  input$dirLocation, sheetName), xlim = c(min(tmp_data$x), max(tmp_data$x)), ylim = c(min(tmp_data$y), max(tmp_data$y)),  xlab=xlabel, ylab=ylabel)
          legend("bottomright", legend = sort(as.numeric(input$renderCycles)), col = unique(tmp_data$color), pch = 19, title = "Cycle")
        } else if (input$plotStyle == "l") {
          newLine <- subset(tmp_data, tmp_data$color == 1)
          plot(newLine$x, newLine$y, type = "l", col = newLine$color, main=paste(titleLabel, "for ",  input$dirLocation, sheetName), xlim = c(min(tmp_data$x), max(tmp_data$x)), ylim = c(min(tmp_data$y), max(tmp_data$y)),  xlab=xlabel, ylab=ylabel)
          
          for (i in 2:length(input$renderCycles)) {
            newLine <- subset(tmp_data, tmp_data$color == i)
            lines(newLine$x, newLine$y, col = newLine$color, main=paste(titleLabel, "for ",  input$dirLocation, sheetName), xlim = c(min(tmp_data$x), max(tmp_data$x)), ylim = c(min(tmp_data$y), max(tmp_data$y)),  xlab=xlabel, ylab=ylabel)
          }
          
          legend("bottomright", legend = sort(as.numeric(input$renderCycles)), col = unique(tmp_data$color), lty = 1, title = "Cycle")
        }
      },
      error=function(cond) {
        text(0.5, 0.5, labels = "You don messed up A-aron!\n (no data to plot)")
        print(cond)
        return(NA)
      })
    })
    
    observeEvent(input$cells, {
      tmp_cycles <<- input$renderCycles
      updateSelectInput(session, "renderCycles", choices = 1:numCycles[numCycles$sheet == input$cells,]$cycles, selected = tmp_cycles)
    })
    
    observeEvent(input$graphBuilder, {
      if(dim(numCycles)[1] == 0 | dim(dQdVData)[1] == 0 | dim(cycle_facts)[1] == 0 | dim(total) == 0) {
        shinyalert("No Data!", "Please run the analysis first or load a previous environment.", "error")
      } else {
        updateRadioButtons(session, "cells", choices = data$sheet)
        showModal(graphbuilder)
      }
    })
    
    observeEvent(input$saveGraph, {
      png(paste(input$fileName, ".png"))
      if (input$plotStyle == "o") {
        plot(tmp_data$x, tmp_data$y, type = "p", col = tmp_data$color, main=paste(titleLabel, "for ",  input$dirLocation, input$cells), xlim = c(min(tmp_data$x), max(tmp_data$x)), ylim = c(min(tmp_data$y), max(tmp_data$y)),  xlab=xlabel, ylab=ylabel)
        legend("bottomright", legend = sort(as.numeric(input$renderCycles)), col = unique(tmp_data$color), pch = 19, title = "Cycle")
      } else if (input$plotStyle == "l") {
        newLine <- subset(tmp_data, tmp_data$color == 1)
        plot(newLine$x, newLine$y, type = "l", col = newLine$color, main=paste(titleLabel, "for ",  input$dirLocation, input$cells), xlim = c(min(tmp_data$x), max(tmp_data$x)), ylim = c(min(tmp_data$y), max(tmp_data$y)),  xlab=xlabel, ylab=ylabel)
        
        for (i in 2:length(input$renderCycles)) {
          newLine <- subset(tmp_data, tmp_data$color == i)
          lines(newLine$x, newLine$y, col = newLine$color, main=paste(titleLabel, "for ",  input$dirLocation, input$cells), xlim = c(min(tmp_data$x), max(tmp_data$x)), ylim = c(min(tmp_data$y), max(tmp_data$y)),  xlab=xlabel, ylab=ylabel)
        }
        
        legend("bottomright", legend = sort(as.numeric(input$renderCycles)), col = unique(tmp_data$color), lty = 1, title = "Cycle")
      }
      dev.off()
      
      shinyalert("Success!", paste("Plot saved in working directory:\n", getwd()), "success")
    })
  }
}
shinyApp(ui, server)