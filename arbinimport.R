require(readxl)
require(tcltk)
require(svDialogs)
require(dplyr)
require(shiny)
require(DT)
require(shinyjs)
require(shinyalert)
require(pracma)
require(purrr)
require(IDPmisc)
require(zoo)
library(gifski)

if (interactive()) {
  
  data <- reactiveValues(data = data.frame())
  final <- data.frame()
  lowV <- reactiveVal(0)
  highV <- reactiveVal(0)
  dirLocation <- reactiveVal("")
  
  ui <- fluidPage(
    useShinyjs(),
    useShinyalert(),
    
    fluidRow(headerPanel("Arbin Import")),
    
    column(4, 
      fluidRow(
        fileInput("rerun", "Optional: Import Previous R Environment", multiple = FALSE, accept = ".RData"),
        actionButton("load", "Load"),
        style = "border: 4px double red;"
      ),
      
      fluidRow(
        fileInput("files", "Select All files of A single Set", multiple = TRUE)
      ),
      
      fluidRow(
        numericInput("lowV", "Lower Voltage [V]", 2.8, min = 0, max = 5),
        numericInput("highV", "Upper Voltage [V]", 4.25, min = 0, max = 5),
      ),
      
      fluidRow(
        numericInput("area", "Limiting Electrode Area [cm^2]", 2.74, min = 0),
        numericInput( "perActive","Active Loading of Limiting Electrode [wt%]", 96, min = 0, max = 100),
        numericInput( "capActive","Capacity of Limiting Active Material [mAh/g]", 155, min = 0, max = 100),
      ),
      
      fluidRow(
        textInput("dirLocation", "Enter Directory Name"),
      ),
    ),
    
    column(8, align = "center",
      fluidRow(
        actionButton("submit", "Enter", width = '100%', class = 'btn-success')
      ),
      
      fluidRow(
        actionButton("excelImport", "Import Masses from Excel", width = '100%', class = "btn-secondary")
      ),
      
      fluidRow(
        checkboxGroupInput("gGraphs", "Choose Graphs to Generate:", choices = c("dQdV Graphs", "Voltage Profiles", "Voltage vs. Time", "Discharge Capacity", "Discharge Areal Capacity",
                                                                      "Total Discharge Capacity", "Average Voltage"), inline = TRUE)
      ),
      
      fluidRow(
        radioButtons("peakFit", "Do Peak Fitting on  dQdV Graphs? (BETA)", choices = c("Yes" = "fit", "No" = "noGenGraphs"), inline = TRUE)
      ),
    ),
    
    fluidRow(
      dataTableOutput("channels")
    )
  )
  
  server <- function(input, output) {
    options(shiny.maxRequestSize=30*1024^2)
    
    excelModal <- modalDialog("Copy masses from Excel now.", footer = actionButton("excelMasses", "Import"))
    
    observeEvent(input$load, {
      load(paste("history/", input$rerun[[1]], sep = ""))
      
      data <<- filter(data, grepl('Channel', sheet))
      
      output$channels <- renderDataTable(data, editable = TRUE, options=list(columnDefs = list(list(visible=FALSE, targets=c(4)))), 
                                         colnames = c("File", "Sheet", "Mass (g)", "Filepath", "Lower Voltage", "Upper Voltage"))
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
                                                   datapath = rep(files[["datapath"]][i], length(sheets)), lowV = rep(input$lowV, length(sheets)),
                                                   highV = rep(input$highV, length(sheets))))
      }
      
      data <<- filter(file_sheet, grepl('Channel', sheet))
      
      data

    }, editable = TRUE, options=list(columnDefs = list(list(visible=FALSE, targets=c(4)))), 
    colnames = c("File", "Sheet", "Mass (g)", "Filepath", "Lower Voltage", "Upper Voltage"))
    
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
      
      disable("files")
      disable("lowV")
      disable("highV")
      disable("dirLocation")
      disable("submit")
      disable("excelImport")
      disable("gGraphs")
      disable("peakFit")

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
          
          if (is.element("Mass", data)) {
            tmp_excel$Q.d <- as.numeric(tmp_excel$`Discharge_Capacity(Ah)` * (1000 / data$Mass[[1]][row]))
            tmp_excel$Q.c <- as.numeric(tmp_excel$`Charge_Capacity(Ah)`* (1000 / data$Mass[[1]][row]))
            tmp_excel$Cell <- row
            
            tmp_excel$CC <- tmp_excel$Q.d - tmp_excel$Q.c
          }
          
          cycle_facts <- data.frame()
          dQdVData <- data.frame()
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
                  dQdVData <- rbind(dQdVData, data.frame(cycle=rep(i, length(dQCdV)+1), c_d=rep(0, length(dQCdV)+1), voltage=step$`Voltage(V)`, dQdV=c(0, dQCdV), F_L=rep(0,length(dQCdV)+1)))
                } else {
                  dchV <- (1 / (tail(step$`Discharge_Capacity(Ah)`,1) - step$`Discharge_Capacity(Ah)`[[1]])) * trapz(step$`Discharge_Capacity(Ah)`, step$`Voltage(V)`)
                  dQDdV <- diff(step$`Discharge_Capacity(Ah)`)/diff(step$`Voltage(V)`)
                  
                  if (abs(prev_c - step$`Current(A)`[[1]]) > 0.0005) {
                    dQdVData <- rbind(dQdVData, data.frame(cycle=rep(i, length(dQDdV)+1),  c_d=rep(1, length(dQDdV)+1), voltage=step$`Voltage(V)`, dQdV=c(0, dQDdV), F_L=rep(1,length(dQDdV)+1)))
                    prev_c = step$`Current(A)`[[1]]
                  } else {
                    dQdVData <- rbind(dQdVData, data.frame(cycle=rep(i, length(dQDdV)+1),  c_d=rep(1, length(dQDdV)+1), voltage=step$`Voltage(V)`, dQdV=c(0, dQDdV), F_L=rep(0, length(dQDdV)+1)))
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
                if (is.element("Mass", data)) {
                  plot(tmp_excel[tmp_excel$`Cycle_Index` == i,]$`Q.d`, tmp_excel[tmp_excel$`Cycle_Index` == i,]$`Voltage(V)`, type="l", main=paste("Voltage Profile for ",  input$dirLocation, data$sheet[row]), xlab="Discharge Capacity (mAh/g)", ylab="Voltage (V)")
                } else {
                  plot(tmp_excel[tmp_excel$`Cycle_Index` == i,]$`Discharge_Capacity(Ah)`, tmp_excel[tmp_excel$`Cycle_Index` == i,]$`Voltage(V)`, type="l", main=paste("Voltage Profile for ",  input$dirLocation, data$sheet[row]), xlab="Discharge Capacity (Ah)", ylab="Voltage (V)")
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
              plot((tmp_excel[tmp_excel$`Cycle_Index` == i,]$`Test_Time(s)` - tmp_excel[tmp_excel$`Cycle_Index` == i,]$`Test_Time(s)`[[1]]) / 60, (tmp_excel[tmp_excel$`Cycle_Index` == i,]$`Voltage(V)` - tmp_excel[tmp_excel$`Cycle_Index` == i,]$`Voltage(V)`[[1]]) / 60, type="l", main=paste("Voltage vs. Time for ",  input$dirLocation, data$sheet[row]), xlab="Time (min)", ylab="Voltage (V)")
              dev.off()
            }
            
            avgV <- (dchV + chV) / 2
            cycle_facts <- rbind(cycle_facts, data.frame(cycle=i, chV=chV, dchV=dchV, avgV=avgV))
            i <- i + 1
            ch_dch <- FALSE
          }
          
          if (is.element("Mass", data)) {
            meanDCap <- aggregate(tmp_excel$`Q.d`, by=list(tmp_excel$`Cycle_Index`), last)
          } else {
            meanDCap <- aggregate(tmp_excel$`Discharge_Capacity(Ah)`, by=list(tmp_excel$`Cycle_Index`), last)
          }
          
          if (is.element("Discharge Capacity", input$gGraphs)) {
            png(paste(input$dirLocation, "/", data$sheet[row], "/", data$name[row], data$sheet[row]," Discharge Capacity Plot.png", sep = ""))
            eol <- meanDCap[1,2] * 0.8
            plot(meanDCap[,1], meanDCap[,2], main=paste("Discharge Capacity for ",  input$dirLocation, data$sheet[row]), xlab="Cycle", if (is.element("Mass", data)) ylab="Discharge Capacity (mAh/g)" else ylab="Discharge Capacity (Ah)")
            abline(h=eol, lty = "dotted")
            dev.off()
          }
           
          # if (is.element("Discharge Areal Capacity", input$gGraphs)) {
          #   png(paste(input$dirLocation, "/", data$sheet[row], "/", data$name[row], data$sheet[row]," Discharge Areal Capacity Plot.png", sep = ""))
          #   eol <- ((meanDCap[1,2] * 1000) / data$Mass[[1]][row]) * 0.8
          #   plot(meanDCap[,1], ((meanDCap[,2] * 1000) / data$Mass[[1]][row]), main=paste("Discharge Capacity for ",  input$dirLocation, data$sheet[row]), xlab="Cycle", ylab="Discharge Capacity (mAh/cm^2)")
          #   abline(h=eol, lty = "dotted")
          #   dev.off()
          # }
            
          if (is.element("dQdV Graphs", input$gGraphs)) {
            png_files <- list.files(paste(input$dirLocation, data$sheet[row], "dQdV Plots", sep="/"), pattern = "*.png", full.names = TRUE)
            gifski(png_files, gif_file = paste(input$dirLocation, data$sheet[row], "dQdV Plots", "dQdV All Cycles.gif", sep = "/"), delay = 0.1)
          }

         if (is.element("Average Voltage", input$gGraphs)) {
            png(paste(input$dirLocation, "/", data$sheet[row], "/", data$name[row], data$sheet[row]," Average Voltage Plot.png", sep = ""))
            plot(cycle_facts$cycle, cycle_facts$chV, col="blue", main=paste("Average Voltage Plot for ",  input$dirLocation, data$sheet[row]), xlab="Cycle", ylab="Voltage (V)", ylim=c(min(cycle_facts[,2:4]), max(cycle_facts[,2:4])))
            points(cycle_facts$cycle, cycle_facts$dchV, col="red", main=paste("Average Voltage Plot for ",  input$dirLocation, data$sheet[row]), xlab="Cycle", ylab="Voltage (V)")
            points(cycle_facts$cycle, cycle_facts$avgV, col="black", main=paste("Average Voltage Plot for ",  input$dirLocation, data$sheet[row]), xlab="Cycle", ylab="Voltage (V)")
            legend("top", c("Charge Voltage", "Discharge Voltage", "Average Voltage"), col=c("blue", "red", "black"), pch=19)
            dev.off()
          }
          
          write.csv(tmp_excel, file = paste(input$dirLocation, "/", data$sheet[row], "/", data$sheet[row], ".csv", sep = ""))
          write.csv(dQdVData, file = paste(input$dirLocation, "/", data$sheet[row], "/", data$sheet[row], " dQdV Data.csv", sep = ""))
          write.csv(cycle_facts, file = paste(input$dirLocation, "/", data$sheet[row], "/", data$sheet[row], " Charge-Discharge Voltages.csv", sep = ""))
          
          final <<- rbind(final, tmp_excel)
          
          incProgress(row/(nrow(data)+ 1))
        }
      
        if (is.element("Mass", data)) {
        stats <- data.frame("Cell" = "Total", "Mean Discharge Capacity" = aggregate(final[["Q.d"]], list(final[["Cycle_Index"]]), mean), "Mean Charge Capacity" = aggregate(final[["Q.c"]], list(final[["Cycle_Index"]]), mean),
                            "St. Error Discharge Capacity" = aggregate(final[["Q.d"]], list(final[["Cycle_Index"]]), se), "St. Error Charge Capacity" =
                              aggregate(final[["Q.c"]], list(final[["Cycle_Index"]]),  se))
        } else {
          stats <- data.frame("Cell" = "Total", "Mean Discharge Capacity" = aggregate(final[["Discharge_Capacity(Ah)"]], list(final[["Cycle_Index"]]), mean), "Mean Charge Capacity" = aggregate(final[["Charge_Capacity(Ah)"]], list(final[["Cycle_Index"]]), mean),
                              "St. Error Discharge Capacity" = aggregate(final[["Discharge_Capacity(Ah)"]], list(final[["Cycle_Index"]]), se), "St. Error Charge Capacity" =
                                aggregate(final[["Charge_Capacity(Ah)"]], list(final[["Cycle_Index"]]),  se))
        }
        
        stats <- select(stats, Mean.Discharge.Capacity.x, Mean.Charge.Capacity.x, St..Error.Discharge.Capacity.x, St..Error.Charge.Capacity.x)
        stats$Cycle <- 1:length(stats[,1])

        if (is.element("Total Discharge Capacity", input$gGraphs)) {
          png(paste(getwd(),"/", input$dirLocation, "/", data$name[row], "Total Discharge Capacity Plot.png", sep = ""))
          eol <- stats[1,1] * 0.8
          plot(stats$Cycle, stats$`Mean.Discharge.Capacity.x`, main=paste("Discharge Capacity for ",  input$dirLocation), xlab="Cycle", if (is.element("Mass", data)) ylab="Discharge Capacity (mAh/g)" else ylab="Discharge Capacity (Ah)")
          # arrows(stats$Cycle, stats[,1] - stats[,3], stats$Cycle, stats[,1] + stats[,3], length=0.05, angle=90, code=3)
          abline(h=eol, lty = "dotted")
          dev.off()
        }
        
        write.csv(stats, file = paste(getwd(),"/", input$dirLocation, "/", data$name[row], " Summary.csv", sep = ""))
        write.csv(final, file = paste(getwd(),"/", input$dirLocation, "/", data$name[row], " Total.csv", sep = ""))
        
        if (!dir.exists("history/")) {
          dir.create("history/")
          save(data, file = paste("history/", input$dirLocation, ".RData"))
        } else {
          save(data, file = paste("history/", input$dirLocation, ".RData"))
        }
        
        
        shinyalert("Analysis Complete!", paste("All your data are now in ", input$dirLocation), "success")
        
        remove(list=ls())
        
        incProgress((nrow(data)+ 1)/(nrow(data)+ 1))
        
        enable("files")
        enable("lowV")
        enable("highV")
        enable("dirLocation")
        enable("submit")
        enable("excelImport")
        enable("gGraphs")
        enable("peakFit")
      })
    }
    
  }
  
  shinyApp(ui, server)
}

shinyApp(ui, server)