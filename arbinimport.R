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
        numericInput("lowV", "Lower Voltage", 2.8, min = 0, max = 5),
        numericInput("highV", "Upper Voltage", 4.25, min = 0, max = 5)
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
        radioButtons("gGraphs", "Generate Graphs?", choices = c("Yes (may take longer)" = "genGraphs", "No" = "noGenGraphs"), inline = TRUE)
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
      dir.create(input$dirLocation)
      
      se <- function(x) {sd(x) / sqrt(length(x))}
      
      withProgress(message = "Computing...", {
      
        for (row in 1:nrow(data)) {
          tmp_excel <- read_excel(toString(data$datapath[row]), toString(data$sheet[row]))
          
          dir.create(paste(input$dirLocation, data$sheet[row], sep = "/"))
          if (input$gGraphs == "genGraphs") {
            dir.create(paste(input$dirLocation, data$sheet[row], "dQdV Plots", sep = "/"))
            dir.create(paste(input$dirLocation, data$sheet[row], "Voltage Profiles", sep = "/"))
            dir.create(paste(input$dirLocation, data$sheet[row], "Voltage v Time", sep = "/"))
          }
          
          tmp_excel$Q.d <- as.numeric(tmp_excel$`Discharge_Capacity(Ah)` * (1000 / data$Mass[[1]][row]))
          tmp_excel$Q.c <- as.numeric(tmp_excel$`Charge_Capacity(Ah)`* (1000 / data$Mass[[1]][row]))
          tmp_excel$Cell <- row
          
          tmp_excel$CC <- tmp_excel$Q.d - tmp_excel$Q.c
          
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
                  dQdVData <- rbind(dQdVData, data.frame(cycle=rep(i, length(dQCdV)+1), voltage=step$`Voltage(V)`, dQdV=c(0, dQCdV), F_L=rep(0,length(dQCdV)+1)))
                } else {
                  dchV <- (1 / (tail(step$`Discharge_Capacity(Ah)`,1) - step$`Discharge_Capacity(Ah)`[[1]])) * trapz(step$`Discharge_Capacity(Ah)`, step$`Voltage(V)`)
                  dQDdV <- diff(step$`Discharge_Capacity(Ah)`)/diff(step$`Voltage(V)`)
                  
                  if (abs(prev_c - step$`Current(A)`[[1]]) > 0.0005) {
                    dQdVData <- rbind(dQdVData, data.frame(cycle=rep(i, length(dQDdV)+1), voltage=step$`Voltage(V)`, dQdV=c(0, dQDdV), F_L=rep(1,length(dQDdV)+1)))
                    prev_c = step$`Current(A)`[[1]]
                  } else {
                    dQdVData <- rbind(dQdVData, data.frame(cycle=rep(i, length(dQDdV)+1), voltage=step$`Voltage(V)`, dQdV=c(0, dQDdV), F_L=rep(0, length(dQDdV)+1)))
                  }
                }
              }
            }
            
            
            
            if (ch_dch && input$gGraphs == "genGraphs") {
              png(paste(input$dirLocation, "/", data$sheet[row], "/", "dQdV Plots/", data$name[row], data$sheet[row], "Cycle ", toString(i)," dQdV Plot.png", sep = ""))
              plot(dQdVData[dQdVData$cycle == i,]$voltage, dQdVData[dQdVData$cycle == i,]$dQdV, main=paste("dQdV Plot for ",  input$dirLocation, data$sheet[row], "Cycle ", toString(i)), xlab="Voltage (V)", ylab="dQdV (mAh/V)")
              dev.off()
              
              png(paste(input$dirLocation, "/", data$sheet[row], "/", "Voltage Profiles/", data$name[row], data$sheet[row], "Cycle ", toString(i)," Voltage Profile Plot.png", sep = ""))
              plot(tmp_excel[tmp_excel$`Cycle_Index` == i,]$`Q.d`, tmp_excel[tmp_excel$`Cycle_Index` == i,]$`Voltage(V)`, type="l", main=paste("Voltage Profile for ",  input$dirLocation, data$sheet[row]), xlab="Discharge Capacity (mAh/g)", ylab="Voltage (V)")
              dev.off()
            }
            
            if (input$gGraphs == "genGraphs") {
              png(paste(input$dirLocation, "/", data$sheet[row], "/", "Voltage v Time/", data$name[row], data$sheet[row], "Cycle ", toString(i)," Voltage Profile Plot.png", sep = ""))
              plot((tmp_excel[tmp_excel$`Cycle_Index` == i,]$`Test_Time(s)` - tmp_excel[tmp_excel$`Cycle_Index` == i,]$`Test_Time(s)`[[1]]) / 60, (tmp_excel[tmp_excel$`Cycle_Index` == i,]$`Voltage(V)` - tmp_excel[tmp_excel$`Cycle_Index` == i,]$`Voltage(V)`[[1]]) / 60, type="l", main=paste("Voltage vs. Time for ",  input$dirLocation, data$sheet[row]), xlab="Time (min)", ylab="Voltage (V)")
              dev.off()
            }
            
            avgV <- (dchV + chV) / 2
            cycle_facts <- rbind(cycle_facts, data.frame(cycle=i, chV=chV, dchV=dchV, avgV=avgV))
            i <- i + 1
            ch_dch <- FALSE
          }
          
          meanDCap <- aggregate(tmp_excel$`Q.d`, by=list(tmp_excel$`Cycle_Index`), last)
          
          if (input$gGraphs == "genGraphs") {
            png(paste(input$dirLocation, "/", data$sheet[row], "/", data$name[row], data$sheet[row]," Discharge Capacity Plot.png", sep = ""))
            plot(meanDCap[,1], meanDCap[,2], main=paste("Discharge Capacity for ",  input$dirLocation, data$sheet[row]), xlab="Cycle", ylab="Discharge Capacity (mAh/g)")
            dev.off()
          }

          if (input$gGraphs == "genGraphs") {
            png(paste(input$dirLocation, "/", data$sheet[row], "/", data$name[row], data$sheet[row]," Average Voltage Plot.png", sep = ""))
            plot(cycle_facts$cycle, cycle_facts$chV, col="blue", main=paste("dQdV Plot for ",  input$dirLocation, data$sheet[row]), xlab="Cycle", ylab="Voltage (V)", ylim=c(min(cycle_facts[,2:4]), max(cycle_facts[,2:4])))
            points(cycle_facts$cycle, cycle_facts$dchV, col="red", main=paste("dQdV Plot for ",  input$dirLocation, data$sheet[row]), xlab="Cycle", ylab="Voltage (V)")
            points(cycle_facts$cycle, cycle_facts$avgV, col="black", main=paste("dQdV Plot for ",  input$dirLocation, data$sheet[row]), xlab="Cycle", ylab="Voltage (V)")
            legend("top", c("Charge Voltage", "Discharge Voltage", "Average Voltage"), col=c("blue", "red", "black"), pch=19)
            dev.off()
          }
          
          write.csv(tmp_excel, file = paste(input$dirLocation, "/", data$sheet[row], "/", data$sheet[row], ".csv", sep = ""))
          write.csv(dQdVData, file = paste(input$dirLocation, "/", data$sheet[row], "/", data$sheet[row], " dQdV Data.csv", sep = ""))
          write.csv(cycle_facts, file = paste(input$dirLocation, "/", data$sheet[row], "/", data$sheet[row], " Charge-Discharge Voltages.csv", sep = ""))
          
          final <<- rbind(final, tmp_excel)
          
          incProgress(row/nrow(data))
        }
      })
      
      stats <- data.frame("Cell" = "Total", "Mean Discharge Capacity (mAh/g)" = aggregate(final[["Q.d"]], list(final[["Cycle_Index"]]), mean), "Mean Charge Capacity (mAh/g)" = aggregate(final[["Q.c"]], list(final[["Cycle_Index"]]), mean),
                          "St. Error Discharge Capacity (mAh/g)" = aggregate(final[["Q.d"]], list(final[["Cycle_Index"]]), se), "St. Error Charge Capacity (mAh/g)" =
                            aggregate(final[["Q.c"]], list(final[["Cycle_Index"]]),  se))
      
      write.csv(stats, file = paste(getwd(),"/", input$dirLocation, "/", data$name[row], " Summary.csv", sep = ""))
      write.csv(final, file = paste(getwd(),"/", input$dirLocation, "/", data$name[row], " Total.csv", sep = ""))
      
      # save(data, file = paste("history/", input$dirLocation, ".RData"))
      
      shinyalert("Analysis Complete!", paste("All your data is now in ", input$dirLocation), "success")
      
      remove(list=ls())
    })
    
  }
  
  shinyApp(ui, server)
}

shinyApp(ui, server)