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
    
    column(8,
      fluidRow(
        actionButton("submit", "Enter", width = '100%', class = 'btn-success')
      ),
      
      fluidRow(
        actionButton("excelImport", "Import Masses from Excel", width = '100%', class = "btn-secondary")
      )
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
          tmp_excel$Q.d <- as.numeric(tmp_excel$`Discharge_Capacity(Ah)` * (1000 / data$Mass[[1]][row]))
          tmp_excel$Q.c <- as.numeric(tmp_excel$`Charge_Capacity(Ah)`* (1000 / data$Mass[[1]][row]))
          tmp_excel$Cell <- row
          
          dQCdV <- diff(tmp_excel$`Charge_Capacity(Ah)`)/diff(tmp_excel$`Voltage(V)`)
          dQDdV <- diff(tmp_excel$`Discharge_Capacity(Ah)`)/diff(tmp_excel$`Voltage(V)`)
          tmp_excel$dQCdV <- c(0, dQCdV)
          tmp_excel$dQDdV <- c(0, dQDdV)
          
          # tmp <- tmp_excel %>% filter(tmp_excel$`Voltage(V)` > input$lowV & tmp_excel$`Voltage(V)` < input$highV)
          # png(paste(getwd(),"/", input$dirLocation, "/", toString(data$name[row]), toString(data$sheet[row]), " dQdV Plot.png", sep = ""))
          # plot(tmp$`Voltage(V)`, tmp$dQCdV, col=tmp$Cycle_Index)
          # points(tmp$`Voltage(V)`, tmp$dQDdV, col=tmp$Cycle_Index)
          # legend("bottomleft", legend = unique(tmp$Cycle_Index), pch = 1, col=tmp$Cycle_Index)
          # dev.off()
          
          tmp_excel$CC <- tmp_excel$Q.d - tmp_excel$Q.c
          
          cycle_facts <- data.frame(cycle=NA, chV=NA, dchV=NA, avgV=NA)
          cycles <- split(tmp_excel, tmp_excel$Cycle_Index)
          dchV <- 0
          chV <- 0
          i <- 1
          for (cycle in cycles) {
            steps <- split(cycle, cycle$Step_Index)
            for (step in steps) {
              if (abs(tail(step$'Voltage(V)',1) - step$'Voltage(V)'[[1]]) > 0.5) {
                if (step$'Current(A)'[[1]] > 0) {
                  chV <- (1 / (tail(step$`Charge_Capacity(Ah)`,1) - step$`Charge_Capacity(Ah)`[[1]])) * trapz(step$`Charge_Capacity(Ah)`, step$`Voltage(V)`)
                } else {
                  dchV <- (1 / (tail(step$`Discharge_Capacity(Ah)`,1) - step$`Discharge_Capacity(Ah)`[[1]])) * trapz(step$`Discharge_Capacity(Ah)`, step$`Voltage(V)`)
                }
              }
            }
            
            avgV <- (dchV + chV) / 2
            cycle_facts <- rbind(cycle_facts, data.frame(cycle=i, chV=chV, dchV=dchV, avgV=avgV))
            i <- i + 1
          }
          
          final <<- rbind(final, tmp_excel)
          
          write.csv(tmp_excel, file = paste(getwd(),"/", input$dirLocation, "/", data$name[row], data$sheet[row], ".csv", sep = ""))
          
          incProgress(row/nrow(data))
        }
      })
      
      stats <- data.frame("Cell" = "Total", "Mean Discharge Capacity (mAh/g)" = aggregate(final[["Q.d"]], list(final[["Cycle_Index"]]), mean), "Mean Charge Capacity (mAh/g)" = aggregate(final[["Q.c"]], list(final[["Cycle_Index"]]), mean),
                          "St. Error Discharge Capacity (mAh/g)" = aggregate(final[["Q.d"]], list(final[["Cycle_Index"]]), se), "St. Error Charge Capacity (mAh/g)" =
                            aggregate(final[["Q.c"]], list(final[["Cycle_Index"]]),  se))
      
      write.csv(stats, file = paste(getwd(),"/", input$dirLocation, "/", data$name[row], data$sheet[row], " Summary.csv", sep = ""))
      write.csv(final, file = paste(getwd(),"/", input$dirLocation, "/", data$name[row], data$sheet[row], " Total.csv", sep = ""))
      
      # save(data, file = paste("history/", input$dirLocation, ".RData"))
      
      shinyalert("Success!")
      
      remove(list=ls())
    })
    
  }
  
  shinyApp(ui, server)
}

shinyApp(ui, server)


# data = data.frame()
# dir_name <- strsplit(file_path, "/")
# dir_name <- strsplit(tail(dir_name[[1]], n=1), "_")
# dir.create(dir_name[[1]][1])
# 
# total_excel <- data.frame()
# summary_stats <- data.frame()
# 
# lowerV <- readline(prompt = "Enter lower voltage of cycle: ")
# upperV <- readline(prompt = "Enter upper voltage of cycle: ")
# 
# se <- function(x) {sd(x) / sqrt(length(x))}
# dlg_message("Copy the cell masses from excel now.")
# 
# masses <- read.table("clipboard",sep="\t")
# i = 1
# for (file in file_path) {file_name = tail(strsplit(file, "/")[[1]], 1)
#  desired_sheets <- dlg_list(excel_sheets(file), multiple = TRUE, title = paste("Please select the desired sheets from ", file_name, 
# "\n !!!Warning: Must select sheets in order of masses entered!!!"))
#   for (sheet in desired_sheets$res) {
#     tmp_excel <- read_excel(file, sheet)
#     tmp_excel$Q.d <- as.numeric(tmp_excel$`Discharge_Capacity(Ah)` * (1000 / masses[[1]][i]))
#     tmp_excel$Q.c <- as.numeric(tmp_excel$`Charge_Capacity(Ah)`* (1000 / masses[[1]][i]))
#     tmp_excel$Cell <- i
#     
#     # Differential capacity using spline fitting
#     dQCdV <- diff(tmp_excel$`Charge_Capacity(Ah)`)/diff(tmp_excel$`Voltage(V)`)
#     dQDdV <- diff(tmp_excel$`Discharge_Capacity(Ah)`)/diff(tmp_excel$`Voltage(V)`)
#     tmp_excel$dQCdV <- c(0, dQCdV)
#     tmp_excel$dQDdV <- c(0, dQDdV)
#     
#     # Plotting differential capacity
#     tmp <- tmp_excel %>% filter(tmp_excel$`Voltage(V)` > lowerV & tmp_excel$`Voltage(V)` < upperV)
#     png(paste(getwd(),"/", dir_name[[1]][1], "/", file_name, sheet, " dQdV Plot.png", sep = ""))
#     plot(tmp$`Voltage(V)`, tmp$dQCdV, col=tmp$Cycle_Index)
#     points(tmp$`Voltage(V)`, tmp$dQDdV, col=tmp$Cycle_Index)
#     legend("bottomleft", legend = unique(tmp$Cycle_Index), pch = 1, col=tmp$Cycle_Index)
#     dev.off()
#     
#     # Continuous Capacity
#     tmp_excel$CC <- tmp_excel$Q.d - tmp_excel$Q.c
#     
#     # stats <- data.frame("Cell" = i, "Mean Discharge Capacity (mAh/g)" = aggregate(tmp_excel[["Q.d"]], list(tmp_excel[["Cycle_Index"]]), mean), "Mean Charge Capacity (mAh/g)" = aggregate(tmp_excel[["Q.c"]], list(tmp_excel[["Cycle_Index"]]), mean),
#     #                    "St. Error Discharge Capacity (mAh/g)" = aggregate(tmp_excel[["Q.d"]], list(tmp_excel[["Cycle_Index"]]),  se), "St. Error Charge Capacity (mAh/g)" =
#     #                    aggregate(tmp_excel[["Q.c"]], list(tmp_excel[["Cycle_Index"]]), se))
#     
#     total_excel <- rbind(total_excel, tmp_excel)
#     # summary_stats <- rbind(summary_stats, stats)
#     
#     write.csv(tmp_excel, file = paste(getwd(),"/", dir_name[[1]][1], "/", file_name, sheet, ".csv", sep = ""))
#     
#     i = i + 1
#   }
# }
# 
# # mean <- mean(total_excel[["Q.d"]])
# 
# stats <- data.frame("Cell" = "Total", "Mean Discharge Capacity (mAh/g)" = aggregate(total_excel[["Q.d"]], list(total_excel[["Cycle_Index"]]), mean), "Mean Charge Capacity (mAh/g)" = aggregate(total_excel[["Q.c"]], list(total_excel[["Cycle_Index"]]), mean),
#                     "St. Error Discharge Capacity (mAh/g)" = aggregate(total_excel[["Q.d"]], list(total_excel[["Cycle_Index"]]), se), "St. Error Charge Capacity (mAh/g)" =
#                       aggregate(total_excel[["Q.c"]], list(total_excel[["Cycle_Index"]]),  se))
# 
# summary_stats <- rbind(summary_stats, stats)
# 
# names(summary_stats)[names(summary_stats) == "Mean.Discharge.Capacity..mAh.g..Group.1"] <- "Cycle Index"
# 
# drop <- c("Mean.Charge.Capacity..mAh.g..Group.1","St..Error.Discharge.Capacity..mAh.g..Group.1", "St..Error.Charge.Capacity..mAh.g..Group.1")
# summary_stats = summary_stats[,!(names(summary_stats) %in% drop)]
# 
# write.csv(total_excel[["Q.d"]], file = paste(getwd(),"/", dir_name[[1]][1], "/", file_name, "total.csv", sep = ""))
# write.csv(summary_stats, file = paste(getwd(), "/", dir_name[[1]][1], "/", file_name, "Summary.csv", sep = ""))
# #plot the capacity data, color by cell number to allow for trouble shooting. 
# plot(total_excel[["Cycle_Index"]],total_excel[["Q.c"]], col=total_excel[["Cell"]], main=file_name)
# legend('topright', legend = unique(total_excel[["Cell"]]), fill = unique(total_excel[["Cell"]]))
# plot(total_excel[["Cycle_Index"]],total_excel[["Q.d"]], col=total_excel[["Cell"]], main=file_name)
# legend('topright', legend = unique(total_excel[["Cell"]]), fill = unique(total_excel[["Cell"]]))