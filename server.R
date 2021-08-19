library(shiny)

server <- function(input, output, session) {

    utils <- c("modal.R")
    lapply(utils, source)
    
    # This sets the maximum file size Shiny will import, the default of 5Mb is not large enough to handle Arbin files
    options(shiny.maxRequestSize=1000*1024^2)
    
    split_path <- function(x) if (dirname(x)==x) x else c(basename(x),split_path(dirname(x)))
    
    se <- function(x) {sd(x) / length(x)}
    
    proxy = dataTableProxy("channels")
    
    batch.elements <- c("rerun", "load", "area", "whatGraph", "gGraphs", "gAnim", "graphBuilder")
    
    deleteButtonCol <- function(df, id, ...) {
      f <- function(i) {
        as.character(
          actionButton(
            paste(id, i, sep = "_"),
            label = icon("trash", lib = "glyphicon"),
            onclick = 'Shiny.setInputValue(\"deletePressed\", this.id, {priority: "event"})'
          )
        )
      }
    
      deleteCol <- unlist(lapply(seq_len(nrow(df)), f))
    
      cbind(data, delete = deleteCol)
    }
    
    generateDataTable <- function(df) {
      DT::datatable(df,escape = FALSE, editable = FALSE, options = list(columnDefs = list(list(visible=FALSE, targets=c(5, 6, 7)))), 
                    colnames = c("Group","Program","File","Sheet","Mass (g)","Filepath","Group Path","Limiting Electrode Area (cm^2)", "Delete"), rownames = FALSE)
    }
    
    parseDeleteEvent <- function(idstr) {
      res <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
      if (! is.na(res)) {
        res
      }
    }
    
    observeEvent(input$deletePressed, {
      rowNum <- parseDeleteEvent(input$deletePressed)
      
      data <<- data[-rowNum,]
      
      output$channels <- renderDataTable(generateDataTable(deleteButtonCol(data, 'delete_button')))
    })
    
    observeEvent(input$batchProcessing, {
      if (input$batchProcessing) {
        lapply(batch.elements, hide)
      } else {
        lapply(batch.elements, show)
      }
    })
    
    observeEvent(input$clearTable, {
      shinyalert("Warning!", "Are you sure you wish to clear all cells?", 
                 type ="warning", showConfirmButton = TRUE, showCancelButton = TRUE, confirmButtonText = "Continue", cancelButtonText = "Abort",
                 callbackR = function(x) {
                   if (x) {
                     data <<- data[0,]
                     output$channels <- renderDataTable(generateDataTable(deleteButtonCol(data, 'delete_button')))
                   }
                 })
    })
    
    export_to_origin <- function() {
      if (!("reticulate" %in% installed.packages()[, "Package"])) {
        install.packages("reticulate")
      }
      require(reticulate)
      
      py_location <- py_config()
      
      py_install("OriginExt", pip = TRUE)
      py_install("pandas", pip = TRUE)
      
      filtered_location <- shQuote(paste(dirLocation, "/", input$dirName, sep = ''))
      
      system(paste(py_location$python, " rPyO.py ", filtered_location, sep=''))
    }
    
    # Ensures the files imported for analysis are Excel files
    observeEvent(input$files, {
      validFile <- FALSE
      
      for (file in 1:nrow(input$files)) {
        if (file_ext(input$files[file, "name"]) == "xlsx" | file_ext(input$files[file, "name"]) == "xls") {
          validFile <- TRUE
          arbinCM <<- FALSE
        } else if (file_ext(file) == "csv") {
          arbinCM <<- TRUE
          validFile <- TRUE
        }
      }
      
      if (validFile) {
        renderTable()
      } else {
        shinyalert("That isn't right...","Please upload an Excel or CSV file.","error")
      }
    })
    
    observeEvent(input$chooseDir, {
      chosenDir <- tk_choose.dir()
      x<- 0
      #if (input$batchProcessing) {
      #  append(dirLocation, chosenDir)
      #} else {
      dirLocation <<- chosenDir
      #}
      
      internal_folders <- list.dirs(path = dirLocation, recursive = FALSE)
      base_names = vector()
      for (folder in internal_folders) {
        base_names <- append(base_names, split_path(folder)[1])
      }
      
      if ("history" %in% base_names) {
        past <- new.env()
        
        load(paste(dirLocation, "/history/", "Formation.RData", sep =""), envir = past)
        
        if (exists("past$group")) {
          if (input$batchProcessing) {
            data <<- rbind(data, past$data)
          } else {
            data <<- past$data
          }
          
          output$channels <- renderDataTable(generateDataTable(deleteButtonCol(data, 'delete_button')))
        }
      }
      
      if (!is.na(dirLocation)) {
        output$currDir <- renderText({paste(split_path(dirLocation)[1], "(", split_path(dirLocation)[2], ")")})
      }
    })
    
    # Method for importing the previous R environment
    observeEvent(input$load, {
      if (is.null(input$rerun)) {
        shinyalert("Uh oh!", "It appears you haven't selected a .RData file to import.", "error")
      } else {
        load(input$rerun$datapath[[1]])
        

        if (file_ext(input$rerun$datapath) == "RData") {
          validFile <- TRUE
        }
        
        if (validFile) {
          data <<- dataSubset
          dirLocation <<- dirLocation
          numCycles <<- numCycles
          dQdVData <<- dQdVData
          total <<- total
          cycle_facts <<- cycle_facts
          
          output$channels <- renderDataTable(generateDataTable(deleteButtonCol(data, 'delete_button')))
          
          enable("graphBuilder")
        } else {
          shinyalert("That isn't right...","Please upload an RData file.","error")
        }
      }
    })
    
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
          data$Mass <<- masses[[1]]
        }, error = function(cond) {
          print(cond)
          shinyalert("Something isn't right...","The number of masses imported did not match the amount of cells present or the text contained some special characters. Please try again.","error")
          removeModal()
        }, finally = {
          output$channels <- renderDataTable(generateDataTable(deleteButtonCol(data, 'delete_button')))
        })
      }
    })
    
    # After the validation of the Arbin files, they macros (file name, and sheets) are taken and rendered in to a datatable
    renderTable <- function() {
      
      files <- input$files
      
      if (is.null(files)) {
        return(NULL)
      }
      
      file_sheet <- data.frame()
      if (arbinCM) {
        raw_data <- data.frame(name = files[["name"]], "sheet" = 1:nrow(files), datapath = files[["datapath"]])
      } else {
        for (i in 1:nrow(files)) {
          sheets <- excel_sheets(files[i, 4])
          file_sheet <- rbind(file_sheet, data.frame(group = basename(dirLocation), program = rep(input$dirName, length(sheets)), name = rep(files[["name"]][i], length(sheets)),"sheet" = sheets,"Mass" = rep(0, length(sheets)),
                                                     datapath = rep(files[["datapath"]][i], length(sheets)), grouppath = rep(dirLocation, length(sheets)), area = rep(input$area, length(sheets))))
        }
        
        raw_data <- filter(file_sheet, grepl('Channel', sheet) & !grepl('Chart', sheet))
      }
      
      if (!is.null(dim(data)[1]) & !is.null(size(raw_data))) {
        new_rows <- intersect(raw_data$sheet, data[data["group"] == basename(dirLocation)]$sheet)
        data <<- data[which(data$sheet %in% new_rows),]
        data$datapath <<- raw_data$datapath
      } else {
        if (input$batchProcessing) {
          tryCatch( {
            data <<- rbind(data, raw_data)
          }, error = function(cond) {
            data <<- raw_data
          })
        } else {
          data <<- raw_data
        }
      }
      
      if (arbinCM) {
        output$channels <- renderDataTable(data, editable = TRUE, options = list(columnDefs = list(list(visible=FALSE, targets=c(3)))), 
                                           colnames = c("File", "Sheet", "Filepath"))
      } else {
        output$channels <- renderDataTable(generateDataTable(deleteButtonCol(data, 'delete_button')))
      }
    }
    
    # After some data validation, the main analysis is run on click of the"Run Analysis" button
    observeEvent(input$submit, {
      if (length(names(data)) <= 1) {
        shinyalert("Uh oh!", "You need to import cells first!", "error")
      } else if (is.na(dirLocation) | dirLocation == "") {
        shinyalert("Uh oh!", "You need to enter a directory name first!", "error")
      } else if (input$dirName == "") {
        shinyalert("Uh oh!", "You need to enter an analysis name first!", "error")
      } else if (sum(data$Mass) == 0) {
        shinyalert("Uh oh!", "You have not entered any masses. Do you wish to continue?", 
                   type ="warning", showConfirmButton = TRUE, showCancelButton = TRUE, confirmButtonText = "Continue", cancelButtonText = "Abort",
                   callbackR = function(x) {
                     if (x) {
                       runscript()
                     }
                   }
        )
      } else {
        runscript()
      }
    })
    
    # This function responsible for analysis of the 
    runscript <- function() {
      
      groupList <- unique(data[["group"]])
      
      for (group in groupList) {
        
        dataSubset <- data[data[["group"]] == group,]
        dirLocation <- dataSubset[["grouppath"]][1]
        programName <- dataSubset[["program"]][1]
      
        # Sets up a progress bar in which to estimate how long the execution of the code will take
        progress <- Progress$new(session, min = 0, max = nrow(dataSubset))
        progress$set(message = paste(group, ": Plugging and chugging...\n"), detail ="Starting up...")
        
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
        disable("dirLocation")
        disable("submit")
        disable("excelImport")
        disable("gGraphs")
        disable("peakFit")
        disable("area")
        disable("perActive")
        disable("capActive")
        
        # Creates the directory in which all dataSubset will be stored
        dir.create(paste(dirLocation, programName, sep = "/"))
        
        # Update the status once all set-up functions are complete
        progress$set(detail ="Starting first cell...")
        
        # ######
        # 
        # The bulk of the analysis occurs within the loop. Each iteratin of the loop corresponds to a cell.
        # 
        # ######
        for (row in 1:nrow(dataSubset)) {
          
          # ######
          # 
          # This is where all code that should be executed on a"per cell" basis, to prepare for analysis
          # 
          # ######
          
          # Import the excel sheet corresponding to cell of interest
          if (arbinCM) {
            tmp_excel <- read.csv(toString(dataSubset$dataSubsetpath[row]))
            names(tmp_excel) <- c("Index", "Test_Time(s)", "Date_Time", "Step_Time(s)", "Step_Index", "Cycle_Index", "Current(A)", "Voltage(V)", "Charge_Capacity(Ah)", "Discharge_Capacity(Ah)", "Charge_Energy(Wh)", "Discharge_Energy(Wh)")
          } else {
            tmp_excel <- read_excel(toString(dataSubset$datapath[row]), toString(dataSubset$sheet[row]))
          }
          
          # Create an nested directory for all the dataSubset and, if applicable, then further folders for graphs of interest
          dir.create(paste(dirLocation, "/",  programName, dataSubset$sheet[row], sep ="/"))
          if (is.element("dQdV Graphs", input$gGraphs)) dir.create(paste(dirLocation, programName, dataSubset$sheet[row],"dQdV Plots", sep ="/"))
          if (is.element("Voltage Profiles", input$gGraphs)) dir.create(paste(dirLocation, programName, dataSubset$sheet[row],"Voltage Profiles", sep ="/"))
          if (is.element("Voltage vs. Time", input$gGraphs)) dir.create(paste(dirLocation, programName, dataSubset$sheet[row],"Voltage v Time", sep ="/"))
          
          # Check if masses have been imported, if they have not then all future calculations will be done on a raw capacity basis
          if (sum(dataSubset$Mass) != 0) {
            ylabel <-"Capacity (mAh/g)"
            
            tmp_excel$Q.d <- as.numeric(tmp_excel$`Discharge_Capacity(Ah)` * (1000 / dataSubset$Mass[row]))
            tmp_excel$Q.c <- as.numeric(tmp_excel$`Charge_Capacity(Ah)`* (1000 / dataSubset$Mass[row]))
            
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
          lastCC <- 0
          ch_dch <- TRUE
          durations <- vector(length = 4)
          caps <- vector(length = 4)
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
            n <- 1
            
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
                lastCC <- n
                if (step$'Current(A)'[[1]] > 0) {
                  chV <- (1 / (tail(step$`Charge_Capacity(Ah)`,1) - step$`Charge_Capacity(Ah)`[[1]])) * trapz(step$`Charge_Capacity(Ah)`, step$`Voltage(V)`)
                  dQCdV <- diff(step$`Charge_Capacity(Ah)`)/diff(step$`Voltage(V)`)
                  dQdVData <<- rbind(dQdVData, data.frame(cycle=rep(i, length(dQCdV)+1), cell = rep(row, length(dQCdV)+1), c_d=rep(0, length(dQCdV)+1), voltage=step$`Voltage(V)`, dQdV=c(0, dQCdV), F_L=rep(0,length(dQCdV)+1)))
                  
                  durations[1] <- tail(step$'Test_Time(s)', 1) - step$'Test_Time(s)'[[1]]
                  caps[1] <- tail(step$'Charge_Capacity(Ah)', 1) - step$'Charge_Capacity(Ah)'[[1]]
                  ch_dch <- TRUE
                } else {
                  dchV <- (1 / (tail(step$`Discharge_Capacity(Ah)`,1) - step$`Discharge_Capacity(Ah)`[[1]])) * trapz(step$`Discharge_Capacity(Ah)`, step$`Voltage(V)`)
                  dQDdV <- diff(step$`Discharge_Capacity(Ah)`)/diff(step$`Voltage(V)`)
                  
                  durations[3] <- tail(step$'Test_Time(s)', 1) - step$'Test_Time(s)'[[1]]
                  caps[3] <- tail(step$'Discharge_Capacity(Ah)', 1) - step$'Discharge_Capacity(Ah)'[[1]]
                  ch_dch <- FALSE
                  if (abs(prev_c - step$`Current(A)`[[1]]) > 0.0005) {
                    if (!arbinCM) dQdVData <<- rbind(dQdVData, data.frame(cycle=rep(i, length(dQDdV)+1), cell = rep(row, length(dQDdV)+1), c_d=rep(1, length(dQDdV)+1), voltage=step$`Voltage(V)`, dQdV=c(0, dQDdV), F_L=rep(1,length(dQDdV)+1)))
                    prev_c = step$`Current(A)`[[1]]
                  } else {
                    if (!arbinCM) dQdVData <<- rbind(dQdVData, data.frame(cycle=rep(i, length(dQDdV)+1), cell = rep(row, length(dQDdV)+1), c_d=rep(1, length(dQDdV)+1), voltage=step$`Voltage(V)`, dQdV=c(0, dQDdV), F_L=rep(0, length(dQDdV)+1)))
                  }
                }
              } else if (n - lastCC == 1 & abs(tail(step$'Voltage(V)',1) - step$'Voltage(V)'[[1]]) < 0.001) {
                if (step$'Current(A)'[[1]] > 0) {
                  durations[2] <- tail(step$'Test_Time(s)', 1) - step$'Test_Time(s)'[[1]]
                  caps[2] <- tail(step$'Charge_Capacity(Ah)', 1) - step$'Charge_Capacity(Ah)'[[1]]
                } else {
                  durations[4] <- tail(step$'Test_Time(s)', 1) - step$'Test_Time(s)'[[1]]
                  caps[4] <- tail(step$'Discharge_Capacity(Ah)', 1) - step$'Discharge_Capacity(Ah)'[[1]]
                }
              }
              ch_dch <- FALSE
              n <- n + 1
            }
            
            # ######
            # 
            # Code meant to be run on dataSubset"per cycle" should be written here
            # 
            # ######
            
            dQdVData <<- dQdVData[is.finite(dQdVData$voltage),]
            dQdVData <<- dQdVData[is.finite(dQdVData$dQdV),]
            
            if (sum(dataSubset$Mass) != 0) {
              DCap <- tail(cycle$Q.d, 1)
              CCap <- tail(cycle$Q.c, 1)
            } else {
              DCap <- tail(cycle$`Discharge_Capacity(Ah)`, 1)
              CCap <- tail(cycle$`Charge_Capacity(Ah)`, 1)
            }
            
            timeCVFracCh <- durations[1] / (durations[1] + durations[2])
            timeCVFracDch <- durations[3] / (durations[3] + durations[4])
            capCVFracCh <- caps[1] / (caps[1] + caps[2])
            capCVFracDch <- caps[3] / (caps[3] + caps[4])
            
            if (max(tmp_excel$voltage) > 2) {
              CE <- DCap / CCap
            } else {
              CE <- CCap / DCap
            }
            
            # Record charge and discharge voltage, then calculate the delta and average voltage
            cycle_facts <<- rbind(cycle_facts, data.frame(cycle=i, cell=row, chV=chV, dchV=dchV, avgV=(dchV + chV) / 2, 
                                                          dV=chV-dchV, DCap = DCap, raw_DCap = tail(cycle$'Discharge_Capacity(Ah)', 1), raw_CCap = tail(cycle$'Charge_Capacity(Ah)', 1), CCap = CCap, CE = CE * 100, lostCap = CCap - DCap, cellFade = if (i == 1) 0 else {DCap - tail(cycle_facts$DCap, 1)}, 
                                                          cycleTime = tail(cycle$`Test_Time(s)`, 1) - cycle$`Test_Time(s)`[[1]], timeCVFracCh = timeCVFracCh, timeCVFracDch = timeCVFracDch, 
                                                          capCVFracCh = capCVFracCh, capCVRatioDch = capCVFracDch))
            
            i <- i + 1
          }
          
          # ######
          # 
          # Code meant to be run on dataSubset"per cell" should be written here
          # 
          # ######
          
          cell_dataSubset <- cycle_facts[cycle_facts$cell == row,]
          
          # Discharge capacity plotting, with coulombic efficiency being plotted alongside
          if (is.element("Discharge Capacity", input$gGraphs)) {
            png(paste(dirLocation, "/",  programName,"/", dataSubset$sheet[row],"/", dataSubset$sheet[row]," Discharge Capacity Plot.png", sep =""))
            eol <- cell_dataSubset$`DCap`[[1]] * 0.8
            plot(cell_dataSubset$cycle, cell_dataSubset$DCap, type ="p", main=paste("Discharge Capacity for",  programName), xlab=NA, ylab=paste("Discharge",  ylabel), mai=c(1,1,1,1))
            abline(h=eol, lty ="dotted")
            par(new = T)
            plot(cell_dataSubset$cycle, cell_dataSubset$CE, type ="p", axes=F, col ="red", ylab=NA, xlab="Cycle", ylim = c(0, 105))
            mtext(side = 4, line = 3,"Coulombic Efficiency (%)", col = "red")
            axis(side = 4, col ="red", col.axis = "red")
            dev.off()
          }
          
          # Discharge areal capacity plotting, with coulombic efficiency being plotted alongside
          if (is.element("Discharge Areal Capacity", input$gGraphs)) {
            png(paste(dirLocation, "/",  programName,"/", dataSubset$sheet[row],"/", dataSubset$sheet[row]," Discharge Areal Capacity Plot.png", sep =""))
            new_par <- old_par <- par("mar")
            new_par[4] <- old_par[2]
            par(mar = new_par)
            eol <- ((cell_dataSubset$DCap[[1]] * 1000) / dataSubset$area[row]) * 0.8
            plot(cell_dataSubset$cycle, ((cell_dataSubset$DCap * 1000) / dataSubset$area[row]), type ="p", main=paste("Discharge Areal Capacity for",  programName), xlab=NA, ylab="Discharge Capacity (mAh/cm^2)", mai = c(1,1,1,2))
            abline(h=eol, lty ="dotted")
            par(new = T)
            plot(cell_dataSubset$cycle, cell_dataSubset$CE, type ="p", axes=F, col ="red", ylab=NA, xlab="Cycle", ylim = c(0, 105))
            mtext(side = 4, line = 3,"Coulombic Efficiency (%)", col = "red")
            axis(side = 4, col ="red", col.axis = "red")
            dev.off()        
          }
          
          
          # Average voltage plotting
          if (is.element("Average Voltage", input$gGraphs)) {
            png(paste(dirLocation, "/",  programName,"/", dataSubset$sheet[row],"/", dataSubset$sheet[row]," Average Voltage Plot.png", sep =""))
            plot(cell_dataSubset$cycle, cell_dataSubset$chV, col="blue", main=paste("Average Voltage Plot for",  programName, dataSubset$sheet[row]), xlab="Cycle", ylab="Voltage (V)", ylim=c(min(cell_dataSubset[,2:4]), max(cell_dataSubset[,2:4])))
            points(cell_dataSubset$cycle, cell_dataSubset$dchV, col="red", main=paste("Average Voltage Plot for",  programName, dataSubset$sheet[row]), xlab="Cycle", ylab="Voltage (V)")
            points(cell_dataSubset$cycle, cell_dataSubset$avgV, col="black", main=paste("Average Voltage Plot for",  programName, dataSubset$sheet[row]), xlab="Cycle", ylab="Voltage (V)")
            legend("bottomright", c("Charge Voltage","Discharge Voltage","Average Voltage"), col=c("blue","red","black"), pch=19)
            dev.off()
          }
          
          # Delta voltage plotting
          if (is.element("Delta Voltage", input$gGraphs)) {
            png(paste(dirLocation, "/",  programName,"/", dataSubset$sheet[row],"/", dataSubset$sheet[row]," Delta Voltage Plot.png", sep =""))
            plot(cell_dataSubset$cycle, cell_dataSubset$dV, main=paste("Delta Voltage Plot for",  programName, dataSubset$sheet[row]), xlab="Cycle", ylab="Voltage (V)", ylim =c(0, 0.5))
            dev.off()
          }
          
          # Capacity Loss plotting
          if (is.element("Capacity Loss", input$gGraphs)) {
            png(paste(dirLocation, "/",  programName,"/", dataSubset$sheet[row],"/", dataSubset$sheet[row]," Capacity Loss Plot.png", sep =""))
            plot(cell_dataSubset$cycle, cell_dataSubset$lostCap, main=paste("Capacity Loss Plot for",  programName, dataSubset$sheet[row]), xlab="Cycle", ylab= ylabel, ylim = c(mean(cell_dataSubset$lostCap) + (2* sd(cell_dataSubset$lostCap)), mean(cell_dataSubset$lostCap) - (1.5* sd(cell_dataSubset$lostCap))))
            abline(h=median(cell_dataSubset$lostCap), lty="dotted")
            dev.off()
          }
          
          if (is.element("dQdV Plots", input$gAnim)) {
            dQdVplot <- function(){
              tmp_dataSubset <- dQdVData[dQdVData$cell == row,]
              first_cycle <- dQdVData[dQdVData$cell == row & dQdVData$cycle == 2,]
              dataSubsetlist <- split(tmp_dataSubset, tmp_dataSubset$cycle)
              lapply(dataSubsetlist, function(plotdataSubset){
                p <- plot(plotdataSubset$voltage, plotdataSubset$dQdV, main=paste("dQdV Plot for",  programName, dataSubset$sheet[row], "Cycle", plotdataSubset$cycle[[1]]), xlab="Voltage (V)", ylab= "dQdV (Ah/V)", 
                          xlim = c(min(tmp_dataSubset$voltage), max(tmp_dataSubset$voltage)), ylim = c(min(tmp_dataSubset$dQdV), max(tmp_dataSubset$dQdV))) + 
                  points(first_cycle$voltage, first_cycle$dQdV, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
              })
            }
            save_gif(dQdVplot(), paste(dirLocation, programName, dataSubset$sheet[row], "dQdV Animation.gif", sep = "/"), delay = 0.2)
          }
          
          if (is.element("Voltage Profiles", input$gAnim)) {
            vpPlot <- function(){
              first_cycle <- tmp_excel[tmp_excel$`Cycle_Index` == 2,]
              dataSubsetlist <- split(tmp_excel, tmp_excel$`Cycle_Index`)
              lapply(dataSubsetlist, function(plotdataSubset){
                p <- plot(plotdataSubset$CC, plotdataSubset$`Voltage(V)`, main=paste("Voltage Profile for",  programName, dataSubset$sheet[row], "Cycle", plotdataSubset$`Cycle_Index`[[1]]), xlab=ylabel, ylab= "Voltage (V)", 
                          xlim = c(min(tmp_excel$CC), max(tmp_excel$CC)), ylim = c(min(tmp_excel$`Voltage(V)`), max(tmp_excel$`Voltage(V)`))) + 
                  points(first_cycle$CC, first_cycle$`Voltage(V)`, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
              })
            }
            save_gif(vpPlot(), paste(dirLocation, programName, dataSubset$sheet[row], "Voltage Profile Animation.gif", sep = "/"), delay = 0.2)
          }
          
          # Save all dataSubset within the cell's directory
          write.csv(tmp_excel, file = paste(dirLocation, "/",  programName,"/", dataSubset$sheet[row],"/", dataSubset$sheet[row],".csv", sep =""))
          
          # Append summation dataSubset to the larger dataSubsetsets to be worked with later
          final <- rbind(final, tmp_excel)
          numCycles <<- rbind(numCycles, data.frame(sheet=dataSubset$sheet[row], cycles=nrow(cell_dataSubset)))
          
          # Update progress bar
          progress$set(value = row, detail = paste("Finished", row," of", nrow(dataSubset)," cells."))
        }
        
        # ######
        # 
        # Code meant to be run on all dataSubset of all cells should be written here.
        # 
        # ######
        
        # With iterations complete, final calculations are being worked
        progress$set(detail ="Wrapping up...")
        
        # Get the last status of each cycle for each cell (namely capacity)
        DCap <- cycle_facts[c("cycle","DCap")] %>% group_by(cycle) %>% summarise_each(mean)
        CE <- cycle_facts[c("cycle","CE")] %>% group_by(cycle) %>% summarise_each(mean)
        capSEs <- cycle_facts[c("cycle","DCap")] %>% group_by(cycle) %>% summarise_each(se)
        ceSEs <- cycle_facts[c("cycle","CE")] %>% group_by(cycle) %>% summarise_each(se)
        stats <- data.frame(cbind(cycle = capSEs$cycle, DCap = DCap$DCap, CE = CE$CE, capSE = capSEs$DCap, ceSE = ceSEs$CE))
        
        # Send all the dataSubset to a global variable to be used elsewhere
        total <<- final
        
        tryCatch({
          # Total dishcharge capacity plotting
          if (is.element("Total Discharge Capacity", input$gGraphs)) {
            png(paste(dirLocation, "/",  programName,"/", "Total Discharge Capacity Plot.png", sep =""))
            eol <- max(stats$DCap) * 0.8
            plot(stats$cycle, stats$DCap, type ="p", main=paste("Discharge Capacity for",  programName), xlab=NA, ylab=paste("Discharge", ylabel), mai=c(1,1,1,1))
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
        
        # Save total dataSubset and stats
        write.csv(stats, file = paste(dirLocation, "/",  programName,"/", basename(dirLocation)," Summary.csv", sep =""))
        if (!arbinCM) write.csv(dQdVData, file = paste(dirLocation, "/",  programName,"/", basename(dirLocation)," dQdV data.csv", sep =""))
        write.csv(cycle_facts, file = paste(dirLocation, "/",  programName,"/", basename(dirLocation)," Cycle Facts.csv", sep =""))
        
        # If a histor directory does not exist, create it. Save all the dataSubset revelant to plotting to a RdataSubset file.
        if (!dir.exists(paste(dirLocation, "history", sep = "/"))) {
          dir.create(paste(dirLocation, "history", sep = "/"))
        }
        
        dirName <<- programName
        
        save(dirLocation, dirName, dataSubset, dQdVData, total, cycle_facts, numCycles, file = paste(dirLocation, "/history/", programName, ".Rdata", sep = ""))
      }
      
      # Modal for completed analysis
      shinyalert("Analysis Complete!", paste("All your data are now in ", dirLocation, "/", programName, sep = ""), 
                 type ="success", showConfirmButton = TRUE, showCancelButton = TRUE, confirmButtonText = "Generate Origin File", cancelButtonText = "Continue",
                 callbackR = function(x) {
                   if (x) {
                     export_to_origin()
                   }
                 }
      )
      
      # Finish progress bar
      progress$set(value = nrow(data))
      
      # Re-enable all input fields, including graph builder
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
      
      
      for (num in 1:size(groupList)[2]) {
        # Close progress bar
        progress$close()
      }
    }
    
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
      if (input$perType =="Within Analysis") {
        sheetName <<- TRUE
        
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
                 tmp_data <<- data.frame(x = total[total$Cell %in% cellIndex,]$CC, y=total[total$Cell %in% cellIndex,]$`Voltage(V)`, cycle=total[total$Cell %in% cellIndex,]$`Cycle_Index`, cell=total[total$Cell %in% cellIndex,]$Cell)
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
        
        tmp_data$color <<- sapply(tmp_data$cycle, function(x) {match(x, input$renderCycles, nomatch = 1)})
        tmp_data$symbol <<- sapply(tmp_data$cell, function(x) {match(x, cellIndex)})
        
      } else if (input$perType == "Between Analyses") {
        
        switch(input$typeGraph,
               "Charge Voltage" = {
                 x<-0
                 chV <- cycle_facts[c("cycle","chV")] %>% group_by(cycle) %>% summarise_each(mean)
                 chVSE <- cycle_facts[c("cycle","chV")] %>% group_by(cycle) %>% summarise_each(se)
                 
                 tryCatch({
                   comp_chV <- compCycleFacts[c("cycle","chV")] %>% group_by(cycle) %>% summarise_each(mean)
                   comp_chVSE <- compCycleFacts[c("cycle","chV")] %>% group_by(cycle) %>% summarise_each(se)
                   
                   tmp_data <<- data.frame(x=c(chV$cycle, comp_chV$cycle), y=c(chV$chV, comp_chV$chV), se=c(chVSE$chV, comp_chVSE$chV), cell=c(rep(1, length(chV$cycle)), rep(2, length(comp_chV$cycle))), cycle=c(chV$cycle, comp_chV$cycle))
                   tmp_data$symbol <<- rep(1, nrow(tmp_data))
                   tmp_data$color <<- sapply(tmp_data$cell, function(x) {match(x, c(1,2))})
                   
                   titleLabel <<- "Charge Voltage Plot "
                   xlabel <<- "Cycle"
                   ylabel <<- "Voltage (V)"
                 },
                 error = function(x) {
                   print(x)
                 })
               },
               "Discharge Voltage" = {
                 x<-0
                 dchV <- cycle_facts[c("cycle","dchV")] %>% group_by(cycle) %>% summarise_each(mean)
                 dchVSE <- cycle_facts[c("cycle","dchV")] %>% group_by(cycle) %>% summarise_each(se)
                 
                 tryCatch({
                   comp_dchV <- compCycleFacts[c("cycle","dchV")] %>% group_by(cycle) %>% summarise_each(mean)
                   comp_dchVSE <- compCycleFacts[c("cycle","dchV")] %>% group_by(cycle) %>% summarise_each(se)
                   
                   tmp_data <<- data.frame(x=c(dchV$cycle, comp_dchV$cycle), y=c(dchV$dchV, comp_dchV$dchV), se=c(dchVSE$dchV, comp_dchVSE$dchV), cell=c(rep(1, length(dchV$cycle)), rep(2, length(comp_dchV$cycle))), cycle=c(dchV$cycle, comp_dchV$cycle))
                   tmp_data$symbol <<- rep(1, nrow(tmp_data))
                   tmp_data$color <<- sapply(tmp_data$cell, function(x) {match(x, c(1,2))})
                   
                   titleLabel <<- "Disharge Voltage Plot "
                   xlabel <<- "Cycle"
                   ylabel <<- "Voltage (V)"
                 },
                 error = function(x) {
                   print(x)
                 })
               },
               "Average Voltage" = {
                 avgV <- cycle_facts[c("cycle","avgV")] %>% group_by(cycle) %>% summarise_each(mean)
                 avgVSE <- cycle_facts[c("cycle","avgV")] %>% group_by(cycle) %>% summarise_each(se)
                 
                 tryCatch({
                   comp_avgV <- compCycleFacts[c("cycle","avgV")] %>% group_by(cycle) %>% summarise_each(mean)
                   comp_avgVSE <- compCycleFacts[c("cycle","avgV")] %>% group_by(cycle) %>% summarise_each(se)
                   
                   tmp_data <<- data.frame(x=c(avgV$cycle, comp_avgV$cycle), y=c(avgV$avgV, comp_avgV$avgV), se=c(avgVSE$avgV, comp_avgVSE$avgV), cell=c(rep(1, length(avgV$cycle)), rep(2, length(comp_avgV$cycle))), cycle=c(avgV$cycle, comp_avgV$cycle))
                   tmp_data$symbol <<- rep(1, nrow(tmp_data))
                   tmp_data$color <<- sapply(tmp_data$cell, function(x) {match(x, c(1,2))})
                   
                   titleLabel <<- "Average Voltage Plot "
                   xlabel <<- "Cycle"
                   ylabel <<- "Voltage (V)"
                 },
                 error = function(x) {
                   print(x)
                 })
               },
               "Delta Voltage" = {
                 dV <- cycle_facts[c("cycle","dV")] %>% group_by(cycle) %>% summarise_each(mean)
                 dVSE <- cycle_facts[c("cycle","dV")] %>% group_by(cycle) %>% summarise_each(se)
                 
                 tryCatch({
                   comp_dV <- compCycleFacts[c("cycle","dV")] %>% group_by(cycle) %>% summarise_each(mean)
                   comp_dVSE <- compCycleFacts[c("cycle","dV")] %>% group_by(cycle) %>% summarise_each(se)
                   
                   tmp_data <<- data.frame(x=c(dV$cycle, comp_dV$cycle), y=c(dV$dV, comp_dV$dV), se=c(dVSE$dV, comp_dVSE$dV), cell=c(rep(1, length(dV$cycle)), rep(2, length(comp_dV$cycle))), cycle=c(dV$cycle, comp_dV$cycle))
                   tmp_data$symbol <<- rep(1, nrow(tmp_data))
                   tmp_data$color <<- sapply(tmp_data$cell, function(x) {match(x, c(1,2))})
                   
                   titleLabel <<- "Delta Voltage Plot "
                   xlabel <<- "Cycle"
                   ylabel <<- "Voltage (V)"
                 },
                 error = function(x) {
                   print(x)
                 })
               },
               "Discharge Capacity" = {
                 DCap <- cycle_facts[c("cycle","DCap")] %>% group_by(cycle) %>% summarise_each(mean)
                 DCapSE <- cycle_facts[c("cycle","DCap")] %>% group_by(cycle) %>% summarise_each(se)
                 
                 tryCatch({
                   comp_DCap <- compCycleFacts[c("cycle","DCap")] %>% group_by(cycle) %>% summarise_each(mean)
                   comp_DCapSE <- compCycleFacts[c("cycle","DCap")] %>% group_by(cycle) %>% summarise_each(se)
                   
                   tmp_data <<- data.frame(x=c(DCap$cycle, comp_DCap$cycle), y=c(DCap$DCap, comp_DCap$DCap), se=c(DCapSE$DCap, comp_DCapSE$DCap), cell=c(rep(1, length(DCap$cycle)), rep(2, length(comp_DCap$cycle))), cycle=c(DCap$cycle, comp_DCap$cycle))
                   tmp_data$symbol <<- rep(1, nrow(tmp_data))
                   tmp_data$color <<- sapply(tmp_data$cell, function(x) {match(x, c(1,2))})
                   
                   titleLabel <<- "Discharge Capacity Plot "
                   xlabel <<- "Cycle"
                   if (sum(data$Mass) != 0) {
                     ylabel <<- "Discharge Capacity (mAh/g)"
                   } else {
                     ylabel <<- "Discharge Capacity (Ah)"
                   }
                 },
                 error = function(x) {
                   print(x)
                 })
               },
               "Charge Capacity" = {
                 CCap <- cycle_facts[c("cycle","CCap")] %>% group_by(cycle) %>% summarise_each(mean)
                 CCapSE <- cycle_facts[c("cycle","CCap")] %>% group_by(cycle) %>% summarise_each(se)
                 
                 tryCatch({
                   comp_CCap <- compCycleFacts[c("cycle","CCap")] %>% group_by(cycle) %>% summarise_each(mean)
                   comp_CCapSE <- compCycleFacts[c("cycle","CCap")] %>% group_by(cycle) %>% summarise_each(se)
                   
                   tmp_data <<- data.frame(x=c(CCap$cycle, comp_CCap$cycle), y=c(CCap$CCap, comp_CCap$CCap), se=c(CCapSE$CCap, comp_CCapSE$CCap), cell=c(rep(1, length(CCap$cycle)), rep(2, length(comp_CCap$cycle))), cycle=c(CCap$cycle, comp_CCap$cycle))
                   tmp_data$symbol <<- rep(1, nrow(tmp_data))
                   tmp_data$color <<- sapply(tmp_data$cell, function(x) {match(x, c(1,2))})
                   
                   titleLabel <<- "Charge Capacity Plot "
                   xlabel <<- "Cycle"
                   if (sum(data$Mass) != 0) {
                     ylabel <<- "Charge Capacity (mAh/g)"
                   } else {
                     ylabel <<- "Charge Capacity (Ah)"
                   }
                 },
                 error = function(x) {
                   print(x)
                 })
               },
               "Capacity Loss" = {
                 lostCap <- cycle_facts[c("cycle","lostCap")] %>% group_by(cycle) %>% summarise_each(mean)
                 lostCapSE <- cycle_facts[c("cycle","lostCap")] %>% group_by(cycle) %>% summarise_each(se)
                 
                 tryCatch({
                   comp_lostCap <- compCycleFacts[c("cycle","lostCap")] %>% group_by(cycle) %>% summarise_each(mean)
                   comp_lostCapSE <- compCycleFacts[c("cycle","lostCap")] %>% group_by(cycle) %>% summarise_each(se)
                   
                   tmp_data <<- data.frame(x=c(lostCap$cycle, comp_lostCap$cycle), y=c(lostCap$lostCap, comp_lostCap$lostCap), se=c(lostCapSE$lostCap, comp_lostCapSE$lostCap), cell=c(rep(1, length(lostCap$cycle)), rep(2, length(comp_lostCap$cycle))), cycle=c(lostCap$cycle, comp_lostCap$cycle))
                   tmp_data$symbol <<- rep(1, nrow(tmp_data))
                   tmp_data$color <<- sapply(tmp_data$cell, function(x) {match(x, c(1,2))})
                   
                   titleLabel <<- "Charge Capacity Plot "
                   xlabel <<- "Cycle"
                   if (sum(data$Mass) != 0) {
                     ylabel <<- "Capacity (mAh/g)"
                   } else {
                     ylabel <<- "Capacity (Ah)"
                   }
                 },
                 error = function(x) {
                   print(x)
                 })
               },
        )
      }
      
      tmp_data <<- tmp_data[is.finite(tmp_data$x),]
      tmp_data <<- tmp_data[is.finite(tmp_data$y),]
      tmp_data <<- tmp_data[is.finite(tmp_data$cycle),]
      tmp_data <<- tmp_data[is.finite(tmp_data$cell),]
      
      if (any(sapply(bounds, is.na))) {
        if (is.na(bounds[1])) bounds[1] <<- min(tmp_data$x)
        if (is.na(bounds[2])) bounds[2] <<- max(tmp_data$x)
        if (is.na(bounds[3])) bounds[3] <<- min(tmp_data$y)
        if (is.na(bounds[4])) bounds[4] <<- max(tmp_data$y)
      }
      
      tryCatch({
        if (input$plotStyle =="o" | input$plotStyle =="p" | "se" %in% colnames(tmp_data)) {
          par(mar=c(5.1, 6.1, 4.1, 2.1))
          
          plot(tmp_data$x, tmp_data$y, type = input$plotStyle, col = tmp_data$color, pch = tmp_data$symbol, main=titleLabel, xlim = c(bounds[1], bounds[2]), ylim = c(bounds[3], bounds[4]),  xlab=xlabel, ylab=ylabel, cex = input$pointSize, cex.axis = input$textSize, cex.lab = input$textSize, cex.main = input$textSize)
          if ("se" %in% colnames(tmp_data)) {
            arrows(tmp_data$x, tmp_data$y - tmp_data$se, tmp_data$x, tmp_data$y + tmp_data$se, col = tmp_data$color, length=0.05, angle=90, code=3)
            #legend("bottomright", legend = c(input$originalData, input$compareData), col = c(1,2), pch = 19)
          } else {
            if (input$typeGraph %in% c("dQdV Graphs", "Voltage Profiles", "Voltage vs. Time")) {
              #legend("bottomright", legend = c(sort(as.numeric(input$renderCycles)), input$cells), col = c(unique(tmp_data$color), rep("black", length(input$cells))), pch = c(rep(19, length(unique(tmp_data$color))), 1:length(input$cells)), title ="Cycle", ncol=2)
            } else {
              #legend("bottomright", legend = c(sort(as.numeric(input$renderCycles)), input$cells), col = c(unique(tmp_data$color), rep("black", length(input$cells))), pch = c(1:length(input$cells)), title ="Cycle", ncol=2)
            }
          }
        } else if (input$plotStyle =="l") {
          newLine <- subset(tmp_data, tmp_data$color == 1 & tmp_data$symbol == 1)
          plot(newLine$x, newLine$y, type ="l", col = newLine$color, lty = newLine$symbol, main=titleLabel, xlim = c(bounds[1], bounds[2]), ylim = c(bounds[3], bounds[4]),  xlab=xlabel, ylab=ylabel, lwd = input$pointSize, cex.axis = input$textSize, cex.lab = input$textSize, cex.main = input$textSize)
          
          for (i in 1:length(input$renderCycles)) {
            for (n in 1:length(cellIndex)) {
              newLine <- subset(tmp_data, tmp_data$color == i & tmp_data$symbol == n)
              lines(newLine$x, newLine$y, col = newLine$color, lty = newLine$symbol, lwd = input$pointSize)
            }
          }
          if (input$typeGraph %in% c("dQdV Graphs", "Voltage Profiles", "Voltage vs. Time")) {
            legend("topright", legend = c(sort(as.numeric(input$renderCycles)),  if ("se" %in% colnames(tmp_data)) {c(input$dirName, compName)}), col = c(unique(tmp_data$color), rep("black", length(input$cells))), lty = c(rep(19, length(unique(tmp_data$color))), 1:length(input$cells)), title ="Cycle", ncol=2)
          } else {
            legend("topright", legend = c(sort(as.numeric(input$renderCycles)), input$cells, if ("se" %in% colnames(tmp_data)) {c(input$dirName, compName)}), col = c(unique(tmp_data$color), rep("black", length(input$cells))), lty = c(1:length(input$cells)), title ="Cycle", ncol=2)
          }
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
    }, res = 125)
    
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
    
    # Method for handling changes in cell selection
    observeEvent(input$cells, {
      tmp_cycles <<- input$renderCycles
      updateSelectInput(session,"renderCycles", choices = 1:max(numCycles$cycles), selected = tmp_cycles)
    })
    
    observeEvent(input$compAnalysis, {
      load(input$compAnalysis$datapath[[1]])
      
      compName <<- basename(input$compAnalysis$datapath[[1]])
      compCycleFacts <<- cycle_facts
      
      sheetName <<- !sheetName
    })
    
    # Error handling for graphBuilder and then showing modal
    observeEvent(input$graphBuilder, {
      if(dim(numCycles)[1] == 0 | dim(dQdVData)[1] == 0 | dim(cycle_facts)[1] == 0) {
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
    
    observeEvent(input$perType, {
      if (input$perType == "Within Analysis") {
        updateRadioButtons(session, "typeGraph", choices = c("dQdV Graphs","Voltage Profiles", "Voltage vs. Time", 
                                                             "Charge Voltage", "Discharge Voltage", 
                                                             "Average Voltage", "Delta Voltage", "Discharge Capacity", "Charge Capacity" ))
        
        show("cells")
        show("renderCycles")
        show("plotStyle")
        hide("originalData")
        hide("compareData")
        hide("compAnalysis")
        hide("analysis")
      } else if (input$perType == "Between Analyses") {
        updateRadioButtons(session, "typeGraph", choices = c("Charge Voltage", "Discharge Voltage", 
                                                             "Average Voltage", "Delta Voltage", "Discharge Capacity", "Charge Capacity",
                                                             "Capacity Loss"))
        
        sheetName <<- !sheetName
        
        hide("cells")
        hide("renderCycles")        
        show("originalData")
        show("compareData")
        hide("plotStyle")
        show("compAnalysis")
        show("analysis")
      }
    })
    
    # Method for saving graph generated by graphBuilder
    observeEvent(input$saveGraph, {
      png(paste(input$fileName,".png"), res = 125)
      
      if (input$plotStyle =="o" | input$plotStyle =="p") {
        plot(tmp_data$x, tmp_data$y, type = input$plotStyle, col = tmp_data$color, pch = tmp_data$symbol, main=titleLabel, xlim = c(bounds[1], bounds[2]), ylim = c(bounds[3], bounds[4]),  xlab=xlabel, ylab=ylabel, cex = input$pointSize, cex.axis = input$textSize, cex.lab = input$textSize, cex.main = input$textSize)
        #legend("bottomright", legend = c(sort(as.numeric(input$renderCycles)), input$cells), col = c(unique(tmp_data$color), rep("black", length(input$cells))), pch = c(rep(19, length(unique(tmp_data$color))), 1:length(input$cells)), title ="Cycle", ncol=2)
      } else if (input$plotStyle =="l") {
        newLine <- subset(tmp_data, tmp_data$color == 1 & tmp_data$symbol == 1)
        plot(newLine$x, newLine$y, type ="l", col = newLine$color, lty = newLine$symbol, main=titleLabel, xlim = c(bounds[1], bounds[2]), ylim = c(bounds[3], bounds[4]),  xlab=xlabel, ylab=ylabel, lwd = input$pointSize, cex.axis = input$textSize, cex.lab = input$textSize, cex.main = input$textSize)
        
        for (i in 1:length(input$renderCycles)) {
          for (n in 1:length(cellIndex)) {
            newLine <- subset(tmp_data, tmp_data$color == i & tmp_data$symbol == n)
            lines(newLine$x, newLine$y, col = newLine$color, lty = newLine$symbol, lwd = input$pointSize)
          }
        }
        
        #legend("bottomright", legend = c(sort(as.numeric(input$renderCycles)), input$cells), col = c(unique(tmp_data$color), rep("black", length(input$cells))), lty = c(rep(19, length(unique(tmp_data$color))), 1:length(input$cells)), title ="Cycle", ncol=2)
      }
      
      dev.off()
      
      shinyalert("Success!", paste("Plot saved in working directory:\n", getwd()),"success")
    })
}