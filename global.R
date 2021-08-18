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

options(warn = 2)

list.of.packages <- c("readxl", "dplyr", "shiny", "tcltk", "DT", "shinyjs", "shinyalert", "pracma", "purrr", "zoo", "plotrix", "tools", "shinyWidgets", "gifski")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

source('ui.R', local = TRUE)
source('server.R')
source('info.R')

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
  dirLocation <- vector()
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
  arbinCM <- FALSE
  catMetric <<- vector()
  legTitle <<-""
  sheetName <<-""
  bounds <<- vector()
  compCycleFacts <<- data.frame()
}

shinyApp(ui, server)
