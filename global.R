library(shiny)
library(shinydashboard)
library(DT)
library(data.table)
library(dplyr)
library(kohonen)
library(plotly)
library(shinyjs)
library(promises)
library(future)
plan(sequential)

source('src/helper.r')
source('src/custom_plot.r')

# Model/data names
model_name <- "carsommodel_105_100_pbatch_42"

# Read data
cars <- readRDS("data/cars.rds")

# maximum async size ist 500M
options(future.globals.maxSize= 2000*1024^2)

# Supress warnings
storeWarn<- getOption("warn")
options(warn = -1)

enableBookmarking("server")
