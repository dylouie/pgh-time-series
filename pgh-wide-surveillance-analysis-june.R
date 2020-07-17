library(tidyverse) # dataframe functions
library(dslabs) # dataframe functions
library(dplyr) # math functions
library(readr) # for parsing csv files
library(ggplot2) # for graphing
library(lubridate) # for parsing date
library(scales) # scaling functions
library(gridExtra) # arranging graphs
library(fpp2) # for time series

# PGH data

up_pgh_daily_report <- read.csv("up-pgh-daily-report.csv")
colnames(up_pgh_daily_report)[1] <- "report_date"

