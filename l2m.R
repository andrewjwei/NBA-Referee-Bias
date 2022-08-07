############## Data Gathering
library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)
library(lubridate)
library(stargazer)
library(fuzzyjoin)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

L2M <- read.csv("L2M.csv")

