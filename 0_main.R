rm(list = ls())
library(here)
library(tidyverse)
library(config)

Sys.setenv(R_CONFIG_ACTIVE = "default")
config <- config::get()

#####################
## Build analysis data set
#####################
source('1_dataIngestAndCleaning.R')

#####################
## Physio
#####################

