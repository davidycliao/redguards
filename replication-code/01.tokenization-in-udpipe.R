#!/usr/bin/Rscript

#===============================================================================
# File Names       : 01.pooled-redgaurds-estimates.R 
# Date             : 31st Oct 2021
# Authors          : David Yen-Cheih Liao
# Purpose          : tokenizing the document on UD framework 
# Required Dataset : incident-group.csv   
# Output Data      : conll.csv
#===============================================================================

timer_task01 <- system.time({
# PACKAGES 
#===============================================================================
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(
#   tidyverse, lubridate, dplyr, purrr, tibble,          # Tidyverse
#   data.table,
#   parallel, future, furrr, future.apply, parallelMap,  # Parallel Computing
#   doParallel, foreach, doFuture,
#   quanteda, tmcn, austin, udpipe, textrank             # NLP toolkit
# )
#   
  
# REQUIRED DATASET 
#===============================================================================
# incident <- read_csv("data/incident-group.csv", show_col_types = TRUE)

# TOKENIZATION ON UNIVERSAL DEPENDENCIES
#===============================================================================
# conll <- pos_tagging(incident, individuals = FALSE)

# SAVE OUTPUTS
#===============================================================================
# save(conll, file = "data/conll.RData")
  
  
  
})

#====================================END========================================


cat(" ============================================================================================================\n",
    "Replication Task 01 is done", "|",  
    names(timer_task01[1]), ":", timer_task01[[1]],  "|",
    names(timer_task01[2]), ":", timer_task01[[2]],  "|",
    names(timer_task01[3]), ":", timer_task01[[3]],  "|",
    "Core used :",parallel::detectCores())

