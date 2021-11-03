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
# (No need to load the packages if replication is running in tbe pacakge )
#===============================================================================
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(
#   tidyverse, lubridate, dplyr, purrr, tibble, stringr, # Tidyverse  
#   data.table,
#   parallel, future, furrr, future.apply, parallelMap,  # Parallel Computing
#   doParallel, foreach, doFuture, 
#   quanteda, tmcn, austin, udpipe, textrank,            # NLP toolkit
# )
  
  
# REQUIRED DATASET 
#===============================================================================
# inciden <- read_csv("data/incident-group.csv", show_col_types = TRUE)

  
# TOKENIZATION ON UNIVERSAL DEPENDENCIES
#===============================================================================
# conll <- pos_tagging(incident, individuals = FALSE)


# SAVE OUTPUTS
#===============================================================================
# write.csv(conll, file = "data/conll.csv", row.names = FALSE )


#====================================END========================================
  
  
})

cat(" ====================\n",
    "=",
    "Task 01 Is Done!", "=", "\n",
    "====================",
    "\n Core used :",  detectCores(), 
    "\n Time spent \n", 
    names(timer_task01[1]), ":"  , timer_task01[[1]], "\n",
    names(timer_task01[2]), " :" , timer_task01[[2]], "\n",
    names(timer_task01[3]), "  :", timer_task01[[3]])
