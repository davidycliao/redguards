#===============================================================================
# File Names       : 01.pooled-redgaurds-estimates.R 
# Date             : 31st Oct 2021
# Authors          : David Yen-Cheih Liao
# Purpose          : tokenizing the document on UD framework 
# Required Dataset : incident-group.csv   
# Output Data      : conll.csv
#===============================================================================

timer_task01 <- system.time({

# REQUIRED DATASET 
#===============================================================================
# incident <- read_csv("data/incident-group.csv", show_col_types = FALSE)

# TOKENIZATION ON UNIVERSAL DEPENDENCIES
#===============================================================================
# conll <- pos_tagging(incident, individuals = FALSE)

# SAVE OUTPUTS
#===============================================================================
# save(conll, file = "data/conll.RData")
  
  
  
})

#====================================END========================================


cat("========================================= \n",
    "Task 00 is done..", "",  
    "\n", names(timer_task01[1]), ": ", timer_task01[[1]], 
    "\n", names(timer_task01[2]), " : ", timer_task01[[2]], 
    "\n", names(timer_task01[3]), "  : ", timer_task01[[3]], 
    "\n","Core used :",parallel::detectCores())

