#!/usr/bin/Rscript

#===============================================================================
# File Names       : 03.pooled-redgaurds-estimates.R 
# Date             : 31st Oct 2021
# Authors          : David Yen-Cheih Liao
# Purpose          : ideal point estimation based on text features.
# Required Dataset : incident-group.csv; redgaurds_wfm.Rdata       
# Output Data      : pooled_outcome.RData; redguard_estimates.csv
#===============================================================================

timer_task03 <- system.time({
# REQUIRED PACKAGES
#===============================================================================
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(
#   tidyverse, lubridate, dplyr, purrr, tibble,           # Tidyverse
#   tidyr, tidyr, data.table,                             # Data Pre-processings
#   parallel, future, furrr, future.apply,                # Parallel Computing
#   doParallel, foreach, doFuture,
#   quanteda, tmcn, austin, udpipe, textrank,             # NLP toolkit
#   emIRT                                                 # Generalized Wordfish
# )

# REQUIRED DATASET 
#===============================================================================
# load("data/dfm_list.RData")
# load("data/incident_list.RData")
# load("data/dict.Rdata")

data(dfm_list)  
data(incident_list)  
data(dict)  

  
# BUILDING DOCUNEBTS-TERM-MATRIX IN QUANTEDA & AUSTIN
#===============================================================================
# Tokenize the document based on individual participant
# Note : split the data by each incident and save them into list object re-index 
# the incident index in to numeric number in order 

# dfm_list <- incident_list %>%
#   map("content") %>%
#   map(corpus)
# 
# for (i in 1:length(dfm_list)) {
#   docnames(dfm_list[[i]])  <- incident_list[[i]][["id_doc"]]}

doParallel::registerDoParallel(parallel::makeCluster(detectCores()-1))
for (i in names(dfm_list)){docnames(dfm_list[[i]]) <- incident_list[[i]]$id_doc}

# Create DTM objects from each incident, and bind them by row into DTMs
  redgaurds_dfm <- foreach::foreach(i = 1:length(dfm_list), 
                                    .combine= rbind,
                                    .multicombine=TRUE) %dopar%
    {quanteda::dfm(dfm_list[[i]], dictionary = dict[[i]])}
parallel::stopCluster(parallel::makeCluster(detectCores()-1))

# TURNING DOCUNEBTS-TERM-MATRIX INTO WORD-FREQUENCY MATRIX
#===============================================================================
redgaurds_wfm <- dtm_wfm(redgaurds_dfm)


# GENERATE START & PRIOR
#===============================================================================
set.seed(1234)
s <- create_start(redgaurds_wfm)
p <- create_prior()

# RUN GENERALIZED WORDFISH & CREATE A TIDY DATAFRAME
#===============================================================================
# poisIRT: approximate Bayesian inference for textual data
# 
# Iteration: 50
# Iteration: 100
# Iteration: 150
# Iteration: 200
# Iteration: 250
# Iteration: 300
# Iteration: 350
# Iteration: 400
# Done in 440 iterations, using 0 threads.

control <- list(threads = parallel::detectCores()-1, verbose = FALSE, 
                thresh = 1e-6, maxit = 5000)
pooled_outcome <- emIRT::poisIRT(.rc = redgaurds_wfm, 
                                 i = 0:(ncol(redgaurds_wfm)-1), 
                                 NI = ncol(redgaurds_wfm), 
                                 .starts = s,
                                 .priors = p,
                                 .control = control)

redguard_estimates <- get_estimates(pooled_outcome) %>%
  left_join(incident[,c("id_doc", "activist", "fact_eng")], by = "id_doc")  
# CLEAN UNUSED OBJECTS TO SAVE MEMORIES
#===============================================================================
rm(list=setdiff(ls(), c("redgaurds_dfm", "redgaurds_wfm", "kyw_object", 
                        "redguard_estimates", "pooled_outcome", "conll")))


# SAVE OUTPUTS
#===============================================================================
# save(pooled_outcome, file="data/pooled_outcome.RData")
# save(redgaurds_wfm, file="data/redgaurds_wfm.RData")
# save(redgaurds_dfm, file="data/redgaurds_dfm.RData")
# save(redguard_estimates, file="data/redguard_estimates.RData")

#====================================END========================================

})



cat(" ============================================================================================================\n",
    "=",
    "Replication Task 03 is done!", "|",  
    names(timer_task03[1]), ":", timer_task03[[1]],  "|",
    names(timer_task03[2]), ":", timer_task03[[2]],  "|",
    names(timer_task03[3]), ":", timer_task03[[3]],  "|",
    "Core used :",parallel::detectCores(), "              =", "\n", 
    "============================================================================================================")
