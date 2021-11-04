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
# (No need to load the packages if replication is running in the pacakge )
#===============================================================================
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(
#   tidyverse, lubridate, dplyr, purrr, tibble,           # Tidyverse  
#   tidyr, tidyr, readxl, data.table,                     # Data Pre-processings
#   parallel, future, furrr, future.apply,                # Parallel Computing
#   doParallel, foreach, doFuture, 
#   quanteda, tmcn, austin, udpipe, textrank,             # NLP toolkit
#   emIRT                                                 # Generalized Wordfish
# )


# REQUIRED DATASET 
#===============================================================================
load("data/dfm_list.RData")
load("data/incident_list.RData")
# load("data/dict.Rdata")


# BUILDING DOCUNEBTS-TERM-MATRIX IN QUANTEDA & AUSTIN
#===============================================================================
# Tokenize the document based on each political incidents
# incident_list <- split(incident, incident$incident_index)
# 
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
p <- list(
  # x$mu numeric, prior mean for actor ideal points x_i.; 
  # x$sigma2 numeric, prior variance for actor ideal points x_i.
  psi = list(mu = 0,  sigma2 = 10000),         
  # beta$mu numeric, prior mean for β_j
  # beta$sigma2 numeric, prior variance for β_j.        
  alpha = list(mu = 0, sigma2 = 10000),    
  # alpha$mu numeric, prior mean for α_j
  # alpha$sigma2 numeric, prior variance for α_j        
  beta = list(mu = 0, sigma2 = 10000),
  # psi$mu numeric, prior mean for ψ_k
  # psi$sigma2 numeric, prior variance for ψ_k.
  x = list(mu = 0, sigma2 = 10000)
)   

set.seed(1234)
s <- create_start(redgaurds_wfm)



# RUN GENERALIZED WORDFISH & CREATE A TIDY DATAFRAME
#===============================================================================
control <- {list(threads = 1, verbose = TRUE, thresh = 1e-6, maxit = 1000)}
pooled_outcome <- poisIRT(.rc = redgaurds_wfm, 
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
                        "redguard_estimates", "pooled_outcome")))


# SAVE OUTPUTS
#===============================================================================
# save(pooled_outcome, file="data/pooled_outcome.RData")
# save(redgaurds_wfm, file="data/redgaurds_wfm.RData")
# save(redgaurds_dfm, file="data/redgaurds_dfm.RData")
# save(redguard_estimates, file="data/redguard_estimates.RData")

#====================================END========================================


})


cat(" ====================\n",
    "=",
    "Task 03 Is Done!", "=", "\n",
    "====================",
    "\n Core used :",  detectCores(), 
    "\n Time spent \n", 
    names(timer_task03[1]), ":",   timer_task03[[1]], "\n",
    names(timer_task03[2]), " :",  timer_task03[[2]], "\n",
    names(timer_task03[3]), "  :", timer_task03[[3]], "\n",
    "====================\n")
