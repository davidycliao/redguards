#!/usr/bin/Rscript

#===============================================================================
# File Names       : 04.incidents-ideal-estimates.R 
# Date             : 31st Oct 2021
# Authors          : David Yen-Cheih Liao
# Purpose          : ideal point estimation based on text features.
# Required Dataset : redgaurds_wfm.Rdata;       
# Output Data      : 
#===============================================================================

timer_task04 <- system.time({
# REQUIRED PACKAGES
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
load("data/individual_list.RData")
load("data/dfm_individual_list.RData")
load("data/dict.RData")


# BUILDING A DICTIONARY OBJECT IN QUANTEDA
#===============================================================================
# Tokenize the document based on individual participant
# Note : split the data by each incident and save them into list object re-index 
# the incident index in to numeric number in order 

# individual_list <- split(individuals, individuals$incident_index)
# dfm_individual_list <- individual_list %>%
#   map("content") %>%
#   map(corpus)
# for (i in 1:length(dfm_individual_list)){
#   docnames(dfm_individual_list[[i]]) <- individual_list[[i]]$id_doc}
# 
# save(dfm_individual_list, file= "data/dfm_individual_list.RData")
# save(individual_list, file= "data/individual_list.RData")


doParallel::registerDoParallel(parallel::makeCluster(detectCores()-1))   

redgaurds_dfm_individual <- foreach::foreach(i = 1:length(dfm_individual_list),
                             .combine= list, .multicombine=TRUE) %dopar% 
  {quanteda::dfm(dfm_individual_list[[i]], dictionary = dict[[i]])}

parallel::stopCluster(parallel::makeCluster(detectCores()-1))                  


# BUILDING DOCUNEBTS-TERM-MATRIX IN QUANTEDA & AUSTIN
#===============================================================================
redgaurds_wfm_individual <- map(redgaurds_dfm_individual, dtm_wfm)
redgaurds_wfm_individual[[10]]

# GENERATE STARTS & PRIORS
#===============================================================================
# set.seed(1234)
s_list <- vector("list")
for (i in 1:length(redgaurds_wfm_individual)){
  s_list[[i]] <- create_start(redgaurds_wfm_individual[[i]])
}
p <- create_prior()

# RUN GENERALIZED WORDFISH 
#===============================================================================
em_poisIRT <- list()
control <- {list(threads = parallel::detectCores()-1, verbose = FALSE, thresh = 1e-6, maxit = 20000)}
for (i in 1:length(redgaurds_wfm_individual)) {
  em_poisIRT[[i]] <- poisIRT(.rc = redgaurds_wfm_individual[[i]],
                             i = 0:(ncol(redgaurds_wfm_individual[[i]])-1),
                             NI = ncol(redgaurds_wfm_individual[[i]]),
                             .starts = s_list[[i]],
                             .priors = p,
                             .control = control)
}



# CREATE A TIDY DATAFRAME FOR VISUALIZATION
#===============================================================================
# poisIRT_dataframe <- list()
# for (i in 1:length(em_poisIRT)){
#   poisIRT_dataframe[[i]]  <- data.frame(x = em_poisIRT[[i]]$means$x,
#                                         sd = sqrt(em_poisIRT[[i]]$vars$x),
#                                         lower = em_poisIRT[[i]]$means$x - 1.96*sqrt(em_poisIRT[[i]]$vars$x),
#                                         upper = em_poisIRT[[i]]$means$x + 1.96*sqrt(em_poisIRT[[i]]$vars$x),
#                                         id_doc = as.double(rownames(em_poisIRT[[i]]$means$psi))) %>%
#     left_join(individual_list[[i]][,c("id_doc", "unit_id", "fact_eng")], by = "id_doc")
# }
# 

poisIRT_dataframe <- NULL
poisIRT_dataframe <- map(em_poisIRT, get_estimates)
for (i in 1:length(em_poisIRT)){
  poisIRT_dataframe[[i]] <- poisIRT_dataframe[[i]] %>%
    left_join(individual_list[[i]][,c("id_doc", "fact_eng")], by = "id_doc")
}





incidents <- list("The First Marxist-Leninist \n  Wall Poster",
                  "Red August",
                  "Zhou Enlai's Declaration",
                  "The Announcement of New Public \n Security Regulations", 
                  "The February Countercurrent",
                  "The Little General",
                  "The Wuhan Incident",
                  "The First Great \n Tiananmen Rally",
                  "100-day Clashes",
                  "Mao Zedong's Summons",
                  "Shanghai January Storm")

incidents_index <- NULL
for (i in 1:length(poisIRT_dataframe)){
  poisIRT_dataframe[[i]]["incidents"] <- incidents[[i]]
  poisIRT_dataframe[[i]]["incidents_index"] <- i
}

individual_idea_point <- do.call(rbind.data.frame, poisIRT_dataframe)

individual_idea_point$incidents <- factor(rbind_idealpoint$incidents, 
                                          levels=c("The First Marxist-Leninist \n  Wall Poster",
                                                    "Red August",
                                                    "Zhou Enlai's Declaration",
                                                    "The Announcement of New Public \n Security Regulations", 
                                                    "The February Countercurrent",
                                                    "The Little General",
                                                    "The Wuhan Incident",
                                                    "The First Great \n Tiananmen Rally",
                                                    "100-day Clashes",
                                                    "Mao Zedong's Summons",
                                                    "Shanghai January Storm"))




# SAVE OUTPUTS
#===============================================================================
# save(individual_idea_point, file = "data/pooled_outcome.RData")
# save(poisIRT_dataframe, file = "data/poisIRT_dataframe.RData")




})




cat(" ============================================================================================================\n",
    "=",
    "Replication Task 04 is done!", "|",  
    names(timer_task04[1]), ":", timer_task04[[1]],  "|",
    names(timer_task04[2]), ":", timer_task04[[2]],  "|",
    names(timer_task04[3]), ":", timer_task04[[3]],  "|",
    "Core used :",parallel::detectCores(), "              =", "\n", 
    "============================================================================================================")

# 
# while (isTRUE(exists("timer_task04")))
# {
#   for (i in 1:length(myVec)) {
#     
#     setTxtProgressBar(bar, i)
#     
#     Sys.sleep(0.1)  
#     
#   } 
# }
# 
# rm(timer_task04)
