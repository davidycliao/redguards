
#===============================================================================
# File Names       : 00.package.R 
# Date             : 31st Oct 2021
# Authors          : David Yen-Cheih Liao
# Purpose          : load required packages beforehand
# Required Dataset : 
# Output Data      : 
#===============================================================================

# REQUIRED PACKAGES
#===============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, lubridate, dplyr, purrr, tibble,           # Tidyverse
  tidyr, tidyr, data.table,                             # Data Pre-processings
  parallel, future, furrr, future.apply,                # Parallel Computing
  doParallel, foreach, doFuture,
  ggplot2, ggpubr, ggrepel, wesanderson, ggraph,        # Visualization Toolkit
  cowplot, lattice, igraph, ggforce, tidygraph,
  tidyverse, lubridate, dplyr, purrr, tibble,           # Tidyverse Toolkit  
  quanteda, tmcn, austin, udpipe, textrank,             # NLP toolkit
  emIRT,                                                # Generalized Wordfish
  redguards
)


#====================================END========================================

cat(" Replication starts ..... \n")

