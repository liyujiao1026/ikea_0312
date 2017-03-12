library(Synth)
library(plyr)
library(compiler)
library(dplyr)
library(kohonen)

path <- 'https://raw.githubusercontent.com/liyujiao1026/ikea_0312/master/'
source(paste0(path,'1_Func_SCM.R'), echo = F)
source(paste0(path,'2_Func_cluster.R'), echo = F)
source(paste0(path,'3_Func_bootstrapSCM.R'), echo = F)
source(paste0(path,'4_Func_parametric.R'), echo = F)
source(paste0(path,'5_Func_paper.R'), echo = F)

