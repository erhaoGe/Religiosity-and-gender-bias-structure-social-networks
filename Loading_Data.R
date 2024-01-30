##### Data
Edgelist_socialsupport <- 
  readRDS("Edgelist_socialsupport.rds")
networkcapital <- 
  readRDS("networkcapital.rds")
networkcapital2 <- 
  readRDS("networkcapital2.rds")
networkcapital3 <- 
  readRDS("networkcapital3.rds")
Other_covariate_matrix <- 
  readRDS("Other_covariate_matrix.rds")

kinship_matrix <- Other_covariate_matrix[[1]]
Kinship_affinal_all_matrix <- Other_covariate_matrix[[2]]
gps_matrix <- Other_covariate_matrix[[3]]

Personal_network <- 
  readRDS("Personal_network.rds")

##### Packages
library(tidyverse)
library(magrittr)
library(lme4)
library(AICcmodavg)
library(gtsummary)
library(ggeffects)
library(naniar)
library(broom)
library(broom.mixed)
library(sna)
library(ergm)
library(intergraph)
library(texreg)
library(hrbrthemes)
library(viridis)
library(ggpubr)
library(stargazer)
library(performance)
library(MASS)
library(MuMIn)
