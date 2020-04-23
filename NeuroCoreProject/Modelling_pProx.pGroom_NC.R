######## Modelling Logistic Regressions on A2

library(lme4)# Generalized Linear Mixed Models
library(glmmTMB)
library(bbmle)#Tools for General Maximum Likelihood Estimation
library(DHARMa) #residual diagnostic fr hierarchical (multi-level/mixed) regression models
library(jtools)
library(data.table)
library(ggpubr)
library(dplyr)
library(fitdistrplus)
library(lmtest)
library(performance)

#Load AllScans file
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("Social_Network_Analysis/CalcSubsampledScans.R")
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")

#Run Subssampling procedure
ExSubScans = calcRandomScans(allScans)

#Model predicting p(proximity)
isNotAlone <- glmer(isProx~ isPost + sex + age + percentrank + group + (1|focalID) + (1|year), data = ExSubScans, family = binomial) #Note: might want to try MCMCglmm?
summary(isNotAlone)
check_model(isNotAlone) #Check model assumptions

export_summs(isNotAlone, model.names = c("p(Proximity).Model"), digits=3,
             to.file = "docx", file.name = "pProximityModel.docx") #Export Model

#Model predicting p(grooming)
isSocial <- glmer(isSocial~ isPost + sex + age + percentrank + group + (1|focalID) + (1|year), data = ExSubScans, family = binomial)
summary(isSocial)
check_model(isSocial)
export_summs(isSocial, model.names = c("p(Grooming).Model"), digits=3,
             to.file = "docx", file.name = "pGroomingModel.docx")