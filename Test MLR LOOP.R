#load libraries
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(ggpubr)
library(plyr)
library(tidyverse)
library(fs)


file_paths <- fs::dir_ls("E:\\PYTHON_ST\\breakCSV_PYTHON\\AIM_2_regions\\Afghanistan")
file_paths

#create empty list and fill with file paths and loop through them
file_contents <- list()
for (i in seq_along(file_paths)) { #seq_along for vectors (list of file paths is a vector)
  file_contents[[i]] <- read_csv(file = file_paths[[i]])
  
  for (i in seq_len(file_contents[[i]])){ # redundant?
    
    # do all the following steps in every file                                        
    
    # Step 1) 
    # Define years to divide table
    
    #select conflict year in df 
    ConflictYear = file_contents[[i]][1,9]
    ConflictYear
    
    # select Start year of regression in df
    SlopeYears = file_contents[[i]][1,7] #to get slope years (e.g.17)
    BCStartYear = ConflictYear-SlopeYears #to get start year for regression
    BCStartYear
    
    #End year of regression
    ACEndYear = ConflictYear+(SlopeYears-1) # -1 because the conflict year is included
    ACEndYear
    
    
    # Step 2
    
    #select needed rows from df
    #no headers but row numbers. NDVI.Year = [r1-r34,c2]
    NDVI.Year <- file_contents[[i]][1:34,2]
    NDVI <- file_contents[[i]][1:34,21]
    T.annual.max <- file_contents[[i]][1:34,19]
    Prec.annual.max <- file_contents[[i]][1:34,20]
    soilM.annual.max <- file_contents[[i]][1:34,18]
    
    #Define BeforeConf and AfterConf depending on Slope Year number and Conflict Years
    #Go through NDVI.Year till Conflict.Year (-1 year) since the conflict year is not included in bc
    BeforeConf1 <- file_contents[[i]][ which(file_contents[[i]]$NDVI.Year >= BCStartYear & file_contents[[i]]$NDVI.Year < ConflictYear),] #eg. 1982 to 1999
    BeforeConf2 <-  c(NDVI.Year, NDVI, T.annual.max, Prec.annual.max, soilM.annual.max) #which columns to include
    BeforeConf <- BeforeConf1[BeforeConf2] #create table
    
    AfterConf1 <- myFiles[ which(file_contents[[i]]$NDVI.Year >= ConflictYear & file_contents[[i]]$NDVI.Year <= ACEndYear),] #eg. 1999 to 2015
    AfterConf2 <-  c(NDVI.Year, NDVI, T.annual.max, Prec.annual.max, soilM.annual.max)
    AfterConf <- AfterConf1[AfterConf2]
    
    #Step 3)a)
    #create empty list, to fill with coefficient results from each model results for each csv file and safe in new list
    
    #Create an empty df for the output coefficients
    names <- c("(Intercept)","BeforeConf$T.annual.max","BeforeConf$Prec.annual.max","BeforeConf$soilM.annual.max")
    coef_df <- data.frame()
    for (k in names) coef_df[[k]] <- as.character() 
    
    #Apply Multiple Linear Regression
    plyrFunc <- function(x){
      model <- lm(NDVI ~ T.annual.max + Prec.annual.max + soilM.annual.max, data = BeforeConf)
      return(summary(model)$coefficients[1,1:4])
    }
    
    coef_df <- ddply(BeforeConf, .(), x)
    coef_DF
  }}
#Or: Step 4)b)
#Getting Coefs from all datasets and safe in new list
model <- lm(NDVI ~ T.annual.max + Prec.annual.max + soilM.annual.max, data = BeforeConf)
summary(model)
coef_df <- list()
coef_df <- coefficients(model)
