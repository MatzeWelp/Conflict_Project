#load libraries
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(ggpubr)
library(plyr)
library(tidyverse)
library(fs)

csv_path <- ("E:\\PYTHON_ST\\breakCSV_PYTHON\\AIM_2_regions\\Afghanistan\\selected0")

#set work directory for folder with all csv. files
file_paths <- fs::dir_ls("E:\\PYTHON_ST\\breakCSV_PYTHON\\AIM_2_regions\\Afghanistan")

#call function
results_matrix <- sapply(file_paths, get_coeffs)

#coefs_DF <- function(file = "E:\\PYTHON_ST\\breakCSV_PYTHON\\AIM_2_regions\\Afghanistan\\selected0.csv") {
get_coeffs <- function(csv_path) {  
  #df <- read.csv("E:\\PYTHON_ST\\breakCSV_PYTHON\\AIM_2_regions\\Afghanistan\\selected{}.csv", header = TRUE)
  df <- read.csv("E:\\PYTHON_ST\\breakCSV_PYTHON\\AIM_2_regions\\Afghanistan\\selected0", header = TRUE)
  
  # do all the following steps in every file                                        
  
  # Step 1) 
  # Define years to divide table
  
  #select conflict year in df 
  ConflictYear = df[1,'year']
  ConflictYear
  
  # select Start year of regression in df
  SlopeYears = df[1,'slopeYears'] #to get slope years (e.g.17)
  SlopeYears
  BCStartYear = ConflictYear-SlopeYears #to get start year for regression
  BCStartYear
  
  #End year of regression
  ACEndYear = ConflictYear+(SlopeYears-1) # -1 because the conflict year is included
  ACEndYear
  
  
  # Step 2
  
  #Define BeforeConf and AfterConf depending on Slope Year number and Conflict Years
  #Go through NDVI.Year till Conflict.Year (-1 year) since the conflict year is not included in bc
  BeforeConf1 <- df[ which(df$NDVI_Year >= 1999 & df$NDVI_Year <= 2015),] #eg. 1982 to 1999
  BeforeConf2 <- c("NDVI_Year", "NDVI", "T", "Prec", "soilM") #which columns to include
  BeforeConf <- BeforeConf1[BeforeConf2] #create table
  
  AfterConf1 <- df[which(df$NDVI_Year >= ConflictYear & df$NDVI_Year <= ACEndYear),] #eg. 1999 to 2015
  AfterConf2 <-  c("NDVI_Year", "NDVI", "T", "Prec", "soilM")
  AfterConf <- AfterConf1[AfterConf2]
  
  #Step 3)a)
  #create empty list, to fill with coefficient results from each model results for each csv file and safe in new list
  
  
  #Apply Multiple Linear Regression
  model <- lm(NDVI ~ T + Prec + soilM, data = BeforeConf)
  return(summary(model)$coefficients[1:4,1])
}
    




#Or: Step 4)b)
    #Getting Coefs from all datasets and safe in new list
    model <- lm(NDVI ~ T.annual.max + Prec.annual.max + soilM.annual.max, data = BeforeConf)
    summary(model)
    coef_df <- list()
    coef_df <- coefficients(model)


#Part 2

#average coeficients and put them in formular
mean_intercept <- mean(coef_df[["(Intercept)"]]) 
mean_b1 <- mean(coef_df[["BeforeConf$T.annual.max"]])
mean_b2 <- mean(coef_df[["BeforeConf$Prec.annual.max"]])
mean_b3 <- mean(coef_df[["BeforeConf$soilM.annual.max"]])

#                      =      intercept +     b1    *        x1              +        b2  *       x2                  +     b3    *         x3     
NDVI_AfterConf_Climate = mean_intercept + ((mean_b2)*AfterConf$T.annual.max) +  ((mean_b2)*AfterConf$Prec.annual.max) + ((mean_b3)*AfterConf$soilM.annual.max)
NDVIDifference = NDVI_AfterConf_Climate - AfterConf$NDVI
    
    