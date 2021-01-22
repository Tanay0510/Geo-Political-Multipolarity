# --- [~] Stepwise Regression --- [~] #
# http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/

# --- Regression in R --- #

# Set Working Directory [Mac = ~/Desktop | PC = C:/Users/Default/Desktop]
setwd("~/Desktop")

# Get working directory
getwd()

# Get list of files in working directory
list.files()

if("dplyr" %in% rownames(installed.packages()) == FALSE) {
install.packages("dplyr", dependencies = TRUE)}

if("rms" %in% rownames(installed.packages()) == FALSE) {
install.packages("rms", dependencies = TRUE)}

if("psych" %in% rownames(installed.packages()) == FALSE) {
install.packages("psych", dependencies = TRUE)}

if("ggplot2" %in% rownames(installed.packages()) == FALSE) {
install.packages("ggplot2", dependencies = TRUE)}

if("ggpubr" %in% rownames(installed.packages()) == FALSE) {
install.packages("ggpubr", dependencies = TRUE)}

if("ggcorrplot" %in% rownames(installed.packages()) == FALSE) {
install.packages("ggcorrplot", dependencies = TRUE)}

# Add Package Libraries
library(dplyr)
library(rms)
library(psych)
library(ggplot2)
library(ggpubr)
library(ggcorrplot)

# Load the dataset by reading CSV file

firepower.data <- read.csv("globalization.csv")

# Convert all blank values to NA
firepower.data[firepower.data==""] <- NA

# Get summary of all data
summary(firepower.data)

# Get summary of all obesity rate data
summary(firepower.data$CINC)
	        
# --- Regression --- #           
               
# Create a basic linear regression formula
# Compare the obesity rate against all of the variables
# CINC ~ .
# Remove Country,Full.Name, Region,Sub.Region, United.Nations.Status, Political.Region, Military.Alliance, UN.HDI.Rank, CINC.x.10.000, Regional.GDP, Regional.UN.HDI, Nuclear.WeaponsDetails b/c they are categorical variables


df = subset(firepower.data, select = -c(Country,Full.Name,Region,Sub.Region,United.Nations.Status,Political.Region,Military.Alliance,UN.HDI.Rank,CINC.x.10.000,Regional.GDP....,Regional.UN.HDI....,Nuclear.Weapons..Details.))


firepower.model <- lm(CINC ~ ., data = df)

summary(firepower.model)

# Create a pruned model by removing all variables that do not have asterisks next to them (*, **, ***)

# Stars (*) means significant predictors

# (*) 95% of the time
# (**) = 99% of the time
# (***) = true almost all the cases (less than 1/1000)
# One intercept is always significant (intercept should be called offset)

firepower.model.pruned <- lm(CINC ~ Country.GDP..US.million.  + Population + Oil.Reserves..millions.barrels. + HDI...Change..1.Yr.  + IEF + Final.Military.Str..Score + Active.Military  + Reserve.Military + X1000.Capita..Tot.  + Aircraft.Carriers + Amphibious.War.Ship  + Cruisers + Destroyers + Frigates + Corvettes + Attack.Helicopters + Military.Satellites + Nuclear.Weapons..Total. + Nuclear.Weapons..Exist., data = df)

summary(firepower.model.pruned)

# Get VIF values from model for multicollinearity

vif(firepower.model.pruned)

# Remove any variables with a VIF over 10

# Country.GDP..US.million. - 67.491718
# Final.Military.Str..Score  - 16.581231 
# Active.Military - 13.056200  
# Aircraft.Carriers - 83.034172   
# Amphibious.War.Ship - 66.255562                      
# Cruisers - 646.868943 
# Destroyers  - 116.555426 
# Attack.Helicopters - 88.210877          
# Military.Satellites  - 202.588644
# Nuclear.Weapons..Exist. - 870.301259 
# price.ratio.fruit.per.pkg.savory.snacks - 6.563584 

# Create a final model

firepower.model.final <- lm(CINC ~  + HDI...Change..1.Yr. + Population + Oil.Reserves..millions.barrels. + IEF + Reserve.Military + X1000.Capita..Tot. + Frigates + Nuclear.Weapons..Total. + Corvettes, data = df)

summary(firepower.model.final)

# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                     -1.027e-03  1.230e-03  -0.835  0.40490    
# HDI...Change..1.Yr.              9.280e-02  3.048e-01   0.305  0.76108    
# Population                       5.105e-11  6.177e-12   8.264 2.43e-14 ***
# Oil.Reserves..millions.barrels.  6.732e-09  1.590e-08   0.423  0.67252    
# IEF                             -2.642e-06  1.020e-05  -0.259  0.79587    
# Reserve.Military                -8.527e-09  2.831e-09  -3.012  0.00295 ** 
# X1000.Capita..Tot.               1.103e-05  2.490e-05   0.443  0.65837    
# Frigates                         1.443e-03  1.710e-04   8.438 8.28e-15 ***
# Nuclear.Weapons..Total.          2.587e-03  1.064e-03   2.431  0.01598 *  
# Corvettes                        6.186e-04  1.272e-04   4.862 2.44e-06 ***
  

# Multiple R-squared:  0.8243,	Adjusted R-squared:  0.8159 
# F-statistic: 98.51 on 9 and 189 DF,  p-value: < 2.2e-16

# Get VIF values from model for final multicollinearity check
vif(firepower.model.final)

# HDI...Change..1.Yr. - 1.157262                   
# Population - 2.484355 
# Oil.Reserves..millions.barrels - 1.154360 
# IEF - 1.080729             
# Reserve.Military - 2.691129       
# X1000.Capita..Tot. - 1.476128
# Frigates - 2.585946        
# Nuclear.Weapons..Total. - 1.784793        
# Corvettes - 2.767763


# Model Coefficients
coefficients(firepower.model.final)

# Confidence Intervals for Model Parameters
confint(firepower.model.final, level=0.95) # CIs for model parameters

# diagnostic plots to check model validity
layout(matrix(c(1,2,3,4),2,2))
plot(firepower.model.final)

# ANOVA Table
anova(firepower.model.final)

# Residuals from Analysis
# residuals(obesity.model.final)

# Statistics for Residuals Analysis
# influence(obesity.model.final)

