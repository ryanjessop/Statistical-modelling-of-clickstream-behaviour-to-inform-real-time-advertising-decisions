# Prepare data for models

library(ggplot2)
library("PerformanceAnalytics")
library(dplyr)
library(ggplot2)
library(pscl)
library(boot)
library(foreign)
library(MASS)
library(lmtest)
library(vcd)
library(car)
library(caret)
library(caTools)
library(car)
library(qualityTools)
library(stats)
library(readr)
library(plotly)
library(minerva)
library(reshape2)
library(gridExtra) 
options(scipen = 999) 
library(corrplot) 
library(RColorBrewer)
library(funModeling) 
library(corrplot)
library(lubridate)
library(tidyverse)

sample_active_adwords$click_bool <- NA
zero_index = which(sample_active_adwords$ClicksDelta == 0)
non_zero_index = which(sample_active_adwords$ClicksDelta >= 1)
sample_active_adwords$click_bool[zero_index] <- FALSE
sample_active_adwords$click_bool[non_zero_index] <- TRUE

# Data sampling 
set.seed(101); sample = sample.split(sample_active_adwords$PartitionKey, SplitRatio = .75)
train = subset(sample_active_adwords, sample == TRUE)
test  = subset(sample_active_adwords, sample == FALSE)

# Factor levels
train$CreativeQualityScore <- factor(train$CreativeQualityScore)
train$CreativeQualityScore <- relevel(train$CreativeQualityScore, ref = 'Average')

train$KeywordMatchType<- factor(train$KeywordMatchType)
train$KeywordMatchType <- relevel(train$KeywordMatchType, ref = 'Broad')

train$hod <- factor(train$hod)
train$hod <- relevel(train$hod, ref = '6am-12pm')


