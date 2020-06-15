# Adwords code - Chapter 2

# Imports
library(ggplot2)
library("PerformanceAnalytics")
library(dplyr)
library(readr)
library(pscl)
library(foreign)
library(MASS)
library(plotly)
library(minerva)
library(reshape2)
library(gridExtra) 
options(scipen = 999) 
library(caret)
library(corrplot)
library(RColorBrewer)
library(funModeling) 
library(vcd)
library(lubridate)
library(tidyverse)
library(arm)
library(webshot)

#-----------------------------------------------------------
# Read in data

# Data from keyword reports 
# Date range: 01/08/2017-01/10/2017
adword_data <- read_csv("adword_data.csv")
View(adword_data)
nrow(adword_data)

# Check for non-UK data - filter 
uk_adword_data <- adword_data[which(adword_data$AccountTimeZone == "(GMT+00:00) GMT (no daylight saving)"),] 
nrow(uk_adword_data)

# List all variables 
names(uk_adword_data) # Full list of variables 
# List of variables to keep (for this test manual selection)
keep_vars <- c("RowKey" ,"PartitionKey", "AdGroupId", "CampaignId" , 
               "Clicks", "ClicksDelta", "Conversions" , "ConversionDelta", 
               "CreativeQualityScore", "FirstPageCpc" , "FirstPositionCpc",
               "Impressions" , "ImpressionDelta", "Keyword" , "KeywordId" , 
               "KeywordMatchType",  "AveragePosition" , "Cost" , 
               "EstimatedActualPosition", "MaxCpc" , "QualityScore" )
keep_vars
# Remove redundant variables
uk_adword_data <- uk_adword_data[keep_vars] 
names(uk_adword_data)
nrow(uk_adword_data)
View(uk_adword_data)

#-----------------------------------------------------------
# Low volume mature keywords (LVMK)

# Using MaxCpc to look at low volume mature keywords and hence only model active keywords
table(uk_adword_data$MaxCpc)  # 1,472,991 observations with 2.22

lmvk <- uk_adword_data[ which(uk_adword_data$MaxCpc == 2.22),] 
nrow(lmvk)
View(lmvk)

# Plot for lmvk - Impression vs Clicks, color=MaxCpc
plot_ly(data=lmvk[sample(nrow(lmvk), 500), ],
        x=~Impressions, 
        y=~Clicks, 
        color=~MaxCpc, 
        type="scatter", 
        mode="markers")

# Some clicks due to random boost
table(lmvk$Impressions)
table(lmvk$Clicks)

#-----------------------------------------------------------
# Subset of data - not LMVK

# Filter out lmvk
active_adwords <- uk_adword_data[which(uk_adword_data$MaxCpc != 2.22),]  # 383,256 observations
nrow(active_adwords) # 383256

# Convert to data.frame
active_adwords = data.frame(active_adwords)

# CampaignId distribution
table(active_adwords$CampaignId) 

# KeywordId distribution
table(active_adwords$KeywordId) 

#-----------------------------------------------------------
# Cleaning steps

# Bad value for Creative Quality score 
active_adwords <- active_adwords[which(active_adwords$CreativeQualityScore != "--"),]
# Removes 576 observations
table(active_adwords$CreativeQualityScore)

# Quality score, 0 == No score given, fix this for modelling later
active_adwords[, c('QualityScore')][active_adwords[, c('QualityScore')] == 0] <- NA
table(active_adwords$QualityScore)

# Average position, 0 == No position calculated due to no impressions
active_adwords[, c('AveragePosition')][active_adwords[, c('AveragePosition')] == 0] <- NA
table(active_adwords$AveragePosition)

# Estimated actual position, 100, <1 == No position calculated due to no impressions. 
active_adwords[, c('EstimatedActualPosition')][active_adwords[, c('EstimatedActualPosition')] == 100] <- NA
active_adwords[, c('EstimatedActualPosition')][active_adwords[, c('EstimatedActualPosition')] < 1 ] <- NA
active_adwords[, c('EstimatedActualPosition')][active_adwords[, c('EstimatedActualPosition')] > 10 ] <- NA
table(active_adwords$EstimatedActualPosition)

# Negative ImpressionDelta, ClickDelta, ConversionDelta
active_adwords <- active_adwords[ which(active_adwords$ImpressionDelta >= 0),]
table(active_adwords$ImpressionDelta)
active_adwords <- active_adwords[ which(active_adwords$ClicksDelta >= 0),]
table(active_adwords$ClicksDelta)
active_adwords <- active_adwords[ which(active_adwords$ConversionDelta >= 0),]
table(active_adwords$ConversionDelta)

# Clean when impressions delta are less than clicks delta
active_adwords <- active_adwords[-which(active_adwords$ImpressionDelta < active_adwords$ClicksDelta),]

#-----------------------------------------------------------
# New variables 

# Creates the variables for click-through-rate, conversion rate, cost-per-click 
impression_index = which(active_adwords$ImpressionDelta > 0)
click_index = which(active_adwords$ClicksDelta > 0)
active_adwords$ctr <- NA
active_adwords$cvr <- NA
active_adwords$cpc <- NA
active_adwords$ctr[impression_index] <- active_adwords$ClicksDelta[impression_index] / active_adwords$ImpressionDelta[impression_index]
active_adwords$cvr[click_index] <- active_adwords$ConversionDelta[click_index] / active_adwords$ClicksDelta[click_index] 
active_adwords$cpc[click_index] <- active_adwords$Cost[click_index] / active_adwords$Clicks[click_index]
View(active_adwords)

# Discretization of current variables

# Convert string to timestamp
active_adwords$PartitionKey <- as.POSIXlt(active_adwords$PartitionKey)

# Find day of week split as binary/categorical variable
# Mon-Fri == Weekday
# Sat-Sun == Weekend
active_adwords$dow <- NA
weekend_index = which(active_adwords$PartitionKey$wday >= 5)
weekday_index = which(active_adwords$PartitionKey$wday < 5 )
active_adwords$dow[weekend_index] <- "Weekend"
active_adwords$dow[weekday_index] <- "Weekday"

# Find hour of day split as categorical variable
# 12am-6am == Night
# 6am-12pm == Morning
# 12pm-6pm == Midday
# 6pm-12am == Evening
active_adwords$hod <- NA
morning_index = which(active_adwords$PartitionKey$hour >= 6 & active_adwords$PartitionKey$hour< 12)
midday_index = which(active_adwords$PartitionKey$hour >= 12 & active_adwords$PartitionKey$hour < 18)
evening_index = which(active_adwords$PartitionKey$hour >= 18)
night_index = which(active_adwords$PartitionKey$hour <= 5)
active_adwords$hod[morning_index] <- "6am-12pm"
active_adwords$hod[midday_index] <- "12pm-6pm"
active_adwords$hod[evening_index] <- "6pm-12am"
active_adwords$hod[night_index] <- "12am-6am"

# Quality score discretization: 
# 1-3 = Low,
# 4-7 = Medium
# 8-10 = High
active_adwords$quality <- NA
low_index = which(active_adwords$QualityScore <= 3)
medium_index = which(active_adwords$QualityScore >= 4 | active_adwords$QualityScore <= 7)
high_index = which(active_adwords$QualityScore >= 8)
active_adwords$quality[low_index] <- "Low"
active_adwords$quality[medium_index] <- "Medium"
active_adwords$quality[high_index] <- "High"

# Impressions label
# TRUE = At elast one impression in hour
# FALSE = No impressions in hour
active_adwords$impression <- NA
true_index = which(active_adwords$ImpressionDelta > 0)
false_index = which(active_adwords$ImpressionDelta == 0)
active_adwords$impression[true_index] <- TRUE
active_adwords$impression[false_index] <- FALSE

# Estimated actual position discretization 
active_adwords$eap <- NA
high_index = which(active_adwords$EstimatedActualPosition <= 2)
mid_index = which(active_adwords$EstimatedActualPosition >= 3 & active_adwords$EstimatedActualPosition <= 5)
low_index = which(active_adwords$EstimatedActualPosition >= 6)
active_adwords$eap[high_index] <- "1-2"
active_adwords$eap[mid_index] <- "3-5"
active_adwords$eap[low_index] <- "6+"

#-----------------------------------------------------------
# Variable distributions

# Shows the distribution of the AdGroup ID's
table(active_adwords$AdGroupId) 

# Shows the distribution of the Campiagn ID's
table(active_adwords$CampaignId) 

# Sample of active keywords for plots
sample_active_adwords <- active_adwords[which(active_adwords$CampaignId == 908693882 | active_adwords$CampaignId == 908693885),]
set.seed(100)
sample_active_adwords = sample_active_adwords[sample(nrow(sample_active_adwords), 100000), ]
nrow(sample_active_adwords)
View(sample_active_adwords)

# Time
fig = plot_ly(data = sample_active_adwords,
        x=~as_date(PartitionKey),
        type="histogram",
        color = I('black')) %>%
        layout(
          xaxis = list(title = "", tickfont=list(size=20)),
          yaxis = list(title = "Count", titlefont=list(size=30), tickfont=list(size=20)))

orca(fig, file = "images/date_dist.pdf")

quantile(sample_active_adwords$PartitionKey, probs = c(seq(0, 1, 0.25)))
         
# Keyword match type distribution
table(sample_active_adwords$KeywordMatchType)
fig = plot_ly(data = sample_active_adwords,
        x=~KeywordMatchType, 
        type="histogram") %>%
  layout(
    xaxis = list(title = "", tickfont=list(size=20)),
    yaxis = list(title = "Count", titlefont=list(size=30), tickfont=list(size=20)))

orca(fig, file = "images/kmt_dist.pdf")

fig = plot_ly(data=impr_data,
        x=~ImpressionDelta,
        y=~ClicksDelta, 
        symbol = ~KeywordMatchType, 
        symbols = c('o','x'),
        color = I('black'), 
        type="scatter", 
        mode="markers") %>%
  layout(
    xaxis = list(title = "ImpressionsDelta", titlefont=list(size=30), tickfont=list(size=20)),
    yaxis = list(title = "ClicksDelta", titlefont=list(size=30), tickfont=list(size=20)),
    legend = list(font=list(size=20)))

orca(fig, file = "images/kmt_scatter.pdf")

# Average position distribution
table(active_adwords$AveragePosition)
fig = plot_ly(data = sample_active_adwords,
        x=~AveragePosition, 
        type="histogram", 
        color = I('orange')) %>%
  layout(
    xaxis = list(title = "", tickfont=list(size=20)),
    yaxis = list(title = "Count", titlefont=list(size=30), tickfont=list(size=20)))

orca(fig, file = "images/avg_pos_dist.pdf")

fig = plot_ly(data=impr_data,
        x=~ImpressionDelta,
        y=~ClicksDelta, 
        type="scatter", 
        mode="markers", 
        marker = list(
          color=~AveragePosition, 
          colorscale = 'Hot',
          colorbar=list(tickfont=list(size=20))
          )
        ) %>%
  layout(
    xaxis = list(title = "ImpressionsDelta", titlefont=list(size=30), tickfont=list(size=20)),
    yaxis = list(title = "ClicksDelta", titlefont=list(size=30), tickfont=list(size=20)))

orca(fig, file = "images/position_scatter.pdf")

quantile(impr_data$AveragePosition, probs = c(seq(0, 1, 0.25)))

# Structure: 
# |-- tabular form of distribution
# |-- histogram of single variable
# |-- scatter plot of impressions vs clicks - coloured by variable

# Histogram of impressions distribution
table(active_adwords$ImpressionDelta)
impr_data = sample_active_adwords[which(sample_active_adwords$ImpressionDelta > 0),]
fig = plot_ly(data = sample_active_adwords,
        x=~ImpressionDelta,
        type="histogram",
        color=I('purple')) %>%
  layout(
    xaxis = list(title = "", tickfont=list(size=20)),
    yaxis = list(title = "Count", titlefont=list(size=30), tickfont=list(size=20),
                 type = "log"))

orca(fig, file = "images/impressions_dist.pdf")

quantile(sample_active_adwords$ImpressionDelta, probs = c(seq(0, 1, 0.25)))


# Histogram of distribution of clicks, Exponential decay curve
click_data <- sample_active_adwords[which(sample_active_adwords$ClicksDelta > 0),]
table(active_adwords$ClicksDelta)
table(impr_data$ClicksDelta)
fig = plot_ly(data=impr_data,
        x=~ClicksDelta,
        type="histogram",
        color=I('red')) %>%
  layout(
    xaxis = list(title = "", tickfont=list(size=20)),
    yaxis = list(title = "Count", titlefont=list(size=30), tickfont=list(size=20)))

orca(fig, file = "images/clicks_dist.pdf")

quantile(impr_data$ClicksDelta, probs = c(seq(0, 1, 0.25)))

# Histogram of distribution of conversions 
table(active_adwords$ConversionDelta)  
table(impr_data$ConversionDelta)
fig = plot_ly(data=impr_data,
        x=~as.factor(ConversionDelta),
        type="histogram",
        color=I('pink')) %>%
  layout(
    xaxis = list(title = "", tickfont=list(size=20)),
    yaxis = list(title = "Count", titlefont=list(size=30), tickfont=list(size=20)))

orca(fig, file = "images/conversions_dist.pdf")

quantile(impr_data$ConversionDelta, probs = c(seq(0, 1, 0.25)))

# Creative quality score distribution 
table(active_adwords$CreativeQualityScore)
fig = plot_ly(data = sample_active_adwords[which(sample_active_adwords$CreativeQualityScore != 'Not applicable'),],
        x=~CreativeQualityScore, 
        type="histogram",
        color=I('dark red')) %>%
  layout(
    xaxis = list(title = "", tickfont=list(size=20)),
    yaxis = list(title = "Count", titlefont=list(size=30), tickfont=list(size=20)))

orca(fig, file = "images/cqs_dist.pdf")

fig = plot_ly(data=impr_data,
        x=~ImpressionDelta,
        y=~ClicksDelta, 
        color=~CreativeQualityScore, 
        type="scatter", 
        mode="markers") %>%
  layout(
    xaxis = list(title = "ImpressionsDelta", titlefont=list(size=30), tickfont=list(size=20)),
    yaxis = list(title = "ClicksDelta", titlefont=list(size=30), tickfont=list(size=20)),
    legend = list(font=list(size=20)))

orca(fig, file = "images/creative_scatter.pdf")

# Quality score distibution before discretization
table(active_adwords$QualityScore)
fig = plot_ly(data = sample_active_adwords,
        x=~QualityScore, 
        type="histogram",
        color=I('dark green')) %>%
  layout(
    xaxis = list(title = "", tickfont=list(size=20)),
    yaxis = list(title = "Count", titlefont=list(size=30), tickfont=list(size=20)))

orca(fig, file = "images/kqs_dist.pdf")

plot_ly(data=impr_data,
        x=~ImpressionDelta,
        y=~ClicksDelta, 
        color=~QualityScore, 
        type="scatter", 
        mode="markers")%>%
  layout(
    xaxis = list(title = "ImpressionsDelta"),
    yaxis = list(title = "ClicksDelta"))

# Quality score after discretization
table(active_adwords$quality)
plot_ly(data = sample_active_adwords,
        x=~quality, 
        type="histogram") %>%
  layout(
    xaxis = list(title = "KeywordQualityScore Discretized"),
    yaxis = list(title = "Count"))
plot_ly(data=impr_data,
        x=~ImpressionDelta,
        y=~ClicksDelta, 
        color=~quality, 
        type="scatter", 
        mode="markers") %>%
  layout(
    xaxis = list(title = "ImpressionsDelta"),
    yaxis = list(title = "ClicksDelta"))

# MaxCpc distribution
table(active_adwords$MaxCpc)
fig = plot_ly(data = sample_active_adwords,
        x=~MaxCpc, 
        type="histogram",
        color=I('dark grey')) %>%
  layout(
    xaxis = list(title = "", tickfont=list(size=20)),
    yaxis = list(title = "Count", titlefont=list(size=30), tickfont=list(size=20),
                 type = 'log'))

orca(fig, file = "images/maxcpc_dist.pdf")

table(sample_active_adwords$KeywordMatchType)
table(sample_active_adwords$KeywordMatchType, sample_active_adwords$ImpressionDelta)

# 4% of broad match have at least one impression
# 6% of exact match have at least one impression

sum(sample_active_adwords$ImpressionDelta[sample_active_adwords$KeywordMatchType == 'Broad'])
sum(sample_active_adwords$ImpressionDelta[sample_active_adwords$KeywordMatchType == 'Exact'])

# 81% of impressions are from broad match
# 19% of impressions are from exact match

plot_ly(data=impr_data,
        x=~ImpressionDelta,
        y=~ClicksDelta, 
        color=~MaxCpc, 
        size = ~MaxCpc,
        type="scatter", 
        mode="markers") %>%
  layout(
    xaxis = list(title = "ImpressionsDelta"),
    yaxis = list(title = "ClicksDelta"))

# Day of week distribution
table(active_adwords$dow)
plot_ly(data = sample_active_adwords,
        x=~dow, 
        type="histogram") %>%
  layout(
    xaxis = list(title = "Day of week"),
    yaxis = list(title = "Count"))

plot_ly(data=impr_data,
        x=~ImpressionDelta,
        y=~ClicksDelta, 
        color=~dow, 
        type="scatter", 
        mode="markers") %>%
  layout(
    xaxis = list(title = "ImpressionsDelta"),
    yaxis = list(title = "ClicksDelta"))

# Hour of day distribution
table(active_adwords$hod)
plot_ly(data = sample_active_adwords,
        x=~hod, 
        type="histogram") %>%
  layout(
    xaxis = list(title = "Hour of day"),
    yaxis = list(title = "Count"))
quantile(sample_active_adwords$PartitionKey$hour, probs = c(seq(0, 1, 0.25)))

sum(sample_active_adwords$ImpressionDelta[sample_active_adwords$hod == '12pm-6pm'])
sum(sample_active_adwords$ImpressionDelta[sample_active_adwords$hod == '12am-6am'])
sum(sample_active_adwords$ImpressionDelta[sample_active_adwords$hod == '6am-12pm'])
sum(sample_active_adwords$ImpressionDelta[sample_active_adwords$hod == '6pm-12am'])

impr_data$hod <- NA
morning_index = which(impr_data$PartitionKey$hour >= 6 & impr_data$PartitionKey$hour< 12)
midday_index = which(impr_data$PartitionKey$hour >= 12 & impr_data$PartitionKey$hour < 18)
evening_index = which(impr_data$PartitionKey$hour >= 18)
night_index = which(impr_data$PartitionKey$hour <= 5)
impr_data$hod[morning_index] <- "6am-12pm"
impr_data$hod[midday_index] <- "12pm-6pm"
impr_data$hod[evening_index] <- "6pm-12am"
impr_data$hod[night_index] <- "12am-6am"

plot_ly(data = impr_data,
        x=~hod, 
        type="histogram") %>%
  layout(
    xaxis = list(title = "Hour of day"),
    yaxis = list(title = "Count"))

fig = plot_ly(data=impr_data[which(!is.na(impr_data$hod)),],
        x=~ImpressionDelta,
        y=~ClicksDelta, 
        color=~hod, 
        type="scatter", 
        mode="markers") %>%
  layout(
    xaxis = list(title = "ImpressionsDelta", titlefont=list(size=30), tickfont=list(size=20)),
    yaxis = list(title = "ClicksDelta", titlefont=list(size=30), tickfont=list(size=20)),
    legend = list(font=list(size=20)))

orca(fig, file = "images/hod_scatter.pdf")

# Impressions label: binary variable
table(active_adwords$impression)
plot_ly(data = sample_active_adwords,
        x=~impression, 
        type="histogram")
plot_ly(data=sample_active_adwords,
        x=~Impressions,
        y=~Clicks, 
        color=~impression, 
        type="scatter", 
        mode="markers")

# Estimated actual position distribution (after discretization)
table(active_adwords$eap)
plot_ly(data = sample_active_adwords,
        x=~eap, 
        type="histogram")
plot_ly(data=sample_active_adwords,
        x=~Impressions,
        y=~Clicks, 
        color=~eap, 
        type="scatter", 
        mode="markers")

#-----------------------------------------------------------
# Campaign choice - choose richest dataset ?
sample_active_adwords$camp_id <- as.factor(as.character(sample_active_adwords$CampaignId))
table(sample_active_adwords$camp_id)
plot_ly(data=sample_active_adwords, 
        x=~ImpressionDelta,
        y=~ClicksDelta, 
        color=~camp_id,
        type="scatter", 
        mode="markers")

one_campaign <- active_adwords[which(active_adwords$CampaignId == 908693882),]
table(one_campaign$ClicksDelta)

sample_one_campaign <- one_campaign[sample(nrow(one_campaign), 500), ]

# Make sure there is variety in these scatter plots
plot_ly(data=sample_one_campaign,
        x=~ImpressionDelta,
        y=~ClicksDelta,
        color=~CreativeQualityScore,
        type="scatter", 
        mode="markers")

plot_ly(data=sample_one_campaign,
        x=~ImpressionDelta,
        y=~ClicksDelta,
        color=~KeywordMatchType,
        type="scatter", 
        mode="markers")

plot_ly(data=sample_one_campaign,
        x=~ImpressionDelta,
        y=~ClicksDelta,
        color=~QualityScore,
        type="scatter", 
        mode="markers")

#-----------------------------------------------------------
# Investigating relationships between variables 

sample_active_adwords

# No ImpressionDelta and Quality score
no_show <- active_adwords[which(active_adwords$ImpressionDelta == 0),]

table(no_show$QualityScore)
plot_ly(data = no_show, 
        x=~QualityScore, 
        type="histogram")

table(no_show$CreativeQualityScore)
plot_ly(data = no_show,
        x=~CreativeQualityScore, 
        type="histogram")

# Create contingency tables & Association plots
# CQS vs QS
tblA = table(impr_data$QualityScore, impr_data$CreativeQualityScore)
tbla = tblA[,1:3]
chisq.test(tblA)
assoc(tblA, shade=TRUE)
col_names <- c('Below average','Average','Above average')
M <- matrix(tbla, nrow(tbla), ncol(tbla))
M[,col_names]
fig = plot_ly(x=colnames(tbla),
        y=rownames(tbla),
        z=matrix(tbla, nrow(tbla), ncol(tbla)), 
        type='heatmap',
        colors = colorRamp(c("white", "blue")),
        colorbar=list(tickfont=list(size=20))) %>%
  hide_colorbar() %>%
  layout(
    xaxis = list(title = "CreativeQuality", titlefont=list(size=30), tickfont=list(size=20)),
    yaxis = list(title = "KeywordQuality", titlefont=list(size=30), tickfont=list(size=20))
  )

orca(fig, file = "images/quality_heatmap_legend.pdf")

# EPA vs QS
tblB = table(impr_data$eap, 
             impr_data$QualityScore)
tblB
chisq.test(tblB)
assoc(tblB, shade=TRUE)

# KMT vs KQS 
tblC = table(impr_data$QualityScore, impr_data$KeywordMatchType)
tblC
chisq.test(tblC)
assoc(tblC, shade=TRUE)
fig = plot_ly(x=colnames(tblC),
        y=rownames(tblC),
        z=matrix(tblC, nrow(tblC), ncol(tblC)), 
        type='heatmap',
        colors = colorRamp(c("white", "blue")),
        colorbar=list(title='Count', titlefont=list(size=20), tickfont=list(size=20)))  %>%
  layout(
    xaxis = list(title = "KeywordMatchType", titlefont=list(size=30), tickfont=list(size=20)),
    yaxis = list(title = "KeywordQuality", titlefont=list(size=30), tickfont=list(size=20))
  )

orca(fig, file = "images/keyword_heatmap_legend.pdf")

# CQS vs EPA
tblD = table(impr_data$CreativeQualityScore, 
             impr_data$eap)
tblD
chisq.test(tblD)
assoc(tblD, shade=TRUE)
plot_ly(x=colnames(tblD),
        y=rownames(tblD),
        z=matrix(tblD, nrow(tblD), ncol(tblD)), 
        type='heatmap',
        colors = colorRamp(c("white", "blue")))  %>%
  layout(
    xaxis = list(title = "CreativeQuality"),
    yaxis = list(title = "eap"))

# Crossplots
# Only for non-count data as all entries for ZeroImpressions column are FALSE
cross_plot(active_adwords, str_input = "CreativeQualityScore", 
           str_target = "ImpressionDelta", plot_type = "percentual")

cross_plot(active_adwords, str_input = "quality", 
           str_target = "Impressions", plot_type = "percentual")

cross_plot(active_adwords, str_input = "eap", 
           str_target = "impression", plot_type = "percentual")

#-----------------------------------------------------------
# Large plots for used keywords 

# impression == TRUE
used_index <- which(active_adwords$impression == TRUE)
rich_data <- active_adwords[used_index,]
rich_data_sample <- rich_data[sample(nrow(rich_data), 1000), ]
rich_data_sample <- rich_data_sample[!(is.na(rich_data_sample$EstimatedActualPosition)), ]
View(rich_data_sample)

# Numerical variables 
impr_data$ImpressionsDelta <- impr_data$ImpressionDelta 
impr_data$ConversionsDelta <- impr_data$ConversionDelta 
colnames(impr_data)
numerical <- c('ImpressionsDelta', 'ClicksDelta', 
               'ConversionsDelta', 'AveragePosition', 
               'MaxCpc')

# Pairs plot
pairs(impr_data[numerical], upper.panel = NULL)

# Correlation plot
cor_matrix <- cor(impr_data[numerical])
cor_matrix
corrplot(cor_matrix, 
         method='number', 
         type='lower',
         tl.srt = 30)
