# Exploratory data analysis for Chapter 3

library(plyr)
library(funModeling)  # contains heart_disease data
library(minerva)  # contains MIC statistic
library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra)  # allow us to plot two plots in a row
options(scipen = 999) 
library(caret)
library(corrplot) # library wto plot that matrix
library(RColorBrewer)
library("PerformanceAnalytics")
library(pscl)
library(foreign)
library(MASS)
library(lmtest)
library(vcd)
library(car)
library(stats)
library(caTools)
library(plotly)
packageVersion('plotly')
library(corrgram)
library(BBmisc)
library(tidyr)
library(readr)
library(psych)
library(anytime) 

#----------------------------------------------------------------------
# Read in data set

rfmp <- read_csv("rfmp-hit2.csv")
head(rfmp)
names(rfmp)
str(rfmp)
nrow(rfmp)

# Set factor variables 
rfmp$category1 <- as.factor(rfmp$category1)
rfmp$category2 <- as.factor(rfmp$category2)

# Convert dates to seconds to days (decimals)
rfmp$recent_date_day <- as.numeric(rfmp$recent_date)/(60*60*24) - 17537
rfmp$min_date_day <- as.numeric(rfmp$min_date)/(60*60*24) - 17537
rfmp$avg_date_day <- as.numeric(rfmp$avg_date)/(60*60*24) - 17537
rfmp$median_date_day <- as.numeric(rfmp$median_date)/(60*60*24) - 17537

#----------------------------------------------------------------------
# Choose subset to speed up creation of plots 
set.seed(101) 
sample = sample.split(rfmp$pid, SplitRatio = .1)
rfmp_sample = subset(rfmp, sample == TRUE)
nrow(rfmp_sample)

#----------------------------------------------------------------------
# Single variables distributions 

# Tables
table(rfmp_sample$category1)

table(rfmp_sample$day_count)
table(rfmp_sample$weekday_count)
table(rfmp_sample$weekend_count)

table(rfmp_sample$recent_date)
table(rfmp_sample$min_date)
table(rfmp_sample$avg_date)
table(rfmp_sample$date_range)
table(rfmp_sample$median_date)
table(rfmp_sample$max_hit_date)

table(rfmp_sample$avg_inter_visit)
table(rfmp_sample$max_inter_visit)
table(rfmp_sample$min_inter_visit)
table(rfmp_sample$inter_visit_range)
table(rfmp_sample$inter_visit_median)

table(rfmp_sample$avg_hits)
table(rfmp_sample$median_hits)
table(rfmp_sample$max_hits)
table(rfmp_sample$min_hits)
table(rfmp_sample$range_hits)

# Plots
plot_ly(rfmp_sample, x = ~category1, type = "histogram")

# Frequency 
fig = plot_ly(rfmp_sample, 
        x = ~day_count, 
        type = "histogram") %>% 
  layout(
    xaxis = list(title = "", tickfont=list(size=20)),
    yaxis = list(title = "Count", titlefont=list(size=30), tickfont=list(size=20)))

orca(fig, file = "images/DayCount_hist.pdf")

fig = plot_ly(rfmp_sample, 
        x = ~weekday_count, 
        type = "histogram",
        color = "red") %>% 
  layout(
    xaxis = list(title = "", tickfont=list(size=20)),
    yaxis = list(title = "Count", titlefont=list(size=30), tickfont=list(size=20)))

orca(fig, file = "images/WeekdayCount_hist.pdf")

fig = plot_ly(rfmp_sample, 
        x = ~weekend_count, 
        type = "histogram",
        color = I("dark green")) %>% 
  layout(
    xaxis = list(title = "", tickfont=list(size=20)),
    yaxis = list(title = "Count", titlefont=list(size=30), tickfont=list(size=20)))

orca(fig, file = "images/WeekendCount_hist.pdf")

# Variability
fig = plot_ly(rfmp_sample, 
        x = ~round(avg_inter_visit), 
        type = "histogram",
        color = I("pink")) %>% 
  layout(
    xaxis = list(title = "", tickfont=list(size=20)),
    yaxis = list(title = "Count", titlefont=list(size=30), tickfont=list(size=20)))

orca(fig, file = "images/AverageInterVisit_hist.pdf")

fig = plot_ly(rfmp_sample, 
        x = ~max_inter_visit, 
        type = "histogram",
        color = I("black")) %>% 
  layout(
    xaxis = list(title = "", tickfont=list(size=20)),
    yaxis = list(title = "Count", titlefont=list(size=30), tickfont=list(size=20)))

orca(fig, file = "images/MaximumInterVisit_hist.pdf")

plot_ly(rfmp_sample, x = ~min_inter_visit, type = "histogram")

fig = plot_ly(rfmp_sample, 
        x = ~inter_visit_range, 
        type = "histogram",
        color = I("grey")) %>% 
  layout(
    xaxis = list(title = "", tickfont=list(size=20)),
    yaxis = list(title = "Count", titlefont=list(size=30), tickfont=list(size=20)))

orca(fig, file = "images/RangeInterVisit_hist.pdf")

plot_ly(rfmp_sample, x = ~inter_visit_median, type = "histogram")

# Recency
fig = plot_ly(rfmp_sample, 
        x = ~anydate(avg_date), 
        type = "histogram",
        color = I("purple")) %>% 
  layout(
    xaxis = list(title = "", tickfont=list(size=20), tickangle = 90),
    yaxis = list(title = "Count", titlefont=list(size=30), tickfont=list(size=20)))

orca(fig, file = "images/AverageDate_hist.pdf")

plot_ly(rfmp_sample, x = ~date_range, type = "histogram")
plot_ly(rfmp_sample, x = ~median_date, type = "histogram")
plot_ly(rfmp_sample, x = ~max_hit_date, type = "histogram")

fig = plot_ly(rfmp_sample, 
        x = ~recent_date, 
        type = "histogram",
        color = I("light blue")) %>% 
  layout(
    xaxis = list(title = "", tickfont=list(size=20), tickangle = 90),
    yaxis = list(title = "Count", titlefont=list(size=30), tickfont=list(size=20)))

orca(fig, file = "images/RecentDate_hist.pdf")

fig = plot_ly(rfmp_sample, 
        x = ~min_date, 
        type = "histogram",
        color = I("dark red")) %>% 
  layout(
    xaxis = list(title = "", tickfont=list(size=20), tickangle = 90),
    yaxis = list(title = "Count", titlefont=list(size=30), tickfont=list(size=20)))

orca(fig, file = "images/OldestDate_hist.pdf")

# Intensity
fig = plot_ly(rfmp_sample, 
        x = ~round(avg_hits), 
        type = "histogram",
        color = I("red")) %>% 
  layout(
    xaxis = list(title = "", tickfont=list(size=20)),
    yaxis = list(title = "Count", titlefont=list(size=30), tickfont=list(size=20)))

orca(fig, file = "images/AverageHits_hist.pdf") 

plot_ly(rfmp_sample, x = ~median_hits, type = "histogram")

fig = plot_ly(rfmp_sample, 
        x = ~max_hits, 
        type = "histogram",
        color = I("light green")) %>% 
  layout(
    xaxis = list(title = "", tickfont=list(size=20)),
    yaxis = list(title = "Count", titlefont=list(size=30), tickfont=list(size=20)))

orca(fig, file = "images/MaximumHits_hist.pdf") 

plot_ly(rfmp_sample, x = ~min_hits, type = "histogram")

fig = plot_ly(rfmp_sample, 
        x = ~range_hits, 
        type = "histogram",
        color = I("orange")) %>% 
  layout(
    xaxis = list(title = "", tickfont=list(size=20)),
    yaxis = list(title = "Count", titlefont=list(size=30), tickfont=list(size=20)))

orca(fig, file = "images/RangeHits_hist.pdf") 

# Quantiles 
quantile(rfmp_sample$day_count, probs = c(seq(0, 1, 0.25)))
quantile(rfmp_sample$weekday_count, probs = c(seq(0, 1, 0.25)))
quantile(rfmp_sample$weekend_count, probs = c(seq(0, 1, 0.25)))

quantile(rfmp_sample$recent_date, probs = c(seq(0, 1, 0.25)))
quantile(rfmp_sample$min_date, probs = c(seq(0, 1, 0.25)))
quantile(rfmp_sample$avg_date, probs = c(seq(0, 1, 0.25)))
quantile(rfmp_sample$median_date, probs = c(seq(0, 1, 0.25)))
quantile(rfmp_sample$max_hit_date, probs = c(seq(0, 1, 0.25)))

quantile(rfmp_sample$date_range, probs = c(seq(0, 1, 0.25)))
quantile(rfmp_sample$inter_visit_range, probs = c(seq(0, 1, 0.25)))
quantile(rfmp_sample$inter_visit_median, probs = c(seq(0, 1, 0.25)))
quantile(rfmp_sample$avg_inter_visit, probs = c(seq(0, 1, 0.25)))
quantile(rfmp_sample$max_inter_visit, probs = c(seq(0, 1, 0.25)))
quantile(rfmp_sample$min_inter_visit, probs = c(seq(0, 1, 0.25)))

quantile(rfmp_sample$avg_hits, probs = c(seq(0, 1, 0.25)))
quantile(rfmp_sample$median_hits, probs = c(seq(0, 1, 0.25)))
quantile(rfmp_sample$max_hits, probs = c(seq(0, 1, 0.25)))
quantile(rfmp_sample$min_hits, probs = c(seq(0, 1, 0.25)))
quantile(rfmp_sample$range_hits, probs = c(seq(0, 1, 0.25)))

#----------------------------------------------------------------------
# Scatterplots 

plot_ly(data = rfmp_sample, x = ~recent_date, y = ~day_count)
plot_ly(data = rfmp_sample, x = ~recent_date, y = ~min_date)
plot_ly(data = rfmp_sample, x = ~recent_date, y = ~avg_inter_visit)
plot_ly(data = rfmp_sample, x = ~recent_date, y = ~max_inter_visit)
plot_ly(data = rfmp_sample, x = ~recent_date, y = ~min_inter_visit)
plot_ly(data = rfmp_sample, x = ~recent_date, y = ~avg_date)
plot_ly(data = rfmp_sample, x = ~recent_date, y = ~date_range)
plot_ly(data = rfmp_sample, x = ~recent_date, y = ~median_date)
plot_ly(data = rfmp_sample, x = ~recent_date, y = ~inter_visit_range)
plot_ly(data = rfmp_sample, x = ~recent_date, y = ~inter_visit_median)

plot_ly(data = rfmp_sample, x = ~day_count, y = ~min_date)
plot_ly(data = rfmp_sample, x = ~day_count, y = ~avg_inter_visit)
plot_ly(data = rfmp_sample, x = ~day_count, y = ~max_inter_visit)
plot_ly(data = rfmp_sample, x = ~day_count, y = ~min_inter_visit)
plot_ly(data = rfmp_sample, x = ~day_count, y = ~avg_date)
plot_ly(data = rfmp_sample, x = ~day_count, y = ~date_range)
plot_ly(data = rfmp_sample, x = ~day_count, y = ~median_date)
plot_ly(data = rfmp_sample, x = ~day_count, y = ~inter_visit_range)
plot_ly(data = rfmp_sample, x = ~day_count, y = ~inter_visit_median)

plot_ly(data = rfmp_sample, x = ~min_date, y = ~avg_inter_visit)
plot_ly(data = rfmp_sample, x = ~min_date, y = ~max_inter_visit)
plot_ly(data = rfmp_sample, x = ~min_date, y = ~min_inter_visit)
plot_ly(data = rfmp_sample, x = ~min_date, y = ~avg_date)
plot_ly(data = rfmp_sample, x = ~min_date, y = ~date_range)
plot_ly(data = rfmp_sample, x = ~min_date, y = ~median_date)
plot_ly(data = rfmp_sample, x = ~min_date, y = ~inter_visit_range)
plot_ly(data = rfmp_sample, x = ~min_date, y = ~inter_visit_median)

plot_ly(data = rfmp_sample, x = ~avg_inter_visit, y = ~max_inter_visit)
plot_ly(data = rfmp_sample, x = ~avg_inter_visit, y = ~min_inter_visit)
plot_ly(data = rfmp_sample, x = ~avg_inter_visit, y = ~avg_date)
plot_ly(data = rfmp_sample, x = ~avg_inter_visit, y = ~date_range)
plot_ly(data = rfmp_sample, x = ~avg_inter_visit, y = ~median_date)
plot_ly(data = rfmp_sample, x = ~avg_inter_visit, y = ~inter_visit_range)
plot_ly(data = rfmp_sample, x = ~avg_inter_visit, y = ~inter_visit_median)

plot_ly(data = rfmp_sample, x = ~max_inter_visit, y = ~min_inter_visit)
plot_ly(data = rfmp_sample, x = ~max_inter_visit, y = ~avg_date)
plot_ly(data = rfmp_sample, x = ~max_inter_visit, y = ~date_range)
plot_ly(data = rfmp_sample, x = ~max_inter_visit, y = ~median_date)
plot_ly(data = rfmp_sample, x = ~max_inter_visit, y = ~inter_visit_range)
plot_ly(data = rfmp_sample, x = ~max_inter_visit, y = ~inter_visit_median)

plot_ly(data = rfmp_sample, x = ~min_inter_visit, y = ~avg_date)
plot_ly(data = rfmp_sample, x = ~min_inter_visit, y = ~date_range)
plot_ly(data = rfmp_sample, x = ~min_inter_visit, y = ~median_date)
plot_ly(data = rfmp_sample, x = ~min_inter_visit, y = ~inter_visit_range)
plot_ly(data = rfmp_sample, x = ~min_inter_visit, y = ~inter_visit_median)

plot_ly(data = rfmp_sample, x = ~avg_date, y = ~date_range)
plot_ly(data = rfmp_sample, x = ~avg_date, y = ~median_date)
plot_ly(data = rfmp_sample, x = ~avg_date, y = ~inter_visit_range)
plot_ly(data = rfmp_sample, x = ~avg_date, y = ~inter_visit_median)

plot_ly(data = rfmp_sample, x = ~date_range, y = ~median_date)
plot_ly(data = rfmp_sample, x = ~date_range, y = ~inter_visit_range)
plot_ly(data = rfmp_sample, x = ~date_range, y = ~inter_visit_median)

plot_ly(data = rfmp_sample, x = ~median_date, y = ~inter_visit_range)
plot_ly(data = rfmp_sample, x = ~median_date, y = ~inter_visit_median)

plot_ly(data = rfmp_sample, x = ~inter_visit_range, y = ~inter_visit_median)

plot_ly(data = rfmp_sample, x = ~avg_hits, y = ~max_hits)
plot_ly(data = rfmp_sample, x = ~min_hits, y = ~max_hits)
plot_ly(data = rfmp_sample, x = ~range_hits, y = ~avg_hits)
plot_ly(data = rfmp_sample, x = ~avg_hits, y = ~median_hits)
plot_ly(data = rfmp_sample, x = ~range_hits, y = ~max_hit_date)

#----------------------------------------------------------------------
# Correlations

# Frequency
frequency_vars_rfmp_sample <- rfmp_sample %>% 
  select(c('day_count', 'weekday_count', 'weekend_count')) %>%
  rename(DayCount = day_count) %>%
  rename(WeekdayCount = weekday_count) %>%
  rename(WeekendCount = weekend_count)

pairs(frequency_vars_rfmp_sample,
      upper.panel = NULL, 
      cex.axis=2,
      cex.labels = 3)

res <- cor(frequency_vars_rfmp_sample)
round(res, 2)
corrplot(res, method="color",
         type="lower", number.cex=0.6,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="red", tl.srt=60, tl.cex = 0.6,
         diag=FALSE, is.corr = F 
)

# Intensity
intensity_vars_rfmp_sample <- rfmp_sample %>% 
  select( c('avg_hits', 
            'max_hits',
            'median_hits',
            'min_hits', 
            'range_hits')) %>%
  rename(AverageHits = avg_hits) %>%
  rename(MaximumHits = max_hits) %>%
  rename(MedianHits = median_hits) %>%
  rename(MinimumHits = min_hits) %>%
  rename(RangeHits = range_hits)

pairs(intensity_vars_rfmp_sample,
      upper.panel = NULL, 
      cex.axis=2,
      cex.labels = 3)

res <- cor(intensity_vars_rfmp_sample)
round(res, 2)
corrplot(res, method="color",
         type="lower", number.cex=0.6,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="red", tl.srt=60, tl.cex = 0.6,
         diag=FALSE, is.corr = F 
)

# Recency

# Convert dates to seconds
rfmp_sample$recent_date <- (as.numeric(rfmp_sample$recent_date) - 1515196800 ) / 86400
rfmp_sample$min_date <- (as.numeric(rfmp_sample$min_date) - 1515196800 ) / 86400
rfmp_sample$avg_date <- (as.numeric(rfmp_sample$avg_date) - 1515196800 ) / 86400
rfmp_sample$median_date <- (as.numeric(rfmp_sample$median_date) - 1515196800 ) / 86400
rfmp_sample$max_hit_date <- (as.numeric(rfmp_sample$max_hit_date) - 1515196800 ) / 86400

recency_vars_rfmp_sample <- rfmp_sample %>% 
  select(c('avg_date',
           'max_hit_date',
           'median_date',
           'min_date',
           'recent_date')) %>%
  rename(AverageDate = avg_date) %>%
  rename(MaxHitCountDate = max_hit_date) %>%
  rename(MedianDate = median_date) %>%
  rename(OldestDate = min_date) %>%
  rename(RecentDate = recent_date)

pairs(recency_vars_rfmp_sample,
      upper.panel = NULL, 
      cex.axis=2,
      cex.labels = 2.75)

res <- cor(recency_vars_rfmp_sample)
round(res, 2)
corrplot(res, method="color",
         type="lower", number.cex=0.6,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="red", tl.srt=60, tl.cex = 0.6,
         diag=FALSE, is.corr = F 
)

# Variability
variability_vars_rfmp_sample <- rfmp_sample %>% 
  select(c('avg_inter_visit', 
           'date_range',
           'max_inter_visit',
           'inter_visit_median',
           'min_inter_visit',
           'inter_visit_range')) %>%
  rename(AverageInterVisit = avg_inter_visit) %>%
  rename(DateRange = date_range) %>%
  rename(MaximumInterVisit = max_inter_visit) %>%
  rename(MedianInterVisit = inter_visit_median) %>%
  rename(MinimumInterVisit = min_inter_visit) %>% 
  rename(RangeInterVisit = inter_visit_range)

pairs(variability_vars_rfmp_sample,
      upper.panel = NULL, 
      cex.axis=1.8,
      cex.labels = 2.2)

res <- cor(variability_vars_rfmp_sample)
round(res, 2)
corrplot(res, method="color",
         type="lower", number.cex=0.6,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="red", tl.srt=60, tl.cex = 0.6,
         diag=FALSE, is.corr = F 
)

# All statistical variables 
res <- cor(cbind(frequency_vars_rfmp_sample,
                 intensity_vars_rfmp_sample,
                 recency_vars_rfmp_sample,
                 variability_vars_rfmp_sample)
)
corrplot(res, method="color",
         type="lower", number.cex=0.6,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="red", tl.srt=28, tl.cex = 0.6,
         diag=FALSE, is.corr = F 
)

#----------------------------------------------------------------------
# Boxplots for categorical interests 
rfmp_sample_cut <- rfmp_sample[!(rfmp_sample$category1 == "b2b" |
                                   rfmp_sample$category1 == "cameras" |
                                   rfmp_sample$category1 == "legal" |
                                   rfmp_sample$category1 == "leisure" |
                                   rfmp_sample$category1 == "musical instruments" |
                                   rfmp_sample$category1 == "office products" |
                                   rfmp_sample$category1 == "outdoor living" |
                                   rfmp_sample$category1 == "seasonal" |
                                   rfmp_sample$category1 == "renters" |
                                   rfmp_sample$category1 == "seniors" |
                                   rfmp_sample$category1 == "software" |
                                   rfmp_sample$category1 == "tools" |
                                   rfmp_sample$category1 == "video games" |
                                   rfmp_sample$category1 == "wedding" ),]

plot_ly(data = rfmp_sample_cut, 
        y = ~day_count, 
        color = ~category1, 
        type = "box") %>% 
  layout(
    xaxis = list(title = "PageVisitCategoryID"),
    yaxis = list(title = "DayCount"))

plot_ly(data = rfmp_sample_cut, 
        y = ~recent_date, 
        color = ~category1, 
        type = "box") %>% 
  layout(
    xaxis = list(title = "PageVisitCategoryID"),
    yaxis = list(title = "RecentDate"))

plot_ly(data = rfmp_sample_cut, y = ~min_date, color = ~category1, type = "box")
plot_ly(data = rfmp_sample_cut, y = ~avg_date, color = ~category1, type = "box")
plot_ly(data = rfmp_sample_cut, y = ~date_range, color = ~category1, type = "box")
plot_ly(data = rfmp_sample_cut, y = ~median_date, color = ~category1, type = "box")

plot_ly(data = rfmp_sample_cut, 
        y = ~avg_inter_visit, 
        color = ~category1, 
        type = "box") %>% 
  layout(
    xaxis = list(title = "PageVisitCategoryID"),
    yaxis = list(title = "AverageInterVisit"))

plot_ly(data = rfmp_sample_cut, y = ~max_inter_visit, color = ~category1, type = "box")
plot_ly(data = rfmp_sample_cut, y = ~min_inter_visit, color = ~category1, type = "box")
plot_ly(data = rfmp_sample_cut, y = ~inter_visit_range, color = ~category1, type = "box")
plot_ly(data = rfmp_sample_cut, y = ~inter_visit_median, color = ~category1, type = "box")

plot_ly(data = rfmp_sample_cut, 
        y = ~avg_hits, 
        color = ~category1, 
        type = "box") %>% 
  layout(
    xaxis = list(title = "PageVisitCategoryID"),
    yaxis = list(title = "AverageHits"))

plot_ly(data = rfmp_sample_cut, y = ~range_hits, color = ~category1, type = "box")
plot_ly(data = rfmp_sample_cut, y = ~median_hits, color = ~category1, type = "box")
plot_ly(data = rfmp_sample_cut, y = ~max_hits, color = ~category1, type = "box")
plot_ly(data = rfmp_sample_cut, y = ~min_hits, color = ~category1, type = "box")

#----------------------------------------------------------------------
# Discretization

# Frequency 
plot_ly(frequency_vars_rfmp_sample[!(frequency_vars_rfmp_sample$DayCount < 3),], 
        x = ~DayCount, 
        type = "histogram")
quantile(frequency_vars_rfmp_sample[!(frequency_vars_rfmp_sample$DayCount < 3),]$DayCount, 
         probs = c(seq(0, 1, 0.1)))

plot_ly(frequency_vars_rfmp_sample[!(frequency_vars_rfmp_sample$DayCount < 4),], 
        x = ~DayCount, 
        type = "histogram")
quantile(frequency_vars_rfmp_sample[!(frequency_vars_rfmp_sample$DayCount < 4),]$DayCount, 
         probs = c(seq(0, 1, 0.1)))

layout(plot_ly(data = frequency_vars_rfmp_sample, x = ~DayCount, type = "histogram"),
       xaxis = list(title = "DayCount"),
       yaxis = list(title = "Count"),
       shapes = list(
         list(type = "rect",
              fillcolor = brewer.pal(9, "Set1")[1], line = list(color = brewer.pal(9, "Set1")[1]), opacity = 0.4,
              x0 = 1.5, x1 = quantile(frequency_vars_rfmp_sample$DayCount, probs = 0.80) + 0.5, xref = "x",
              y0 = 0, y1 = length(which(frequency_vars_rfmp_sample$DayCount == 2)), yref = "y"), 
         list(type = "rect",
              fillcolor = brewer.pal(9, "Set1")[2], line = list(color = brewer.pal(9, "Set1")[2]), opacity = 0.4,
              x0 = quantile(frequency_vars_rfmp_sample$DayCount, probs = 0.80) + 0.5, 
              x1 = quantile(frequency_vars_rfmp_sample$DayCount, probs = 0.95) + 0.5, xref = "x",
              y0 = 0, y1 = length(which(frequency_vars_rfmp_sample$DayCount == 2)), yref = "y"),
         list(type = "rect",
              fillcolor = brewer.pal(9, "Set1")[3], line = list(color = brewer.pal(9, "Set1")[3]), opacity = 0.4,
              x0 = quantile(frequency_vars_rfmp_sample$DayCount, probs = 0.95) + 0.5, 
              x1 = quantile(frequency_vars_rfmp_sample$DayCount, probs = 1) + 0.5, xref = "x",
              y0 = 0, y1 = length(which(frequency_vars_rfmp_sample$DayCount == 2)), yref = "y"),
         list(type = "rect",
              line = list(color = brewer.pal(9, "Set1")[1]), opacity = 1,
              x0 = 1.5, x1 = quantile(frequency_vars_rfmp_sample$DayCount, probs = 0.80) + 0.5, xref = "x",
              y0 = 0, y1 = length(which(frequency_vars_rfmp_sample$DayCount == 2)), yref = "y"), 
         list(type = "rect",
              line = list(color = brewer.pal(9, "Set1")[2]), opacity = 1,
              x0 = quantile(frequency_vars_rfmp_sample$DayCount, probs = 0.80) + 0.5, 
              x1 = quantile(frequency_vars_rfmp_sample$DayCount, probs = 0.95) + 0.5, xref = "x",
              y0 = 0, y1 = length(which(frequency_vars_rfmp_sample$DayCount == 2)) , yref = "y"), 
         list(type = "rect",
              line = list(color = brewer.pal(9, "Set1")[3]), opacity = 1,
              x0 = quantile(frequency_vars_rfmp_sample$DayCount, probs = 0.95) + 0.5, 
              x1 = quantile(frequency_vars_rfmp_sample$DayCount, probs = 1) + 0.5, xref = "x",
              y0 = 0, y1 = length(which(frequency_vars_rfmp_sample$DayCount == 2)) , yref = "y")))

day_count_disc <- function(col) {
  disc_col <- character(length=length(col))
  low <- which(col <= quantile(col, probs = 0.80))
  med <- which(col > quantile(col, probs = 0.80) & col <= quantile(col, probs = 0.95))
  high <- which(col > quantile(col, probs = 0.95))
  disc_col[low] <- 'Low'
  disc_col[med] <- 'Medium'
  disc_col[high] <- 'High'
  return(disc_col)
}
rfmp_sample$day_count_disc <- day_count_disc(rfmp_sample$day_count)

plot_ly(
  data = rfmp_sample,
  x =~c("High", "Medium",  "Low"),
  y =~c(length(which(rfmp_sample$day_count_disc == 'High')),
        length(which(rfmp_sample$day_count_disc == 'Medium')),
        length(which(rfmp_sample$day_count_disc == 'Low'))),
  type = "bar")  %>% 
  layout(
    xaxis = list(title = "Frequency Levels"),
    yaxis = list(title = "Count"))

# Intensity
layout(plot_ly(data = intensity_vars_rfmp_sample, x = ~AverageHits, type = "histogram"),
       xaxis = list(title = "AverageHits"),
       yaxis = list(title = "Count"),
       shapes = list(
         list(type = "rect",
              fillcolor = brewer.pal(9, "Set1")[1], line = list(color = brewer.pal(9, "Set1")[1]), opacity = 0.4,
              x0 = 1.5, x1 = quantile(intensity_vars_rfmp_sample$AverageHits, probs = 0.80) + 0.5, xref = "x",
              y0 = 0, y1 = length(which(intensity_vars_rfmp_sample$AverageHits == 1)), yref = "y"), 
         list(type = "rect",
              fillcolor = brewer.pal(9, "Set1")[2], line = list(color = brewer.pal(9, "Set1")[2]), opacity = 0.4,
              x0 = quantile(intensity_vars_rfmp_sample$AverageHits, probs = 0.80) + 0.5, 
              x1 = quantile(intensity_vars_rfmp_sample$AverageHits, probs = 0.95) + 0.5, xref = "x",
              y0 = 0, y1 = length(which(intensity_vars_rfmp_sample$AverageHits == 1)), yref = "y"),
         list(type = "rect",
              fillcolor = brewer.pal(9, "Set1")[3], line = list(color = brewer.pal(9, "Set1")[3]), opacity = 0.4,
              x0 = quantile(intensity_vars_rfmp_sample$AverageHits, probs = 0.95) + 0.5, 
              x1 = quantile(intensity_vars_rfmp_sample$AverageHits, probs = 1) + 0.5, xref = "x",
              y0 = 0, y1 = length(which(intensity_vars_rfmp_sample$AverageHits == 1)), yref = "y"),
         list(type = "rect",
              line = list(color = brewer.pal(9, "Set1")[1]), opacity = 1,
              x0 = 1.5, x1 = quantile(intensity_vars_rfmp_sample$AverageHits, probs = 0.80) + 0.5, xref = "x",
              y0 = 0, y1 = length(which(intensity_vars_rfmp_sample$AverageHits == 1)), yref = "y"), 
         list(type = "rect",
              line = list(color = brewer.pal(9, "Set1")[2]), opacity = 1,
              x0 = quantile(intensity_vars_rfmp_sample$AverageHits, probs = 0.80) + 0.5, 
              x1 = quantile(intensity_vars_rfmp_sample$AverageHits, probs = 0.95) + 0.5, xref = "x",
              y0 = 0, y1 = length(which(intensity_vars_rfmp_sample$AverageHits == 1)) , yref = "y"), 
         list(type = "rect",
              line = list(color = brewer.pal(9, "Set1")[3]), opacity = 1,
              x0 = quantile(intensity_vars_rfmp_sample$AverageHits, probs = 0.95) + 0.5, 
              x1 = quantile(intensity_vars_rfmp_sample$AverageHits, probs = 1) + 0.5, xref = "x",
              y0 = 0, y1 = length(which(intensity_vars_rfmp_sample$AverageHits == 1)) , yref = "y")))

avg_hits_disc <- function(col) {
  disc_col <- character(length=length(col))
  low <- which(col <= quantile(col, probs = 0.80))
  med <- which(col > quantile(col, probs = 0.80) & col <= quantile(col, probs = 0.95))
  high <- which(col > quantile(col, probs = 0.95))
  disc_col[low] <- 'Low'
  disc_col[med] <- 'Medium'
  disc_col[high] <- 'High'
  return(disc_col)
}
rfmp_sample$avg_hits_disc <- avg_hits_disc(rfmp_sample$avg_hits)

plot_ly(
  data = rfmp_sample,
  x =~c("High", "Medium",  "Low"),
  y =~c(length(which(rfmp_sample$avg_hits_disc == 'High')),
        length(which(rfmp_sample$avg_hits_disc == 'Medium')),
        length(which(rfmp_sample$avg_hits_disc == 'Low'))),
  name = "Intensity discretization proportions",
  type = "bar") %>% 
  layout(
    xaxis = list(title = "Intensity Levels"),
    yaxis = list(title = "Count")) 

# Recency 
plot_ly(rfmp_sample, x = ~recent_date, type = "histogram")
quantile(rfmp_sample$recent_date, probs = c(seq(0, 1, 0.1)))

layout(plot_ly(data = recency_vars_rfmp_sample, x = ~RecentDate, type = "histogram"),
       xaxis = list(title = "RecentDate"),
       yaxis = list(title = "Count"),
       shapes = list(
         list(type = "rect",
              fillcolor = brewer.pal(9, "Set1")[1], line = list(color = brewer.pal(9, "Set1")[1]), 
              opacity = 0.4,
              x0 = min(recency_vars_rfmp_sample$RecentDate), 
              x1 = quantile(recency_vars_rfmp_sample$RecentDate, probs = 0.30) + 0.5, xref = "x",
              y0 = 0, 
              y1 = length(which(recency_vars_rfmp_sample$RecentDate == 1519689600)), yref = "y"), 
         list(type = "rect",
              fillcolor = brewer.pal(9, "Set1")[2], line = list(color = brewer.pal(9, "Set1")[2]), 
              opacity = 0.4,
              x0 = quantile(recency_vars_rfmp_sample$RecentDate, probs = 0.30) + 0.5, 
              x1 = quantile(recency_vars_rfmp_sample$RecentDate, probs = 0.65) + 0.5, xref = "x",
              y0 = 0, 
              y1 = length(which(recency_vars_rfmp_sample$RecentDate == 1519689600)), yref = "y"),
         list(type = "rect",
              fillcolor = brewer.pal(9, "Set1")[3], line = list(color = brewer.pal(9, "Set1")[2]), 
              opacity = 0.4,
              x0 = quantile(recency_vars_rfmp_sample$RecentDate, probs = 0.65) + 0.5, 
              x1 = quantile(recency_vars_rfmp_sample$RecentDate, probs = 0.85) + 0.5, xref = "x",
              y0 = 0, 
              y1 = length(which(recency_vars_rfmp_sample$RecentDate == 1519689600)), yref = "y"),
         list(type = "rect",
              fillcolor = brewer.pal(9, "Set1")[4], line = list(color = brewer.pal(9, "Set1")[2]), 
              opacity = 0.4,
              x0 = quantile(recency_vars_rfmp_sample$RecentDate, probs = 0.85) + 0.5, 
              x1 = quantile(recency_vars_rfmp_sample$RecentDate, probs = 0.98) + 0.5, xref = "x",
              y0 = 0, 
              y1 = length(which(recency_vars_rfmp_sample$RecentDate == 1519689600)), yref = "y"),
         list(type = "rect",
              fillcolor = brewer.pal(9, "Set1")[5], line = list(color = brewer.pal(9, "Set1")[3]), opacity = 0.4,
              x0 = quantile(recency_vars_rfmp_sample$RecentDate, probs = 0.975) + 0.5, 
              x1 = quantile(recency_vars_rfmp_sample$RecentDate, probs = 1) + 0.5, xref = "x",
              y0 = 0, 
              y1 = length(which(recency_vars_rfmp_sample$RecentDate == 1519689600)), yref = "y"),
         list(type = "rect",
              line = list(color = brewer.pal(9, "Set1")[1]), 
              opacity = 1,
              x0 = min(recency_vars_rfmp_sample$RecentDate), 
              x1 = quantile(recency_vars_rfmp_sample$RecentDate, probs = 0.30) + 0.5, xref = "x",
              y0 = 0, 
              y1 = length(which(recency_vars_rfmp_sample$RecentDate == 1519689600)), yref = "y"), 
         list(type = "rect",
              line = list(color = brewer.pal(9, "Set1")[2]), 
              opacity = 1,
              x0 = quantile(recency_vars_rfmp_sample$RecentDate, probs = 0.30) + 0.5, 
              x1 = quantile(recency_vars_rfmp_sample$RecentDate, probs = 0.65) + 0.5, xref = "x",
              y0 = 0, 
              y1 = length(which(recency_vars_rfmp_sample$RecentDate == 1519689600)) , yref = "y"),
         list(type = "rect",
              line = list(color = brewer.pal(9, "Set1")[3]), 
              opacity = 1,
              x0 = quantile(recency_vars_rfmp_sample$RecentDate, probs = 0.65) + 0.5, 
              x1 = quantile(recency_vars_rfmp_sample$RecentDate, probs = 0.85) + 0.5, xref = "x",
              y0 = 0, 
              y1 = length(which(recency_vars_rfmp_sample$RecentDate == 1519689600)) , yref = "y"), 
         list(type = "rect",
              line = list(color = brewer.pal(9, "Set1")[4]), 
              opacity = 1,
              x0 = quantile(recency_vars_rfmp_sample$RecentDate, probs = 0.85) + 0.5, 
              x1 = quantile(recency_vars_rfmp_sample$RecentDate, probs = 0.98) + 0.5, xref = "x",
              y0 = 0, 
              y1 = length(which(recency_vars_rfmp_sample$RecentDate == 1519689600)) , yref = "y"), 
         list(type = "rect",
              line = list(color = brewer.pal(9, "Set1")[5]), 
              opacity = 1,
              x0 = quantile(recency_vars_rfmp_sample$RecentDate, probs = 0.975) + 0.5, 
              x1 = quantile(recency_vars_rfmp_sample$RecentDate, probs = 1) + 0.5, xref = "x",
              y0 = 0, 
              y1 = length(which(recency_vars_rfmp_sample$RecentDate == 1519689600)) , yref = "y")))

recent_date_day_disc <- function(col) {
  disc_col <- character(length=length(col))
  very_low <- which(col <= quantile(col, probs = 0.30))
  low <- which(col > quantile(col, probs = 0.30) & col <= quantile(col, probs = 0.65))
  med <- which(col > quantile(col, probs = 0.65) & col <= quantile(col, probs = 0.85))
  high <- which(col > quantile(col, probs = 0.85) & col <= quantile(col, probs = 0.975))
  very_high <- which(col > quantile(col, probs = 0.975))
  disc_col[very_low] <- 'Very Low'
  disc_col[low] <- 'Low'
  disc_col[med] <- 'Medium'
  disc_col[high] <- 'High'
  disc_col[very_high] <- 'Very High'
  return(disc_col)
}

rfmp_sample$recent_date_day_disc <- recent_date_day_disc(rfmp_sample$recent_date_day)

plot_ly(
  data = rfmp_sample,
  x =~c("High", "Medium",  "Low", "Very Low", "Very High"),
  y =~c(length(which(rfmp_sample$recent_date_day_disc == 'High')),
        length(which(rfmp_sample$recent_date_day_disc == 'Medium')),
        length(which(rfmp_sample$recent_date_day_disc == 'Low')),
        length(which(rfmp_sample$recent_date_day_disc == 'Very Low')),
        length(which(rfmp_sample$recent_date_day_disc == 'Very High'))
        ),
  name = "Recency discretization proportions",
  type = "bar") %>% 
  layout(
    xaxis = list(title = "Recency Levels"),
    yaxis = list(title = "Count")) 

# Variability 

plot_ly(rfmp_sample, x = ~round(avg_inter_visit), type = "histogram")
quantile(rfmp_sample$avg_inter_visit, probs = c(seq(0, 1, 0.1)))

plot_ly(rfmp_sample, x = ~max_inter_visit, type = "histogram")
quantile(rfmp_sample$max_inter_visit, probs = c(seq(0, 1, 0.1)))

# Variability
plot_ly(data = rfmp_sample, 
        x = ~avg_inter_visit, 
        y = ~max_inter_visit)

quantile(rfmp_sample$avg_inter_visit, probs = c(seq(0, 1, 0.1)))
quantile(rfmp_sample$max_inter_visit, probs = c(seq(0, 1, 0.1)))

quantile(rfmp_sample$avg_inter_visit, probs = 0.6)
quantile(rfmp_sample$avg_inter_visit, probs = 0.9)

quantile(rfmp_sample$max_inter_visit, probs = 0.6)
quantile(rfmp_sample$max_inter_visit, probs = 0.9)

layout(plot_ly(
    data = variability_vars_rfmp_sample, 
    x = ~AverageInterVisit, 
    y = ~MaximumInterVisit),
    title = 'AverageInterVisit vs MaximumInterVisit - Discretization',
    xaxis = list(title = "AverageInterVisit"),
    yaxis = list(title = "MaximumInterVisit"),
       shapes = list(
         list(type = "rect",
              fillcolor = brewer.pal(9, "Set1")[1], line = list(color = brewer.pal(9, "Set1")[1]), opacity = 0.4,
              x0 = 0, x1 = quantile(variability_vars_rfmp_sample$AverageInterVisit, probs = 0.6), xref = "x",
              y0 = 0, y1 = quantile(variability_vars_rfmp_sample$MaximumInterVisit, probs = 0.6), yref = "y"),
         list(type = "rect",
              line = list(color = brewer.pal(9, "Set1")[1]), opacity = 1,
              x0 = 0, x1 = quantile(variability_vars_rfmp_sample$AverageInterVisit, probs = 0.6), xref = "x",
              y0 = 0, y1 = quantile(variability_vars_rfmp_sample$MaximumInterVisit, probs = 0.6), yref = "y"), 
         list(type = "rect",
              fillcolor = brewer.pal(9, "Set1")[2], line = list(color = brewer.pal(9, "Set1")[2]), opacity = 0.4,
              x0 = quantile(variability_vars_rfmp_sample$AverageInterVisit, probs = 0.6), 
              x1 = quantile(variability_vars_rfmp_sample$AverageInterVisit, probs = 1), xref = "x",
              y0 = 0, y1 = quantile(variability_vars_rfmp_sample$MaximumInterVisit, probs = 0.6), yref = "y"),
         list(type = "rect",
              line = list(color = brewer.pal(9, "Set1")[2]), opacity = 1,
              x0 = quantile(variability_vars_rfmp_sample$AverageInterVisit, probs = 0.6), 
              x1 = quantile(variability_vars_rfmp_sample$AverageInterVisit, probs = 1), xref = "x",
              y0 = 0, y1 = quantile(variability_vars_rfmp_sample$MaximumInterVisit, probs = 0.6), yref = "y"), 
         list(type = "rect",
              fillcolor = brewer.pal(9, "Set1")[2], line = list(color = brewer.pal(9, "Set1")[2]), opacity = 0.4,
              x0 = 0, x1 = quantile(variability_vars_rfmp_sample$AverageInterVisit, probs = 0.6), xref = "x",
              y0 = quantile(variability_vars_rfmp_sample$MaximumInterVisit, probs = 0.6),
              y1 = quantile(variability_vars_rfmp_sample$MaximumInterVisit, probs = 1), yref = "y"),
         list(type = "rect",
              line = list(color = brewer.pal(9, "Set1")[2]), opacity = 1,
              x0 = 0, x1 = quantile(variability_vars_rfmp_sample$AverageInterVisit, probs = 0.6), xref = "x",
              y0 = quantile(variability_vars_rfmp_sample$MaximumInterVisit, probs = 0.6),
              y1 = quantile(variability_vars_rfmp_sample$MaximumInterVisit, probs = 1), yref = "y"),
         list(type = "rect",
              fillcolor = brewer.pal(9, "Set1")[3], line = list(color = brewer.pal(9, "Set1")[3]), opacity = 0.4,
              x0 = quantile(variability_vars_rfmp_sample$AverageInterVisit, probs = 0.6),
              x1 = quantile(variability_vars_rfmp_sample$AverageInterVisit, probs = 1), xref = "x",
              y0 = quantile(variability_vars_rfmp_sample$MaximumInterVisit, probs = 0.6),
              y1 = quantile(variability_vars_rfmp_sample$MaximumInterVisit, probs = 1), yref = "y"),
         list(type = "rect",
              line = list(color = brewer.pal(9, "Set1")[3]), opacity = 1,
              x0 = quantile(variability_vars_rfmp_sample$AverageInterVisit, probs = 0.6), 
              x1 = quantile(variability_vars_rfmp_sample$AverageInterVisit, probs = 1), xref = "x",
              y0 = quantile(variability_vars_rfmp_sample$MaximumInterVisit, probs = 0.6),
              y1 = quantile(variability_vars_rfmp_sample$MaximumInterVisit, probs = 1), yref = "y")))

avg_inter_visit_disc <- function(col) {
  disc_col <- character(length=length(col))
  low <- which(col <= quantile(col, probs = 0.6))
  high <- which(col > quantile(col, probs = 0.6))
  disc_col[low] <- 'Low'
  disc_col[high] <- 'High'
  return(disc_col)
}
rfmp_sample$avg_inter_visit_disc <- avg_inter_visit_disc(rfmp_sample$avg_inter_visit)

max_inter_visit_disc <- function(col) {
  disc_col <- character(length=length(col))
  low <- which(col <= quantile(col, probs = 0.6))
  high <- which(col > quantile(col, probs = 0.6))
  disc_col[low] <- 'Low'
  disc_col[high] <- 'High'
  return(disc_col)
}
rfmp_sample$max_inter_visit_disc <- max_inter_visit_disc(rfmp_sample$max_inter_visit)

variability_variable <- function(col1, col2){
  disc_col <- character(length=length(col1))
  low <- which(col1 == 'Low' & col2 == 'Low')
  med <- union(which(col1 == 'High' & col2 == 'Low'),
               which(col1 == 'Low' & col2 == 'High'))
  high <- which(col1 == 'High' & col2 == 'High')
  disc_col[low] <- 'Low'
  disc_col[med] <- 'Medium'
  disc_col[high] <- 'High'
  return(disc_col)
}

rfmp_sample$variability <- variability_variable(rfmp_sample$avg_inter_visit_disc, 
                                               rfmp_sample$max_inter_visit_disc)


variability2_variable <- function(col1, col2){
  disc_col <- character(length=length(col1))
  low <- which((col2 - quantile(rfmp_sample$max_inter_visit, probs = 0.6) + 
                  (quantile(rfmp_sample$max_inter_visit, probs = 0.6)/quantile(rfmp_sample$avg_inter_visit, probs = 0.6))*col1) <= 0)
  med <- which((col2 - quantile(rfmp_sample$max_inter_visit, probs = 0.6) + 
                  (quantile(rfmp_sample$max_inter_visit, probs = 0.6)/quantile(rfmp_sample$avg_inter_visit, probs = 0.6))*col1) > 0 
               & (col2 - quantile(rfmp_sample$max_inter_visit, probs = 0.9) +
                    (quantile(rfmp_sample$max_inter_visit, probs = 0.9)/quantile(rfmp_sample$avg_inter_visit, probs = 0.9))*col1) <= 0)
  high <- which((col2 - quantile(rfmp_sample$max_inter_visit, probs = 0.9) + 
                   (quantile(rfmp_sample$max_inter_visit, probs = 0.9)/quantile(rfmp_sample$avg_inter_visit, probs = 0.9))*col1) > 0)
  disc_col[low] <- 'Low'
  disc_col[med] <- 'Medium'
  disc_col[high] <- 'High'
  return(disc_col)
}

rfmp_sample$variability2 <- variability2_variable(rfmp_sample$avg_inter_visit, 
                                               rfmp_sample$max_inter_visit)

variability_vars_rfmp_sample$variability2 <- 
  variability2_variable(
    variability_vars_rfmp_sample$AverageInterVisit, 
    variability_vars_rfmp_sample$MaximumInterVisit)

plot_ly(data=variability_vars_rfmp_sample, 
        x=~AverageInterVisit,
        y=~MaximumInterVisit,
        color=~variability2,
        type = "scatter",
        mode = "markers")

plot_ly(
  data = rfmp_sample,
  x =~c("High", "Medium",  "Low"),
  y =~c(length(which(rfmp_sample$variability2 == 'High')),
        length(which(rfmp_sample$variability2 == 'Medium')),
        length(which(rfmp_sample$variability2 == 'Low'))),
  name = "Variability discretization proportions",
  type = "bar") %>% 
  layout(
    xaxis = list(title = "Variability Levels"),
    yaxis = list(title = "Count"))