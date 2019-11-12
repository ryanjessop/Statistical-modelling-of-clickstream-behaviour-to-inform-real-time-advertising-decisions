# Clickstream EDA - Chapter 2

# Imports
library(sparklyr)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library("PerformanceAnalytics")
library(pscl)
library(foreign)
library(MASS)
library(plotly)
library(minerva)
library(reshape2)
library(gridExtra) 
options(scipen = 999) 
library(caret)
library(RColorBrewer)
library(funModeling) 
library(vcd)
library(corrplot)
library(corrgram)
library(lmtest)
library(car)
library(stats)
library(caTools)
library(BBmisc)
library(Hmisc)
library(lubridate)
library(fitdistrplus)

#-----------------------------------------------------------
# Read in data created in Spark

# Data from tag collection - One source of data for entire page
page_visits <- read_csv("C:/Users/ryana/Desktop/Ch_2/data/2019-01-31/page-visits.csv")
View(page_visits)
nrow(page_visits)
str(page_visits)
head(page_visits)

# Data from data science features - Create from page visits using aggregation dplyr
# Date range: 
profiles <- read_csv("C:/Users/ryana/Desktop/Ch_2/data/2019-01-31/profiles.csv")
View(profiles)
nrow(profiles)
str(profiles)
head(profiles)

# Read in demographics for profiles
demos <- read_csv("C:/Users/ryana/Desktop/Ch_2/data/2019-01-31/demos.csv")
View(demos)
nrow(demos)
str(demos)
head(demos)

# Read in session level data from spyro
spyro_page_visits <- read_csv("C:/Users/ryana/Desktop/Ch_2/data/2019-01-31/spyro-page-visits.csv")
View(spyro_page_visits)
nrow(spyro_page_visits)
str(spyro_page_visits)
head(spyro_page_visits)

# Read in inter session data from spyro
spyro_inter_visits <- read_csv("C:/Users/ryana/Desktop/Ch_2/data/2019-01-31/spyro-inter-visits.csv")
View(spyro_inter_visits)
nrow(spyro_inter_visits)
str(spyro_inter_visits)
head(spyro_inter_visits)

# Read in inter session data from spyro
spyro_inter_visits_v2 <- read_csv("C:/Users/ryana/Desktop/Ch_2/data/2019-01-31/spyro-inter-visits-v2.csv")
View(spyro_inter_visits_v2)
nrow(spyro_inter_visits_v2)
str(spyro_inter_visits_v2)
head(spyro_inter_visits_v2)


#-----------------------------------------------------------
# Sampling - if we need to
# Choose sites/categories
# Currently only use UK timezone - stored as UTC

#page_visits = page_visits %>% 
#  filter(pv_script_id == 'www.history.com')

#profiles = profiles %>% 
#  filter(country == 'United Kingdom') %>% 


#-----------------------------------------------------------
# Basic plots of single useful variables
# Variables relating to page visits

# Number of page visits 
nrow(page_visits)

# Number of unique ProfileIDs
nrow(profiles)

# Number of unique SessionIDs - from Spark
392532

# Number of unique SiteIDs - from Spark
6856

# Number of unique DeviceIPs - from Spark
249218

# Bot proportion 
table(profiles$bot)
nrow(profiles)

# PageVisitStartTime distribution
plot_ly(data = page_visits, 
        x = ~pv_start, 
        type = "histogram") %>%
  layout(
    xaxis = list(title = "PageVisitStartTime"),
    yaxis = list(title = "Count"))

# PageVisitEndtimeType distribution
table(page_visits$pv_endtime_type)
plot_ly(data = page_visits, 
        x = ~pv_endtime_type, 
        type = "histogram",
        color=I('green')) %>%
  layout(
    xaxis = list(title = "PageVisitEndtimeType"),
    yaxis = list(title = "Count"))

# Categories
table(page_visits$pv_interest)
plot_ly(data = page_visits, 
        x = ~pv_interest, 
        type = "histogram",
        color=I('purple')) %>%
  layout(
    xaxis = list(title = "PageVisitCategoryID"),
    yaxis = list(title = "Count"))

table(page_visits$taxonomy_tier1)
plot_ly(data = page_visits, 
        x = ~taxonomy_tier1, 
        type = "histogram",
        color=I('purple')) %>%
  layout(
    xaxis = list(title = "PageVisitCategoryID"),
    yaxis = list(title = "Count"))

page_visits_category = page_visits %>% 
  group_by(pv_interest) %>% 
  summarise(count = n()) %>% 
  filter(count > 20000)

plot_ly(data = page_visits_category, 
        x = ~pv_interest,
        y = ~count,
        type = "bar",
        color=I('purple')) %>%
  layout(
    xaxis = list(title = "PageVisitCategoryID"),
    yaxis = list(title = "Count"))

# Referrer type - No referrer signal
table(page_visits$referrer)

page_visits_referrer = page_visits %>% 
  group_by(referrer) %>% 
  summarise(count = n()) %>% 
  filter(count > 2000)

plot_ly(page_visits_referrer, 
        x = ~referrer,
        y = ~count,
        type = "bar",
        color=I('dark red')) %>%
  layout(
    xaxis = list(title = "Referrer"),
    yaxis = list(title = "Count"))

View(page_visits_referrer)

# Basic plots of single useful variables
# Variables relating to profiles

# Country 
table(profiles$country)
factor(profiles$country)

profiles_country = profiles %>% 
  group_by(country) %>% 
  summarise(count = n()) %>% 
  filter(count > 2000) %>% 
  filter(country != 'None')

table(profiles_country$country)
plot_ly(profiles_country, 
        x = ~country,
        y = ~count,
        type = "bar",
        color=I('black')) %>%
  layout(
    xaxis = list(title = "Country"),
    yaxis = list(title = "Count"))

# DeviceType
plot_ly(profiles, 
        x = ~dev_type, 
        type = "histogram",
        color=I('grey')) %>%
  layout(
    xaxis = list(title = "DeviceType"),
    yaxis = list(title = "Count", 
    type = 'log'))

# DeviceHardware
profiles_hardware = profiles %>% 
  group_by(dev_hardware) %>% 
  summarise(count = n()) %>% 
  filter(count > 1000) %>%
  filter(dev_hardware != 'Unknown')

plot_ly(profiles_hardware, 
        x = ~dev_hardware,
        y = ~count,
        type = "bar",
        color=I('red')) %>%
  layout(
    xaxis = list(title = "DeviceHardware"),
    yaxis = list(title = "Count", type = 'log'))

# DeviceOS
profiles_os = profiles %>% 
  group_by(dev_os) %>% 
  summarise(count = n()) %>% 
  filter(count > 1000)

plot_ly(profiles_os, 
        x = ~dev_os,
        y = ~count,
        type = "bar",
        color=I('light blue')) %>%
  layout(
    xaxis = list(title = "DeviceOS"),
    yaxis = list(title = "Count"))

# DeviceScreenSize
profiles_screen = profiles %>% 
  group_by(dev_screen) %>% 
  summarise(count = n()) %>% 
  filter(count > 1000) %>% 
  filter(dev_screen != 'Unknown')

profiles_screen$dev_screen <- factor(profiles_screen$dev_screen, levels = c("4", "5", "6", "7", "8", "10"))

plot_ly(profiles_screen, 
        x = ~dev_screen,
        y = ~count,
        type = "bar",
        color=I('orange')) %>%
  layout(
    xaxis = list(title = "DeviceScreensize"),
    yaxis = list(title = "Count"))

# Demographics 
demos_name = demos %>% 
  group_by(demographic_name) %>% 
  summarise(count = n()) %>% 
  filter(count > 15000)

plot_ly(demos_name, 
        x = ~demographic_name,
        y = ~count,
        type = "bar",
        color=I('dark green')) %>%
  layout(
    xaxis = list(title = "ProfileDemographics"),
    yaxis = list(title = "Count"))

#-----------------------------------------------------------
# Page visits aggregated to session variables

# New variables 
# Filter out potential bots for visualisation purposes

quantile_probs = c(seq(0, 1, 0.25))
head(spyro_page_visits)

# PageVisitDuration
plot_ly(spyro_page_visits, 
        x = ~est_pv_duration,
        type = "histogram",
        color=I('dark green')) %>%
  layout(
    xaxis = list(title = "PageVisitDuration"),
    yaxis = list(title = "Count", type='log'))

quantile(spyro_page_visits$est_pv_duration, 
         probs = quantile_probs)

# PageVisitsInSession
spyro_page_visits_in_session = spyro_page_visits %>% 
  group_by(new_sid) %>%  
  summarise(count = n()) %>%
  filter(count < 100) %>%
  filter(count > 1)

quantile(spyro_page_visits_in_session$count, 
         probs = quantile_probs)

plot_ly(spyro_page_visits_in_session, 
        x = ~count,
        type = "histogram",
        color=I('orange')) %>%
  layout(
    xaxis = list(title = "PageVisitsInSession"),
    yaxis = list(title = "Count"))


# SessionDuration
spyro_session_length = spyro_page_visits %>% 
  group_by(new_sid) %>% 
  summarise(session_duration = sum(est_pv_duration)) %>%
  filter(session_duration < 2*60*60) %>%
  filter(session_duration >= 30)

max(spyro_session_length$session_duration)

quantile(spyro_session_length$session_duration, 
         probs = quantile_probs)

plot_ly(data = spyro_session_length, 
        x = ~session_duration,
        type = "histogram",
        color=I('light blue')) %>%
  layout(
    xaxis = list(title = "SessionDuration"),
    yaxis = list(title = "Count"))

plot_ly(data = spyro_session_length, 
        x = ~session_duration/60,
        type = "histogram",
        color=I('light blue')) %>%
  layout(
    xaxis = list(title = "SessionDuration"),
    yaxis = list(title = "Count", type='log'))

# AveragePageVisitDuration
spyro_av_pv = spyro_page_visits %>% 
  group_by(new_sid) %>% 
  summarise(session_duration = sum(est_pv_duration), num_pv = n()) %>%
  filter(session_duration < 2*60*60) %>%
  filter(session_duration >= 30) %>%
  filter(num_pv > 1) %>%
  filter(num_pv < 100) %>%
  mutate(av_pv_length = session_duration/num_pv)

quantile(spyro_av_pv$av_pv_length, 
         probs = quantile_probs)

plot_ly(data = spyro_av_pv, 
        x = ~av_pv_length,
        type = "histogram",
        color=I('dark red')) %>%
  layout(
    xaxis = list(title = "AveragePageVisitDuration"),
    yaxis = list(title = "Count")) 

# Since last session 
# Overlapping sessions set to zero
head(spyro_inter_visits)

quantile(spyro_inter_visits$inter_session_duration, 
         probs = quantile_probs, na.rm=TRUE)

plot_ly(data = spyro_inter_visits, 
        x = ~inter_session_duration, 
        type = "histogram",
        color=I('black')) %>%
  layout(
    xaxis = list(title = "InterSessionDuration"),
    yaxis = list(title = "Count", type='log'))  

# Bounce rate
spyro_under_bounce = spyro_page_visits %>% 
  group_by(new_sid) %>% 
  summarise(session_duration = sum(est_pv_duration), num_pv = n()) %>%
  filter(num_pv == 1) %>%
  filter(session_duration < 30)
nrow(spyro_under_bounce) 

spyro_over_bounce = spyro_page_visits %>% 
  group_by(new_sid) %>% 
  summarise(session_duration = sum(est_pv_duration), num_pv = n()) %>%
  filter(num_pv > 1) %>%  
  filter(session_duration >= 30)
nrow(spyro_over_bounce) 


#-----------------------------------------------------------
# Associations/Correlations between variables

# Continuous variables 
spyro_session_data = spyro_page_visits %>% 
  group_by(pid, new_sid) %>% 
  summarise(session_duration = sum(est_pv_duration), num_pv = n(), session_start = min(pv_start)) %>%
  filter(session_duration < 2*60*60 & session_duration > 30) %>%
  filter(num_pv < 100 & num_pv > 1) %>%
  mutate(av_pv_length = session_duration/num_pv)

spyro_session_numerical <- spyro_session_data %>% 
  select(c("num_pv", "session_duration", "av_pv_length")) %>%
  rename(PageVisitsInSession = num_pv) %>%
  rename(SessionDuration = session_duration) %>%
  rename(AveragePageVisitDuration = av_pv_length)

# Pairs plot
pairs(spyro_session_numerical[c("PageVisitsInSession", 
                                "SessionDuration", 
                                "AveragePageVisitDuration")], upper.panel = NULL)

# Correlation plot
cor_matrix <- cor(spyro_session_numerical[
  c("PageVisitsInSession", 
    "SessionDuration", 
    "AveragePageVisitDuration")])
cor_matrix
corrplot(cor_matrix, 
         method='number', 
         type='lower',
         tl.srt = 35)

# Compare numericals with device variables
numerical_device_data <- left_join(spyro_session_data, profiles, by='pid') 

numerical_device_data <- numerical_device_data %>%
  filter(dev_type != 'NA')

# Scatterplots 
plot_ly(data = numerical_device_data, 
        x = ~session_duration, 
        y = ~num_pv,
        type = 'scatter',
        mode = 'markers',
        color=I('black')) %>%
  layout(
    xaxis = list(title = "SessionDuration"),
    yaxis = list(title = "PageVisitsInSession"))  

plot_ly(data = spyro_session_numerical, 
        x = ~session_duration, 
        y = ~av_pv_length,
        type = 'scatter',
        mode = 'markers',
        color=I('black')) %>%
  layout(
    xaxis = list(title = "SessionDuration"),
    yaxis = list(title = "AveragePageVisitDuration"))  

plot_ly(data = spyro_session_numerical, 
        x = ~av_pv_length, 
        y = ~num_pv,
        type = 'scatter',
        mode = 'markers',
        color=I('blue')) %>%
  layout(
    xaxis = list(title = "AveragePageVisitDuration"),
    yaxis = list(title = "PageVisitsInSession")) 

# Split by device
plot_ly(data = numerical_device_data, 
        y = ~session_duration, 
        color = ~dev_type, 
        type = "box") %>%
  layout(
    xaxis = list(title = "DeviceType"),
    yaxis = list(title = "SessionDuration")) 

plot_ly(data = numerical_device_data, 
        x = ~session_duration, 
        y = ~num_pv,
        color = ~dev_type, 
        type = "scatter") %>%
  layout(
    xaxis = list(title = "SessionDuration"),
    yaxis = list(title = "PageVisitsInSession"))

# Categorical device correlations/heatmap might be useful here
device_data = numerical_device_data %>%
  select(c('dev_type', 'dev_hardware', 'dev_os', 'dev_screen'))

tblA = table(device_data$dev_type, device_data$dev_screen)
tblA
plot_ly(x=colnames(tblA),
        y=rownames(tblA),
        z=matrix(tblA, nrow(tblA), ncol(tblA)), 
        type='heatmap',
        colors = colorRamp(c("white", "blue")))  %>%
  layout(
    xaxis = list(title = "DeviceScreensize"),
    yaxis = list(title = "DeviceType"))

# Repeat visitor variables
head(spyro_inter_visits_v2)

spyro_corr_data <- spyro_inter_visits_v2 %>%
  filter(session_duration < 2*60*60 & session_duration > 30) %>%
  filter(num_page_visits < 100 & num_page_visits > 1) %>%
  filter(!is.na(inter_session_duration))
  
spyro_corr_data <- spyro_corr_data %>% 
  select(c("num_page_visits", 
           "session_duration",
           "av_pv_duration",
           "inter_session_duration")) %>%
  rename(PageVisitsInSession = num_page_visits) %>%
  rename(SessionDuration = session_duration) %>%
  rename(AveragePageVisitDuration = av_pv_duration) %>%
  rename(InterSessionDuration = inter_session_duration)

nrow(spyro_corr_data)

# Pairs plot
pairs(spyro_corr_data[c("PageVisitsInSession", 
                        "SessionDuration", 
                        "AveragePageVisitDuration",
                        "InterSessionDuration")], 
      upper.panel = NULL)

# Correlation plot
cor_matrix <- cor(spyro_corr_data[
  c("PageVisitsInSession", 
    "SessionDuration", 
    "AveragePageVisitDuration",
    "InterSessionDuration")])
cor_matrix
corrplot(cor_matrix, 
         method='number', 
         type='lower',
         tl.srt = 35)

#-----------------------------------------------------------
# Heartbeat

# Page visit endtime type - Artificial or Real 
spyro_page_visits_real =  spyro_page_visits %>%
  filter(pv_endtime_type == 'Real')
nrow(spyro_page_visits_real)

spyro_page_visits_art = spyro_page_visits %>%
  filter(pv_endtime_type == 'Artificial')
nrow(spyro_page_visits_art)

# PageVisitDuration, split by endtime type
plot_ly(data = spyro_page_visits_real, 
        x = ~est_pv_duration, 
        type = "histogram",
        color=I('blue')) %>%
  layout(
    xaxis = list(title = "RealPageVisitDuration"),
    yaxis = list(title = "Count"))

plot_ly(data = spyro_page_visits_art, 
        x = ~est_pv_duration, 
        type = "histogram",
        color=I('red')) %>%
  layout(
    xaxis = list(title = "ArtificialPageVisitDuration"),
    yaxis = list(title = "Count"))


# Mixture models/Weibull distribution 
plot_ly(spyro_page_visits_in_session, 
        x=~count, 
        type = "histogram", 
        histnorm = "probability")

# Maximum likelihood methods 

descdist(spyro_page_visits_in_session$count, discrete = FALSE)

fw <- fitdist(spyro_page_visits_in_session$count - 1, 
         distr = "weibull", method = 'mle')
summary(fw)

alpha = 1.02828944
lambda = 8.58016948
a <- seq(1, 100, 1)

w_dist <- (alpha/lambda)*((a/lambda)^(alpha - 1))*exp(-((a/lambda)^(alpha)))
summary(w_dist)
w_dist = dweibull(a, shape=0.8833657, scale =6.9225031, log = FALSE)
g_dist = dgamma(a, shape = 1.2539404, rate = 0.1482407)
e_dist = dexp(a, rate = 0.1182497)
p_dist = dpois(a, lambda = 7.456682)
b_dist = dnbinom(a, size = 1.372226, mu = 8.457522)

plot_ly(spyro_page_visits_in_session) %>% 
  add_trace(x=~count, 
            type = "histogram", 
            histnorm = "probability",
            color=I('black')) %>%
  add_trace(x =~(a+1), 
            y =~w_dist,
            color=I('red')) %>%
  layout(
    xaxis = list(title = "PageVisitsInSession"),
    yaxis = list(title = "Probability Density"))

qqplot(g_dist, sort(spyro_page_visits_in_session$count), 
       xlab="Weibull distribution",
       ylab="PageVisitsInSession")

length(w_dist)

length(spyro_page_visits_in_session$count)
?qqplot
#|-------------------------------------------------

# Experimentation with adding noise and fitting the distribution by 'hand'

# Box-percentile plots
bpplot(spyro_over_bounce$num_pv, srtx =0)
# Can we model the number of page visits using a Weibull distribution using the graphical method?

# Add unif random value for integer to continuous data type
model_df <- features

model_df[c(which(model_df$num_page_visits > 2)),]$num_page_visits <- 
  model_df[c(which(model_df$num_page_visits > 2)),]$num_page_visits + 
  runif(nrow(model_df[c(which(model_df$num_page_visits > 2)),]), min=-0.5, max=0.5)

model_df[c(which(model_df$num_page_visits == 2)),]$num_page_visits <- 
  model_df[c(which(model_df$num_page_visits == 2)),]$num_page_visits + 
  runif(nrow(model_df[c(which(model_df$num_page_visits == 2)),]), min=0.001, max=0.5)

summary(model_df$num_page_visits)

model_df$num_page_visits <- model_df$num_page_visits - 2
model_df$num_page_visits[c(which(model_df$num_page_visits < 0))] <- NA

cum_dist <- ecdf(model_df$num_page_visits)
cum_dist
plot_ly(model_df, 
        x =~log(num_page_visits), 
        y =~log(log((1-cum_dist(num_page_visits))^(-1))))

plot(ecdf(model_df$num_page_visits))
plot(ecdf(model_df$num_page_visits - 2))

model_df$num_page_visits[which(!is.finite(model_df$num_page_visits))] = NA

x <- log(log((1-cum_dist(model_df$num_page_visits))^(-1)))
x[which(!is.finite(x))] = NA
y <- log(model_df$num_page_visits)
summary(x)
summary(y)
lm(y ~ x)
lm(x ~ y)
alpha = 0.654713343
lambda = exp(1.3793/alpha)
lambda = 4.413093244

a <- seq(0.5, 100, 0.01)
w_dist <- (alpha/lambda)*((a/lambda)^(alpha - 1))*exp(-((a/lambda)^(alpha)))
summary(w_dist)

plot_ly(model_df) %>% 
  add_trace(x=~num_page_visits, 
            type = "histogram", 
            histnorm = "probability") %>%
  add_trace(x =~a, y = ~ w_dist)

# Session duration variable 
fit <- density(log(model_df$session_duration))

plot_ly(model_df, 
        x=~log(session_duration), 
        type = "histogram")  %>% 
  add_trace(x = fit$x, 
            y = fit$y, 
            type = "scatter", 
            mode = "lines", 
            fill = "tozeroy", 
            yaxis = "y2", 
            name = "Density") %>% 
  layout(yaxis2 = list(overlaying = "y", 
                       side = "right"))

plot_ly(model_df, 
        x=~num_page_visits, 
        y=~session_duration, 
        type = "scatter")

plot_ly(model_df, 
        x=~log(num_page_visits), 
        y=~log(session_duration), 
        type = "scatter")

lm(log(session_duration) ~ log(num_page_visits), model_df)
