#--------------------------------------------------------------------------------------
# Train GLM
glm_data <- read_csv("data/glm-data/shark_features_glm.csv") %>%
  mutate(pid, Conv = ifelse(num_conversions > 0, TRUE, FALSE)) %>%
  mutate(pid, Conv_val = ifelse(num_conversions > 0, 1, 0))

glm_data_lag <- glm_data %>%
  group_by(pid) %>% 
  arrange(local_session_start) %>% 
  mutate(total_conversions = cumsum(Conv_val)) %>%
  mutate(total_conversions_lag = ifelse(total_conversions > 0, total_conversions-1, 0))

trim_data <- glm_data_lag %>% 
  select(
    'pid',
    'Conv',
    'num_page_visits',
    'session_duration',
    "dev_type",
    'intent_score',
    "local_bucketed_hours",
    "local_bucketed_days",
    "local_num_prev_sess",
    "local_total_duration",
    "local_total_page_visits",
    "local_prev_sess_length",
    "local_since_last_sess",
    "total_conversions_lag"
    ) %>% 
  mutate(local_since_last_sess=replace(local_since_last_sess, 
                                       local_since_last_sess > 172800,
                                       172800)) 

for_corr_plot <- trim_data %>%
  rename(PageVisitsInSession = num_page_visits) %>%
  rename(SessionDuration = session_duration) %>%
  rename(Device = dev_type) %>%
  rename(IntentScore = intent_score) %>%
  rename(HourOfDay = local_bucketed_hours) %>%
  rename(DayOfWeek = local_bucketed_days) %>%
  rename(TotalSessions = local_num_prev_sess) %>%
  rename(TotalDuration = local_total_duration) %>%
  rename(TotalPageVisits = local_total_page_visits) %>%
  rename(PreviousSessionDuration = local_prev_sess_length) %>%
  rename(InterSessionDuration = local_since_last_sess) %>%
  rename(TotalConversions = total_conversions_lag)

drops <- c('Device', 'Conv', 'pid')
res <- cor(for_corr_plot[ , !(names(for_corr_plot) %in% drops)])
corrplot::corrplot(res, method="color",
         type="lower", number.cex=1.1,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="red", tl.srt=20, tl.cex = 1.08,
         diag=FALSE, is.corr = F)

trim_data <- subset(trim_data, dev_type!='EReader')

set.seed(666); sample = sample.split(trim_data$Conv, SplitRatio = .75)
training_data = subset(trim_data, sample == TRUE)
test_data  = subset(trim_data, sample == FALSE)

full_model <- glm(
  formula = Conv ~ num_page_visits + session_duration + dev_type +
    intent_score + local_bucketed_hours + local_bucketed_days + 
    local_num_prev_sess + local_total_page_visits + local_total_duration + 
    local_prev_sess_length + local_since_last_sess + total_conversions_lag, 
  family='gaussian', 
  data = training_data)

summary(full_model)

intercept_model <- glm(
  formula = Conv ~ 1, 
  family='gaussian', 
  data = training_data)

summary(intercept_model)

step_back_model <- stepAIC(full_model, 
                      direction = "backward", 
                      trace = TRUE)

step_for_model <- stepAIC(intercept_model,
                           direction="forward",
                           scope=list(upper=full_model,
                                      lower=intercept_model))

step_both_model <- stepAIC(intercept_model,
                           direction="both",
                           scope=list(upper=full_model,
                                      lower=intercept_model))

summary(step_for_model)
exp(coef(step_for_model))
AIC(step_for_model)
logLik(step_for_model)

plot(predict(step_model, training_data, type = 'response'),
     resid(step_model),
     xlab = 'Fitted values',
     ylab = 'Residuals')

binnedplot(fitted(step_model), 
           residuals(step_model, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")

logistic_predictions <- predict(step_model, test_data, type = 'response')

logistic_predictions <- as.data.frame(logistic_predictions) %>% 
  mutate(logistic_predictions=replace(logistic_predictions, 
                                       logistic_predictions < 0,
                                       0)) %>%
  mutate(logistic_predictions=replace(logistic_predictions, 
                                      logistic_predictions > 1,
                                      1)) 

hist(logistic_predictions$logistic_predictions, xlab = 'Predicted probabilities', main = '')
abline(v = 0.3, col='red')
logistic_predictions_bool <- logistic_predictions > 0.3
table(logistic_predictions_bool, test_data$Conv)

plot(training_data$num_page_visits,
     resid(step_model),
     xlab = 'PageVisitsInSession',
     ylab = 'Residuals',
     col = factor(training_data$Conv), 
     cex.axis = 2.5,
     cex.lab = 2.5
     )
legend(x=75,
       y=1,
       c("Non-conversion","Conversion"),
       cex=1.8,
       col=c("red","black"),
       pch=c(1,1))

plot(training_data$intent_score,
     resid(step_model),
     xlab = 'IntentScore',
     ylab = 'Residuals',
     col = factor(training_data$Conv), 
     cex.axis = 2.5,
     cex.lab = 2.5)
legend(x=0.665,
       y=-0.65,
       c("Non-conversion","Conversion"),
       cex=1.8,
       col=c("red","black"),
       pch=c(1,1))

plot(training_data$total_conversions_lag,
     resid(step_model),
     xlab = 'TotalConversions',
     ylab = 'Residuals',
     col = factor(training_data$Conv), 
     cex.axis = 2.5,
     cex.lab = 2.5)
legend(x=3.,
       y=1,
       c("Non-conversion","Conversion"),
       cex=1.8,
       col=c("red","black"),
       pch=c(1,1))

plot(training_data$local_total_page_visits,
     resid(step_model),
     xlab = 'TotalPageVisits',
     ylab = 'Residuals',
     col = factor(training_data$Conv), 
     cex.axis = 2.5,
     cex.lab = 2.5)
legend(x=100,
       y=1,
       c("Non-conversion","Conversion"),
       cex=1.8,
       col=c("red","black"),
       pch=c(1,1))

# glm(formula = Conv ~ num_page_visits + dev_type + intent_score + 
#       local_num_prev_sess + local_total_duration + local_total_page_visits + 
#       local_prev_sess_length, 
#     family = "gaussian", 
#     data = training_data)
# 
# exp(coef(step_model))
# AIC(step_model)
