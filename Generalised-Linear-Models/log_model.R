# Logistic regression model

logistic_regression_model <- glm(click_bool ~ ImpressionDelta + KeywordMatchType 
                                 + MaxCpc + CreativeQualityScore + hod, 
                                 family = binomial(link='logit'),
                                 data = train)
summary(logistic_regression_model)
exp(coef(logistic_regression_model))
AIC(logistic_regression_model)

plot(logistic_regression_model$fitted.values,
     resid(logistic_regression_model, type='pearson'),
     xlab = 'Fitted values',
     ylab = 'Pearson residuals')
plot(logistic_regression_model$fitted.values,
     resid(logistic_regression_model),
     xlab = 'Fitted values',
     ylab = 'Residuals')
plot(resid(logistic_regression_model),
     xlab = 'Index',
     ylab = 'Residuals')

binnedplot(fitted(logistic_regression_model), 
           residuals(logistic_regression_model, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")

residuals(logistic_regression_model, type = "deviance")
anova(logistic_regression_model)
stepAIC(logistic_regression_model)

outliers_in_data <- find_outlier_resid_func(train, residuals(logistic_regression_model, type='response'))
train_wo_outliers <- remove_outliers_func(train, outliers_in_data)

logistic_predictions <- predict(logistic_regression_model, test, type = 'response')
hist(logistic_predictions, xlab = 'Predicted probabilities', main = '')
abline(v = 0.1, col='red')
logistic_predictions_bool <- logistic_predictions > 0.1
table(logistic_predictions_bool, test$click_bool)

AIC(logistic_regression_model)
BIC(logistic_regression_model)
