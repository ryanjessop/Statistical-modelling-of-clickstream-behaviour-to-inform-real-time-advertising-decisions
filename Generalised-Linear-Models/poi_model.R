# Poisson regression model

poisson_regression_model <- glm(ClicksDelta ~ ImpressionDelta + KeywordMatchType 
                                + MaxCpc + CreativeQualityScore + hod, 
                                family = "poisson", 
                                data = train)
summary(poisson_regression_model)
exp(coef(poisson_regression_model))
plot(poisson_regression_model$fitted.values)
plot(poisson_regression_model$residuals)
plot(poisson_regression_model$fitted.values, 
     resid(poisson_regression_model), 
     xlab = 'Fitted values',
     ylab = 'Response residuals')
plot(rstandard(poisson_regression_model)) 

poisson_predictions <- round(predict(poisson_regression_model, test, type = 'response'))
hist(poisson_predictions, xlab = 'Predicted counts', main = '')
table(poisson_predictions, test$ClicksDelta)

max(resid(poisson_regression_model))
max(poisson_regression_model$residuals)

AIC(poisson_regression_model)
BIC(poisson_regression_model)
