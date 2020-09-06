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
     ylab = 'Response residuals',
     cex.lab=2.5, 
     cex.axis=2)
plot(rstandard(poisson_regression_model)) 

poisson_predictions <- round(predict(poisson_regression_model, test, type = 'response'))
poisson_predictions <- poisson_predictions[which(poisson_predictions>0)]
hist(poisson_predictions, xlab = 'Predicted counts', main = '', cex.lab=2.5, cex.axis=2, breaks=seq(1,30,1))
table(poisson_predictions, test$ClicksDelta)

max(resid(poisson_regression_model))
max(poisson_regression_model$residuals)

AIC(poisson_regression_model)
BIC(poisson_regression_model)
