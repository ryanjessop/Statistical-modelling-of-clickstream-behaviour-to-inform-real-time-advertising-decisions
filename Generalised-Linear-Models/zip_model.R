# Zero inflated Poisson model

zip_model <- zeroinfl(ClicksDelta ~ ImpressionDelta + KeywordMatchType 
                      + MaxCpc + CreativeQualityScore + hod | ImpressionDelta,
                      data = train)
summary(zip_model)
exp(coef(zip_model))
AIC(zip_model)

zip_model$df.residual

plot(zip_model$residuals, ylab = 'Residuals')

plot(zip_model$fitted.values,
     resid(zip_model), 
     xlab = 'Fitted values',
     ylab = 'Residuals')

identify(zip_model$residuals, zip_model$fitted.values)  # 2 huge outliers 

mean(zip_model$fitted.values)
var(zip_model$fitted.values)  # Check for mean = variance
hist(zip_model$fitted.values)

zip_predictions <- round(predict(zip_model, test, type = 'response'))
hist(zip_predictions, main = '', xlab = 'Predicted count')
table(zip_predictions, test$ClicksDelta)

AIC(zip_model)
BIC(zip_model)
