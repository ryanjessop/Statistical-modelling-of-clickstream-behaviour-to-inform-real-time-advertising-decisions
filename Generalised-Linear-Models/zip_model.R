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
     ylab = 'Residuals',
     cex.lab=2.5, 
     cex.axis=2)

identify(zip_model$residuals, zip_model$fitted.values)  # 2 huge outliers 

mean(zip_model$fitted.values)
var(zip_model$fitted.values)  # Check for mean = variance
hist(zip_model$fitted.values)

zip_predictions <- round(predict(zip_model, test, type = 'response'))
zip_predictions <- zip_predictions[which(zip_predictions>0)]
zip_predictions <- zip_predictions[which(zip_predictions<=31)]
hist(zip_predictions, main = '', xlab = 'Predicted count', cex.lab=2.5, cex.axis=2, breaks=seq(1,31,1))
table(zip_predictions, test$ClicksDelta)

AIC(zip_model)
BIC(zip_model)


zero_index <- which(test$ImpressionDelta==0)
zip_predictions <- zip_predictions[which(zip_predictions<=31)]
hist(zip_predictions, main = '', xlab = 'Predicted count', cex.lab=2.5, cex.axis=2, breaks=seq(1,31,1))
table(zip_predictions[zero_index], test$ClicksDelta[zero_index])

table(zip_predictions[-zero_index], test$ClicksDelta[-zero_index])

1/(1+exp(-(17.59 - 16.79*4)))

