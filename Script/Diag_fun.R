diagnostic_lm <- function(model, y_obs){
  par(mfrow=c(2,2))
  # Residui
  hist(model$residuals, freq = F, breaks = 10, main = "Istogramma dei residui",
       xlab = "Residui")
  curve(dnorm(x, mean(model$residuals), sd(model$residuals)), add = T, col = 2, lty = 2, lwd = 2)

  # qqnorm
  qqnorm(scale(model$residuals), pch = 20)
  abline(0,1) 

  # Fitted vs Residuals
  plot(model$fitted.values, model$residuals, pch = 20, main = "Fit VS Res", xlab = "Fitted values",
       ylab = "Residuals")
  abline(h = 0, lty = 2, col = "grey", lwd = 2)  
  
  # Fitted vs Observed
  plot(y_obs, model$fitted.values, pch = 20, main = "Obs VS Fit", xlab = "Observed",
       ylab = "Fitted")  
  abline(0,1, lwd = 2, lty = 2, col = 2)  
  legend("bottomright",legend = paste("R2 = ",round(summary(model)$adj.r.squared, 3)), bty = "n", cex = .7)

}
