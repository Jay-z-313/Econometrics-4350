library(wooldridge)
data(wage2)
mean_wage <- mean(wage2$wage, na.rm=TRUE)
mean_iq   <- mean(wage2$IQ,   na.rm=TRUE)
sd_iq     <- sd(wage2$IQ,     na.rm=TRUE)
predict(m, newdata = data.frame(IQ = 101))
b1 <- coef(m)["IQ"]
15*b1
summary(m)$r.squared
mean_wage; mean_iq; sd_iq

#The estimated regression shows that each additional IQ point is associated with 
#about $8.30 higher monthly wage, and a person with an IQ of 101 is predicted to 
#earn about $956 per month. However, the R² value of 0.096 indicates that IQ 
#explains less than 10% of the variation in wages, meaning most differences in 
#earnings are driven by other unobserved factors such as education, experience,
#and occupation.
