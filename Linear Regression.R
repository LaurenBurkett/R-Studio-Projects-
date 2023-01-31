#import ggplot for usage in linear regression
library(ggplot2)


summary(MIS470GDP)

# Scatter plot for GDP & USEUR

x <- MIS470GDP$GDP
y <- MIS470GDP$USEUR

plot(x, y, main = "A Comparison")

# Linear regression for GDP and USEUR variables 

linRegModel <- ggplot(MIS470GDP, aes(GDP, USEUR)) +
  geom_point()  +
  stat_smooth(method = lm)



# Computation of correlation coefficient 

 cor(x, y, method = c("pearson"))
   
# Computation for coefficients for GDP and USEUR

model <- lm(USEUR ~ GDP, data = MIS470GDP)
model

# predict usueur for gdp

coeff <- coef(model)
intercept <- coeff[1]
slope <- coeff[2]
prediction <- intercept + (slope*22007.372)
prediction


# Summary of the linear regression model

summary(model)

# Computation of confidence interval

confint(model)
