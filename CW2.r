library(ggplot2)
library(MASS)

ggplot(data = global, aes(x = year, y = temp)) + geom_point() +
  geom_smooth(formula = y ~ x, method = "loess", se = TRUE) +
  scale_x_continuous(name = "Years") + scale_y_continuous(name = "Temperature (°C)") +
  ggtitle("Air surface temperature (in °C) from 1857 to 2021")

model = lm(temp ~ year, data = global)
summary(model)
confint(model, level = 0.95)

x_bar = sum(global$temp)/length(global$temp)
sigma_sq = 0.000401

ggplot(data = global, aes(x = year, y = temp)) + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE) +
  scale_x_continuous(name = "Years") + scale_y_continuous(name = "Temperature (°C)") +
  ggtitle("Air surface temperature (in °C) from 1857 to 2021")


cd_cont_pos <- function(leverage, level, model)
  sqrt(level*length(coef(model))*(1-leverage)/leverage)
cd_cont_neg <- function(leverage, level, model)
  -cd_cont_pos(leverage, level, model)
level <- 4/nrow(model$model)
## Residuals vs Fitted values
plot1 <- ggplot(data = model, mapping = aes(x = .fitted, y = .resid)) +
  geom_point() + geom_smooth(method = "loess", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(x = "Fitted values", y = "Residuals")
## Quantile-Quantile plot
plot2 <- ggplot(data = model, mapping = aes(sample = .stdresid)) +
  geom_qq() + geom_qq_line() +
  labs(x = "Theoretical quantiles", y = "Standardised residuals")
## Standardised residuals vs Leverage
plot3 <- ggplot(data = model, mapping = aes(x = .hat, y = .stdresid)) +
  geom_point() + geom_smooth(method = "loess", se = FALSE) +
  stat_function(fun = cd_cont_pos, args = list(level = level, model = model)) +
  stat_function(fun = cd_cont_neg, args = list(level = level, model = model)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(x = "Leverage", y = "Standardised residuals") +
  lims(x = c(0,NA), y = c(-3,+3))
grid.arrange(plot1, plot2, plot3, nrow = 1, ncol = 3)

year_2=(global$year)^2
year_3=(global$year)^3
year_4=(global$year)^4
year_5=(global$year)^5
y = global$temp
x = global$year
new_global = data.frame(y, x, year_2, year_3, year_4, year_5)
step(lm(y~1, data = new_global), scope = y~x+year_2^2+year_3^3+year_4^4+year_5^5, direction="forward")

set.seed(165)
new_model = lm(formula = y ~ x + year_2 + year_3 + year_4, data = global)
fit=lm(temp~poly(year, 4), data = global)
model = lm(temp ~ year, data = global)
anova(model, fit)

ggplot(data = global, aes(x = year, y = temp)) + geom_point() + 
  stat_smooth(method = lm, formula = y ~ poly(x, 4)) +
  ylab("Temperature") + xlab("Years")

new_global <- global
new_global$pred1 <- predict(lm(temp ~ poly(year, 4), data=new_global))
pred2 <- data.frame(year=1849:2040)
pred2$temp <- predict(lm(temp ~ poly(year, 4), data=new_global),newdata=pred2)


predicted.intervals <- predict(lm(temp ~ poly(year, 4), data=new_global),newdata=pred2, interval = "confidence", level = 0.95)

ggplot(data = global, aes(x = year, y = temp)) + geom_point() + 
  stat_smooth(method = lm, formula = y ~ poly(x, 4)) +
  ylab("Temperature") + xlab("Years") + geom_line(color = "red", data = pred2)+
  ggtitle("Extrapolation") 
