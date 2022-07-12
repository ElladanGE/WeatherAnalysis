library(ggplot2)
n=3650
day <- alfheim$x
january <- c("Jan 1st Y1", "Jan 1st Y2", "Jan 1st Y3", "Jan 1st Y4","Jan 1st Y5","Jan 1st Y6","Jan 1st Y7","Jan 1st Y8","Jan 1st Y9","Jan 1st Y10","")
rainfall <- alfheim$y
ggplot(data = alfheim, aes(x = x, y = y)) + geom_point() + 
  scale_x_continuous(name ="Time", breaks = seq(0,3650, 365), labels = january) + 
  ylab("Daily Rainfall (mm/day)") +
  ggtitle("Daily Rainfall in the village of Alfheim recorded over 10 years (3650 days)")

ggplot(data = alfheim) + geom_histogram(aes(x = y, y = stat(density)), bins=20) +
  xlab("Rainfall (mm/day)") + ylab("Density") + ggtitle("Daily Rainfall Histogram")

ggplot(data = alfheim %>% filter(y > 1)) + geom_histogram(aes(x = y, y = stat(density)), bins=30) +
  scale_x_continuous(name = "Rainfall (mm/day)",  breaks = seq(0,150,10),limit = c(1,150)) + ylab("Density") + ggtitle("Daily Rainfall Histogram (rainfall over 1mm)")

ggplot(data = alfheim %>% filter(y <= 1)) + geom_histogram(aes(x = y, y = stat(density)), bins=100) +
  xlab("Rainfall (mm/day)") + ylab("Density") + ggtitle("Daily Rainfall Histogram (rainfall under 1mm)")

alfheim$year[1:365]=1
alfheim$year[366:730]=2
alfheim$year[731:1094]=3
alfheim$year[1095:1459]=4
alfheim$year[1460:1825]=5
alfheim$year[1826:2190]=6
alfheim$year[2191:2555]=7
alfheim$year[2556:2920]=8
alfheim$year[2921:3285]=9
alfheim$year[3286:3650]="x10"

ggplot(data = alfheim, aes(x = year, y = y), fct_inseq()) + 
  geom_boxplot(alpha = 0.8, na.rm = TRUE) +
  scale_x_discrete(name = "Year", labels = c(1:10)) + 
  scale_y_continuous(name = "Rainfall (mm)",breaks = seq(0, 150, 25), limit = c(1,160)) +
  ggtitle("Daily Boxplot (rainfall over 1mm)")

ggplot(data = alfheim, aes(x = year, y = y)) + 
  geom_boxplot(alpha = 0.8, na.rm = TRUE) +
  scale_x_discrete(name = "Year", labels = c(1:10)) + 
  scale_y_continuous(name = "Rainfall (mm)",breaks = seq(0, 1, 0.1), limit = c(0,1)) +
  ggtitle("Daily Boxplot (rainfall under 1mm)")



m=sum((alfheim$y)>1)
phi_hat = m/3650
y_bar = with(alfheim, sum(y[y > 1]))
theta_hat = 1/y_bar


y1 <- alfheim %>% select(y) %>% filter(y>1)
y2 <- alfheim %>% select(y) %>% filter(y<=1)

loglik <- function(p, y1, y2) {
  m <- length(y1[,1])
  n <- length(y2[,1])
  if(p[1] <= 0) return(-1e20)
  if(p[2] <= 0) return(-1e20)
  n*log(1-p[2]) + m*log(p[1]*p[2]) - p[1]*m*sum(y1)
}
optim(c(0.00009, 0.3),control = list(fnscale = -1), loglik, y1 = y1, y2 = y2)


