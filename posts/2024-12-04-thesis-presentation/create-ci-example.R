library(ggplot2)
library(gridExtra)

nsim = 25
n = 100
ciArray <- array(NA, dim = c(nsim, 3))

for(i in 1:nsim) {
  x = rnorm(n, mean = 10, sd = 0.5) # true mean 10, true sd 1
  ciArray[i,] <- c(t.test(x)$conf.int[1], t.test(x)$conf.int[2], t.test(x)$estimate)
}

ciArray <- as.data.frame(ciArray)
ciArray$nsim <- c(1:nsim)
ciArray$nsim <- as.numeric(ciArray$nsim)
colnames(ciArray) <- c("Low", "High", "Point", "Simulation")

p_smallci <- ggplot(ciArray, aes(Simulation, Point)) +
  geom_point() + 
  geom_errorbar(aes(ymin = Low, ymax = High)) +
  geom_hline(yintercept = 10) + ylim(9.5,10.5)


## big
ciArray_b <- array(NA, dim = c(nsim, 3))
for(i in 1:nsim) {
  x = rnorm(n, mean = 10, sd = 1) # true mean 10, true sd 1
  ciArray_b[i,] <- c(t.test(x)$conf.int[1], t.test(x)$conf.int[2], t.test(x)$estimate)
}
ciArray_b <- as.data.frame(ciArray_b)
ciArray_b$nsim <- c(1:nsim)
ciArray_b$nsim <- as.numeric(ciArray_b$nsim)
colnames(ciArray_b) <- c("Low", "High", "Point", "Simulation")

p_bigci <- ggplot(ciArray_b, aes(Simulation, Point)) +
  geom_point() + 
  geom_errorbar(aes(ymin = Low, ymax = High)) +
  geom_hline(yintercept = 10) + ylim(9.5,10.5)

## smaller
ciArray_s <- array(NA, dim = c(nsim, 3))
for(i in 1:nsim) {
  x = rnorm(n, mean = 10, sd = 0.2) # true mean 10, true sd 1
  ciArray_s[i,] <- c(t.test(x)$conf.int[1], t.test(x)$conf.int[2], t.test(x)$estimate)
}
ciArray_s <- as.data.frame(ciArray_s)
ciArray_s$nsim <- c(1:nsim)
ciArray_s$nsim <- as.numeric(ciArray_s$nsim)
colnames(ciArray_s) <- c("Low", "High", "Point", "Simulation")

p_minici <- ggplot(ciArray_s, aes(Simulation, Point)) +
  geom_point() + 
  geom_errorbar(aes(ymin = Low, ymax = High)) +
  geom_hline(yintercept = 10) + ylim(9.5,10.5)

## smallest
ciArray_mm <- array(NA, dim = c(nsim, 3))
for(i in 1:nsim) {
  x = rnorm(n, mean = 10, sd = 0.09) # true mean 10, true sd 1
  ciArray_mm[i,] <- c(t.test(x)$conf.int[1], t.test(x)$conf.int[2], t.test(x)$estimate)
}
ciArray_mm <- as.data.frame(ciArray_mm)
ciArray_mm$nsim <- c(1:nsim)
ciArray_mm$nsim <- as.numeric(ciArray_mm$nsim)
colnames(ciArray_mm) <- c("Low", "High", "Point", "Simulation")

p_miniminici <- ggplot(ciArray_mm, aes(Simulation, Point)) +
  geom_point() + 
  geom_errorbar(aes(ymin = Low, ymax = High)) +
  geom_hline(yintercept = 10) + ylim(9.5,10.5)

## check true proportion is 95% coverage
#mean(1*(ciArray[,1]<10 & ciArray[,2]>10)) # this interval will contain "10"

dta_all <- rbind(ciArray_b, ciArray_mm, ciArray_s)
dta_all <-dta_all[sample(1:nrow(dta_all)), ]
dta_some <- sample_n(dta_all, 25)
dta_some$Simulation <- c(1:25)

p_some <- ggplot(dta_some, aes(Simulation, Point)) +
  geom_point() + 
  geom_errorbar(aes(ymin = Low, ymax = High)) +
  geom_hline(yintercept = 10) + ylim(9.5,10.5) +
  labs(y = NULL)

svg(file = "~/Documents/thesis/images/ci-example-weird.svg")
cis <- grid.arrange(p_smallci, p_some, nrow = 1)
dev.off()
