library(ggpubr)
library(fitdistrplus)
library(dplyr)
library(PairedData , warn.conflicts = FALSE)
library(ggplot2)

#TASK 1.A:

generateMiceData <- function(no_1) {
  weight_before = c(round(rnorm(200, mean = 20, sqrt(2)), 1))
  weight_after = c(round(rnorm(200, mean = 21, sqrt(2.5)), 1))
  weight <- append(weight_before, weight_after, after = length(weight_before))
  groups <- c(rep("Before", no_1), rep("After", no_1))
  data.frame(weight, groups = groups)
}

Mice = generateMiceData(200)

#TASK 1.B:

generateRatsData <- function(no_2) {
  weight_before = c(rweibull(200, shape = 10, scale = 20))
  weight_after = c(rweibull(200, shape = 9, scale = 21))
  weight <- append(weight_before, weight_after, after = length(weight_before))
  groups <- c(rep("Before", no_2), rep("After", no_2))
  data.frame(weight, groups = groups)
}

Rats = generateRatsData(200)

#TASK 1.C:

qplot(weight,ylab = "Density", data = Mice, geom = "density",
      color = groups, linetype = groups,  main = "                      Mice Density Curve")

qplot(weight,ylab = "Density", data = Rats, geom = "density",
      color = groups, linetype = groups, main = "                      Rats Density Curve")

#TASK 1.D:

qplot(weight, data = Mice, geom = "boxplot",
      color = groups, linetype = groups)

qplot(weight, data = Rats, geom = "boxplot",
      color = groups, linetype = groups, )

#PAIRED DATA PLOT FOR MICE:

# Subset weight data for Mice 'before'
before <- subset(Mice, groups == "Before", weight, 
                 drop = TRUE)

# subset weight data for Mice 'after'
after <- subset(Mice, groups == "After", weight,
                drop = TRUE)

pd <- paired(before, after)
plot(pd, type = "profile",) + theme_bw() 


#PAIRED DATA PLOT FOR RATS:

# Subset weight data for Mice 'before'
before <- subset(Rats, groups == "Before", weight, 
                 drop = TRUE)

# subset weight data for Mice 'after'
after <- subset(Rats, groups == "After", weight,
                drop = TRUE)

pd <- paired(before, after)
plot(pd, type = "profile",) + theme_bw()


#HISTOGRAMS:

qplot(weight,ylab = "Frequency", data = Mice, geom = "histogram",
      fill = groups, main = "                      Histogram of Mice")

qplot(weight, ylab = "Frequency", data = Rats, geom = "histogram",
      fill = groups, main = "                      Histogram of Rats")


#TASK 2.A:

##QQPLOT:

qqnorm(Mice$weight, pch = 1, frame = FALSE)
qqline(Mice$weight, col = "steelblue", lwd = 2)

qqnorm(Rats$weight, pch = 1, frame = FALSE)
qqline(Rats$weight, col = "steelblue", lwd = 2)

ggqqplot(Mice$weight, main = "                         Normal Q-Q Plot" )
ggqqplot(Rats$weight, main = "                         Normal Q-Q Plot" )

##SHAPIRO-WILK TEST:

shapiro.test(Mice$weight)
shapiro.test(Rats$weight)

mst <- with(Mice,
            weight[groups == "Before"] - weight[groups == "After"])

shapiro.test(mst)

rst <- with(Rats,
            weight[groups == "Before"] - weight[groups == "After"])

shapiro.test(rst)



#TASK 3 T-TEST:
res <- t.test(weight ~ groups,data = Mice,paired = TRUE)
res

mes <- t.test(weight ~ groups,data = Rats,paired = TRUE)
mes

#TASK 3.B:
#PERFORM WILCOXON TEST FOR RATS

wes <- wilcox.test(weight ~ groups,data = Rats,paired = TRUE)
wes

#task 4:

fit_w  <- fitdist(Rats$weight, "weibull")
fit_g  <- fitdist(Rats$weight, "gamma")
fit_ln <- fitdist(Rats$weight, "lnorm")

summary(fit_w)
summary(fit_g)
summary(fit_ln)

par(mfrow=c(2,2))
plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fit_w, fit_g, fit_ln), legendtext = plot.legend)
cdfcomp (list(fit_w, fit_g, fit_ln), legendtext = plot.legend)
qqcomp  (list(fit_w, fit_g, fit_ln), legendtext = plot.legend)
ppcomp  (list(fit_w, fit_g, fit_ln), legendtext = plot.legend)


