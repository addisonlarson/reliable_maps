# Map Classification Error Calculator
# Great in R Studio. Doesn't translate to R Shiny.

# Script proceeds in the following sections:
# 1. INSTALL AND LOAD PACKAGES
# 2. FUNCTIONS
# 3. LOAD DATA (this section will likely change)
# 4. DEFINE BREAKS
# 5. CALCULATE EXPECTED PERCENTAGE CLASSIFICATION ERROR
#   5a. Equal Interval
#   5b. Jenks
#   5c. Quantiles
#   5d. Standard Deviation
#   5e. User-Defined Breaks
# 6. VIEW RESULTS

# 1. INSTALL AND LOAD PACKAGES 
rm(list=ls())
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("foreign", "rgdal", "BAMMtools", "sp")
pack(packages)

# 2. FUNCTIONS
# Obtain percentage lower-bound errors
lower <- function(x, i){
  pnorm(as.numeric(as.character(x[i][, 6])),
        mean = x[i][, 1], sd = x[i][, 3],
        lower.tail = TRUE) * 100
}
# Obtain percentage upper-bound errors
upper <- function(x, i){
  pnorm(as.numeric(as.character(x[i][, 7])),
        mean = x[i][, 1], sd = x[i][, 3],
        lower.tail = FALSE) * 100
}
# Obtain the mean error by map class
average <- function(x, i){
  mean(x[i])
}
# Set up the function and data frame you'll need
# to suggest the lowest-error choropleth mapping scheme
suggestion <- function(x, i){
  round(weighted.mean(x[i][, 4], x[i][, 3]), digits = 4)
}
Method <- c("Equal Interval", "Jenks", "Quantile",
            "Standard Deviation", "User-Defined Interval")
suggest <- data.frame(Method)
# Rounding
rounding <- function(x, i){
}

# 3. LOAD AND CLEAN DATA
ipd <- read.csv("D:/alarson/MapReliabilityTool/1_TAZ_C13.csv")
# ipd <- read.csv("D:/alarson/MapReliability/full_ipd.csv")
# ipd[which(ipd$GEO_ID == 42101980900),] <- NA # Alex deleted these records
# estimate <- ipd$Disabled_PercEst * 100
# moe <- ipd$Disabled_PercMoe * 100
# geoid <- ipd$GEO_ID
geoid <- "FAKE"
ipd <- cbind(geoid, ipd)
noClasses <- 3
estimate <- ipd$taz_est
moe <- ipd$taz_moe
# According to https://www.census.gov/content/dam/Census/programs-surveys/acs/guidance/training-presentations/20180418_MOE.pdf
# Slide 12, MOE = 1.645 * sqrt(variance)
sd <- moe / 1.645
dat <- data.frame(estimate = estimate, moe = moe, sd = sd, geoid = geoid)
dat[dat == "-"] <- NA; dat[dat == "**"] <- NA
dat <- dat[complete.cases(dat), ]
estimate <- dat$estimate
moe <- dat$moe

# # Want to test this data in NYC's calculator?
# exportData <- data.frame(estimate = estimate, moe = moe)
# write.csv(exportData, file = "D:/alarson/MapReliability/test data.csv", row.names = FALSE)

# 4. DEFINE BREAKS
# Equal Interval
eqIntervalBreaks <- seq(from = min(estimate),
                        to = max(estimate),
                        by = (max(estimate) - min(estimate)) / noClasses)
# Jenks
jenksBreaks <- getJenksBreaks(estimate, noClasses + 1)
# Quantile
quantileBreaks <- quantile(estimate,
                           probs = seq(0, 1, length = noClasses + 1))
# Half-Standard Deviation (+- 0.5, +- 1.5, etc.)
halfStDevCount <- c(-1 * rev(seq(1, noClasses, by = 2)),
                    seq(1, noClasses, by = 2))
if((noClasses %% 2) == 1) {
  halfStDevBreaks <- unlist(lapply(halfStDevCount,
                                   function(i) (0.5 * i * sd(estimate)) + mean(estimate)))
  halfStDevBreaks[[1]] <- ifelse(min(estimate) < halfStDevBreaks[[1]],
                                 min(estimate), halfStDevBreaks[[1]])
  halfStDevBreaks[[noClasses + 1]] <- ifelse(max(estimate) > halfStDevBreaks[[noClasses + 1]],
                                             max(estimate), halfStDevBreaks[[noClasses + 1]])
} else {
  halfStDevBreaks <- NA
}
# User-Defined
userBreaks <- c(min(estimate), 8, 12, 15, 40, max(estimate)) # Must include lowest and highest observation: if you want 4 classes, you'll have 5 numbers here

# 5. CALCULATE EXPECTED PERCENTAGE CLASSIFICATION ERROR
# 5a. Equal Interval
dat$classCode <- cut(dat$estimate, labels = FALSE, breaks = eqIntervalBreaks,
                     include.lowest = TRUE, right = TRUE)
dat$lowerBound <- cut(dat$estimate,
                      breaks = eqIntervalBreaks,
                      labels = c(eqIntervalBreaks[1:length(eqIntervalBreaks) - 1]),
                      include.lowest = TRUE, right = TRUE)
dat$upperBound <- cut(dat$estimate,
                      breaks = eqIntervalBreaks,
                      labels = c(eqIntervalBreaks[2:length(eqIntervalBreaks)]),
                      include.lowest = TRUE, right = TRUE)
dat$classCode <- paste0("Class", dat$classCode)
dat2 <- split(dat, dat$classCode)
dat2[[1]]$lowerBound <- -Inf
dat2[[length(dat2)]]$upperBound <- Inf

lowerBoundErrors <- lapply(dat2, lower)
upperBoundErrors <- lapply(dat2, upper)
totalObs <- lapply(dat2, function(i) nrow(i))
totalErrorsE <- as.data.frame(sapply(lowerBoundErrors, average))
colnames(totalErrorsE)[1] <- "LowerBound"
totalErrorsE$UpperBound <- sapply(upperBoundErrors, average)
totalErrorsE$totalObs <- totalObs
totalErrorsE[4] <- totalErrorsE[1] + totalErrorsE[2]
colnames(totalErrorsE)[4] <- "OvrAvr"

# 5b. Jenks
dat$classCode <- cut(dat$estimate, labels = FALSE, breaks = jenksBreaks,
                     include.lowest = TRUE, right = TRUE)
dat$lowerBound <- cut(dat$estimate,
                      breaks = jenksBreaks,
                      labels = c(jenksBreaks[1:length(jenksBreaks) - 1]),
                      include.lowest = TRUE, right = TRUE)
dat$upperBound <- cut(dat$estimate,
                      breaks = jenksBreaks,
                      labels = c(jenksBreaks[2:length(jenksBreaks)]),
                      include.lowest = TRUE, right = TRUE)
dat$classCode <- paste0("Class", dat$classCode)
dat2 <- split(dat, dat$classCode)
dat2[[1]]$lowerBound <- -Inf
dat2[[length(dat2)]]$upperBound <- Inf

lowerBoundErrors <- lapply(dat2, lower)
upperBoundErrors <- lapply(dat2, upper)
totalObs <- lapply(dat2, function(i) nrow(i))
totalErrorsJ <- as.data.frame(sapply(lowerBoundErrors, average))
colnames(totalErrorsJ)[1] <- "LowerBound"
totalErrorsJ$UpperBound <- sapply(upperBoundErrors, average)
totalErrorsJ$totalObs <- totalObs
totalErrorsJ[4] <- totalErrorsJ[1] + totalErrorsJ[2]
colnames(totalErrorsJ)[4] <- "OvrAvr"

# 5c. Quantile
dat$classCode <- cut(dat$estimate, labels = FALSE, breaks = quantileBreaks,
                     include.lowest = TRUE, right = TRUE)
dat$lowerBound <- cut(dat$estimate,
                      breaks = quantileBreaks,
                      labels = c(quantileBreaks[1:length(quantileBreaks) - 1]),
                      include.lowest = TRUE, right = TRUE)
dat$upperBound <- cut(dat$estimate,
                      breaks = quantileBreaks,
                      labels = c(quantileBreaks[2:length(quantileBreaks)]),
                      include.lowest = TRUE, right = TRUE)
dat$classCode <- paste0("Class", dat$classCode)
dat2 <- split(dat, dat$classCode)
dat2[[1]]$lowerBound <- -Inf
dat2[[length(dat2)]]$upperBound <- Inf

lowerBoundErrors <- lapply(dat2, lower)
upperBoundErrors <- lapply(dat2, upper)
totalObs <- lapply(dat2, function(i) nrow(i))
totalErrorsQ <- as.data.frame(sapply(lowerBoundErrors, average))
colnames(totalErrorsQ)[1] <- "LowerBound"
totalErrorsQ$UpperBound <- sapply(upperBoundErrors, average)
totalErrorsQ$totalObs <- totalObs
totalErrorsQ[4] <- totalErrorsQ[1] + totalErrorsQ[2]
colnames(totalErrorsQ)[4] <- "OvrAvr"

# 5d. Standard Deviation
if((noClasses %% 2) == 1) {
  dat$classCode <- cut(dat$estimate, labels = FALSE, breaks = halfStDevBreaks,
                       include.lowest = TRUE, right = TRUE)
  dat$lowerBound <- cut(dat$estimate,
                        breaks = halfStDevBreaks,
                        labels = c(halfStDevBreaks[1:length(halfStDevBreaks) - 1]),
                        include.lowest = TRUE, right = TRUE)
  dat$upperBound <- cut(dat$estimate,
                        breaks = halfStDevBreaks,
                        labels = c(halfStDevBreaks[2:length(halfStDevBreaks)]),
                        include.lowest = TRUE, right = TRUE)
  dat$classCode <- paste0("Class", dat$classCode)
  dat2 <- split(dat, dat$classCode)
  dat2[[1]]$lowerBound <- -Inf
  dat2[[length(dat2)]]$upperBound <- Inf
  
  lowerBoundErrors <- lapply(dat2, lower)
  upperBoundErrors <- lapply(dat2, upper)
  totalObs <- lapply(dat2, function(i) nrow(i))
  totalErrorsS <- as.data.frame(sapply(lowerBoundErrors, average))
  colnames(totalErrorsS)[1] <- "LowerBound"
  totalErrorsS$UpperBound <- sapply(upperBoundErrors, average)
  totalErrorsS$totalObs <- totalObs
  totalErrorsS[4] <- totalErrorsS[1] + totalErrorsS[2]
  colnames(totalErrorsS)[4] <- "OvrAvr"
} else {
  totalErrorsS <- NULL
}

# 5e. User-Defined
dat$classCode <- cut(dat$estimate, labels = FALSE, breaks = userBreaks,
                     include.lowest = TRUE, right = TRUE)
dat$lowerBound <- cut(dat$estimate,
                      breaks = userBreaks,
                      labels = c(userBreaks[1:length(userBreaks) - 1]),
                      include.lowest = TRUE, right = TRUE)
dat$upperBound <- cut(dat$estimate,
                      breaks = userBreaks,
                      labels = c(userBreaks[2:length(userBreaks)]),
                      include.lowest = TRUE, right = TRUE)
dat$classCode <- paste0("Class", dat$classCode)
dat2 <- split(dat, dat$classCode)
dat2[[1]]$lowerBound <- -Inf
dat2[[length(dat2)]]$upperBound <- Inf

lowerBoundErrors <- lapply(dat2, lower)
upperBoundErrors <- lapply(dat2, upper)
totalObs <- lapply(dat2, function(i) nrow(i))
totalErrorsU <- as.data.frame(sapply(lowerBoundErrors, average))
colnames(totalErrorsU)[1] <- "LowerBound"
totalErrorsU$UpperBound <- sapply(upperBoundErrors, average)
totalErrorsU$totalObs <- totalObs
totalErrorsU[4] <- totalErrorsU[1] + totalErrorsU[2]
colnames(totalErrorsU)[4] <- "OvrAvr"

# VIEW RESULTS
# Boundary errors by choropleth map classification scheme
errorsList <- list(totalErrorsE,
                   totalErrorsJ,
                   totalErrorsQ,
                   totalErrorsS,
                   totalErrorsU)
errorsList <- lapply(errorsList, rounding)
errorsList
# Suggest scheme with overall lowest percentage error
suggest$PctError <- sapply(errorsList, suggestion)
suggest
# Note if user put in the wrong number of breaks
if (length(userBreaks) != noClasses + 1){
  print("Incorrect number of user-defined breaks.")
}
