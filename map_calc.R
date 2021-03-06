# 1. INSTALL AND LOAD PACKAGES
require(BAMMtools) # required
require(tidycensus); require(tidyverse) # not required: used for test data download

# 2. FUNCTIONS
# Obtain percentage lower-bound errors
lower <- function(x, i){
  pnorm(as.numeric(as.character(x[i]$lowerBound)),
        mean = x[i]$estimate, sd = x[i]$sd,
        lower.tail = TRUE) * 100
}
# Obtain percentage upper-bound errors
upper <- function(x, i){
  pnorm(as.numeric(as.character(x[i]$upperBound)),
        mean = x[i]$estimate, sd = x[i]$sd,
        lower.tail = FALSE) * 100
}
# Obtain the mean error by map class
average <- function(x, i){
  mean(x[i])
}

# 3. LOAD AND CLEAN DATA
dat <- get_acs(state = "PA", county = 101, geography = "tract", variables = "B03003_003") %>%
  select(GEOID, estimate, moe) %>%
  mutate(sd = moe / 1.645) %>%
  drop_na()
noClasses <- 3

# 4. DEFINE BREAKS
eqInterval <- function(x, i){
  seq(from = min(x),
      to = max(x),
      by = (max(x) - min(x)) / i)
}
eqIntervalBreaks <- eqInterval(dat$estimate, noClasses)
jenksBreaks <- getJenksBreaks(dat$estimate, noClasses + 1)
quantileBreaks <- quantile(dat$estimate, probs = seq(0, 1, length = noClasses + 1))
stDevBreaks <- function(x, i){
  halfStDevCount <- c(-1 * rev(seq(1, i, by = 2)),
                      seq(1, i, by = 2))
  if((i %% 2) == 1) {
    halfStDevBreaks <- unlist(lapply(halfStDevCount,
                                     function(i) (0.5 * i * sd(x)) + mean(x)))
    halfStDevBreaks[[1]] <- ifelse(min(x) < halfStDevBreaks[[1]],
                                   min(x), halfStDevBreaks[[1]])
    halfStDevBreaks[[i + 1]] <- ifelse(max(x) > halfStDevBreaks[[i + 1]],
                                               max(x), halfStDevBreaks[[i + 1]])
  } else {
    halfStDevBreaks <- NA
  }
  return(halfStDevBreaks)
}
halfStDevBreaks <- stDevBreaks(dat$estimate, noClasses)
# User-Defined
# Must include lowest and highest observation: if you want 4 classes, you'll have 5 numbers here
userBreaks <- c(min(dat$estimate), 100, 200, 400, 1000, max(dat$estimate))

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
errorsList <- lapply(errorsList, function(x) {
  x[] <- lapply(x, as.numeric)
  x
})
errorsList[] <- lapply(errorsList, round, 3)
errorsList
