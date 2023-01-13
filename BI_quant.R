################################################################################

# Exercise 1 + Data manipulation

# Take Home Exam - Quantitative Risk and Asset Management

rm(list = ls())
cat("\014")
library(moments)
library(tidyverse)
setwd("~/a_UNIVERSITA'/Esami BI Norvegia/Quant. Risk & Asset Man. BI/Take Home Exam")

# Preliminary work on the dataset

# Loading the data
data <- read.csv("Industry17PortfoliosDaily.csv", skip = 9, header = T)

# First, renaming the column names for readability  
colnames(data) <- c('date','food','mines','oil','clothes','durables','chemicals', 
                    'consumables','construction','steel','fabricated','machinery',
                    'cars','transport','utilities','retail','finance','others')
summary(data) # looks like there are no -99.99 or -999 values, so no missing
# values in the dataset

# Converting the 'date' column variable into a R-compatible format

year      <- substring(data$date,1,4)           # store substring representing year
mont      <- substring(data$date,5,6)           # store substring representing month
day       <- substring(data$date,7,8)           # substring representing the day
temp      <- paste(year, mont, day, sep = '-')  # create string representing full date 
date1     <- as.Date(temp) 
data$date <- date1

# Dividing the full sample by 100 in order not to have returns in percentage
data <- data.frame(data[, "date"] , data[,-1] / 100)
names(data)[names(data) == "data....date.."] <- "date" # renaming first column

################################################################################

# Question 1
# For the sample 1960-2022: on average, do more volatile industries have higher
# average log returns? And higher average returns?

# Extracting the data from 1960 to 2022 from the full dataset
data60.22 <- data[data[, 1] > "1960-01-01", ] 

# Computing the log of the 1960-2022 dataset
log.data.60.22 <- data.frame(data60.22$date, log(data60.22[, -1] + 1) )

# Average annualized returns for every industry
avg.ret60.22 <- colMeans(data60.22[, -1] ) * 252 

# Annualized Volatility (standard deviation) for every industry
industry.std60.22 <- apply(data60.22[, -1], 2, sd) * sqrt(252) 

# Defining Log returns for every industry, daily basis
log.ret60.22 <- apply((data60.22[, -1] + 1), 2, log) # + 1 to compute log returns

# Checking for NaNs and infinite values in the log returns:
which(is.infinite(log.ret60.22)) # no infinite values
which(is.nan(log.ret60.22))      # no NaNs

# Computing annualized average log returns for every industry
avg.log.ret60.22 <- colMeans(log.ret60.22, na.rm = T) * 252

# Checking for non Gaussianity in the data 1960 - 2022
skew <- apply(data60.22[, -1], 2, skewness ) 
kurt <- apply(data60.22[, -1], 2, kurtosis )
mean(kurt) 
median(kurt)

# Trying to remove the observation that might be an outlier compared to the others
avg.ret60.22.no.outlier      <- avg.ret60.22[avg.ret60.22 > 0.09]
industry.std60.22.no.outlier <- industry.std60.22[industry.std60.22 < 0.25] 
avg.log.ret60.22.no.outlier  <- avg.log.ret60.22[avg.log.ret60.22 > 0.07]

# Correlation among average annualized returns and standard deviation
cor1 <- cor(avg.ret60.22, industry.std60.22)    
cor2 <- cor(avg.log.ret60.22, industry.std60.22) 
cor3 <- cor(avg.log.ret60.22.no.outlier , industry.std60.22.no.outlier)
cor4 <- cor(industry.std60.22.no.outlier, avg.ret60.22.no.outlier)

### Plot 1: Average returns and average returns' standard deviation
y <- data.frame(industry.std60.22)
x <- data.frame(avg.ret60.22)
M1 <- cbind(y,x)

p1 <- ggplot(M1, aes(x=industry.std60.22, y=avg.ret60.22)) +
  geom_point(size = 1.5, shape=23) +
  geom_smooth(se = FALSE, color = "red", lwd = 0.55, method = "lm") +
  scale_y_continuous(labels = scales :: percent) +
  scale_x_continuous(labels = scales :: percent) +
  theme_bw() + 
  labs(title = "Average Returns to Volatility (1.1)", x = "Standard Deviations", y ="Average Returns") +
  theme(axis.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5))

p1 + theme(plot.title = element_text(size = 17))

### Plot 2: Average Log returns and average returns' standard deviation
z <- data.frame(avg.log.ret60.22)
M2 <- cbind(z, y)

p2 <- ggplot(M2,aes(x = industry.std60.22, y = avg.log.ret60.22 )) +
  geom_point(size = 2.5, shape=23) +
  geom_smooth(method = "lm", formula = y~x, se = FALSE, color = "red", lwd = 0.55) + 
  labs(title = "Average Log Returns to Volatility (1.2)", x = "Standard Deviations", y ="Average Log Returns") + 
  scale_y_continuous(labels = scales :: percent) +
  scale_x_continuous(labels = scales :: percent) + 
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5))

p2  + theme(plot.title = element_text(size = 17))

### Plot 3: Average returns and average returns' standard deviation without 
# the outlying observation 
y7 <- data.frame(industry.std60.22.no.outlier)
x7 <- data.frame(avg.log.ret60.22.no.outlier)
M9 <- cbind(y7,x7)

p9 <- ggplot(M9, aes(x= industry.std60.22.no.outlier, y=avg.ret60.22.no.outlier)) +
  geom_point(size = 1.5, shape=23) +
  geom_smooth(se = FALSE, color = "red", lwd = 0.55, method = "lm") +
  scale_y_continuous(labels = scales :: percent) +
  scale_x_continuous(labels = scales :: percent) +
  theme_bw() + 
  labs(title = "Average Returns to Volatility (Outlier Removed)", x = "Standard Deviations", y ="Average Returns") +
  theme(axis.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5))

p9 + theme(plot.title = element_text(size = 17))

################################################################################

# Question 2
# For the sample 1960-2022:

# 2.1- Plot the CUMULATIVE LOG RETURNS for an equal-weighted portfolio (each industry
# with weight 1/17). Compute mean, std, and Sharpe ratio for returns.

# Computing the portfolio average returns over the 1960-2022 period
bal.port.weights <- rep(1/17,17)                                
bal.port.ret     <- rowSums(bal.port.weights * data60.22[, -1]) 
bal.port.avg.ret <- mean(bal.port.ret) * 252                    

# Portfolio standard deviation
bal.port.std <- sd(bal.port.ret)*sqrt(252)

# Sharpe Ratio
bal.port.Sharpe <- bal.port.avg.ret / bal.port.std

# Computing the cumulative log returns and then plotting 
bal.port.log.ret     <- log(1 + bal.port.ret)    
bal.port.cum.log.ret <- cumsum(bal.port.log.ret) 

### Plot 4: Cumulative Log returns of the Equally weighted portfolio 
x1 <- data.frame(data60.22$date)
y1 <- data.frame(bal.port.cum.log.ret)
M3 <- cbind(x1, y1)

p3 <- ggplot(M3, aes(x = data60.22$date, y = bal.port.cum.log.ret)) +
  geom_line(size = 0.25) +
  labs(title = "Cumulative Log Returns (2.1)", x = "Year", y ="Log Returns") + 
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales :: percent)

p3 + theme(plot.title = element_text(size = 17))

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# 2.2- Repeat (1), but now weight each industry by the inverse of its full-sample
# standard deviation, with weights normalized so they sum up to one.

# Computing the full sample industry annualized standard deviation
industry.std.full.sample <- apply(data[, -1], 2, sd) 

# Normalizing the weights to sum up to 1
normalizer        <- (1 / sum(industry.std.full.sample))   # normalizing coefficient
norm.port.weights <- industry.std.full.sample * normalizer # normalized weights
sum(norm.port.weights)                                     # they sum up to 1 
mean(norm.port.weights) == 1/17                            # equal to 1/17

# Annualized average normalized Portfolio returns, 1960-2022 sample data
norm.port.ret     <- rowSums(norm.port.weights * data60.22[, -1])
norm.port.avg.ret <- mean(norm.port.ret) * 252 # norm port returns, annualized

# Normalized portfolio, annualized standard deviation
norm.port.std <- sd(norm.port.ret)*sqrt(252)

# Sharpe Ratio
norm.port.Sharpe <- norm.port.avg.ret / norm.port.std

# Plotting cumulative portfolio log returns 
norm.port.log.ret     <- log(1 + norm.port.ret)   
norm.port.cum.log.ret <- cumsum(norm.port.log.ret) 

### Plot 5: Cumulative Log returns of the normalized portfolio 
x1 <- data.frame(data60.22$date)
y2 <- data.frame(norm.port.cum.log.ret)
M4 <- cbind(x1, y2)

p4 <- ggplot(M4, aes(x = data60.22$date, y = norm.port.cum.log.ret)) +
  geom_line(size = 0.25) +
  labs(title = "Cumulative Log Returns (2.2)", x = "Year", y ="Log Returns") + 
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales :: percent) +
  theme(axis.text.x = element_text(angle = 0))

p4 + theme(plot.title = element_text(size = 17)) 

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# 2.3- Repeat (1), but now each month select the 6 industries (each allocated
# 1/6 of capital) with the highest return in the past 12 months (computed
# as[ P(i;t) / P(i;t-250) ] - 1).

# Generating year.month variable for the 1960-2022 dataset, so that then we can
# select directly both the year and month at the same time
year.mont  <- substring(data60.22$date, 1, 7) 

# New dataframe without the 'day' index in the date variable
data.year.mont60.22 <- data.frame(year.mont = year.mont, 
                                  data60.22[ , -1])

# Aggregating the daily returns in the new dataframe by month to have cumulative 
# log returns for every month
log.data.year.mont60.22 <- data.frame(year.mont = year.mont, 
                                      log(data.year.mont60.22[,-1] +1) )

month.rets <- aggregate(log.data.year.mont60.22[, -1],  
                        by  = list(log.data.year.mont60.22$year.mont), 
                        FUN = sum)
colnames(month.rets)[1] <- "year.mont"

log.month.rets.no.date  <- month.rets[, -1] 

# For loop to get 12 months rolling window for the best 6 asset classes
# Initializing all the variables we need
rm(list)
industry <- colnames(log.month.rets.no.date) # list of names of every industry
n        <- nrow(month.rets)                 # dimension for the loop, 745 iterations
list     <- NULL                             # initializing the variable that will contain all the 
                                                # names of the best 6 industries

for (i in (13 : n) ) { # starting from 13 since we start building portfolios from 1961/01
  yearly.Rets <- NULL  # initializing the yearly returns
  best.Six    <- NULL  # initialize the best.six vector that will be erased at every iteration
  for (j in industry) {
    yearly.Rets[j] <- colSums( as.matrix(log.month.rets.no.date[ ( (i-12):(i-1) ) , j] )) 
  } 
  best.Six <- names(sort(yearly.Rets, decreasing = TRUE)[1:6]) # best six industries for the previous 12 months
  list     <- rbind(list, best.Six)                            # creating the list with the names of the best industries
}

# Reducing the monthly returns dataset, excluding the first year of data (which
# is lost in the for loop to find the first best six industries)

# Aggregating simple returns to compute avg port rets
month.rets2 <- aggregate(data.year.mont60.22[, -1],  
                        by  = list(data.year.mont60.22$year.mont), 
                        FUN = sum)

new.month.rets <- month.rets2[ (13:n), -1 ] 

# For loop that assigns 1/6 only when the 'industry' is in the 'list', meaning 
# that such industry was one of the best 6 performing in the previous 12 months
# Initializing the weight matrix for the portfolio
w <- data.frame() 

for (i in (1 : nrow(list) ) ) {
  for (j in industry) {
    w[i,j] <- ( (1/6) * (j %in% list[i, ]) )
  }
}

# Computing portfolio returns with the previously defined weights
best.six.port.ret      <- rowSums( w * new.month.rets )
best.six.avg.port.ret  <- mean(best.six.port.ret)*12  # time 12 since those are monthly
                                                      # returns that we want to annualize
# Portfolio standard deviation
best.six.std <- sd(best.six.port.ret)*sqrt(12)

# Portfolio Sharpe Ratio
best.six.sharpe <- best.six.avg.port.ret  / best.six.std 

# Plotting the cumulative log portfolio returns
# Converting months to date variables to plot them on the X axis
month.rets$year.mont   <- as.Date(paste(month.rets$year.mont, '01', sep = '-') )
 
best6.port.log.ret     <- log(1 + best.six.port.ret)  
best6.port.cum.log.ret <- cumsum(best6.port.log.ret ) 

### Plot 6: Cumulative Log returns for the 12 months momentum strategy portfolio 
y3 <- data.frame(month.rets$year.mont[-(1:12)])
x3 <- data.frame(best6.port.cum.log.ret)
M5 <- cbind(x3, y3)

p5 <- ggplot(M5, aes(x = month.rets$year.mont[-(1:12)], y = best6.port.cum.log.ret)) +
  geom_line(size = 0.25) + 
  labs(title = "Cumulative Log Returns (2.3)", x = "Year", y ="Log Returns") + 
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales :: percent) 

p5 + theme(plot.title = element_text(size = 17)) 

################################################################################

# Question 3

# Repeat the momentum strategy of Question 2.3, but instead of a constant, full
# allocation of capital, in each period (day), compute the covariance matrix
# between the 6 chosen industry in the previous 3 months (63 trading days), and
# use it to target a constant portfolio standard deviation of 20% annually.

# First, we need to find the best 6 asset classes in the previous 3 months 
# (63 trading days). We proceed as above, but using daily returns dataset 

rm(list2)
industry2 <- colnames(data60.22[, -1]) # industry variable as before, with no date
n2        <- nrow(data60.22)           # dimension of the for loop
list2     <- NULL

# For loop that computes the daily list of the 6 best performing portfolios in 
# the 3 months before
for (i in (64 : n2) ) { # from 64 since we start from the first day of the 4th month
  daily.Rets <- NULL    # initialize the daily returns
  best.Six   <- NULL
  for (j in industry2) {
    daily.Rets[j] <- colSums(as.matrix(log.data.60.22[ (i-63):(i-1)  , j] )) # USE LOG DATA
  } 
  best.Six <- names(sort(daily.Rets, decreasing = TRUE)[1:6])
  list2    <- rbind(list2, best.Six)
}

# Covariances computed daily among the 6 best asset classes, on the 3 months 
# rolling window of data before
N            <- nrow(list2) 
cov.best.six <- array(rep(NA, 6*6*N), dim = c(6,6,N)) # initialize the 3-dimensional array

for (i in 1:N ) {
  best.six.3.mont <- data.year.mont60.22[ (i:(i+62)), c(list2[i, ])]
  cov.best.six[ , , i] <- cov(best.six.3.mont)
}

# For loop that computes daily portfolio weights starting from an equally
# weighted portfolio, but with weights that are updated every time as defined by
# the formula [ 0.2/sigma_p_annualized ]
weights.star <- NULL 

for (i in (1:N) ) {      
  initial.w <- rep(1/6, 6) # initial weights, equally weighted portfolio of 6 assets      
  port.std.rebal <- sqrt( t(initial.w ) %*% cov.best.six[ , , (i)] %*% initial.w  )
  weights.star[i]  <- (1/6)*( 0.2 / (port.std.rebal*sqrt(252) )) 
}

# Last for loop that computes (as before) the weight matrix for every period. This
# time weights are not constant and are extracted from the previous loop. A weight
# different from zero is assigned only for those days and those industries that
# were in the best 6 performing ones
weights.star.matrix <- data.frame()

for (i in (1 : N ) ) {
  for (j in industry) {
    weights.star.matrix[i,j] <- ( weights.star[i] * (j %in% list2[i, ]) )
  }
}

# Portfolio returns
data60.22.cut     <- data60.22[-(1:63), -1 ] # removing the first 3 months

port.ret.star     <- rowSums(weights.star.matrix * data60.22.cut)
avg.port.ret.star <- mean(port.ret.star) * 252

# Portfolio standard deviation
port.ret.star.std <- sd(port.ret.star)*sqrt(252) 

# Portfolio Sharpe Ratio
Sharpe.star <- avg.port.ret.star / port.ret.star.std

# Plotting portfolio cumulative log returns
port.log.ret.star     <- log(port.ret.star + 1)
port.cum.log.ret.star <- cumsum(port.log.ret.star)

### Plot 7: Cumulative Log returns of the 3 months momentum strategy portfolio, 
# with a target volatility of 20% per annum
x4 <- data.frame(data60.22$date[-(1:63)])
y4 <- data.frame(port.cum.log.ret.star)
M6 <- cbind(x4, y4)

p6 <- ggplot(M6, aes(x = data60.22$date[-(1:63)], y = port.cum.log.ret.star)) +
  geom_line(size = 0.25) +
  labs(title = "Cumulative Log Returns (3)", x = "Year", y ="Log Returns") + 
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales :: percent)

p6 + theme(plot.title = element_text(size = 17))

################################################################################

# Question 4

# Compute the equal-weighted portfolio as in Question 2.1 (on the 1960-2022
# sample) and the yearly return in each industry as in Question 2.3. In each
# period (each day), compute the share alpha_t of industries with positive
# momentum (positive yearly return), and invest in the equal-weighted portfolio
# each month a share alpha_t, with the rest going to cash, which you should
# assume yields 3% annually

n.60.22 <- nrow(data60.22) 

# Defining a new empty matrix to be filled in the loop, that will contain all the 
# yearly returns computed on a daily basis
new.yearly.Rets <- matrix(NA, ncol = 17, nrow = (n.60.22- 252 ) )

for (i in (253:n.60.22) ) {
   new.yearly.Rets[(i-252), ] <- colSums( data60.22[  (i-252):(i-1) , -1 ] )
}

# Changing the column names of the matrix with yearly (daily) returns
colnames(new.yearly.Rets) <- industry

# Computing number of industries that have a positive yearly return in every period (day)
positive.yearly.rets  <- (new.yearly.Rets > 0) * 1  # 1s and 0s if the condition is met in every cell

# Column vector containing the number of industries for every period that had a
# previous yearly return > 0
number.pos.industries <- as.matrix(rowSums(positive.yearly.rets))

# Computing the share we invest in the portfolio and the share we invest in the 
# risk free rate for every period 
alpha.portfolio <- number.pos.industries / 17
alpha.rf        <- 1 - alpha.portfolio

# Creating a vector of risk free rate daily returns
rf_rets <- rep( (0.03/252), nrow(alpha.rf) ) 

# Creating an equally weighted portfolio that invests in all assets from 1960 to
# 2022
bal.port.weights     <- rep(1/17,17)
bal.portfolio.rets.4 <- rowSums(bal.port.weights * data60.22[ (253:n.60.22), -1]) 

# Final vector of portfolio returns, with alpha in the assets and (1-alpha) in rf
portfolio_rets_final <- (alpha.portfolio * bal.portfolio.rets.4  + alpha.rf * rf_rets)
excess_rets          <- portfolio_rets_final - (0.03/252) 
mean_excess_rets     <- mean(excess_rets)*252

# Portfolio Standard deviation 
excess_rets_std  <- sd(excess_rets)*sqrt(252)

Sharpe_ratio_final <- mean_excess_rets / excess_rets_std 

# Cheching for correlation among industries
cor(data60.22[, -1]) # They are all positively correlated

### Plot 8: Cumulative Log returns for the portfolio with alpha wealth invested
# in an EW portfolio in the industries and (1-alpha) weight in a Risk Free rate
# asset that yields 3% per annum
port.cum.log.exc.ret <- cumsum(log(excess_rets + 1))

x5 <- data.frame(data60.22$date[-(1:252)])
y5 <- data.frame(port.cum.log.exc.ret)
M7 <- cbind(x5, y5)

p7 <- ggplot(M7, aes(x = data60.22$date[-(1:252)], y = port.cum.log.exc.ret)) +
  geom_line(size = 0.25) +
  labs(title = "Cumulative Log Returns (4)", x = "Year", y ="Log Returns") + 
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales :: percent)

p7 + theme(plot.title = element_text(size = 17)) 

### plot 9: Daily allocation in the EW portfolio of industries

x8 <- data.frame(data60.22$date[-(1:252)])
y8 <- data.frame(alpha.portfolio)
M8 <- cbind(x8, y8)

p10 <- ggplot(M8, aes(x = data60.22$date[-(1:252)], y = alpha.portfolio)) +
  geom_line(size = 0.25) +
  geom_smooth(se = FALSE, color = "red", lwd = 1) +
  geom_smooth(se = FALSE, color = "blue", lwd = 1, method = "lm") +
  labs(title = "Asset Portfolio Allocation (4)", x = "Year", y ="Allocation") + 
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales :: percent)

p10 + theme(plot.title = element_text(size = 17)) 

#################################################################################

# Question 5

# For the full sample, repeat the momentum strategy in Question 2.3, but use the
# returns of the previous five years (252 * 5 days) as a measure of momentum.
# Briefly comment on the results

# Full log dataset to aggregate
log.data.no.date <- log(data[, -1] + 1) 

year.mont.full   <- substring(data$date, 1, 7) 

# New dataframe without the 'day' index in the date variable
log.data.year.mont.full <- data.frame(year.mont.full = year.mont.full, 
                                      log.data.no.date) 

# Aggregating the daily returns by month 
log.month.rets.full <- aggregate(log.data.year.mont.full[, -1], 
                                 by = list(log.data.year.mont.full$year.mont.full), 
                                 FUN = sum)


# Creating the dataset with monthly simple returns
data.year.mont.full <- data.frame(year.mont.full = year.mont.full, data[, -1] )
month.rets.full     <- aggregate(data.year.mont.full[, -1], 
                                 by = list(data.year.mont.full$year.mont.full), 
                             FUN = sum)



# For loop to get 5 years rolling window for the best 6 asset classes
# Initializing all the variables we need
rm(list3)
industry3        <- colnames(log.month.rets.full[-1]) 
yearly.Rets.full <- NULL                           
n.full           <- nrow(log.month.rets.full)           
list3            <- NULL                            

# 60 MONTHS = 5 YEARS
for (i in (61 : n.full) ) { # starting from 61 since we start building portfolios from 5 years after 1926
  best.Six = NULL           # initialize the best.six vector that will be erased at every iteration
  for (j in industry) {
    yearly.Rets.full[j] <- sum( log.month.rets.full[ ( (i-60):(i-1) ) , j] ) # cumulating monthly rets into 5-years rets
  }
  best.Six <- names( sort(yearly.Rets.full, decreasing = TRUE)[1:6]) # best six industries for the previous 12 months
  list3    <- rbind(list3, best.Six)                            # creating the list with the names of the best industries
}

# Initializing the weight matrix for the portfolio
w.full <- data.frame()

# For loop that assigns 1/6 only when the 'industry' is inside 'list3', meaning 
# that such industry was one of the best 6 performing in the previous 12*5 = 60 months
for (i in (1 : nrow(list3) ) ) {
  for (j in industry) {
    w.full[i,j] <- ( (1/6) * (j %in% list3[i, ]) )
  }
}

# Portfolio returns with the previously defined weights
best.six.port.ret.full      <- rowSums(w.full * month.rets.full[-(1:60), -1])
best.six.avg.port.ret.full  <- mean(best.six.port.ret.full) * 12 

# Portfolio standard deviation
best.six.std.full <- sd(best.six.port.ret.full)*sqrt(12)

# Portfolio Sharpe Ratio
best.six.sharpe.full <- best.six.avg.port.ret.full  / best.six.std.full

# Plotting cumulative portfolio returns
colnames(month.rets.full)[1] <- "year.mont"
month.rets.full$year.mont    <- as.Date(paste(month.rets.full$year.mont, '01', sep = '-') )

port.log.ret.full     <- log(best.six.port.ret.full  + 1)
port.cum.log.ret.full <- cumsum(port.log.ret.full)

### Plot 8: Cumulative Log Returns for the 5-years momentum strategy portfolio 
x6 <- data.frame(month.rets.full$year.mont[-(1:60)])
y6 <- data.frame(port.cum.log.ret.full)
M8 <- cbind(x6, y6)

p8 <- ggplot(M8, aes(x = month.rets.full$year.mont[-(1:60)], y = port.cum.log.ret.full)) +
  geom_line(size = 0.25) +
  labs(title = "Cumulative Log Returns (5)", x = "Year", y ="Log Returns") + 
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales :: percent)

p8 + theme(plot.title = element_text(size = 17)) 




