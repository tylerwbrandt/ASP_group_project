## ASP_Group_Project 
## Development File 

## Load libraries
library(devtools)
library(roxygen2)
library(testthat)
library(MASS)

## Set working directory
setwd("~/Documents/GitHub/ASP_group_project") # Gangyi
setwd("~/Documents/Applied_Statistical_Programming/ASP_group_project") # Tyler
setwd("/Users/rohangupta/Documents/WUSTL/SP2018/Pol Sci/ASP_group_project") # Rohan
setwd("/Users/Marcus/Documents/ASP_group_project") # Marcus
getwd()

## Creates package skeleton
# Only run this the first time to create the skeleton 
# Testdevtools::create("gaussianComparisons")


## Updates the package based on R scripts written, man and NAMESPACE automatically updates. 
# Manually update DESCRIPTION if need be.
current.code <- as.package("gaussianComparisons")
load_all(current.code)
document(current.code)
check(current.code)


## Sample code to test that functions work 
# cleaner Function
data_montgomery <- read.csv("gaussianComparisons/data/exampleHITs.csv")
data_small <- read.csv("gaussianComparisons/data/exampleHITsSmall.csv")
data_clean <- cleaner(data_montgomery)
omega_data <- omega(data_montgomery,2)
table(unique(omega_data))

draw<-gPrior(omega_data)
draw[data_clean$first==5053]
draw[data_clean$second==5053]
draw[data_clean$second==5069]
draw[data_clean$first==5069]


my.g.hat<-newtonMethod(omega_data, data_clean$y, tol=.00000001)
plot(my.g.hat, data_clean$y)
head(data_clean)

# muMaker Function 
# (5059,5091) and (5058,5086) are comparisons that have not been made in the sample data
little_omega <- rep(0, nrow(data_clean))
for (j in 1:nrow(data_clean)){
  if (5059 == data_clean$first[j]){
    little_omega[j] <- little_omega[j] + sigma^2
  }
  if (5059 == data_clean$second[j]){
    little_omega[j] <- little_omega[j] - sigma^2
  }
  if (5091 == data_clean$first[j]){
    little_omega[j] <- little_omega[j] - sigma^2
  }
  if (5091 == data_clean$second[j]){
    little_omega[j] <- little_omega[j] + sigma^2
  }
}
muMaker(5059, 5091, omega_data, data_clean, 2, 0.1, little_omega)

# rhoSquaredMaker Function
# (5059,5091) and (5058,5086) are comparisons that have not been made in the sample data
little_omega <- rep(0, nrow(data_clean))
for (j in 1:nrow(data_clean)){
  if (5059 == data_clean$first[j]){
    little_omega[j] <- little_omega[j] + sigma^2
  }
  if (5059 == data_clean$second[j]){
    little_omega[j] <- little_omega[j] - sigma^2
  }
  if (5086 == data_clean$first[j]){
    little_omega[j] <- little_omega[j] - sigma^2
  }
  if (5086 == data_clean$second[j]){
    little_omega[j] <- little_omega[j] + sigma^2
  }
}
rhoSquaredMaker(5058, 5086, omega_data, data_clean, 2, 0.01, little_omega)

# bigEGStar Function [Doesn't work independnetly anymore because little_omega only created in finalS]
little_omega <- rep(0, nrow(data_clean))
for (j in 1:nrow(data_clean)){
  if (5059 == data_clean$first[j]){
    little_omega[j] <- little_omega[j] + sigma^2
  }
  if (5059 == data_clean$second[j]){
    little_omega[j] <- little_omega[j] - sigma^2
  }
  if (5091 == data_clean$first[j]){
    little_omega[j] <- little_omega[j] - sigma^2
  }
  if (5091 == data_clean$second[j]){
    little_omega[j] <- little_omega[j] + sigma^2
  }
}
bigEGStar(5059, 5091, omega_data, data_clean, 2, 0.01, little_omega)

# lowercaseH Function
lowercaseH(0.1) 

# finalS Function
finalS(5059, 5091, omega_data, data_clean, 2, 0.01)

# maxInfoComp Function 
# [CURRENTLY TAKES ABOUT 45mins TO RUN on exampleHITs dataset]
a<-maxInfoComp(data_montgomery, 2, 0.01, 1000)


## Microbenchmarking 
library(microbenchmark)
microbenchmark(finalS(5059, 5091, omega_data, data_clean, 2, 0.01), times = 50)
finalS(5059, 5091, omega_data, data_clean, 2, 0.01)


### Create dataframe for final report

# Use docid 5059 to compare all
docids <- unique(data_montgomery$document_id)
# Create s values
finalS_values <- NULL
for (i in docids){
  new_Svalue <- finalS(doc1 = 5059, doc2 = i, omega1 = omega_data, cleaned_data = data_clean,
                       sigma = 2, tolerance = 0.01)
  finalS_values <- c(finalS_values, new_Svalue)
}

# Create mu hat values
mu_values <- NULL
sigma <- 2
for (i in docids){
  little_omega <- rep(0, nrow(data_clean))
  for (j in 1:nrow(data_clean)){
    if (5059 == data_clean$first[j]){
      little_omega[j] <- little_omega[j] + sigma^2
    }
    if (5059 == data_clean$second[j]){
      little_omega[j] <- little_omega[j] - sigma^2
    }
    if (i == data_clean$first[j]){
      little_omega[j] <- little_omega[j] - sigma^2
    }
    if (i == data_clean$second[j]){
      little_omega[j] <- little_omega[j] + sigma^2
    }
  }
  new_muvalue <- muMaker(doc1 = 5059, doc2 = i, omega1 = omega_data, cleaned_data = data_clean,
                         sigma = 2, tolerance = 0.01, little_omega = little_omega)
  mu_values <- c(mu_values, new_muvalue)
}

# Create rho squared values
rhoSquared_values <- NULL
sigma <- 2
for (i in docids){
  little_omega <- rep(0, nrow(data_clean))
  for (j in 1:nrow(data_clean)){
    if (5059 == data_clean$first[j]){
      little_omega[j] <- little_omega[j] + sigma^2
    }
    if (5059 == data_clean$second[j]){
      little_omega[j] <- little_omega[j] - sigma^2
    }
    if (i == data_clean$first[j]){
      little_omega[j] <- little_omega[j] - sigma^2
    }
    if (i == data_clean$second[j]){
      little_omega[j] <- little_omega[j] + sigma^2
    }
  }
  new_rhoSquaredValue <- rhoSquaredMaker(doc1 = 5059, doc2 = i, omega1 = omega_data,
                                         cleaned_data = data_clean, sigma = 2, tolerance = 0.01,
                                         little_omega = little_omega)
  rhoSquared_values <- c(rhoSquared_values, new_rhoSquaredValue)
}

# Create dataframe
summary_df <- data.frame(rep(5059, 50), docids, finalS_values, mu_values, rhoSquared_values)
colnames(summary_df) <- c("doc1", "doc2", "s_values", "mu_hat", "rho_squared")
summary_df <- summary_df[order(summary_df$s_values), ]
View(summary_df)

## Plot s values versus mu hat
plot(summary_df$mu_hat, summary_df$s_values, xlab = "mu hat values", ylab = "s values",
     main = "Information Gain By Expected g Values")

## Plot s values versus rho squared hat
plot(summary_df$rho_squared, summary_df$s_values, xlab = "rho^2 values", ylab = "s values",
     main = "Information Gain By Expected Variance Values")




# ## Old Sample Code, before cleaner was implemented in other functions. Won't work properly anymore. 
# # omega Function (Equation 1.5)
# gdata <- data.frame(c(1,1,1,2,2,2,3,3,4,4,4), c(2,3,4,1,3,4,1,4,1,2,3),
#                     c(1,1,1,-1,-1,-1,-1,1,-1,1,-1))
# colnames(gdata) <- c("first", "second", "y")
# omega_test <- omega(gdata, 2)
# 
# # dLogPri Function (Equation 4)
# g_test <- sample(1:10, 11, replace = T)
# dLogPri(omega_test, g_test)
# diag(omega_test) <- diag(omega_test) + .01
# (-solve(omega_test)) %*% g_test
# 
# # d2LogPri Function (Equation 5)
# d2LogPri(omega_test, g_test)
# 
# # logLike Function (Equation 5.9)
# g1<-c(2,3,4,5,6)
# y1<-c(1,1,-1,1,-1)
# logLike(g1,y1)
# 
# # dLogLike Function (Equation 6)
# dLogLike(g1,y1)
# 
# # d2LogLike Function (Equation 7)
# d2LogLike(g1,y1)
# 
# # dPsi Function (Equation 8)
# y_test <- gdata$y
# dLogPri(omega_test, g_test)
# dLogLike(g_test, y_test)
# dLogPri(omega_test, g_test) + dLogLike(g_test, y_test)
# dPsi(omega_test, g_test, y_test) == dLogPri(omega_test, g_test) + dLogLike(g_test, y_test)
# 
# # d2Psi Function (Equation 9)
# d2Psi(omega_test, g_test, y_test)
# 
# # newtonMethodPrequel Function (pre Equation 9.5)
# newtonMethodPrequel(omega_test, g_test, y_test)
# 
# # newtonMethod Function (Equation 9.5)
# newtonMethod(omega_test, y_test, .01)
