## ASP_Group_Project 
## Development File 

## Load libraries
library(devtools)
library(roxygen2)
library(testthat)
library(MASS)
library(ANTsR)

## Set working directory
setwd("~/Documents/GitHub/ASP_group_project") # Gangyi
setwd("~/Documents/Applied_Statistical_Programming/ASP_group_project") # Tyler
setwd("/Users/rohangupta/Documents/WUSTL/SP2018/Pol Sci/ASP_group_project") # Rohan
setwd("/Users/Marcus/Documents/ASP_group_project") # Marcus


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
data_montgomery <- read.csv("gaussianComparisons/Data/exampleHITs.csv")
data_clean <- cleaner(data_montgomery)
omega_data <- omega(data_montgomery,2)

# muMaker Function
# (5059,5091) and (5058,5086) are comparisons that have not been made in the sample data
muMaker(5059, 5091, omega_data, data_clean, 2, 0.1)
muMaker(5058, 5086, omega_data, data_clean, 2, 0.1)

# rhoSquaredMaker Function [CURRENTLY RETURNING NEGATIVE RESULTS]
# (5059,5091) and (5058,5086) are comparisons that have not been made in the sample data
rhoSquaredMaker(5059, 5091, omega_data, data_clean, 2, 0.01)
rhoSquaredMaker(5058, 5086, omega_data, data_clean, 2, 0.01)

# bigEGStar Function [CURRENTLY DOESNT WORK BECAUSE OF rhoSquaredMaker]
bigEGStar(5059, 5091, omega_data, data_clean, 2, 0.01)

# lowercaseH Function
lowercaseH(0.1) 

# finalS Function [CURRENTLY DOESNT WORK BECAUSE OF rhoSquaredMaker]
finalS(5059, 5091, omega_data, data_clean, 2, 0.01)

# maxInfoComp Function [CURRENTLY DOESNT WORK BECAUSE OF rhoSquaredMaker]
# [CURRENTLY TAKES ABOUT 4 HOURS TO RUN]
a<-maxInfoComp(data_montgomery, 2, 0.01, 10)





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

library(microbenchmark)
microbenchmark(finalS(5059, 5091, omega1, cleaned_data, 2, 0.01), times = 50)

omega1 <- omega(data_montgomery, 2)
cleaned_data <- cleaner(data_montgomery)

finalS(5059, 5091, omega1, cleaned_data, 2, 0.01)
