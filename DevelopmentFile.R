## ASP_Group_Project 
## Development File 

## Load libraries
library(devtools)
library(roxygen2)
library(testthat)

## Set working directory
setwd("~/Documents/GitHub/ASP_group_project")

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

# omega Function (Equation 1.5)
gdata <- data.frame(c(1,1,1,2,2,2,3,3,4,4,4), c(2,3,4,1,3,4,1,4,1,2,3),
                    c(1,1,1,-1,-1,-1,-1,1,-1,1,-1))
colnames(gdata) <- c("first", "second", "y")
#View(gdata)
omega(gdata, 2)

# dLogPri Function (Equation 4)
omega_test <- omega(gdata, 2)
g_test <- sample(1:10, 11, replace = T)
dLogPri(omega_test, g_test)
diag(omega_test) <- diag(omega_test) + .01
(-solve(omega_test)) %*% g_test

# d2LogPri Function (Equation 5)
d2LogPri(omega_test, g_test)

# logLike Function (Equation 5.9)
g1<-c(2,3,4,5,6)
y1<-c(1,1,-1,1,-1)
logLike(g1,y1)

# dLogLike Function (Equation 6)
dLogLike(g1,y1)

# d2LogLike Function (Equation 7)
d2LogLike(g1,y1)

# dPsi Function (Equation 8)
y_test <- gdata$y
dLogPri(omega_test, g_test)
dLogLike(g_test, y_test)
dLogPri(omega_test, g_test) + dLogLike(g_test, y_test)
dPsi(omega_test, g_test, y_test) == dLogPri(omega_test, g_test) + dLogLike(g_test, y_test)

# d2Psi Function (Equation 9)
d2Psi(omega_test, g_test, y_test)

# newtonMethodPrequel Function (pre Equation 9.5)
newtonMethodPrequel(omega_test, g_test, y_test)

# newtonMethod Function (Equation 9.5)
newtonMethod(omega_test, g_test, y_test, .01)
