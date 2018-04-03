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

