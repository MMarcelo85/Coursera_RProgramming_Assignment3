
# getwd()
# setwd('/home/marcelo/Documentos/Programación/Rstudio/Coursera - Data Science Specialization/02 R Programming/ProgAssignment3-data')
# Finding the best Hospital by lowest 30-day mortality
# Function parameters:
# state: abreviated name of the state [, 7] 
# outcome: "heart attack”[, 11], “heart failure”[, 17], “pneumonia”[, 23], 
# Function Return: (Best Hospital in the state): "Hospital Name"[, 2]
# Ties are resolved by alphabetical order

best <- function(state, outcome){
    # Read data
    path <- '/home/marcelo/Documentos/Programación/Rstudio/Coursera - Data Science Specialization/02 R Programming/ProgAssignment3-data/outcome-of-care-measures.csv'
    datos <- read.csv(path, colClasses = "character")
    packages <- c('dplyr')
    #library(dplyr) # load library
    `%!in%` = Negate(`%in%`) # create the negation for later
    ## Cleaning and tidying
    colnames(datos)[c(2,7,11,17,23)] <- c('Hospital_name', 'State', 'heart_attack', 'heart_failure', 'pneumonia')
    datos <- subset(datos, select = c('Hospital_name', 'State', 'heart_attack', 'heart_failure', 'pneumonia'))
    # Changeing the death rate vars to numeric
    cols = c(3:5)
    datos[,cols] <- suppressWarnings(apply(datos[,cols], 2, function(x) as.numeric(as.character(x))))
    # Removing NAs
    datos <- na.omit(datos)
    
    ## Now load or install and load all packages
    package.check <- lapply(
        packages,
        FUN = function(x) {
            if (!require(x, character.only = TRUE)) {
                install.packages(x, dependencies = TRUE)
                library(x, character.only = TRUE, quietly = TRUE)
            }
        }
    )
    
    ## Check that state and outcome are valid
    if (state %in% levels(as.factor(datos[, 'State'])) & outcome %in% colnames(datos)[3:5]) {  # If confition is met:
        d1 <- filter(datos, State==state) # Filter DF by input state
        d1 <- d1[order(d1[, outcome], d1[, 'Hospital_name']), ] # order DF by input outcome and Hospital_name as 2nd criteria
        best <- d1[1,1] # Save the best Hospital's name
    }
    else if (state %!in% levels(as.factor(datos[, 'State']))) { # if condition is not met, guide the user
        stop('invalid state', call. = FALSE)
        #cat("For 'state' choose a two letter abreviattion like 'TX' or 'AL'.")
    }
    else if (outcome %!in% colnames(datos)[3:5]){
        stop('invalid outcome', call. = FALSE)
        #cat("For 'outcome' you can choose 'heart_attack', 'heart_failure' or 'pneumonia'")
    }
    #Return hospital name in that state with lowest 30 day death rate
    
    paste('The best hospital is: ', best)
    
}  

best("SC", "heart_attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
rankhospital("NC", "heart attack", "worst")