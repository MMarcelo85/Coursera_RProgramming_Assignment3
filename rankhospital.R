
rankhospital <- function(state, outcome, num = "best"){
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
    
    ## Now load or install&load all needed packages
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
        d1$rank <- seq_along(d1[, outcome]) # rank the hospitals for the user input variables
        #head(d1)

    }
    else if (state %!in% levels(as.factor(datos[, 'State']))) { # if condition is not met, guide the user
        stop('invalid state', call. = FALSE)
        #cat("For 'state' choose a two letter abreviattion like 'TX' or 'AL'.")
    }
    else if (outcome %!in% colnames(datos)[3:5]) {
        stop('invalid outcome', call. = FALSE)
        #cat("For 'outcome' you can choose 'heart_attack', 'heart_failure' or 'pneumonia'")
    }
    # Conditional statement for num input
    if(num %in% d1$rank) {
            x <- d1[num, 1]
    }
    else if(num == 'best') {
            x <- d1[1,1]
    }
    else if(num == 'worst') {
         x <- d1[nrow(d1), 1]
    }
    else {
         x <- NA
    }
    #Return hospital name in that state with the given rank 30-day death rate
    return(x)
}  


