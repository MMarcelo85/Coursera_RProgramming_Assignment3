
rankall <- function(outcome, num = "best"){
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
            if (!require(x, character.only = TRUE, quietly = TRUE)) {
                install.packages(x, dependencies = TRUE)
                library(x, character.only = TRUE, quietly = TRUE)
            }
        }
    )
    
    ## Check that state and outcome are valid
    if (outcome %in% colnames(datos)[3:5]) {  # If confition is met:
        #d1 <- filter(datos, State==state) # Filter DF by input state
        d1 <- datos[order(datos[, 'State'], datos[, outcome]), ] # order DF by input outcome and Hospital_name as 2nd criteria
        #d1 <- arrange(datos, State, outcome) # aca se rompe por outcome
        d1 <- group_by(d1, State) 
        d1 <- mutate(d1, rank = row_number())
        d2 <- filter(d1, rank == num)
        #d1$rank <- seq_along(d1[, outcome]) # rank the hospitals for the user input variables
        #head(d1)
        x <- d2[, c(1,2)]
        
    }
    else if (outcome %!in% colnames(datos)[3:5]) {
        stop('invalid outcome', call. = FALSE)
        #cat("For 'outcome' you can choose 'heart_attack', 'heart_failure' or 'pneumonia'")
    }
    # Conditional statement for num input
    if (num %in% d2$rank) {
        num = num
    }
    
    else if(num == 'best') {
        d2 <- filter(d1, rank == 1)
        x <- d2[, c(1,2)]
    }
    
    else if(num == 'worst') {
        d2 <- filter(d1, rank == max(rank, na.rm = TRUE))
        x <- d2[, c(1,2)]
    }
    
    else {
        x <- NA
    }
    ## Return a data frame with the hospital names and the (abbreviated) state nam
    return(x)
}  


