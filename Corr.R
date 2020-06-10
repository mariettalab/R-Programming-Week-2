corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
  
        id = 1:332
        filename <- list.files(directory, full.names = TRUE)
        
        result <-vector(mode="numeric", length=0)
        
        for(i in seq(filename)) {
            specdata <- read.csv(filename[i])
            good <- complete.cases(specdata)
            best_airquality <- specdata[good, ]
            if (nrow(best_airquality) > threshold) {
                # We need [[]] around pollutant instead of [] since airquality["sulfate"]
                # is a data.frame but we need a vector here. Hence, [[]]. Please note that using either
                #[[]] or [] gives the same results as the test cases
                correlation <- cor(specdata[["sulfate"]], specdata[["nitrate"]])
                result <- append(result, correlation)
                #print(correlation)
            }
        }
        result
    }