corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
threscorr <- function(fname) {
	data <- read.csv(file.path(directory,fname))
	nobs <- sum(complete.cases(data))
	if (nobs > threshold) {
		return(cor(data$nitrate, data$sulfate, use="complete.obs"))
	}
	}
correlated <- sapply(list.files(directory), threscorr)
correlated <- unlist(correlated[!sapply(correlated, is.null)])
return(correlated)

}