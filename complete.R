complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases

csvfiles <- sprintf("E:/code/r-in-coursera/%s/%03d.csv", directory, id)
nrows <- sapply(csvfiles, function(f) sum(complete.cases(read.csv(f))))
rowlabels <- nrow(nrows)
data.frame(id = sprintf('%3d', id), 
            nobs = sapply(csvfiles,function(x) length(count.fields(x))),
            row.names = rowlabels)
}