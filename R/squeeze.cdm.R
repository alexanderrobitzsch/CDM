## File Name: squeeze.cdm.R
## File Version: 0.03
## File Last Change: 2017-01-31 14:07:30

#*********************************
# copied squeeze function from mice package
squeeze.cdm <- function (x, bounds ){
    x[x < bounds[1] ] <- bounds[1]
    x[x > bounds[2] ] <- bounds[2]
    return(x)
}
