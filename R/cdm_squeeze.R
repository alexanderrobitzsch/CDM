## File Name: cdm_squeeze.R
## File Version: 0.04
## File Last Change: 2017-10-08 19:23:09

#*********************************
# copied squeeze function from mice package
cdm_squeeze <- function (x, bounds ){
    x[x < bounds[1] ] <- bounds[1]
    x[x > bounds[2] ] <- bounds[2]
    return(x)
}

squeeze.cdm <- cdm_squeeze
