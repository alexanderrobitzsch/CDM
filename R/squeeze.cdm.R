
#*********************************
# copied squeeze function from mice package
squeeze.cdm <- function (x, bounds ){
    x[x < bounds[1] ] <- bounds[1]
    x[x > bounds[2] ] <- bounds[2]
    return(x)
}
