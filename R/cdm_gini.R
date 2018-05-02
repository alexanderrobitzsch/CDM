## File Name: cdm_gini.R
## File Version: 0.03


# Gini coefficient, function simply copied from the R ineq package
cdm_gini <- function(x)
{
    n <- length(x)
    x <- sort(x)
    G <- sum(x * 1:n)
    G <- 2 * G/(n * sum(x))
    G - 1 - (1/n)
}
