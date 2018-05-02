## File Name: cdm_scadL2_threshold.R
## File Version: 0.03

cdm_scadL2_threshold <- function(beta, lambda, alpha, a=3.7)
{
    lam1 <- lambda*alpha
    lam2 <- lambda*(1-alpha)
    sign_beta <- sign(beta)
    #-- 2*lambda < abs(beta) < a*lambda
    y <- ( ( a - 1 ) * beta - sign_beta * a * lam1 ) / ( -1 + ( a - 1 )*(1+2*lam2) )
    #-- abs(beta) > a*lambda
    y <- ifelse( abs(beta) > a*lambda , beta / ( 1 + 2*lam2) , y )
    #-- abs(beta) < 2*lambda
    z <- ( abs(beta) - lambda )
    z <- ifelse( z < 0 , 0 , z )
    y <- ifelse( abs(beta) < 2*lambda , z*sign_beta / ( 1 + 2*lam2), y )
    return(y)
}
