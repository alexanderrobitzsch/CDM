## File Name: cdm_print_summary_package.R
## File Version: 0.02


cdm_print_summary_package <- function(pack="CDM")
{
    d1 <- utils::packageDescription(pack)
    cat( paste( d1$Package , " " , d1$Version , " (" , d1$Date , ")" , sep="") , "\n" )
}
