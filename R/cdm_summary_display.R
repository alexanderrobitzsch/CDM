## File Name: cdm_summary_display.R
## File Version: 0.01

cdm_summary_display <- function(symbol="-", len=65)
{
    res <- paste0( paste0( rep(symbol, len), collapse="" ), "\n")
    return(res)
}
