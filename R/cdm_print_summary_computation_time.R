## File Name: cdm_print_summary_computation_time.R
## File Version: 0.04


cdm_print_summary_computation_time <- function(object, time_name="time", time_start="s1",
        time_end="s2")
{
    comp_time <- object[[ time_name ]]
    s1 <- comp_time[[ time_start ]]
    s2 <- comp_time[[ time_end ]]
    cat( "Date of Analysis:", paste( s2 ), "\n" )
    cat("Computation Time:", print( s2 - s1), "\n\n")
}
