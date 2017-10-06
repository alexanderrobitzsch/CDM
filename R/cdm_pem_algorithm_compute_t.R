## File Name: cdm_pem_algorithm_compute_t.R
## File Version: 0.01
## File Last Change: 2017-10-04 17:18:36

cdm_pem_algorithm_compute_t <- function( i, a=1.5, h=0.1)
{
	return( 1 + a^i * h )
}
