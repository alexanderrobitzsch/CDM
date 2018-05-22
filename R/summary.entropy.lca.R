## File Name: summary.entropy.lca.R
## File Version: 0.04


# summary S3 method
summary.entropy.lca <- function( object, digits=2, ...)
{
    obji <- object$entropy
    cdm_print_summary_data_frame(obji, from=2, digits=digits, rownames_null=TRUE)
}
