## File Name: summary.din_identifiability.R
## File Version: 0.04

summary.din_identifiability <- function(object, ...)
{
    cat("Necessary and sufficient conditions for identifiability (Gu & Xu, 2019)\n\n")
    cat("Q-Matrix with\n")
    cat("Number of items", "=", object$I,"\n")
    cat("Number of skills", "=", object$K,"\n")
    cat("Average number of skills per item", "=", round(object$qmat_stat$item_M,2),"\n")

    cat("Number of items per skill: \n  ")
    print(object$qmat_stat$skills_items)

    cat("\n** Check identifiability conditions\n")

    cat("\nC1: Every skill has at least an item with single loading\n  ")
    print(object$is_single)

    cat("\nC2: Every skill has been measured at least three items\n  ")
    print(object$is_three_items)

    cat("\nC3: Q^\\ast submatrix has distinct columns\n  ")
    print(object$submat_distinct)

    if (object$dina_identified){
        sent <- "DINA model is identified."
    } else {
        sent <- "DINA model is not identified."
    }
    cat("\n==> ", sent, "\n")
}
