## File Name: discrim_index_computation.R
## File Version: 0.17

discrim_index_computation <- function( attr_patt, probs, 
    skill_names = NULL, item_names = NULL)
{
    #-- matrix containing attribute vectors for comparison
    comp_matrix <- cdm_rcpp_discrimination_index_attribute_patterns(attr_patt=attr_patt)
    
    #--- compute discrimination indices
    probs_ <- as.vector(probs)
    dim_probs <- dim(probs)
    ncat <- dim_probs[2]
    K <- ncol(attr_patt)        
    discrim_item_attribute <- cdm_rcpp_discrimination_index_calc( comp_matrix=comp_matrix,
                                probs = probs_, dim_probs = dim_probs, K = K )
    colnames(discrim_item_attribute) <- skill_names
    
    #--- item discrimination index IDI
    idi <- cdm_rcpp_discrimination_index_idi( probs=probs_, dim_probs=dim_probs, K=K )
    names(idi) <- item_names
    
    #--- discrimination index at test level
    discrim_test <- cdm_rcpp_discrimination_index_test_level(discrim_item_attribute=discrim_item_attribute)

    #--- labelling
    if ( is.null(skill_names)){
        skill_names <- colnames(attr_patt)
    }
    if ( is.null(item_names) ){
        item_names <- dimnames(probs)[[1]]
    }
    colnames(discrim_item_attribute) <- skill_names
    discrim_item_attribute <- data.frame( item = item_names , discrim_item_attribute )

    #--- output
    res <- list( comp_matrix=comp_matrix, discrim_item_attribute=discrim_item_attribute, discrim_test=discrim_test,
                    idi=idi)
    class(res) <- "discrim.index"
    return(res)
}
