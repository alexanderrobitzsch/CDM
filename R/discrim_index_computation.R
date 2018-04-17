## File Name: discrim_index_computation.R
## File Version: 0.04

discrim_index_computation <- function( attr_patt, probs, dicho = TRUE,
	skill_names = NULL, item_names = NULL )
{

	#-- matrix containing attribute vectors for comparison
	comp_matrix <- cdm_rcpp_discrimination_index_attribute_patterns(attr_patt=attr_patt)
	
	#--- compute discrimination indices
	probs_ <- as.vector(probs)
	dim_probs <- dim(probs)
	ncat <- dim_probs[2]
	if (ncat>2){
		stop("'discrim.index' can only be applied for dichotomous data.")
	}	
	K <- ncol(attr_patt)
	if (dicho){
		discrim_item <- cdm_rcpp_discrimination_index_calc_dich( comp_matrix=comp_matrix,
                probs = probs_, dim_probs = dim_probs, K = K )
	}
	
	#--- discrimination index at test level
	discrim_test <- cdm_rcpp_discrimination_index_test_level(discrim_item=discrim_item)

	#--- labelling
	if ( is.null(skill_names)){
		skill_names <- colnames(attr_patt)
	}
	if ( is.null(item_names) ){
		item_names <- dimnames(probs)[[1]]
	}
	colnames(discrim_item) <- skill_names
	discrim_item <- data.frame( item = item_names , discrim_item )
	
	#--- output
	res <- list( comp_matrix=comp_matrix, discrim_item=discrim_item, discrim_test=discrim_test ) 
	class(res) <- "discrim.index"
	return(res)
}
