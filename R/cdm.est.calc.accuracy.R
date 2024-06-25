## File Name: cdm.est.calc.accuracy.R
## File Version: 2.53


#**** CDM classification accuracy
cdm.est.class.accuracy <- function( cdmobj, n.sims=0, version=2 )
{
    CALL <- match.call()
    #*** version 1
    if (version==1){
        dfr <- cdm_est_calc_accuracy_version1(cdmobj=cdmobj, n.sims=n.sims)
        res <- list( statistics=dfr, version=version)
        cdm_est_calc_accuracy_version1_print(object=res)
    }
    #*** version 2
    if (version==2){
        dfr <- cdm_est_calc_accuracy_version2( cdmobj=cdmobj, n.sims=n.sims )
        res <- list( statistics=dfr, version=version)
        cdm_est_calc_accuracy_version1_print(object=res)
    }
    #--- output
    res$CALL <- CALL
    class(res) <- 'cdm.est.class.accuracy'
    return(res)
}
