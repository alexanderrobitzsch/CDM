//// File Name: cdm_c_calcfx.c
//// File Version: 1.05


#include <Rinternals.h>

SEXP _CDM_calcfx(SEXP sFx, SEXP sRprobs, SEXP sRespIndList, SEXP sResp)
{

    /****************************************************/
    /* Get dimension of sRprobs and sResp               */
    SEXP sRprobsDim = getAttrib(sRprobs, R_DimSymbol);
    int nitems = INTEGER(sRprobsDim)[0];
    int ncats = INTEGER(sRprobsDim)[1];
    int nnodes = INTEGER(sRprobsDim)[2];  
    int nresp = INTEGER(getAttrib(sResp, R_DimSymbol))[0];

    SEXP dims = allocVector(INTSXP, 2);
    PROTECT(dims);
    INTEGER(dims)[0] = nresp; 
    INTEGER(dims)[1] = nnodes;

    /****************************************************/
    /* initialization                                   */
    int *resp = INTEGER(sResp), i, *ni, len, k, l;
    SEXP sResult = allocVector(REALSXP, nresp*nnodes);
    double *rii=REAL(sRprobs), *res=REAL(sResult);

    for(i=0; i<nresp; i++){
        for(k=0; k<nnodes; k++){
            res[i+nresp*k] = REAL(sFx)[i+nresp*k];
        }
    }

    for(i=0; i<nitems; i++){
        // extract non-missing value list
        len = LENGTH(VECTOR_ELT(sRespIndList, i));
        ni = INTEGER(VECTOR_ELT(sRespIndList, i)) ; //ni indices in R, therefore '-1'
        //compute fx
        for(k=0; k<len; k++){
            for(l=0; l<nnodes; l++){
                res[ ni[k] + l*nresp - 1 ] = res[ ni[k] + l*nresp - 1] * rii[ i + resp[ni[k]+i*nresp - 1]*nitems + l*nitems*ncats ];
            }
        }
    }

    setAttrib(sResult, R_DimSymbol, dims);
    UNPROTECT(1);
    return sResult;
}
