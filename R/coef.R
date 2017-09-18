## File Name: coef.R
## File Version: 0.05
## File Last Change: 2017-01-31 14:07:26

########################
# coef for din object
coef.din <-
function (object, ...) {
	cof <- object$coef
    return(cof)
}

########################
# coef for gdina object
coef.gdina <-
function (object, ...) {
	cof <- object$coef
    return(cof)
}

########################
# coef for gdm object
coef.gdm <-
function (object, ...) {
	cof <- object$item
    return(cof)
}


########################
# coef for mcdina object
coef.mcdina <-
function (object, ...) {
	cof <- object$item
    return(cof)
}

########################
# coef for slca object
coef.slca <-
function (object, ...) {
	cof <- object$Xlambda
    return(cof)
}
