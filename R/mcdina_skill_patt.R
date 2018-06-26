## File Name: mcdina_skill_patt.R
## File Version: 0.01


# skill probabilities
mcdina_skill_patt <- function( q.matrix, skillclasses, G, pi.k,
    group0_unique)
{
    maxK <- max( q.matrix[, -c(1:2) ] )
    K <- ncol(skillclasses)
    skill.patt <- matrix( NA, nrow=K, ncol=(maxK+1)*G )
    skill.patt <- as.data.frame( skill.patt)
    zz <- 1
    for (kk in 0:maxK){ # kk <- 0
        for (gg in 1:G){  # gg <- 1
            for (ss in 1:K){   #ss <- 1
                skill.patt[ ss, zz ] <- sum( pi.k[  skillclasses[,ss]==kk,gg]  )
                ind <- which( skillclasses[,ss]==kk )
                if ( length(ind)==0 ){
                    skill.patt[ss,zz] <- NA
                }
            }
            colnames(skill.patt)[zz] <- paste0("skill.prob", kk, ".group", group0_unique[gg] )
            zz <- zz+1
        }
    }
    rownames(skill.patt) <- colnames(q.matrix)[ - c(1:2) ]
    return(skill.patt)
}
