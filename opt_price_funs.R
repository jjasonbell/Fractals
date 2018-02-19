# opt_price functions
get_options <- function(TICKER, target_date_unix) {
    chain_url <- paste0('https://query2.finance.yahoo.com/v7/finance/options/',
                    TICKER, '?date=', target_date_unix)
    chain <- fromJSON(url(chain_url))
    chain <- chain[[1]]$result$options[[1]]
    calls <- chain$calls[[1]]
    puts <- chain$puts[[1]]
    return(list(calls=calls, puts=puts))
}

est_tauq <- function(mommat, qvec, dtvec) { 
    
    ndt <- length(dtvec)
    nq <- length(qvec)
    tauhat <- cqhat <- r2 <- ess <- matrix(0, 1, nq) # concise init 
    errmat <- matrix(0, ndt, nq)
    
    #if (dim(qvec)[1] != 1) qvec = t(qvec)
    if (dim(dtvec)[2] != 1) dtvec = t(dtvec)
    slop_break <- matrix(0, 1, nq)
    
    partf1 <- mommat / (dtvec %*% matrix(1, 1, nq))
    pfgrap <- partf1 / (matrix(1, ndt, 1) %*% partf1[1, ])
    partfn <- log(pfgrap)
    
    #Starting OLS 
    #Could use GLS or WLS. Using OLS since we have a linear scaling function
    
    for (i in 1:nq) {
        y <- partfn[, i]
        x <- cbind(rep(1, ndt), log(dtvec))
        beta <- solve(t(x) %*% x) %*% t(x) %*% y
        tauhat[i] <- beta[2]
        cqhat[i] <- exp(beta[1])
        err <- y - x %*% beta
        ess[i] <- t(err) %*% err
        errmat[, i] <- err
        n1 <- matrix(1, ndt, 1)
        wtmny <- solve(t(n1) %*% n1) %*% t(n1) %*% y
        tss <- t(y - wtmny) %*% (y - wtmny)
        r2[i] <- 1 - t(err) %*% err / tss
    }
    
    tauqhat <- tauhat
    return(list(tauqhat = tauqhat, cqhat = cqhat, errhat = errmat, 
                r2 = r2, ess = ess, slop_break = slop_break))
}

mcalc1 <- function(mfp0, dtvec, qvec) {
    # MCALC1 calculates dataset moments
    # Assumes sequence index is the same as time
    # The "series" must be the series itself, not increments  
    
    mom <- matrix(0, length(dtvec), length(qvec))
    for (i in 1:length(dtvec)) {
        inc <- dtvec[i]
        endpts <- mfp0[seq(1, length(mfp0), by=inc)] 
        absdiff <- abs(diff(endpts))
        for (j in 1:length(qvec)) {
            ndtotheq <- absdiff^qvec[j]
            mom[i,j] <- mean(ndtotheq)
        }
    }
    return(mom)
}

startm <- function(m0, tauq, q, b) {
    qd2 <- q / 2
    diff <- tauq + 1 + 
        log(.5 * (m0 / b) ^ qd2 + .5 * ((2 - m0) / b) ^ qd2) / log(b)
    diff2 <- abs(diff)
    return(diff2)
}


mle_msm2 <- function(pvec0, kvec, nk, tvec, capt, ret, sighat, gam1im) {
    
    # STEP 0                                  
    gam1i <- gam1im %*% capt  
    gam1 <- 1 / gam1i
    m0hat <- pvec0[1]
    b <- pvec0[2]
    gamvec <- 1 - (1 - gam1) ^ (b ^ (kvec - 1))
    m1hat <- 2 - m0hat
    p1 <- .5
    p2 <- .5
    m <- c(m0hat, m1hat)
    p = matrix(c(p1, p2), 2, 1)
    
    # STEP 1                                  
    nstate <- 2 ^ nk
    ucprob <- matrix(1, 1, 2 ^ nk)  
    for (j in 1:nk) {
        ucprob1 <- kronecker(t(p), matrix(1, 1, 2 ^ (nk - j)))
        ucprob <- ucprob * kronecker(matrix(1, 1, 2 ^ (j - 1)), ucprob1)
    }
    
    # STEP 2                                  
    transmat <- matrix(1, nstate, nstate)
    for (j in 1:nk) {
        j_dim1 <- 2 ^ (nk - j)
        j_dim2 <- 2 ^ (j - 1)
        jchange1 <- kronecker(t(p), matrix(1, 1, j_dim1 ))
        jchange2 <- kronecker(matrix(1, 1, j_dim2), jchange1)
        jchangepart <- gamvec[j] * matrix(1, nstate, 1) %*% jchange2
        nojchange1 <- kronecker(diag(2), matrix(1, j_dim1, j_dim1))
        nojchange2 <- kronecker(matrix(1, j_dim2, j_dim2), nojchange1)
        nojchangepart <- (1 - gamvec[j]) * nojchange2
        jpart <- jchangepart + nojchangepart
        transmat <- transmat * jpart 
    }
    
    # STEP 3                                  
    rfind <- function(x) seq(along=x)[as.logical(x)]
    
    lf_vol <- matrix(1, 1, nstate)
    for (j in 1:nk) {
        prodpart <- kronecker(t(m), matrix(1, 1, 2 ^ (nk - j)))
        prodpart2 <- kronecker(matrix(1, 1, 2 ^ (j - 1)), prodpart)
        lf_vol <- lf_vol * prodpart2
    }
    temp2 <- sort(lf_vol)
    lfvals <- unique(temp2)
    srtlfv <- sighat %*% sqrt(lfvals)             
    nlfv <- length(lfvals)
    lfv_index <- matrix(0, 1, nstate)
    for (i in 1:nlfv) {
        f1 <- rfind(lf_vol == lfvals[i])
        lfv_index[f1] <- i
    }
    
    # STEP 4                                  
    thispi <- ucprob
    lfvec <- matrix(0, 1, capt)
    for (t in 1:capt) {
        lastpi <- thispi
        gv <- ret[t] / srtlfv		        
        cd_gv <- exp(-0.5 %*% gv ^ 2) / sqrt(2 * pi)	
        cd_mu1 <- cd_gv / srtlfv		        
        cd_mu <- cd_mu1[lfv_index]
        lpt <- lastpi %*% transmat		        
        num1 <- lpt * cd_mu				
        den1 <- sum(num1)				
        thispi <- num1 / den1 
        lfvec[t] <- den1				
    }
    
    lfvec <- log(lfvec)
    lfvec_sum <- sum(lfvec)
    if (max(is.infinite(lfvec_sum)) == 0) {
        llf <- sum(lfvec)
    } else {
        llf <- -10^100
    }
    
    nlnlf <- -llf
}

MSM_CF <- function(data, rawret, kbar, gamma1) {
    # Takes data: column of prices or raw returns, rawret flag: 1 if
    #   data are in non-log raw returns (i.e. .03 = 3), 0 for price level data,
    #   and kbar: the number of frequency components.  Returns estimates of
    #   m0 and b.
    
    if (rawret == 1) { 
        ret <- 100 * log(1 + data)
    } else {
        ret <- 100 * diff(log(c(as.matrix(data)))) 
    }
    capt <- length(ret) # T
    tvec <- 1:capt 
    xtolr <- .001 * .001 * capt / 10000 
    funtolr <- 1e-7
    dparmin <- 1e-10
    dparmax <- 1e-2
    dlogdt <- .1    
    q <- 3          
    minobs <- 50    
    dtvec <- round(exp(seq(0, log(capt / minobs), by = dlogdt)))
    dtvec <- unique(dtvec) 
    ndt <- length(dtvec)
    nq <- length(q)
    sighat <- sqrt(mean(ret ^ 2)) 
    mmin <- 1.05
    mmax <- 1.99
    bmin <- 1
    bmax <- 10000
    b <- 2.5
    
    # Get initial values for m0hat #
    mommat <- mcalc1(cumsum(ret), dtvec, q)
    out <- est_tauq(mommat, q, t(dtvec))
    tauqhat <- out$tauqhat
    cqhat <- out$cqhat
    errhat <- out$errhat
    r2 <- out$r2
    ess <- out$ess
    opt <- optimize(startm, tauq=tauqhat, q=q, b=b, interval=c(1.10, 1.95))
    m0hat <- opt$minimum
    
    kvec <- 1:kbar
    parvec0 <- c(m0hat, b)
    objective_fun <- function(parvec) {
        return(mle_msm2(parvec, kvec, kbar, tvec, capt, ret, sighat, gamma1))
    }
    out <- optim(parvec0, objective_fun, lower=c(mmin, bmin),
                 upper=c(mmax, bmax), method="L-BFGS-B",
                 control = list(trace = 3))
    parvec1 <- out$par
    cat('FINAL  KVAL = ', kbar, 'B = ', b)
    cat('parvec = ', parvec1)
    cat('obj = ', -out$value)
    
    return(list(parvec = parvec1, sigma = sighat))
}

#pars <- MSM_CF(read.csv('yen_data.csv', header=F), rawret=0, 4, 4)$parvec

MSMsimulation <- function(S0, kbar, b, m0, gamma_1, sigma, NumDays){
    
    M    <- matrix(0, ncol = kbar, nrow = NumDays) 
    Vlty <- matrix(0, ncol = 1, nrow = NumDays)    
    lg   <- matrix(0, ncol = 1, nrow = NumDays)    
    S    <- matrix(0, ncol = 1, nrow = NumDays)    
    #S0   <- 100                                    
    
    #gamma_1 <- 1-(1-gamma_kbar)^(1/(b^(kbar-1))) 
    g <- gamma_1
    
    pc <- function(x){
        if(runif(1) < 0.5){  
            m0} else {2-m0}
    }
    
    M[1,1] <- pc()
    
    for(j in 2:kbar){          
        if(kbar >= j){
            g[j] <- 1-(1-gamma_1)^(b^(j-1))  
        }
        for(i in 2:NumDays){   
            
            if(kbar >= j) {
                if(runif(1) < 0.5){     
                    M[1,j] <- m0           
                } else M[1,j] <- 2- m0
            }
            
            if(runif(1) < g[1]){
                M[i,1] <- pc()            
            } else {M[i,1] <- M[i-1,1]   
            }
            
            if(kbar >= j){              
                if(runif(1) < g[j] ){  
                    M[i,j] <- pc()            
                } else {M[i,j] <- M[i-1,j]} 
            }
            
            
        }
    }
    
    for(i in 1:NumDays){
        Vlty[i] <- (sigma/sqrt(252)) * (prod(M[i,]))^0.5  
        
        lg[i] <- Vlty[i] * rnorm(1)                     
    }
    
    for(i in 2:NumDays){
        S[1] <- S0*exp(lg[1])
        S[i] <- S[i-1]*exp(lg[i])
    }
    
    return(cbind(M,Vlty,lg,S))
    
}

get_date <- function(expirations, target_remainder = 10) {
    min(expirations[days_left(expirations) > target_remainder])
}