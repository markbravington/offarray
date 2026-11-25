## ----setup,cache=FALSE, echo=TRUE, include=FALSE---------------------------------------------------------------------------------------------------------------
#debug::debug_knitr( mtrace=FALSE)

## ----echo=FALSE,cache=FALSE------------------------------------------------------------------------------------------------------------------------------------
kable_autocap <- knitr::kable
formals( kable_autocap)$caption <- quote( .GlobalEnv$'_next_caption')
`_next_caption` <- ''

## ----intro,message=FALSE---------------------------------------------------------------------------------------------------------------------------------------
library( mvbutils)
library( offarray)
options( digits=4)
bloop <- offarray( 1:6, dimseq=list( 11:12, 21:23))
bloop + 3
bloop > 0
bloop[11,]
bloop[SLICE=11,] # drops the dimension (drop is off by default)
## utils::example( 'offarray', 'offarray')...
## ... or even try reading the documentation of '?offarray'...

## ----speed-----------------------------------------------------------------------------------------------------------------------------------------------------
N <- 20 # array has 8000 elements (smallish)
system.time({
  xR <- array( 0, rep( N, 3));
  for( i in 1:N) for( j in 1:N) for( k in 1:N) 
    xR[ i,j,k] <- 1/(i+2*j+3*k)
})

system.time({
  xO <- offarray(0, dimseq=list( 1:N, 1:N, 1:N));
  for( i in 1:N) for( j in 1:N) for( k in 1:N) 
    xO[ i,j,k] <- 1/(i+2*j+3*k)
})

system.time( 
  xA <- autoloop( i=1:N, j=1:N, k=1:N, 
    1/(i+2*j+3*k))
)
N <- 100 # try 400 for a real test; then 8e6 elements and 512 MB
system.time( 
  xA <- autoloop( i=1:N, j=1:N, k=1:N, 1/(i+2*j+3*k))
)
## Don't even TRY the direct loop over offarrays!!!

system.time({
  xR <- array( 0, rep( N, 3));
  for( i in 1:N) for( j in 1:N) for( k in 1:N) 
    xR[ i,j,k] <- 1/(i+2*j+3*k)
})

## ----matmul----------------------------------------------------------------------------------------------------------------------------------------------------
mm <- matrix( 1:6, 2, 3)
xx <- 4:6
mx <- autoloop( i=1:2, SUMOVER=list( j=1:3),
  mm[i,j] * xx[ j]
)
mx
mm %*% xx

## ----ifelse-naive----------------------------------------------------------------------------------------------------------------------------------------------
  SEXES <- c( "Female", "Male")
  adSAMPYEARS <- 1995:2000    # when our "adults" were sampled
  adAGES <- 6:9               # ... of adults (ie potential parents)
  juSAMPYEARS <- 1990:1999    # when our potential offspring were sampled
  juAGES <- 1:3               # ... and their range of ages at sampling
  juBYEARS <- (1990-3):(1999-1)  # implied range of juvenile birth-years

  # Any old values for fecundity-at-age...
  fec_SA <- autoloop( S=SEXES, A=adAGES, log( A))
  
  # Any old values for total fecundity over the years... say from 1e6 typical age-7 adults
  totfec_SB <- autoloop( S=SEXES, B=juBYEARS, 1e6 * fec_SA[ S, 7])

  try( Pr_POP_SYAYA <- autoloop( 
      Sa=SEXES, Ya=adSAMPYEARS, Aa=adAGES, 
      Yj=juSAMPYEARS, Aj=juAGES, {  # curly required coz multiple statements
    Bj <- Yj - Aj;                  # birth of Our juvenile
    Aa_at_Bj <- Aa - (Ya - Bj);     # how old was Our adult then?
    Pr <- ifelse( Ya >= Bj,         # was adult still alive then?
      fec_SA[ Sa, Aa_at_Bj] / totfec_SB[ Sa, Bj],  # yes: competing against other adults
      0)                            # no: dead already
    return( Pr)
   }))

## ----ifelse-still-naive----------------------------------------------------------------------------------------------------------------------------------------
  try( Pr_POP_SYAYA <- autoloop( 
      Sa=SEXES, Ya=adSAMPYEARS, Aa=adAGES, 
      Yj=juSAMPYEARS, Aj=juAGES, {
    Bj <- Yj - Aj;                  # birth of Our juvenile
    Aa_at_Bj <- Aa - (Ya - Bj);     # how old was Our adult then?
    Pr <- ifelse( 
        (Ya >= Bj) &                   # too dead; note single ampersand coz vector
        (Aa_at_Bj >= min( adAGES)) &   # too young
        (Aa_at_Bj <= max( adAGES)),    # too old in future (and will actually be already dead!)
      fec_SA[ Sa, Aa_at_Bj] / totfec_SB[ Sa, Bj],  # yes: competing
      0)                             # no: not around
    return( Pr)
   }))

## ----ifelse-wise-----------------------------------------------------------------------------------------------------------------------------------------------
  # No try() needed this time!
  Pr_POP_SYAYA <- autoloop( 
      Sa=SEXES, Ya=adSAMPYEARS, Aa=adAGES, 
      Yj=juSAMPYEARS, Aj=juAGES, {
    Bj <- Yj - Aj;                  # birth of Our juvenile
    Aa_at_Bj <- Aa - (Ya - Bj);     # how old was Our adult then?
    Pr <- ifelse( 
        (Ya >= Bj) &                  # too dead
        (Aa_at_Bj >= min( adAGES)) &  # too young
        (Aa_at_Bj <= max( adAGES)),   # too old
      fec_SA[ Sa, Aa_at_Bj |> clamp( adAGES)] /
        totfec_SB[ Sa, Bj],          # yes, competing
      0)                             # not around
    return( Pr)
   })

## ----ifelse-gone-----------------------------------------------------------------------------------------------------------------------------------------------
  Pr_POP_SYAYA <- autoloop( 
      Sa=SEXES, Ya=adSAMPYEARS, Aa=adAGES, 
      Yj=juSAMPYEARS, Aj=juAGES, {
    Bj <- Yj - Aj;                  # birth of Our juvenile
    Aa_at_Bj <- Aa - (Ya - Bj);     # how old was Our adult then?
    Pr <-
      (Ya >= Bj) *                   # if FALSE, entire expression will give 0
      (Aa_at_Bj >= min( adAGES)) *   # ditto
      (Aa_at_Bj <= max( adAGES)) *   # ditto
      fec_SA[ Sa, Aa_at_Bj |> clamp( adAGES)] /
        totfec_SB[ Sa, Bj]  

    return( Pr)
   })

## ----twofer----------------------------------------------------------------------------------------------------------------------------------------------------
  # "Sample sizes" (equal for everything!)
  nsamp_ad <- offarray( 1000, dimseq=list( S=SEXES, Y=adSAMPYEARS, A=adAGES))
  nsamp_ju <- offarray( 1000, dimseq=list( Y=juSAMPYEARS, A=juAGES))

  extract.named( autoloop( 
      Sa=SEXES, Ya=adSAMPYEARS, Aa=adAGES, 
      Yj=juSAMPYEARS, Aj=juAGES, {
    Bj <- Yj - Aj;                  # birth of Our juvenile
    Aa_at_Bj <- Aa - (Ya - Bj);     # how old was Our adult then?
    Pr_POP_SYAYA <-
      (Ya >= Bj) *                   # if FALSE, entire expression will give 0
      (Aa_at_Bj >= min( adAGES)) *   # ditto
      (Aa_at_Bj <= max( adAGES)) *   # ditto
      fec_SA[ Sa, Aa_at_Bj |> clamp( adAGES)] /
        totfec_SB[ Sa, Bj]  
    E_POP_SYAYA <- Pr_POP_SYAYA *    # NB no subscripts on Pr_POP_SYAYA; 
                                     # it only gets offarrized on exit from autoloop()
      nsamp_ad[ Sa, Ya, Aa] * nsamp_ju[ Yj, Aj]
    returnList( Pr_POP_SYAYA, E_POP_SYAYA)
  }))
  dimseq( Pr_POP_SYAYA)

## ----autoclamp-------------------------------------------------------------------------------------------------------------------------------------------------
  Pr_POP_SYAYA <- autoloop( 
      Sa=SEXES, Ya=adSAMPYEARS, Aa=adAGES, 
      Yj=juSAMPYEARS, Aj=juAGES, {
    Bj <- Yj - Aj;                  # birth of Our juvenile
    Aa_at_Bj <- Aa - (Ya - Bj);     # how old was Our adult then?
    Pr <-
      (Ya >= Bj) *                   # if FALSE, entire expression will give 0
      (Aa_at_Bj >= min( adAGES)) *   # ditto
      (Aa_at_Bj <= max( adAGES)) *   # ditto
      fec_SA[ Sa, ~Aa_at_Bj] /       # "~" clamps that index to allowable range
        totfec_SB[ Sa, Bj]  

    return( Pr)
   })

## ----zero-pad-autoclamp----------------------------------------------------------------------------------------------------------------------------------------
  fec_SA_zp <- autoloop( S=SEXES, A=(min(adAGES)-1):(max(adAGES)+1), 0)
  fec_SA_zp[,adAGES] <- fec_SA
  dimseq( fec_SA_zp)
  dimseq( fec_SA)

  Pr_POP_SYAYA <- autoloop( 
      Sa=SEXES, Ya=adSAMPYEARS, Aa=adAGES, 
      Yj=juSAMPYEARS, Aj=juAGES, {
    Bj <- Yj - Aj;                  # birth of Our juvenile
    Aa_at_Bj <- Aa - (Ya - Bj);     # how old was Our adult then?
    Pr <-
      (Ya >= Bj) *                   # if FALSE, entire expression will give 0
      fec_SA_zp[ Sa, ~Aa_at_Bj] /       # "~" clamps that index to allowable range
        totfec_SB[ Sa, Bj]  

    return( Pr)
   })

## ----rtmb------------------------------------------------------------------------------------------------------------------------------------------------------
if( FALSE){ # don't wanna run this!
  library( offartmb)

  myobjfun <- function( pars) reclasso( by=pars, { # magic line
      Ey <- X %% pars
      resid <- y-Ey
      REPORTO( Ey, resid) # two at once; could separate
    return( sum( sqr( y-Ey)))
    })

  X <- cbind( 1, 1:5) # regression
  y <- c( 5, 3, 4, 1, 2)
  pp0 <- 1:2

  # Base-R
  myobjfun( pp0)
  numvbderiv( myobjfun, pp0) # numerical deriv (rough)

  # RTMB
  ooo <- RTMB::MakeADFun( myobjfun, pp0)
  ooo$fn( pp0)
  ooo$gr( pp0)
  # ... and they all lived happily ever after
}

