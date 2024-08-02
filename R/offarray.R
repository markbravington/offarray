# This is package offarray 

".onLoad" <-
function( libname, pkgname) {
### NON-GENERIC
  ns <- asNamespace( pkgname)
  e <- new.env( parent=baseenv())
  ns$special_stuff <- e
  e$casting_classes <- list()

### Generic dot-onLoad for mvbutils-created packages c.2021
  ## This part should only kick in when debugging C code with VSCode--
  ## should do nothing otherwise
  oa <- system.file( sprintf( 'R/load_%s_dll.R', pkgname),
      package=pkgname, lib.loc=libname)
  if( !nzchar( oa)) {
    oa <- system.file( sprintf( 'R/load_%s_dll.r', pkgname),
        package=pkgname, lib.loc=libname)
  }

  if( nzchar( oa)) {
    source( oa, local=TRUE)
  }

  # This should always happen; defines R wrappers for dot-calls
  # The function is defined by Cloaders_<pkgname>.R
  run_Cloaders <- get( sprintf( 'run_Cloaders_%s', pkgname))
  run_Cloaders()
}


".onUnload" <-
function( libpath){
  library.dynam.unload( 'offarray', libpath)
}


"[.offarray" <-
function( x, ..., VECSUB, MATSUB, drop=FALSE, nooff=FALSE) {
## 2024 version (1.8) to avoid unclass & deep-copy; also, vector NOOFF
## Trying the "Recall" trick as in '[<-.offarray', 
# drop now ignored, coz will get set on Recall
# still has to be in arglist

  # Is this a Recall, with fudged indices?    
  # Only if drop was an environment() which is INCREDIBLY unlikely as 
  # a user-error..!
  if( !missing( drop) && is.environment( drop)){
    drop <- FALSE
return( NextMethod( '[', x)) # doesn't work without return() !?!
  }

  if( missing( VECSUB) && missing( MATSUB)){ # ie normal subset
    ndots <- ...length() # NB x[] is still length-1 dots
stopifnot( ndots>0) # NEVER unless user is doing naughty stuff. Bad user!

    # Ugly syntax here, prolly can't be helped...
    dotmiss <- ismis_dotlist( as.list( match.call( expand.dots=FALSE)$...))
    if( all( dotmiss)){ # x[]; x[,]
return( x)
    }

stopifnot( ndots==length( dim( x))) # x[] already handled

    # Prolly orta bork next if 'ndots %% length( NOOFF) != 0', but...
    nooff <- rep_len( nooff, ndots) # ... is fast
    drop <- rep_len( drop, ndots) # overridden by slice
    oc <- oldClass( x)
    
    naml <- ...names()
    if( !is.null( naml) && any( nzchar( naml))) {
      named_nooffs <- naml == 'NOOFF' 
      slice <- (naml == 'SLICE') | (naml == 'DROP')
      if( any( nzchar( naml[ !slice & !named_nooffs]))){
stop( "Don't understand index names... " %&% 
        "only SLICE (or, unpreferably but synonymously, DROP) or NOOFF allowed")
      }
      nooff[ named_nooffs] <- TRUE
    } else {
      slice <- rep( FALSE, ndots)
    }
    
    dn <- dimnames( x)
    offs <- attr( x, 'offset')
    offs[ lengths( dn, FALSE)>0] <- NA_integer_ # force chars to NA-offset

    # Get list of called indices, except missing ones
    # list(...) borks on x[,i]
    if( !any( dotmiss)){
      # common enuf to get special-case
      dots <- list(...)
    } else { # manually construct non-missers
      dots <- vector( 'list', ndots)
      for( i in which( !dotmiss)){
        dots[[ i]] <- ...elt(i)
      }
    }
    
    # inds, newoffs
    extract.named( indfix(
      dots, dotmiss, dim( x), offs, slice, nooff))
    newoffs[ nooff] <- NA # should be done by indfix
    
    # Use drop arg to signal that we just wanna NextMethod
    res <- do.call( 'Recall', 
        c( list( x), inds, list( drop=environment())))

    drop <- (drop | slice) & (dim( res)==1L)
    if( any( drop)){
      if( all( drop)) {
return( structure( as.vector( res), 
        class=oc %except% cq( offarray, matrix, array)))
      }

      attr( res, 'offset') <- newoffs[ !drop]
      dimnam <- dimnames( res)[ !drop]
      dim( res) <- dim( res)[ !drop] # might kill dimnames
      dimnames( res) <- dimnam
    } else { # nothing to drop
      attr( res, 'offset') <- newoffs
    }

    oldClass( res) <- oc
return( res)    
  } else if( !missing( MATSUB)){
stopifnot( missing( VECSUB))
    # set_MATSUB() de-offsets and numericizes the indices
    # Don't want class 'offarray' on return
return( as.array( x, make_dimnames=FALSE)[ set_MATSUB( x, MATSUB)])    
  } else { # VECSUB, which  works like base-R
stopifnot( missing( MATSUB))
return( c( x)[ VECSUB]) # no class 'offarray' on return
  }
}


"[<-.offarray" <-
function (x, ..., VECSUB, MATSUB, value) {
r"--{
Deviously minimizes making unnecessary copies, but *cannot* AFAICS avoid at least 1 copy--- unlike 'default.[<-'. For true in-place, need different operator. At least this avoids 'unclass' etc; my first attempt always made 3 copies!

Trick for single-copy is to use 'Recall' *before* 'NextMethod'. Can't directly manipulate ... or ..elt(), so doesn't work to do that then call 'NextMethod' inside the original call. I did check.

Not sure about speed vs 'old[<-.offarray', but in any case use autoloop() if you care about speed, coz it does not over-copy.
}--"

  if( missing( VECSUB) && missing( MATSUB)){ # ie normal subset
    ndots <- ...length()
stopifnot( ndots>0) 
    r"--{ 
    ... NB in normal use ndots>0, since x[]<-y will generate a dot-list of length 1 with an empty element. But it's worth checking, to avoid ismis() borking (for some reason) on empty.
    Efficiency attempt 
    }--"
   
    # Test for special-cases that can go straight to NextMethod()
    #1. x[]<-y ; x[,]<-y
    #2. x[,i] after recall; 1st index filled on recall, but 
    # must avoid looking up attrib on missing first ind during orig call

    # After Recall?  
    if( !missing( ..1) && # avoid crash on 1st pass
        !is.null( attr( ..1, "_i_am_special"))){
return( NextMethod( '[<-', x)) # doesn't work without return() !?!
    }

    # May need to coerce value; presumably cheap if already done
    cc <- match( .Class[-1], names( special_stuff$casting_classes), 0L) %except% 0L
    if( length( cc)){
      value <- eval( special_stuff$casting_classes[[ cc[ 1] ]])( value)
    }

    # Will need dotmiss
    # Not sure the Rcpp solution is working (at least not inside MakeADFun) :
    # dotmiss <- ismis_dotlist( as.list( match.call( expand.dots=F)$...)))
    # So, in R, cumbersomely:
    dotmiss <- logical( ndots)
    for( idot in seq_len( ndots)){
      dotmiss[ idot] <- do.call( 'missing', 
          list( as.name( paste0( '..', idot))))
    }
    
    if( all( dotmiss)){
      # ... then R can handle it
return( NextMethod( '[<-', x)) # doesn't work without return() !?!
    }
    
    # >= 1 non-empty indices; gotta tinker with them.
    # Could special-case if only char indices (ie all numeric indices missing)  # ... which would allow the direct NextMethod route (presumably faster), 
    # but that's pretty unusual; let's not.

    # NB this version does not try to offset-adjust indices in C
    # FBOW, it just bites the slow bullet, and does all that in R, in 
    # the 'fixoffs' for-loop
    off1s <- attr( x, 'offset') - 1L
    fixoffs <- which( 
        !is.na( off1s) & 
        # (off1s != 0L) & # just do it... simpler logic
        !dotmiss
      ) # only these need modding
 
    # Fill in empty args with entire corresponding dimension    
    inds <- vector( 'list', ndots) 
    for( imiss in which( dotmiss)){
      inds[[ imiss]] <- seq_len( dim( x)[ imiss])
    }
    
    # Shouldn't be any char indices with non-1 or non-NA offset, but...
    for( i in fixoffs){
      indi <- ...elt( i)
      inds[[ i]] <- switch( mode( indi),
        'numeric'= indi - off1s[ i],
        'logical'= {
stopifnot( length( indi)==dim(x)[i]); 
          indi
        },
        'character'= indi, # let R do the work, next
stop( sprintf( "Can't index with mode %s (index %i)", mode( indi), i))
      )
    } # for fixoffs
    
    for( inomod in which( !dotmiss & is.na( off1s))){
      inds[[ inomod]] <- ...elt( inomod)
    }
    
    attr( inds[[1]], '_i_am_special') <- TRUE
return( do.call( 'Recall', c( list( x), inds, list( value=value))))
  } else if( !missing( MATSUB)){ # MATSUB
stopifnot( missing( VECSUB))
    
    VECSUB <- set_MATSUB( x, MATSUB) # ready for Recall next  
  } else { # VECSUB, which  works like base-R
stopifnot( missing( MATSUB))
  }
  
  attr( VECSUB, '_i_am_special') <- TRUE
return( do.call( 'Recall', list( x, VECSUB, value=value)))
}


"aperm.offarray" <-
function( a, perm, ...){
  offo <- attr( a, 'offset')
  oc <- oldClass( a)
  a <- NextMethod( a)
  attr( a, 'offset') <- offo[ perm]
  class( a) <- oc
return( a)
}


"as.array.offarray" <-
function (
  x,
  ..., 
  make_dimnames=TRUE
){
  dn <- dimnames( x)
  if( make_dimnames){
    numdim <- !is.na( off <- attr( x, 'offset'))  
    if( any( numdim)){
      if( is.null( dn)) { # shouldn't be...
        dn <- rep( list( NULL), length( dim( x)))
      }

      dn[ numdim] <- FOR( which( numdim), 
          seq( from=off[.], length=dim(x)[.]))
    }
  }

  if( !is.null( dn) && all( lengths( dn)==0)){
    dn <- NULL # tidy up
  }
  dimnames( x) <- dn  
  # Avoid expensive unclass (?)
  oldClass( x) <- oldClass(x) %except% 'offarray'
  attr( x, 'offset') <- NULL

  NextMethod(x)
}


"as.data.frame.offarray" <-
function( x, row.names=NULL, optional=NULL, add_names=NULL,
    name_of_response='response', ...) {
#####
  ## 'optional' and 'row.names' needed for compatibility with generic,
  # but both are ignored
  d <- dim( x)
  nd <- length( d)
  dn <- dimnames( x)
  if( is.null( dn)) {
    dn <- structure( vector( 'list', nd), 
        names='D' %&% seq_along( dim (x)))
  }

  ndna <- add_names
  if( is.null( ndna)) {
    ndna <- names( dn)
  } else {
stopifnot( length( ndna)==nd) # ie bad add_names
  }

  if( is.null( ndna)) {
    ndna <- 'D' %&% seq_along( dim (x)) 
    # else unnamed dimnames could be obliterated in next step
  }

  if( name_of_response %in% ndna) {
stop( "name_of_response must differ from names( dimnames))")
  }

  for(i in which( sapply( dn, is.null))) {
    dn[[ i]] <- attr( x, 'offset')[ i]-1 + (1:d[ i])
  }
  df <- do.call( 'expand.grid', c( dn, 
      list( KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE)))
  df[[ name_of_response]] <- as.vector( x)
return( df)
}


"as.matrix.offarray" <-
function (x, ...) {
  # NextMethod() is very weird
  x <- as.array( x)
  NextMethod( x)
}


"autoloop" <-
function( 
  expr, 
  ..., 
  indices= list(), 
  SUMOVER= list(),
  single_object= NA,
  offarray_wanted= TRUE,
  .BLOCKSIZE= 3e3, 
  .REPORTSEC= 0
){
## Now RTMB-friendly... I think...

  dots <- c( list( ...), indices)
  allargs <- c( dots, SUMOVER)
  nargls <- names( allargs)
  
  boring_checks_autoloop() # I ain't gonna lie...
  
  subex <- substitute( expr)
  subex1 <- do.call( 'substitute', list( subex, 
      list( '['=quote( offarray:::sneaky_subset))))

  ntodo <- prod( lengths( allargs))
  nonsumover_block_size <- prod( lengths( dots))
  ndone <- 0
  time0 <- proc.time()[3]
  percent0 <- as.integer( 100 * ndone / ntodo)
  listy <- !single_object
  
  while( ndone < ntodo) {
    # Do not report unless there's progress AND time-int has elapsed...
    # ... do not even check time unless there's progress
    percent1 <- as.integer( 100 * ndone / ntodo)
    if( (percent1>percent0) && is.finite( .REPORTSEC) && (.REPORTSEC>0) ) {
      time1 <- proc.time()[3]
      if( time1 - time0 > .REPORTSEC) {
        cat( sprintf( '\r%i%% done', percent1))
        flush.console()
        time0 <- time1
        percent0 <- percent1
      }
    }

    nthis <- min( .BLOCKSIZE, ntodo-ndone)
    eg <- partial_expand_dot_grid( 
        allargs, n=nthis, already=ndone, switch_var=ndots+1)
    # ... NB because no force_data.frame, eg is just a list
    # Actually create the envir here, rather than inlining in eval()
    # coz debugging
    egenvir <- list2env( eg, hash=TRUE, parent=parent.frame())
    
    # Direct vector-indexing for output storage. 
    # SUMOVER indices change SLOWEST...
    outind <- 1L + (ndone + 0:(nthis-1)) %% nonsumover_block_size
    
    # In case of SUMOVER, control aggregation
    switch <- c( 1L, attr( eg, 'switch'), length( eg[[1]])+1L) 

    # Gonna force a hashed env here for speed with big exprs; guesswork
    outcomes <- try( eval( subex1, envir=egenvir), silent=TRUE)
    if( outcomes %is.a% 'try-error'){
      callinfo <- deparse1( sys.call()) # gotta do it here, before...
      errmsg <- handle_autoloop_error()
stop( errmsg)
    } # if error was trapped
    
    # Multiple returns? NB single list-mode return could be misinterpreted,
    # so reduce risk by checking lengths. User can enforce singleness
    if( is.na( listy)){ 
      listy <- 
          (outcomes %is.a% 'list') &&
          !is.null( names( outcomes)) &&
          all( nzchar( names( outcomes)))
    }
    
    if( listy) {
      if( !all( lengths( outcomes) %in% c( 1L, nthis))){
        lenok <- lengths( outcomes) %in% c( 1L, nthis)
stop( sprintf( 
        'Bad lengths for (%s)', 
        paste( names( outcomes)[ !lenok], collapse=', ')))
      }
      
      if( ndone==0) { # create list of outputs
        r"--{
        Ensure right "type" to store output--- potentially including logical and char. If SUMOVER, then must pre-zero it, which is safe coz can't add logicals or chars, so user won't request that (or it will bork anyhow if they do). Otherwise, leave result type as-is, so as not to miscoerce.
        
        NB use of [[1]] to extract the first element,
        regardless of offset and without unclass etc
        }--"
        ar <- if( length( SUMOVER)) 
            FOR( outcomes, rep( 0L*.[[1]], noutlen))
          else
            FOR( outcomes, rep( .[[1]], noutlen))
      }
    } else { # single value--- or stuffup!
      if( length( outcomes) %not.in% c( 1L, nthis)){
stop(   'Bad length for single returned object')
      }
      
      if( ndone==0){
        ar <- if( length( SUMOVER)) 
            list( rep( 0L*outcomes[[1]], noutlen))
          else
            list( rep( outcomes[[1]], noutlen))
      }
      outcomes <- list( outcomes)
    } # if first pass

    for( iout in seq_along( outcomes)){
      ar1 <- ar[[ iout]]
      if( length( dim( ar1) %except% 1L)){
stop( "expr must return vector(s), not matrices etc")
      }
      
      ar[[ iout]] <- TRUE # memory saver--- I hope!
      outcome <- rep_len( outcomes[[ iout]], nthis) # in case length-1
      if( length( SUMOVER)) {
        # prolly worth an if() for no-switch case, to avoid [iseq]
        if( length( switch)>2) { 
          for( i in 2:length( switch)) {
            iseq <- switch[i-1]:(switch[i]-1)
            ar1[ outind[ iseq]] <- ar1[ outind[ iseq]] + outcome[ iseq]
          }
        } else { # no switch
          ar1[ outind] <- ar1[ outind] + outcome
        }
       } else {
        ar1[ outind] <- outcome
      } # if switch
      
      ar[[ iout]] <- ar1
    } # for iout

    ndone <- ndone + nthis
  } # while
  if( is.finite( .REPORTSEC) && (.REPORTSEC>0)){
    cat( '\n')
  }

  if( ndots){
    # What dimnames to give the result? Chars as-is; ints get NULL
    # Set attrs for each member of ar[[]]
    out_dimnames <- FOR( dots, if( is.character( .)) . else NULL)

    if( offarray_wanted){
      names( out_dimnames) <- names( dots)
      out_offsets <- do.on( dots, if( is.character( .)) NA_integer_ else .[1])  
      for( iout in seq_along( ar)){
        dim( ar[[ iout]]) <- lengths( dots) 
        dimnames( ar[[ iout]]) <- out_dimnames
        attr( ar[[ iout]], 'offset') <- out_offsets
        oldClass( ar[[ iout]]) <- unique( 
            c( 'offarray', oldClass( ar[[ iout]])))
      }
    } else { # no offsets; regular array
      if( !any( lengths( out_dimnames))){
        out_dimnames <- NULL
      }

      for( iout in seq_along( ar)){
        dim( ar[[ iout]]) <- lengths( dots) 
        dimnames( ar[[ iout]]) <- out_dimnames
      }
    } # if offarray_wanted
  } # if dots; else a bunch of scalars

return( if( listy) ar else ar[[1]])
}


"boring_checks_autoloop" <-
function( nlocal=sys.parent()) mlocal({
## Only to be called from autoloop()
  if( !length( allargs)){
stop( "Nothing to loop over...")
  }

  if( 
    is.null( nargls) || 
    any( !nzchar( nargls)) || 
    any( duplicated( nargls))
  ){
stop( "Everything in dots & SUMOVER  must have a unique name")
  }
  
  if( length( SUMOVER) &&
    !all( sapply( SUMOVER, mode) %in% cq( numeric, character))){
stop( "SUMOVER indices must be numeric or character")
    # but NB more liberal; non-consec-increasing is OK, I guess...
  }
  
  ndots <- length( dots)
  if( ndots){
    dotmodes <- sapply( dots, mode)
    if( !all( dotmodes %in% cq( numeric, character))){
stop( "Indices must be numeric or character")
    }

    if( !all( do.on( dots[ dotmodes=='character'],
       !any( duplicated( .))))){
stop( "Character indices must be unique")
    }
    
    if( !all( do.on( dots[ dotmodes=='numeric'],
       all( diff( .)==1L)))){
stop( "Numeric indices must be consecutive-increasing")
    }
  
    ranges <- do.on( dots, 
        if( is.character(.)) c( 1L, length( unique( .))) else range( .))
    noutlen <- prod( ranges[2,] - ranges[1,] + 1)
  } else { # only SUMOVER, no dots
    noutlen <- 1L
  } # if/not dots, set noutlen
})


"check_consec" <-
function(ii){
  if( is.logical( ii)){
    ii <- which( ii)
  }
  all( diff( ii)==1L)
}


"dim<-.offarray" <-
function( x, value) {
  names( value) <- names( dimnames( x))
  NextMethod()
}


"dimbits" <-
function( x, which=NULL, drop=TRUE) {
stopifnot( x %is.an% 'offarray')

  if( is.null( which)) {
    which <- seq_along( dim( x))
  }

  dn <- dimnames( x)
  if( is.null( dn)) {
    dn <- rep( list( NULL), length( dim( x)))
  }

  names( which) <- names( dimnames( x))
  listo <- FOR( which,
    if( !is.null( dn[[.]])) dn[[.]] else (firstel( x, .) %upto% lastel( x, .))

    )
  if( (length( which)==1) && drop) {
return( listo[[1]])
  } else {
return( listo)
  }
}


"dimnames<-.offarray" <-
function( x, value) {
  if( !is.null( value)) {
stopifnot( 
#     is.list( value), R does this anyway
#      length( value)==length( dim( x)), R does this anyway
#     !is.null( names( value)),   # gone 2024
#     all( nzchar( names( value))), # gone 2024
     !any( duplicated( names( value)))
)
    attr( x, 'offset')[ lengths( value)>0] <- NA # added 2024; enforce this
  }
  
  names( dim( x)) <- names( value) # possibly unwise... but it's been like this for a long time
  
NextMethod( x, value)
}


"dimrange" <-
function( x, which=NULL){
stopifnot( (x %is.an% 'offarray') || (x %is.an% 'array') || is.vector( x))
  if( missing( which)){
    which=rep( TRUE, max( 1L, length( dim( x))))
  }
  cbind( firstel( x), lastel( x))[ which,,drop=FALSE]
}


"dimseq" <-
function( x, which=NULL, drop=TRUE){
# Next line isn't helpful; borks on eg "advector" class
# stopifnot(  x %is.an% 'offarray' || (x %is.an% 'array') || is.vector( x))

  if( is.null( dim( x))) { # just a basic vector
    if( !is.null( which) && !my.all.equal( which, 1L)) {
stop( "Nonsensical 'which' for vector")
    }
    
    if( !is.null( names( x))) {
return( names( x)) 
    } else {
return( seq_along( x))
    }
  } # a mere vector

  if( is.null( which)) {
    which <- seq_along( dim( x))
  }

  ds <- dimnames( x)
  if( is.null( ds)) {
    ds <- rep( list( NULL), length( dim( x)))
  }
  for( iseq in which[ !lengths( ds)]){
    ds[[ iseq]] <- seq.int( from=firstel( x, iseq), 
        length.out=dim( x)[ iseq])
  }

  if( (length( which)==1) && drop) {
return( ds[[1]])
  } else {
return( ds)
  }
}


"firstel" <-
function( x, which=NULL) {
  firstel <- attr( x, 'offset')
  if( is.null( firstel)) {
    firstel <- 1L + 0L*dim( x)
    if( !length( firstel)) { # pure vector
      firstel <- 1L
    }
  }
  firstel[ is.na( firstel)] <- 1L # if has dimname, does not need offset
  if( !is.null( nd <- names( dimnames( x)))) {
    names( firstel) <- nd
  }

  if( !is.null( which)) {
    firstel <- unname( firstel[ which])
  }
return( firstel)
}


"firstel<-" <-
function( x, i, value){
  value <- as.integer( floor( as.numeric( value))) # or fail...
  if( missing( i)){
stopifnot( length( value) %in% c( 1L, length( dim( x))))
    attr( x, 'offset')[] <- value
  }
  
stopifnot( length(i) %in% c( 1, length( value)),
    all( i>0),
    all( i <= length( dim( x))))
  value <- rep_len( value, i)
  
  if( !is.null( dimnames( x)) &&
      length( numind <- which( !is.na( value)))
  ){
    ld <- lengths( dimnames( x))
    if( !all( ld[ i[ numind]]==0)){
stop( "Can only set non-NA offset on dimensions with dimnames")
    }
  }
  
  attr( x, 'offset')[ i] <- value
return( x)    
}


"handle_autoloop_error" <-
function( nlocal=sys.parent()) mlocal({
  # Try again, in debug mode
  # Gotta tell it where to store the line-of-failure
  subex1 <- do.call( 'substitute', list( subex, 
    list( '['=quote( offarray:::sneaky_subset_debug))))
  errinfo <- try( eval( subex1, envir=egenvir), silent=TRUE
    )
  if( errinfo %is.not.a% 'try-error'){
return(  local.return( 
    "Autoloop failed mysteriously, " %&% 
    "but 2nd try for error-trapping succeeded... " %&% 
    "Original error was: \n" %&% 
    substring( 
      paste( as.character( outcomes), collapse=' '), 100)))
  }   

  # Else, disentangle errinfo (if line-specific, which will start "[nnn]")
  errinfo <- as.character( errinfo)
  # |> sub( '(?<=Error in )[^\n]*\n', callinfo %&% ' : ', x=_, perl=T)

  if( grepl( '^ *Error *: *[[]', errinfo)){
    errline <- as.numeric( errinfo |>
        xsub( '\\].*', '') |>
        xsub( '.*\\[', '') )
    # I cannot see any way that errline could be outside range...
    errindvals <- paste( names( eg), 
        do.on( eg, deparse( .[errline])), sep='=') 
    errindvals <- paste( errindvals, collapse=', ')
    errinfo <- sub( '\\[[^]]*\\]', 
        sprintf( '@%s@', errindvals), errinfo)
  }
return( local.return( errinfo |> xsub( '^[^:]*: *', '')))
})


"head.offarray" <-
function( x, n=6L, ...) {
  # Bloody 'NextMethod' behaves as incomprehensibly as usual, so...
  thing <- head( unclass( x), n, ...)

  if( length( dim( x)) == 1) {
    thing <- offarray( thing, first=firstel( x), dim=length( thing))
  } else if( length( dim( x)) == 2) {
    thing <- offarray( thing, first=firstel( x), dim=dim( thing))
  } # else an array, which get brutally vectored

return( thing)
}


"lastel" <-
function( x, which=NULL) {
  firstel <- firstel( x)
  lastel <- unname( dim( x)) + firstel - 1L
  if( !length( lastel)) {
    lastel <- length( x)
  }
  if( !is.null( which)) {
    lastel <- unname( lastel[ which])
  }
return( lastel)
}


"msub" <-
function( nels, offom1, dimmo, ...){
## This is an R version of what should be Rcpp code!
## Compute IND s.t. X[ cbind( i,j,k)] == X[ IND]

  # Make sure we start with right length, in case ...[1] is length-1
  ind <- integer( nels) + ...elt( 1L) - offom1[ 1L]
  for( ii in 2 %upto% length( dimmo)){
    ind <- ind * dimmo[ ii-1L] + ...elt( ii) - offom1[ ii]
  }
return( ind)
}


"offarray" <-
function( x, ...)
  UseMethod( 'offarray', x)


"offarray.data.frame" <-
function( x, data.col, ...) {
## Sorry about the dot in data.col--- it's for consistency with other similar
# functions of mine (in non-public packages)
stopifnot(
    !missing( data.col),
    is.character( data.col),
    length( data.col) == 1,
    data.col %in% names( x)
  )

  is_factor <- sapply( x, is.factor)
  x[ is_factor] <- lapply( x[ is_factor], as.character)
  dimcols <- names( x) %except% data.col
  ndims <- length( dimcols)
  is_int <- do.on( dimcols, is.numeric( x[[.]]) && all( my.all.equal( x[[.]], round( x[[.]]))))
  nels <- do.on( dimcols, length( unique( x[[.]])))
  int_ranges <- do.on( dimcols[ is_int], range( x[[.]]))
  char_sets <- FOR( dimcols[ !is_int], sort( unique( as.character( x[[.]])), method='radix'))
  # ... radix sort (only) is consistent across locales

  dnl <- structure( rep( list( NULL), ndims), names=dimcols)
  dnl[ !is_int] <- char_sets

  firsts <- rep( 1L, ndims)
  firsts[ is_int] <- int_ranges[ 1,]

  lasts <- nels
  lasts[ is_int] <- int_ranges[ 2,]

  output <- offarray( x[[ data.col]][1], first=firsts, last=lasts, dimnames=dnl)
  output[] <- NA # correct mode

  # Now put elements of x[[data.col]] into correct spots in offarray
  # NB if multiple assignments to same element--- last one will be kept
  xlu <- matrix( 1:nrow( x), nrow=nrow( x), ncol=ndims, dimnames=list( NULL, dimcols))
  for( i in dimcols[ is_int]) {
    xlu[ ,i] <- x[[i]]
  }
  for( i in dimcols[ !is_int]) {
    xlu[ ,i] <- match( as.character( x[[i]]), char_sets[[i]])
  }

  output[ MATSUB=xlu] <- x[[ data.col]]
return( output)
}


"offarray.default" <-
function( 
  x = NA,
  first = rep( 1L, length( dim)),
  last = first + dim - 1L,
  dim = NULL,
  dimnames = NULL,
  dimseq,
  ... # required for S3 compat
){
###########################
  if( inherits( x, 'offarray')){
return( x) # quickly--- eg if from Ops
  }
  
  if( !missing( dimseq)) { # then that overrides all others
    if( !is.list( dimseq)){ 
      dimseq <- list( dimseq) # allow just dimseq=0:5
    }
    
    dim <- lengths( dimseq) # has names()
    len <- prod( dim)
    out <- rep( c(x)[[1]], len)
    out[] <- c( x) # auto-rep
    is_char <- sapply( dimseq, is.character)
    sanity <- do.on( dimseq[ !is_char], 
        is.numeric(.) && length( .) > 0 && all( diff( .) == 1))
    # ... zero-extent not OK when setting up via dimseq alone, because what is the offset?
    if( !all( sanity)){
stop( sprintf( "Dimensional insanity for dims(%s)", 
      paste( which( !is_char)[ !sanity], collapse=',')))
    }
    
    offset <- NA_integer_ + dim # might as well give it names()
    if( any( !is_char)){
      # NB if NO !is_char, next line turns offset into a list..!!!
      offset[ !is_char] <- do.on( dimseq[ !is_char], .[1])
    }

    dimnames <- dimseq
    # dimseq[[i]]==character(n) just means offset-NA
    # so set dimnames[i] to NULL
    if( any( is_char)){
      is_char[ is_char] <- do.on( dimnames[ is_char],
        !all( !nzchar( .)))
    }
    dimnames[ !is_char] <- list( NULL)
    if( !all( do.on( dimnames[ is_char], 
      all( !is.na( .)) && all( nzchar( .)) && all( !duplicated( .))))){
stop( "each dimname must be non-empty and unique")
    }
    
    dim( out) <- dim
    dimnames( out) <- dimnames
    attr( out, 'offset') <- offset
    # Next _might_ work with S4, if we are lucky..!
    oldClass( out) <- c( 'offarray', oldClass( x))
return( out)
  }

  oc <- oldClass( x)

# Now will set dim automatically... hope that doesn't conflict...
#  if( missing( dim)) {
#    dim <- NULL
#  }

  imis <- c( missing( first), missing( last), missing( dim))
  if( sum( imis) %not.in% 1:2) {
stop( "Must supply either 1 or 2 of (first, last, dim)")
  }

  if( missing( dim)){
    dim <- if( !missing( last) && !missing( first))
      last - first + 1L # will be auto named
    else if( !is.null( dim( x)))
      dim(x)
    else length( x) # and first or last had better be length-1
  }

  if( missing( dimnames) && !is.null( dimnames( x))) {
    dimnames <- dimnames( x)
  }

  # Sanity checks: first/last/dim must if named all have same names
  ninds <- list(
      if( !missing( first)) names( first),
      if( !missing( last)) names( last),
      names( dim),
      names( dimnames( x))
    )
  wi <- which( !sapply( ninds, is.null))
  if( length( wi)) {
    dimbo <- matrix( unlist( ninds[ wi]), ncol=length( wi))

    # Check for contradictory names...
    namdim <- apply( dimbo, 1, function( x) tail( sort( x), 1))
stopifnot( all( (dimbo==namdim) | (dimbo=='')))
  } else {
    namdim <- NULL
  }

  names( dim) <- namdim # make sure
  if( !is.null( dimnames) && is.null( names( dimnames))){
    names( dimnames) <- namdim
  } # ... if null then best to not create it right now... I *think*...

  robj <- array( data = x, dim = dim, dimnames = dimnames)

  # If explicit elts for one dimname, offset is not used, so set to NA
  # Often useful to name the dimnames, regardless
  # names( dimnames) should dominate any of the others
  if( !is.null( dimnames)) {
    if( !is.null( namdim)) {
      if( !is.null( ndn <- names( dimnames))) {
        # Blatant contradiction ?
        nz1 <- nzchar( namdim)
        nz2 <- nzchar( ndn)
stopifnot( all( (ndn[ nz1 & nz2] == namdim[ nz1 & nz2])))
        namdim[ !nz1] <- ndn[ !nz1] # supply those originally in dimnames but not in namdim
      }

      names( dimnames) <- namdim
    } else { # if no namdim
      namdim <- names( dimnames)
    }

    no_offset_since_named <- !sapply( dimnames, is.null)
    fnosn <- first[ no_offset_since_named]
    if( any(  !is.na( fnosn) & fnosn != 1))
stop( "Can't have none-1 starts for dimensions with names")

    first[ no_offset_since_named] <- NA
  } else if( !is.null( namdim)) {
    # empty named list
    dimnames <- rep( list( NULL), length( dim))
    names( dimnames) <- namdim
  }

  if( !is.null( namdim)) {
    dim( robj) <- structure( unname( dim( robj)), names=namdim)
  }

  dimnames( robj) <- dimnames # maybe NULL
  attr( robj, 'offset') <- as.integer( unname( first))
  oldClass(robj) <- c( "offarray", 
      oc %except% c( 'matrix', 'array', 'offarray'))

return( robj)
}


"offarray.table" <-
function( 
  x, 
  template=NULL, 
  dimseq=NULL, 
  warn_trim=getOption( 'offarray_table_warn_trim', FALSE),
  ... # for S3 compat
){
  dnx <- dimnames( x)
  dranges <- suppressWarnings( lapply( dnx, as.integer))
  is_char <- is.na( sapply( dranges, sum))
  
  if( is.null( dimseq)){
    out_dnx <- dnx
    dranges[ is_char] <- FOR( dranges[ is_char], c( 1, length( .)))    
    out_dranges <- dranges    
  } else {
    if( !is.null( template)){
stop( "Can't use both 'template' and 'dimseq' params")
    }
    
    out_dnx <- dimseq
    out_dranges <- FOR( dimseq, 
        if( is.integer( .)) c( min(.), max(.)) else c( 1L, length(.)))
    if( !my.all.equal( unname( is_char), 
        unname( sapply( dimseq, is.character)))){
stop( "Char dimnames inconsistent...")
    }
  }

  if( is.null( template)) { # make it up from dims of table
    off <- unclass( offarray( 0, first=sapply( out_dranges, min), 
        last=sapply( out_dranges, max)))
    # ... unclass() to avoid problems with 'dimnames<-.offarray'
    
    if( !is.null( namdim <- names( dnx))) {
      dimnames( off) <- structure( rep( list( NULL), length( namdim)),
          names=namdim)
    }
    if( any( is_char)) {
      dimnames( off)[ is_char] <- out_dnx[ is_char]
    }
    offo <- attr( off, 'offset')
    offo [ is_char] <- NA_integer_ # needed before setting dimnames cos of test therein
    attr( off, 'offset') <- offo
    oldClass( off) <- 'offarray'
  } else {
    off <- 0*template
    out_dnx <- dimseq( off)
  }

  # Populate the new structure, based on the table
  # Allows for blanks--- ie indices in the template that don't appear in the table
  # ... and reordering of char indices between template and table

  # Trim x to fit. dnx is always chars; out_dnx might not be
  cout_dnx <- lapply( out_dnx, as.character)
  fit_dnx <- mapply( `%that.are.in%`, 
      dnx, cout_dnx, 
      SIMPLIFY=FALSE, USE.NAMES=FALSE)
  trimdims <- which( lengths( fit_dnx) != lengths( dnx))
  
  if( length( trimdims)){
    if( !isF( warn_trim)){
warning( sprintf( "input being trimmed in dims (%s)",
      paste( trimdims, collapse=',')))
    }
  
    # Construct "efficient" call a la x[,,<shorten>,]
    trimsubs <- rep( list( formals( ls)$name), length( dnx))
    trimsubs[ trimdims] <- fit_dnx[ trimdims]
    x <- do.call( '[', c( list( x),  trimsubs,
      list( drop=FALSE)
    ))

    # Re-get dim info for trimmed x
    dnx <- dimnames( x)
    dranges <- suppressWarnings( lapply( dnx, as.integer))
  }
  
  # Ensure insertion happens "in the right order"; name-order might
  # differ between x and template/dimseq
  dranges[ is_char] <- dnx[ is_char] 
  
  cc <- as.call( c( list( as.name( '['), as.name( 'off')), dranges))
  ccc <- substitute( xx <- x, list( xx=cc))
  eval( ccc)
return( off)
}


"Ops.offarray" <-
function( e1, e2){
## Not needed for arithmetic ops (though these sanity checks are better)
## But default comparison ops lose the class & offset ... :/

  # Dimension sanity check
  o1 <- inherits( e1, 'offarray')
  o2 <- !missing( e2) && inherits( e2, 'offarray')
  if( o1 && o2){
stopifnot( identical( unname( dimseq( e1)), unname( dimseq( e2))))
  } else if( !missing( e2) && (.Generic %not.in% c( '-', '+'))){
stopifnot( is.null( dim( e1)) || is.null( dim( e2)) || 
    identical( unname( dim( e1)), unname( dim( e2)),
    (length( e1) == length( e2)) || min( length( e1), length( e2))==1)
  )}

  if( .Generic %in% c( '+', '-', '*', '/', '^', '%%', '%/%')){
return( NextMethod( .Generic))
  }
  
  ds <- dimseq( if( o1) e1 else e2, drop=FALSE) # always list
offarray( NextMethod( .Generic), dimseq=ds)
}


"partial_expand_dot_grid" <-
function(
    dots,
    n=prod( lengths( dots)),
    already=0,
    force_data.frame=FALSE,
    switch_var=0
){
stopifnot( length( switch_var)==1)
  switch <- integer()

  lens <- lengths( dots)
  clens <- c( 1L, cumprod( lens))

  lout <- vector( 'list', length( dots))

  # I'm not going to explain this via comments. Rather study it in the debugger...
  for( idot in seq_along( dots)) {
    inseq <- already %% clens[ idot+1]
    iseq <- seq.int( from=inseq, length.out=n)
    iseq <- iseq %/% clens[ idot]
    iseq <- iseq %% lens[ idot] + 1L
    if( idot==switch_var) {
      switch <- 1L + which( diff( iseq) != 0L) # numeric(0) if none
    }
    lout[[idot]] <- dots[[ idot]][ iseq]
  }

  names( lout) <- names( dots)
  if( force_data.frame) {
    lout <- data.frame( lout, row.names=NULL, check.names=FALSE, 
        stringsAsFactors=FALSE)
  }

  if( switch_var>0L) {
    attr( lout, 'switch') <- switch
  }
return( lout)
}


"pmax" <-
function( ..., na.rm=FALSE){
  # Workaround for JIT-compiler bug :/
  #  val <- eval( body( base::pmax))
  # ... doesn't effing work, though it's *fine* within the debugger
  
  l <- list( ...)
  is_offa <- sapply( l, inherits, what='offarray')
  l[ is_offa] <- lapply( l[ is_offa], as.array)
  if( !missing( na.rm)) {
    l <- c( l, list( na.rm=na.rm))
  }
  val <- do.call( base::pmax, l)
  
  if( is_offa[1]){ 
    arg1 <- ...elt(1)
    if( length( val)==length( arg1)) {
      # ie not recycled; then force offarrayness
      attributes( val) <- attributes( arg1)
      # dim( val) <- dim( arg1)
      # dimnames( val) <- dimnames( arg1)
    }
  }
return( val)
}


"pmin" <-
function( ..., na.rm=FALSE){
  # Workaround for JIT-compiler bug :/
  #  val <- eval( body( base::pmin))
  # ... doesn't effing work, though it's *fine* within the debugger
  
  l <- list( ...)
  is_offa <- sapply( l, inherits, what='offarray')
  l[ is_offa] <- lapply( l[ is_offa], as.array)
  if( !missing( na.rm)) {
    l <- c( l, list( na.rm=na.rm))
  }
  val <- do.call( base::pmin, l)
  
  if( is_offa[1]){ 
    arg1 <- ...elt(1)
    if( length( val)==length( arg1)) {
      # ie not recycled; then force offarrayness
      attributes( val) <- attributes( arg1)
      # dim( val) <- dim( arg1)
      # dimnames( val) <- dimnames( arg1)
    }
  }
return( val)
}


"print.offarray" <-
function (x, ...) {
#######
# taken directly from 'print.Oarray', except for 'x-at-offset' below
    d <- dim(x)
    dn <- dimnames(x)
    if (is.null(dn)) 
        dn <- vector("list", length(d))
    offset <- attr( x, 'offset')
    offset[ is.na( offset)] <- 1L # new
    oc <- oldClass( x) %except% c( 'array', 'offarray', 'matrix')
    x <- as.array(x)
    for (i in seq(along = dn)) {
      if (is.null(dn[[i]])) { 
        dn[[i]] <- 0 %upto% (d[i] - 1) + offset[i] # upto ICO 0-length
        dn[[i]] <- format( dn[[i]])
        if (i <= 2) {
            dn[[i]] <- sprintf( '[%s%s%s]',
                if( i>1) ',' else '',
                dn[[i]],
                if( i==1 && length( dn) > 1) ',' else '')
        }
      }
    }
    
    if( length( oc)) {
      class( x) <- c( oldClass( x), oc)
    }
    
    dimnames(x) <- dn
    NextMethod("print")
}


"reclasso" <-
function( expr, by) UseMethod( 'reclasso', by)


"reclasso.default" <-
function( expr, by) eval.parent( expr)


"reclasso.list" <-
function( expr, by){ 
# scatn( "Hello! by is a list")

#  Orta be some way to use NextMethod to dispatch on by[[1]]...
#  But I can't see it...
#  .Class <<- structure( class( by[[1]]), previous='list'); # c( 'list', class( by[[1]])); 
#  obj <- by[[1]]
#  NextMethod( 'reclasso', by[[1]]) # doesn't work

# This works, tho mebbe it should specify envir
  getS3method( 'reclasso', class( by[[1]]))( expr, by)
}


"reclasso.numeric" <-
function( expr, by) eval.parent( expr)


"rev.offarray" <-
function( x) x[ VECSUB=rev( seq_along( x))]


"Rsubset.offarray" <-
function( x, ..., VECSUB, MATSUB, drop=FALSE, nooff=FALSE) {
## 2024 version (1.8) to avoid unclass & deep-copy; also, vector nooff
## Trying the "Recall" trick as in '[<-.offarray', 
# drop now ignored, coz will get set on Recall
# still has to be in arglist

  # Is this a Recall, with fudged indices?    
  # Only if drop was an environment() which is INCREDIBLY unlikely as 
  # a user-error..!
  if( !missing( drop) && is.environment( drop)){
    drop <- FALSE
return( NextMethod( '[', x)) # doesn't work without return() !?!
  }

  if( missing( VECSUB) && missing( MATSUB)){ # ie normal subscript
    ndots <- ...length()
stopifnot( ndots>0) # should NEVER happen unless user is doing naughty stuff. Bad user!

    dotmiss <- logical( ndots)
    for( i in 1 %upto% ndots){
      dotmiss[ i] <- eval( substitute( missing( .), 
          list( .=as.name( '..' %&% i))))
    }

    # dotmiss <- ismis( match.call( expand.dots=F)$...)
    if( all( dotmiss)){ # x[]; x[,]
return( x)
    }

stopifnot( ndots==length( dim( x))) # x[] alaready handled

    oc <- oldClass( x)
    # prolly orta bork if 'ndots %% length( NOOFF) != 0', but...
    nooff <- rep_len( nooff, ndots) # ... is fast
    
    naml <- ...names()
    if( !is.null( naml) && any( nzchar( naml))) {
      named_nooffs <- naml == 'NOOFF' 
      drop <- drop | (naml == 'SLICE') | (naml == 'DROP')
      if( any( nzchar( naml[ !drop & !named_nooffs]))){
stop( "Don't understand index names... " %&% 
        "only SLICE (or, unpreferably but synonymously, DROP) or NOOFF allowed")
      }
      nooff[ named_nooffs] <- TRUE
    } else {
      drop <- rep_len( drop, ndots)
    }

    # NB this version does not try to offset-adjust indices in C
    # FBOW, it just bites the slow bullet, and does all that in R, in 
    # the 'fixoffs' for-loop
    dn <- dimnames( x)
    offs <- attr( x, 'offset')
    offs[ lengths( dn, FALSE)>0] <- NA_integer_ # force chars to NA-offset
#  if( FALSE){
#    
#  
#  } else {
    newoffs <- offs # some don't need changing
    
    # Only these need modding before Recall:
    # NB I could exclude explicit offset-1's since they don't need 
    # to have off subtracted--- but they should still be subject to the
    # nooff and drop/SLICE checks
    auto_nooffs <- which( is.na( offs) & !dotmiss) # not just charinds
    fixoffs <- which( !is.na( offs) & !dotmiss)
    missi <- which( dotmiss)
 
    # Fill in empty args with entire corresponding dimension    
    inds <- vector( 'list', ndots) 

    for( i in missi){
      inds[[ i]] <- seq_len( dim( x)[ i])
    }
    for( i in auto_nooffs){
      # Let R handle it...
      # NB completely nutso args might cause weird error msg later. NMP.
      inds[[ i]] <- ...elt( i)
    }
    
    for( ifix in fixoffs){
      indi <- ...elt( ifix)
      if( is.logical( indi)){
stopifnot( length( indi)==dim(x)[ifix])
        indi <- which( indi)
      } else if( is.numeric( indi)){
        indi <- indi - (offs[ ifix]-1L)
      } else {
stop( sprintf( "Unsupported subscript type in dim %i", ifix))      
      }
      
      if( !length( indi)){ # Corner case! 0-length subscript
        # Same mode etc as x, but 0-length...
        # ... avoiding unclass()
        el1 <- x[[1]]
return( el1[ integer()])
      }
      
      if( !drop[ ifix]){ # then shouldn't be dropping
        if( all( diff( indi)==1L)){
          newoffs[ ifix] <- (offs[ ifix]-1L) + indi[ 1]
        } else {
          if( !nooff[ ifix]){
warning( sprintf( "Non-consec indices for dim %i, but 'nooff' not set", 
            ifix))
          }
          newoffs[ ifix] <- NA
        }
      } else if( length( indi) != 1) {
stop( sprintf( "SLICE requested for dim %i, but >1 indices", ifix))
      } # else dropping a genuine length-1 index
      
      if( anyNA( indi) || any( indi<0)){
stop( sprintf( "Bad indices for dim %i", ifix))
      }
      
      inds[[ ifix]] <- indi
    } # for ifix
    
    # Overall check on inds
    # NA, 0, and too-hi indices never allowed. Neg inds only allowed if original offset is NA
    # NA already checked for fixoffs, but I suspect it is cheap to redo (esp in C)
    # Non-existent & NA charinds will be picked up by R on Recall
    inds_to_check <- which( !dotmiss & !sapply( inds, is.character))
    for( icheck in inds_to_check){
      if( 
        anyNA( inds[[ icheck]]) || 
        any( inds[[ icheck]]==0) || 
        any( inds[[ icheck]] > dim( x)[ icheck])
      ){
stop( sprintf( "Bad index in dim %i", icheck))        
      }    
    } # for icheck
    
    res <- do.call( 'Recall', 
        c( list( x), inds, list( drop=environment())))

    drop <- drop & (dim( res)==1L) # did have: | no_offset
    if( any( drop)){
      if( all( drop)) {
return( structure( as.vector( res), class=oc %except% 'offarray'))
      }

      attr( res, 'offset') <- newoffs[ !drop]
      dimnam <- dimnames( res)[ !drop]
      dim( res) <- dim( res)[ !drop] # might kill dimnames
      dimnames( res) <- dimnam
    } else { # nothing to drop
      attr( res, 'offset') <- newoffs
    }

    oldClass( res) <- unique( c( 'offarray', oc))
return( res)    
  } else if( !missing( MATSUB)){
    # MATSUB & VECSUB are simpler, coz they don't have to post-add 
    # dims or offsets
    
stopifnot(
      missing( VECSUB),
      length( dim( MATSUB))==2,
      ncol( MATSUB)== length( offs)
    )
    
    offs <- attr( x, 'offset')
    # Only these need modding:
    fixoffs <- which( !is.na( offs) & (offs != 1L)) 

    if( is.list( MATSUB)){
      modes <- sapply( MATSUB, mode)
stopifnot( all( unique( modes) %in% c( 'numeric', 'character')))
      chari <- which( modes=='character')
      if( length( chari)){
        dn <- dimnames( x)
stopifnot( all( lengths( dn)[ chari]) > 0)        

        for( ichar in chari){
          # Pick up missing vals later
          MATSUB[[ ichar]] <- match( 
              MATSUB[[ ichar]], dn[[ ichar]], NA_integer_)
        }
      } # if chars
      
      for( inum in fixoffs %except% chari){
        MATSUB[[ inum]] <- MATSUB[[ inum]] - (offs[ inum]-1L)
      }
     
      MATSUB <- do.call( 'rbind', MATSUB)
    } else { # MATSUB is matrix
stopifnot( is.numeric( MATSUB))
      for( inum in fixoffs){
        MATSUB[,inum] <- MATSUB[,inum] - (offs[ inum]-1L)
      }
    } # list or matrix MATSUB
    
    if( anyNA( MATSUB)){
stop( "NAs not allowed in subassignment")
    }

    VECSUB <- MATSUB # ready for Recall next
  } else { # VECSUB, which  works like base-R
stopifnot( missing( MATSUB))
  }
  
  attr( VECSUB, "_i_am_special") <- TRUE
return( do.call( 'Recall', list( x, VECSUB, drop=environment())))
}


"set_MATSUB" <-
function( x, MATSUB, debug_MATSUB=FALSE){
  offs <- attr( x, 'offset')
  # Only these need modding:
  fixoffs <- which( !is.na( offs) & (offs != 1L)) 

  if( is.list( MATSUB)){
    lens <- lengths( MATSUB)
stopifnot( 
      length( MATSUB)==length( dim(x)),
      all( lens==1 | lens==max( lens))
    )

    modes <- sapply( MATSUB, mode)
    mode_OK <- (modes == 'numeric') | (modes == 'character')
    if( !all( mode_OK)){
      if( debug_MATSUB){
return( c( badmodeind=match( FALSE, mode_OK))) # first one only     
      } else {
stop( sprintf( 
          'Bad index type(s): %s', paste( which( !mode_OK), collapse=',')))
      }
    } # if mode
    
    chari <- which( modes=='character')
    if( length( chari)){
      dn <- dimnames( x)
      charind_has_dimnames_OK <- lengths( dn)[ chari] > 0
      if( !all( charind_has_dimnames_OK)){
        if( debug_MATSUB){
return( c( sillycharind=match( FALSE, charind_has_dimnames_OK)))
        } else {
stop( sprintf( 'Character index but no dimnames: %s', 
          paste( which( !charind_has_dimnames_OK), collapse=',')))
        }
      } # if charind

      for( ichar in chari){
        # Pick up missing vals later
        MATSUB[[ ichar]] <- match( 
            MATSUB[[ ichar]], dn[[ ichar]], NA_integer_)
      }
    } # if chars

    for( inum in fixoffs %except% chari){
      MATSUB[[ inum]] <- MATSUB[[ inum]] - (offs[ inum]-1L)
    }

    lenmat <- lengths( MATSUB)
    len_OK <- (lenmat==1L) | (lenmat==max( lenmat))
    if( !all( len_OK)){
      if( debug_MATSUB){
return( c( lengths_mismatchind= match( FALSE, len_OK)))
      } else {
stop( sprintf( 
        'Inconsistent index lengths: should be 1 or maximum length: %s',
        paste( which( !len_OK), collapse=',')))
      }
    } # if lens consistent

    MATSUB <- do.call( 'cbind', MATSUB)
  } else { # MATSUB should be numeric matrix
stopifnot( 
      is.numeric( MATSUB),
      # could allow all-char _matrix_ iff all dimnames exist, but...
      length( dim( MATSUB))==2,
      ncol( MATSUB)== length( offs)
    )
    for( inum in fixoffs){
      MATSUB[,inum] <- MATSUB[,inum] - (offs[ inum]-1L)
    }
  } # list or matrix

  if( debug_MATSUB){ # extra checks; only return error row
    if( narow <- match( NA, rowSums( MATSUB), 0))
return( c( narow=narow))

    if( zerow <- match( 0, apply( MATSUB, 1, prod), 0))
return( c( zerow=zerow))

    if( negrow <- match( 0, apply( MATSUB>0, 1, prod), 0))
return( c( negrow=negrow))

    if( oobrow <- match( 0, 
        apply( MATSUB <= rep( dim(x), each=nrow( MATSUB)), 1, prod), 0))
return( c( oobrow=oobrow))

return( 0) # No fail! (Problem presumably in another subexpression)
  } else if( anyNA( MATSUB) || # standard checks
      any( MATSUB == 0)){ 
    # NB other cases will be picked up by R
stop( "NAs/0s not allowed in subset")
  }

return( MATSUB)
}


"slice_atts" <-
function( x, which, ..., drop=FALSE)
  UseMethod( 'slice_atts')


"slice_atts.default" <-
function( x, which, ..., drop=FALSE) {
  atts <- list( dimnames = dimnames( x)[ which], dim=dim( x)[ which])
  if( drop || !length( atts$dim)) {
    if( any( dropme <- atts$dim==1)) {
      atts <- c( atts[[1]][ !dropme], atts[[2]][ !dropme])
    }
    if( length( atts$dim) <= 1) {
      nam <- atts$dimnames[[1]]
      if( !is.null( nam)) {
        atts <- list( names=nam)
      } else {
        atts <- list()
      }
    }
  }
return( atts)
}


"slice_atts.offarray" <-
function( x, which, ..., drop=FALSE) {
  offo <- attr( x, 'offset')
  odims <- dim( x)
  odrop <- drop
  drop <- FALSE
  atts <- NextMethod( 'slice_atts')
  atts$offset <- offo[ which]
  if( odrop) {
    if( any( dropme <- (atts$dim==1 & atts$offset==1))) {
      atts <- c( atts[[1]][ !dropme], atts[[2]][ !dropme], atts[[3]][ !dropme])
    }

    # Always keep it as an offarray, so firstel() etc work
    if( (length( atts$dim == 1) & my.all.equal( atts$offset, 1))) {
      nam <- atts$dimnames[[1]]
      if( !is.null( nam)) {
        atts <- list( names=nam, offset=1L)
      } else {
        atts <- list( offset=1L)
      }
    }
  }

  if( length( atts$offset)) {
    atts$class <- 'offarray'
  } else {
    atts <- list() # contracted over all dimensions
  }

return( atts)
}


"slindoff" <-
function( offa, MARGIN) {
  if( is.character( MARGIN)) {
    IMARGIN <- match( MARGIN, names( dimnames( offa)), 0)
    if( !IMARGIN) {
stop( sprintf( 'No dimension named "%s"', MARGIN))
    }
    MARGIN <- IMARGIN
  }
  y <- firstel( offa, MARGIN) + slice.index( offa, MARGIN) - 1L
  attributes( y) <- attributes( offa)[ cq( class, dim, dimnames, offset)]
return( y)
}


"sneaky_subset" <-
function( x, ...) {
## Only used inside autoloop
  # mc <- match.call( expand.dots=FALSE)
  # sigh... R is stuffing around making pairlists where it didn't use to
  # so...

  mc <- mc2 <- match.call( expand.dots=TRUE)
  if( nargs()==2L) {
stopifnot( 
      is.null( dim( x)) || 
      length( dim( x))==1L
    )  
    mc[[1L]] <- as.name( '[')
    if( x %is.an% 'offarray') {
      # names( mc)[2L] <- 'NOOFF' presumably no better than...
      mc$nooff <- TRUE
      
      # Will give offarray, but we want pure vector
      # as.vector() might damage other class things
      y <- eval.parent( mc)
      dimnames( y) <- NULL
      dim( y) <- NULL
      attr( y, 'offset') <- NULL
      oldClass( y) <- oldClass( y) %except% 'offarray'
return( y)
    } else {
return( eval.parent( mc))
   }
  }


  mc2 <- mc2[-2L]
  if( x %is.an% 'offarray') {
    mc2[[1L]] <- quote( list)
    # ... used to have quote( cbind), but that will coerce to character
    # ... whereas the normal '[.offarray' will unpack list correctly
    # ... MATSUB=list() option did not exist when this was first written

    callo <- call( '[', mc$x, MATSUB=mc2)
  } else {
    mc2[[ 1L]] <- quote( cbind)
    callo <- call( '[', mc$x, mc2)
  }

  eval.parent( callo)
}


"sneaky_subset_debug" <-
function( x, ...){
r"--{
  Called only when there's a known error within an autoloop expr. Return informative error if there is one, or intended value if not (coz error might be inside a different subset operation within the expr
}--"

  xname <- deparse1( substitute( x), width.cutoff=30)
  dots <- paste( sapply( match.call( expand.dots=FALSE)$...,
      deparse1, width.cutoff=20),
      collapse=', ')
  xname <- sprintf( '%s[%s]', xname, dots)
  
  # domain=NA in stop() to avoid translating error messages,
  # coz handle_autoloop_error() relies on parsing the English version

  # This is only called in autoloop, so no missings etc allowed  
  l <- try( list( ...), silent=TRUE) 
  if( l %is.a% 'try-error'){
stop( domain=NA, call.=FALSE, sprintf( 
    'Missing or bonkers index(es) for %s', xname))
  } else if( ...length() != max( 1L, length( dim( x)))){
    # pure vectors have NULL dim
stop( domain=NA, call.=FALSE, sprintf( 
    'Wrong number of index(es) [%i; should be %i] for %s', 
        ...length(), length( dim( x)), xname))
  } else {
    res <- try( set_MATSUB( x, l, debug_MATSUB=TRUE), silent=TRUE)
    if( res %is.a% 'try-error'){
      print( res)
stop( domain=NA, call.=FALSE, sprintf( 
      'Mystery error for %s',
      xname))
    }
    
    if( res != 0){
      # This msg will get further untangling in caller
stop( domain=NA, call.=FALSE, sprintf( 
      '[%i] %s index woe for %s', 
      res, 
      substring( names( res), 1, nchar( res)-3),
      xname))
    }
  }

  # Else, no failure in this particular array-access. 
  # Return the right thing, and check further terms in expr

  if( x %is.an% 'offarray'){
return( x[ MATSUB=l])   
  } else { # standard array etc
return( x[ cbind( l)])
  }
}


"str.offarray" <-
function( object, ...) {
  cat( strguts.offarray( object), '\n')
invisible()
}


"strguts.offarray" <-
function( x) {
  spr <- sprintf( '%i:%i', firstel( x), lastel( x))
  sprch <- sprintf( 'ch(%i)', dim( x))
  is.ch <- lengths( dimnames( x)) > 0
  spr[ is.ch] <- sprch[ is.ch]

  if( !is.null( ndn <- names( dimnames( x)))) {
    spr <- sprintf( '%s=%s', ndn, spr)
  }
return( sprintf( 'offarray: %s', paste( sprintf( '(%s)', spr), collapse='*')))
}


"subass1" <-
function( x, i1, value){
if( F){
  .Generic <- '[<-'
  .Class <- character()
  .GenericCallEnv <- .GlobalEnv
  .GenericDefEnv <- baseenv()
  NextMethod( '[<-')
}

  attr( i1, 'i_am_special') <- TRUE
  scatn( 'In subass1, i1=%i', i1)
  x[ i1] <- value  
x
}


"subset.offarray" <-
function(
  x, 
  ..., 
  VECSUB, 
  MATSUB, 
  drop= FALSE,
  nooff= FALSE
){
  mc <- match.call( expand.dots=TRUE)
  mc[[1]] <- `[`
  eval.parent( mc)
}


"sumover" <-
function( x, mm, drop=FALSE) UseMethod( 'sumover')


"sumover.default" <-
function( x, mm, drop=FALSE) {
# scatn( 'Hello! I am default with class %s', paste0( class( x)))

  # This "default" method also works on offarray objects, thx2 generic slice_atts
  # sumover() is generic, to allow offartmb:::sumover.advector()
  # Not working yet, though!

  ndim <- length( dim( x))
stopifnot( ndim>0)
  if( is.character( mm)) {
    mm <- match( mm, names( dimnames( x)), 0)
  }
stopifnot( all( mm>0 & mm<=ndim))

  smm <- sort( mm)
  ndimsum <- length( mm)
  dims <- dim( x)

  bedims <- 1 %upto% ndimsum
  endims <- seq( to=ndim, length=ndimsum)

  # rowSums, colSums, or aperm required ?
  if( my.all.equal( smm, bedims)) {
    z <- x # no copy
    y <- .colSums( z,
        prod( dims[ bedims]),
        prod( dims[ -bedims]))

  } else {
    if( my.all.equal( smm, endims)) {
      z <- x
    } else {
      z <- aperm( x, c( seq_along( dims) %except% smm, smm))
      dims <- dim( z)
      smm <- endims
    }
    y <- .rowSums( z,
        prod( dims[ -endims]),
        prod( dims[ endims]))
  }

  attributes( y) <- slice_atts( z, -smm, drop=drop)

  # ?drop-stuff goes here?

return( y)
}


"t.offarray" <-
function( x){
  offo <- attr( x, 'offset')
  oc <- oldClass( x)
  x <- NextMethod( x)
  attr( x, 'offset') <- rev( offo)
  oldClass( x) <-  oc
return( x)
}


"tail.offarray" <-
function( x, n=6L, ...) {
  # Bloody 'NextMethod' behaves as incomprehensibly as usual, so...
  thing <- tail( unclass( x), n, ...)

  if( length( dim( x)) == 1) {
    thing <- offarray( thing, first=lastel( x)-length( thing)+1, dim=length( thing))
  } else if( length( dim( x)) == 2) {
    thing <- offarray( thing, first=lastel( x)-dim( thing)+1, dim=dim( thing))
  } # else an array, which get brutally vectored

return( thing)
}


"toint_ferret" <-
function ( sub, checko, dsi, strict=TRUE){
  if( checko %is.a% 'factor'){ 
    lev <- levels( checko)
    if( my.all.equal( lev, dsi)) {
      sub <- substitute( as.vector( sub))
    } else {
      sub <- substitute( match( levels( sub), dsi, NA)[ as.vector( sub)])
    }
  } else if( checko %is.a% 'character'){
    sub <- substitute( as.vector( base::factor( sub), levels=dsi))
  } else if( typeof( sub)=='double') {
    sub <- substitute( as.integer( sub))
  } else if( strict){
stopifnot( typeof( sub)=='integer') # perhaps a more informative errmsg, Mark..?
  }
return( sub)
}


"whichoff" <-
function( x, expr=.){
  if( !missing( expr)){
    warning( "Two-arg 'whichoff' is deprecated in offarray >= 2.0. " %&%
        "Just use it like normal 'which'")
    subex <- substitute( expr)
    subexdot <- do.call( 'substitute', list( subex, list( .=x)))
    y <- eval.parent( subexdot)
  } else {
    y <- x
  }
  
  inds <- which( y, arr.ind=TRUE, useNames=FALSE)
  inds <- inds + rep( firstel( x)-1, each=nrow( inds))
  if( !is.null( nn <- names( dimnames( x)))){
    colnames( inds) <- nn
  }
    
return( inds)
}


"XOps.offarray" <-
function( e1, e2){
## Not needed for arithmetic ops (though these sanity checks are better)
## But default comparison ops lose the class & offset ... :/

  # Dimension sanity check
  o1 <- inherits( e1, 'offarray')
  o2 <- !missing( e2) && inherits( e2, 'offarray')
  if( o1 && o2){
stopifnot( identical( unname( dimseq( e1)), unname( dimseq( e2))))
  } else { # one is an offarray
stopifnot( is.null( dim( e1)) || is.null( dim( e2)) || 
    identical( unname( dim( e1)), unname( dim( e2)),
    (length( e1) == length( e2)) || min( length( e1), length( e2))==1)
  )}

  if( .Generic %in% c( '+', '-', '*', '/', '^', '%%', '%/%')){
return( NextMethod( .Generic))
  }
  
  ds <- dimseq( if( o1) e1 else e2)
offarray( NextMethod( .Generic), dimseq=ds)
}

