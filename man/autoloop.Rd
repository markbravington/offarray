\name{autoloop}
\alias{autoloop}
\alias{sumover}
\alias{eval_autoloop_guts}
\title{Loopless array calculations}
\description{
Suppose you basically want to write some for-loops to fill in an entire (off)array, and you could easily write the loops and the innermost expression in C. But you can't really write the loops in R{}, because they would take \emph{forever}, and you don't want to resort to C because it's a hideous PITA. Instead, you can do it all in R{} with one call to \code{autoloop}, which "does all the loops at once". You just need to say what the loop-indices are (they will also form the dimensions of the result), and what the expression(s)-to-evaluate are (i.e. the "meat" of the "for-loop sandwich"). You can also automatically sum over some indices that aren't needed in the final result. You can return several offarray variables from one call to \code{autoloop}.

Thus \code{autoloop} lets you easily and quickly do e.g. generalized dot products, without writing for-loops or contorting yourself to somehow vectorize all the expressions in direct R{} terms (something that I find excruciatingly difficult); see \bold{Examples}, and the vignette "Fast loops with offarrays". (For now, you have to do that with \code{RShowDoc("autoloop",package="offarray")}.

\code{autoloop} is fast, and also (tries to...) give you nice informative error messages when you screw up--- e.g., what the offending index values are--- which you won't get with base-R. (There is also a debugging facility: see below.) I strongly recommended using \code{autoloop} as much as possible to replace for-loops, at least with \code{\link{offarray}}. However, you can't always avoid \emph{some} for-looping, and that's fine. One situation is population-dynamics with an annual time step, as per \bold{Examples}. The section \bold{Rules} belwo lists a few restrictions (e.g. no \code{if}, though you can use \code{ifelse}), and gives tips on coding style (how to avoid \code{ifelse} ...); see also the vignette mentioned above.

The \code{SUMOVER} argument that lets you automatically "contract" the result(s) just before returning, by summing over certain loop variables (generalizing \code{rowSums} and \code{colSums}). This lets you do a matrix-multiply in one step:

\preformatted{
  # NB this uses base-R matrices, not offarrays; either would work
  M1 <- matrix( 1:6, 2, 3)
  M2 <- matrix( 10+1:12, 3, 4)
  prod <- autoloop(
    I=1:2, J=1:4,
    SUMOVER=list( K=1:3),
    M1[I,K] * M2[K,J]
  ) # same as M1 \%*\% M2
}

You can also do a contraction manually in a second step, by calling \code{sumover()}, although that's less efficient; sometimes it's unavoidable, though, e.g. when you want the denominator for Bayes' theorem. See (future) vignette.

\code{autoloop} is designed for speed and for economy of memory use, especially when you're contracting over temporary indices. It does a whole bunch of calcuations with a single R{} operation in a vectorized way, but only in chunks of size \code{.BLOCKSIZE}. The latter's default should be OK, but you might be able to get a speed-up by tweaking it to your particular case.

For convenience, if you are repeatedly using the same large set of indices in multiple \code{autoloop} calls, you may want to pre-assign them to a \code{list} called eg \code{all_my_indices} and use \code{autoloop( indices=all_my_indices, <expr>)}.
\subsection{Debugging}{
The error messages from \code{autoloop} aren't always perfectly informative; I'm working on that, but sometimes you will just get something that is not obvious. So, what then?

First, I always use my \code{debug} package to debug things, because it's Just Better. And, frankly, so should you... so the assumption here is that you are using it. Anyway, when you've gotten an puzzling situation inside a call to \code{autoloop}, it's not easy to figure out what's going on by simply doing \code{mtrace(autoloop)}; the execution of (a modified version of) \emph{your} code doesn't happen until deep inside the code of \code{autoloop} itself. To make things easier, do the following immediately before executing the offending \code{autoloop} call: \code{mtrace(eval_autoloop_guts)}. Then it will step (almost) directly into your code, doing a block of loop-indices at once. Note that your code will appear slightly modified (mainly for array accesses); that's deliberate. All your statements will (or should) return vectors, by design. Remember to do \code{mtrace(eval_autoloop_guts,F)} after you have come out.

It's possible that the same technique will work with \code{browse} or other inferior debugging tools :)
}
\subsection{Rules}{
What can you do/not-do in an \code{autoloop} expr? Well, you can think of each statement within your \code{expr} as an assignment operation in C, nested within for-loops. At the end of each evaulation of \code{expr}, each returned variable (or the very last assignment, if there is no explicit \code{return}) is stored into an array of the same name, at the element shown by the indices. Thus, this call in R{}:

\preformatted{
  Y <- autoloop( i=1:5, j=2:6, {
      divij <- b[ i] / c[j]
      i*j + divij
    })
}

is functionally equivalent to the following C code:

\preformatted{
  for( i=1; i<=5; i++)
    for( j=2; j<=6; j++){
      double divij = b(i) / c(j);
      double RETURNO = i*j + divij;
      // Store the returned value(s); just one in this case
      Y(i,j) = RETURNO;
    };
}

Here's an example with \code{SUMOVER}, a simple dot-product:

\preformatted{
  Y <- autoloop( i=1:5, SUMOVER=list( j=7:9), x[ i,j] * p[j] )
}

In C, this would be:

\preformatted{
  for( i=1; i<=5; i++){
    double SUM = 0;
    for( j=7; j<=9; j++){
      SUM += x(i,j) * p( j);
    };
    Y( i) = SUM;
  };
}

These analogies covers most situations. Under the hood, however, the calculations in R{} are not scalar; they are automatically vectorized (with some magic so that array accesses work as shown), and the casting of all the results into \code{\link{offarray}} format only happens after all the loops are complete. This has some implications for what you can and can't write inside \code{expr}. I won't try to give an exhaustive set of rules here, but here's a few pointers:

\itemize{
\item Every statement must be a \code{<-} assignment to a variable that looks like a scalar (no subassignments etc).
\item You can't use \code{if} (nor \code{for}, \code{while}, \code{repeat}, \code{break}, \code{next}); you can use \code{ifelse} instead, but see the vignette for guidance.
\item If any functions are called, they should accept vectorized arguments---apart from any arguments that are fixed outside the loop, such as in \code{dpois( x, y, log=TRUE)}--- and should return a vector result of the same length.
\item Each statement can refer to index variables, to variables known in the calling environment, and to "scalars" created by earlier statements within \code{expr}. But you can't do eg \code{{ Y <- i+j; X <- Y[j]}} because (if it actually meant anything...) it would be treating \code{Y} as not-a-scalar. \code{Y} only gets turned into an \code{\link{offarray}} after all the loops are completed.
\item References to arrays (matrices, vectors) must specify \emph{all} indices individually; \code{x[i,j,k]} is good, \code{x[]} or \code{x[,j,k]} or \code{x[cbind(i,j)]} are not.
\item Subscripting of arrays/matrices/vectors can use numeric and character indices (the latter only if the relevant \code{dimnames} exists), but not logical indices: \code{x[i>j]} won't work, because it is really a function-call but does not necessarily return a result the same length as \code{i} and \code{j}.
\item You probably can't use \code{\%*\%}.
}

Basically, if you try something fancy and it doesn't work, that means you shouldn't be doing it. Do something else instead!
}
\subsection{Details}{
For those familiar with R{}'s \code{outer} function: \code{autoloop} is conceptually similar, but generalized and generally better. Its modus operandi is similar to \code{outer}, and amounts to almost this:

\itemize{
\item use \code{expand.grid} to make a \code{data.frame} holding all combinations of indices;
\item use \code{substitute} to ensure array lookups are handled correctly;
\item \code{eval} to apply \code{expr} to the entire \code{data.frame} at once, without loops;
\item add dimension info etc to each returnee, to turn it into an \code{\link{offarray}}.
}

The main practical difference to that, is that \code{\link{partial_expand_dot_grid}} (qv) is used to ensure that only a few thousand combinations of indices are handled at once. That minimizes memory problems in large offarrays, especially but not only when \code{SUMOVER} is used.

The trick for handling array lookups, is to replace \code{x[i,j]} with something close to \code{x[cbind(i,j)]}, which works automatically with R{}'s matrix-subscripting-of-arrays mechanism (something which is too-little-known; it's buried deep in \code{?Extract}). There is an equivalent in \code{\link{offarray}}, via \code{MATSUB}. The details in \code{autoloop} are a little bit more complicated, but that's the idea.
}
}
\usage{
## See examples; this formal version of USAGE makes no sense !
## Normally eg:
## autoloop( AGE=<something>, YEAR=<something>,
##   \{ <multi-part expression involving AGE and YEAR> \}
## )
autoloop(expr, ..., indices=list(), SUMOVER=list(),
  single_object= NA, offarray_wanted= TRUE,
  .BLOCKSIZE=3000, .REPORTSEC=0)
sumover( x, mm, drop=FALSE)
eval_autoloop_guts( expr, envir) ## NEVER call this yourself!!
}
\arguments{
\item{ expr}{Expression to evaluate--- think of it as the body of a function whose arguments are the dotty-args. It should return either one vector argument, or a named list of vector arguments.}
\item{ ...}{Named subscript ranges, eg \code{A=1:5}, \code{B=c( "CAT", "DOG")}}
\item{ indices}{Instead of/as well as \code{...}, you can supply a bunch of subscripts all-in-one, as a \code{list}. This is handy when you are repeatedly using similar \emph{sets} of indices; see \bold{Examples}.}
\item{ SUMOVER}{named \code{list} of \emph{additional} subscript ranges, included in the loops for evaluation but summed-over before the result is returned. Don't duplicate them with \code{...}.}
\item{ single_object}{Normally you can ignore this, and just let \code{autoloop} deduce whether \code{expr} is returning just one (unnamed) object, or a (named) list containing several objects. However, if \code{expr} does return a single object, then it is always safe to set \code{single_object=TRUE}, and you should definitely do so if \code{expr} returns a single object that is (or might be) a named list.}
\item{ offarray_wanted}{whether the returned object(s) should be \code{offarray}, or just regular array/matrix/vector(s).}
\item{ x}{(\code{sumover}) an \code{offarray}, or at any rate something with named dimnames.}
\item{ mm}{(\code{sumover}) which dimensions to sum over; either a character vector with elements in \code{names(dimnames( x))}, or a numeric vector.}
\item{ drop}{(\code{sumover}) as per standard R{} usage, but default is FALSE here because that's better. NB that \code{drop} will always preserve \code{offarray}-ness (unless \emph{all} dimensions are contracted), so the \code{dim} attribute will not necessarily disappear even if it "could".}
\item{ .BLOCKSIZE}{how many "cases" (different "rows" aka different sets of values from the subscript ranges) to do at once. Avoids memory barfing. Too small (eg 1e3) seems to slow things down; too big could cause barfing. Default should be fine.}
\item{ .REPORTSEC}{how many seconds to let pass before reporting on progress. 0 or silly value means don't report.}
\item{ expr, envir}{not your business, since you won't be calling \code{eval_autoloop_guts} yourself...}
}
\value{
\code{autoloop} returns one or more \code{\link{offarray}}s with dimensions ranging over the input subscripts (except the ones in \code{SUMOVER}). The result is always an \code{\link{offarray}} even if all first elements are 1. If \code{expr} returns a list (which must be named), then a list of \code{\link{offarray}}s with the same names will be returned; \code{extract.named} then conveniently creates all corresponding arrays in the caller's environment.
\code{sumover} returns an \code{\link{offarray}} (or an \code{array} if the input was not an \code{\link{offarray}}), or a scalar iff \emph{all} dimensions are summed over.
}
\examples{
library( mvbutils) # for extract.named, cq, etc; already imported
# Normal offarray creation:
directo <- offarray( 0, dimseq=list( A=1:3, B=cq( CAT, DOG)))
# Or you can do it with autoloop!
quicko <- autoloop( A=1:3, B=cq( CAT, DOG), 0)
# Simplissimo (though using return(), which is often unnecessary):
cicada <- autoloop( A=1:5, Y=1:3, \{
  cohort <- Y-A # an intermediate statement
return( cohort \%\% 17)
\})
# Offarray:
cohort <- autoloop( A=0:3, Y=2000:2001, \{
  Y-A
\})
# Two variables at once:
extract.named( autoloop( A=0:2, Y=2000:2001, \{
  cohort <- Y-A
  weird <- Y \%\% A
returnList( cohort, weird)
\}))
# 'indices' feature. Should be same as 'cohort' above.
A_and_Y <- list( A=0:2, Y=2000:2001)
alt_cohort <- autoloop( indices=A_and_Y, \{
  Y-A
\})
# Lookup:
B <- outer( 1:5, 1:3, '*')
weird2 <- autoloop( A=2:4, Y=1:2, \{
return( B[ A, Y]) # ie ALL combos of A & Y
\})
# Dot-product: C[i,k] := A[i,j] \%[j]\% B[j,k] in vecless
A <- matrix( 1:12, 3, 4)
B <- matrix( 1:8, 4, 2)
A \%*\% B
C <- autoloop( I=1:3, K=1:2, SUMOVER=list( J=1:4), \{
  A[ I, J] * B[ J, K]
\})
# Errors
yy <- offarray( 0, dimseq=list( 3:4, 5:6))
try( autoloop( i=3:5, j=5:6, yy[ i,j])) # OOB
try( autoloop( i=3:5, j=5:6, yy[ rep( i, 2), rep( j, 2)])) # weird length
try( autoloop( i=3:5, j=5:6, yy[ i, rep( j, 3)])) # one weird length
try( autoloop( i=3:4, j=5:6, yy[ i,j+'word'])) # WHAT were you THINKING?
try( autoloop( i=3:4, j=5:6, yy[ i])) # wrong number of indices
try( autoloop( i=3:4, j=5:6,
    yy[ expression(),environment()])) # general craziness
try( autoloop( i=3:4, j=5:6, meaning_of_life))
# Old-style:
CCold <- autoloop( I=1:3, J=1:4, K=1:2, \{
  A[ I, J] * B[ J, K]
\})
Cold <- sumover( CCold, 'J')
# autoloop() inside for-loop (eg for population dynamics)
# Also, idiomatic
YMIN <- 2010
YMAX <- 2014
YEARS <- 2010:2014
AMIN <- 2
APLUS <- 5
AGES <- AMIN:APLUS
N <- offarray( 0, dimseq=list( Y=YEARS, A=AGES))
# Some random margins...
N[ YMIN,] <- runif( length( AGES)) # !!!
N[ , AMIN] <- runif( length( YEARS)) # !!!
# ... and mortality rates...
Z <- 0*N + runif( length( N)) # !!!
# Fill in the pop dyn, each year depending on previous
for( y in (YMIN+1) \%upto\% YMAX)\{
  nextN <- autoloop( A=(AMIN+1) \%upto\% APLUS,
    N[ y-1, A-1] * exp( -Z[ y-1, A-1]) +
    # ... maybe add plus-group, or 0 if not
    (A==APLUS) * (
      N[ y-1, A] * exp( -Z[ y-1, A])
    )
  )
  N[ y, (AMIN+1) \%upto\% APLUS] <- nextN
\}
}
\keyword{misc}
