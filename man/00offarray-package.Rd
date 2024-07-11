\name{offarray-package}
\alias{offarray-package}
\docType{package}
\title{How to use the offarray package}
\description{
\code{\link{offarray}} is like a regular array (or matrix, or vector) but its indices do not have to start at +1. So that, for example:

\preformatted{
  dumbo <- offarray( 1:4, dim=c( 2, 2), first=c( 3, 5))
  dumbo
  #       [,5] [,6]
  #  [3,]    1    3
  #  [4,]    2    4
  dumbo[4,6] # 4
}

You can of course have character indices (\code{names} / \code{dimnames}), too. See \code{\link{offarray}} for more detail. Use \code{\link{autoloop}} to get \emph{fast} for-loop-functionality; nested for-loops over \code{\link{offarray}} objects are just toooo slow. There is a vignette for \code{\link{autoloop}}, but "technical difficulties" mean that for now you can only view it via:

\preformatted{
  RShowDoc( "autoloop", package="offarray") # PDF
  # or
  RShowDoc( "autoloop", type="html", package="offarray") # HTML
}

I find \code{\link{offarray}} absolutely indispensible, especially for population dynamics work where datasets do not normally begin at birth-of-Christ, and ages often are only tracked above some non-1 value. If you are a misguided C nut who believes fervently that all array indices \emph{should} all start at 0, you may love it too, albeit for the wrong reasons...

NB indices do still have to be increasing integer sequences with stepsize 1; it's only the starting-point that you can tweak.
}
\section{Byte compiler quirks are now automatically worked around}{
I have fixed things in v2.0, so you don't have to read this bit...

Subassignments, eg \code{x[i,j]<-y}, used to lead to a problematic interaction between package \pkg{offarray} and the default behaviour of R{}'s byte-compiler. Therefore, old versions of \code{\link{offarray}} (eg v1.1.x) used to turn \emph{off} the byte-compiler automatically immediately before \code{\link{offarray}} itself was loaded, via a devious trick in package \pkg{nobcom}.

It turns out that R{}'s byte-compiler subtly changes the way subassignment (an R{} nightmare!) is handled, compared to uncompiled execution eg at the command-line. After years of wondering about this in the absence of any information, in 2023 I \emph{eventually} found a rather well-concealed explanation (p73 of "A Byte Code Compiler for R" by Luke Tierney, 23/08/2023). This behaviour is apparently by design.
}
\keyword{misc}
