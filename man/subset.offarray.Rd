\name{subset.offarray}
\alias{subset.offarray}
\alias{[.offarray}
\alias{[<-.offarray}
\title{Subsetting offarrays}
\description{
These are subset (extraction) and replacement functions for the class \code{\link{offarray}}. You would normally call \code{x[...]} directly, rather than \code{subset(x,...)}, but the latter is a convenient place for documentation!

Although I have tried to write them as efficiently as possible, there is unavoidable overhead compared to base-R let alone raw C. In particular, there is AFAIK \emph{no} safe way to avoid a deep-copy during any subset-replacement, except for "classless" objects. So, every time you try to change one single element of an \code{\link{offarray}} with 1e8 elements, you will trigger a "deep copy" of the whole thing. \bold{Therefore} you should avoid as far as possibly writing for-loops to do replacements. \bold{Instead}, use \code{\link{autoloop}}; it's \emph{fast} and memory-efficient.
\subsection{Special features of subset extraction}{
There are a few tricks on subset-extraction. Normally you use anonymous indices, eg \code{x[i,,3:7]}, but you can optionally \emph{name} each index with one of the values "SLICE" or "NOOFF", eg:

\preformatted{
  x[ 7, 5, c(8,7,6)] # 3D offarray, with warning
  x[ SLICE=7, SLICE=5, NOOFF=c(8,6)] # 1D offarray, no warning
  x[ 7, 5, c( 8, 6), drop=T, nooff=T] # same
  x[ 7, 5, c( 8, 6), drop=c(T,T,F), nooff=c(F,F,T)] # more of same
}

Naming particular indices is usually more convenient than using the vector-forms of the arguments \code{drop} and \code{nooff}.
}
}
\usage{
\method{subset}{offarray}(x, ..., VECSUB, MATSUB, drop=FALSE, nooff=FALSE) # S3 method for offarray
\method{[}{offarray}(x,..., VECSUB, MATSUB, drop = FALSE, nooff = FALSE) # S3 method for offarray
\method{[}{offarray}(x,..., VECSUB, MATSUB) <- value  # S3 method for offarray
}
\arguments{
\item{ x}{thing to subset or subset-assign}
\item{ ...}{subset indices. For extraction only, each of these can have special names.}
\item{ VECSUB}{standard R{} "vector subset" (even for multidimensional \code{offarray}), either logical with the same length as \code{x}, or numeric. Offsets are ignored, so the first element of \code{x} is always 1 here. Negative indices are allowed, but not 0, NA, or beyond the length of the object.}
\item{ MATSUB}{standard R{} "matrix subset of array", respecting offsets, except that \code{MATSUB} does not have to be a numeric matrix, but can also be a \code{data.frame} or \code{list} with character elements for dimensions with non-null \code{dimnames}.}
\item{ drop}{whether to drop dimension(s) that end up with length-1 after extraction. Either a logical vector of length \code{dim(x)}, or a logical scalar which will be replicated to that length. \code{drop} is overridden by naming indices. \code{drop} elements are over-ridden to \code{TRUE} by any indices that are named "SLICE".}
\item{ nooff}{whether to abandon the offset for dimension(s). \code{nooff} should either be a logical vector of length \code{dim(x)}, or a logical scalar which will be replicated to that length. You would only want to set \code{nooff} if you are deliberately extracting a non-consecutive subset from soem dimension(s). R{} will do the extraction regardless, but will warn if a non-consecutive subset is used without the corresponding element of \code{nooff} being set to \code{TRUE}. \code{nooff} elements are over-ridden to \code{TRUE} by any indices that are named \code{NOOFF}.}
\item{ value}{the value...}
}
\value{Subset-extraction will return an \code{\link{offarray}} unless the result is length-1 and \code{drop} or \code{nooff} has been set/over-ridden, in which case a scalar will result. Dimensions of length-1 are dropped iff \code{SLICE} or \code{drop} is set for those dimensions. When non-consecutive elements are extracted from a dimension, the offset of the result will be set to NA in that dimension, so that \emph{that dimension} behaves just like base-R in further subsetting.
}
\details{
Both subset and subset-replacement methods use similar trickery, to avoid unnecessary deep-copies. The typical operation of an S3 method is to call \code{NextMethod} after modifying the arguments, but this can't be done directly for any \code{...} arguments. And re-calling the generic leads to deep-copies. So I modify the arguments, set a special flag, then use \code{Recall}; the code checks for the special flag first, and if so calls \code{NextMethod}. That seems to work and to be byte-compiler-robust, fortunately.

Previous versions of \code{\link{offarray}} did not work properly when the byte-compiler was in use, because of truly obscure features in the latter (p73 of the manual, and the manual is not exactly easy to find...).
}
\seealso{\code{\link{offarray}}, \code{\link{autoloop}}
}
\examples{
## See ?offarray
}
\keyword{misc}
