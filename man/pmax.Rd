\name{pmax}
\alias{pmax}
\alias{pmin}
\title{Parallel min and max}
\description{Just like base-R \code{\link{pmax}} and \code{\link{pmin}} , except that my ones work properly with \code{\link{offarray}} :/ so that \code{pmax(offar,0)} will return an \code{\link{offarray}} of the same dimension as \code{offar}. Note that \code{pmax(0,offar)} will return a vector, as per \code{base::pmax} (attributes of the return-value are based on the first argument).
\subsection{Details}{This is a pretty ugly fix, to necessitate a special-case like this. A theoretically-better option might be to make \code{pmax/pmin} into S3 generics in \code{\link{offarray}}, but since they are already in some sense S4-aware, I didn't want to accidentally mess that up; also, functions with dotdotdot args are awkward with S3 anyway, viz \code{cbind}. It's possible that this could all be handled "gracefully" by making \code{\link{offarray}} S4 and doing some wizardry, but I hate S4 and don't want to get embroiled in it just to fix bugs in other packages that might still manifest anyway. A simple option is \code{base::pmax( unclass( offar), 0)} but of course that does not return an \code{\link{offarray}}, which I think \code{pmax} should.
}
}
\usage{
pmax(..., na.rm = FALSE)
pmin(..., na.rm = FALSE)
}
\arguments{\item{ ..., na.rm}{as per \code{base::pmax} (qv), a logical indicating whether missing values should be removed.}
}
\value{If the \emph{first} argument is an \code{\link{offarray}}, and the return-value from uncompiled base \code{pmax/pmin} is of the same length (ie the first element wasn't auto-replicated), then the result is an \code{\link{offarray}} with the same attributes. If not, the result is whatever base-R gives (without compilation).
}
\examples{
dumbo <- offarray( 1:4, dim=c( 2, 2), first=c( 3, 5))
try( base::pmax( 0, dumbo))
#Error in (function (sub, this_offset, ii, e)  :
#  in offarray, logical subsets must match length of object's dimension
try( base::pmax( dumbo, 0))
#Error in `[<-.offarray`(`*tmp*`, change, value = numeric(0)) :
#  Wrong number of indices
# 1D case
dumbo <- offarray( 1:2, dim=2, first=3)
try( base::pmax( 0, dumbo))
# [1] 1 2
# ... sorta works (!), but loses offarrayness
pmax(dumbo, 0)
# [3] [4]
#   1   2
# yahooo!
}
\keyword{misc}
