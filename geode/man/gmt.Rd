\name{gmt}
\alias{gmt}
\docType{data}
\title{The currently loaded gmtfile
}
\description{
When a GMT file is loaded e.g. by \code{data("GeneOntology_BP.gmt")} the gmt file is stored in the variable \code{gmt}
}
\usage{gmt}
\format{
  The format is alist of character variables defining gene sets and their labels (first element).
}

\examples{
data("GeneOntology_BP.gmt")
## maybe str(gmt) ; plot(gmt) ...
}
\keyword{datasets}
