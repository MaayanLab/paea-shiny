\name{AllGMTfiles}
\alias{AllGMTfiles}
\docType{data}
\title{
The gmt files included in GeoDE
}
\description{
This list contains the names of all the GMT files included in GeoDE. 
}
\usage{data(AllGMTfiles)}
\format{
  The format is:
 chr "AllGMTfiles"
}

\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~

Chen, Edward Y., et al. "Enrichr: interactive and collaborative HTML5 gene list enrichment analysis tool." BMC bioinformatics 14.1 (2013): 128.

}

\examples{
# load the GMT file data

data(AllGMTfiles)

# load the first gmt file in the list

data(list=AllGMTfiles[[1]])


## maybe str(AllGMTfiles) ; plot(AllGMTfiles) ...
}
\keyword{datasets}
