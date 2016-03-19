\name{chdirAnalysis}
\alias{chdirAnalysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Function to Perform Characteristic Direction Analysis.
}
\description{
This function takes genome-wide expression data as input and returns the characteristic direction - a unit vector in expression space which characterizes the differential expression. Also produced are 2D projections of the data and the characteristic direction. Optionaly this function will produce an evaluation of the signifcance of the result.
}
\usage{
chdirAnalysis(datain, sampleclass, gammas = list(1), nnull = 10, CalculateSig = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datain}{
A data frame containing the common gene names (first) and the expression profiles.
}
  \item{sampleclass}{
A factor with levels "\'1\'" and "\'2\'" indicating the class of the samples in the data. For each column of the data frame (excluding the gene names) this factor should contain an entry indicating the class from which the sample derives (e.g. controll sample,"\'1\'"", or perturbed sample, "\'2\'".)
}
  \item{gammas}{
A set of values for the shrinkage parameter. The default value is gammas=c(1.0).
}
  \item{nnull}{
If a significance estimate is to be made the number of random directions used is set with this value.
}
  \item{CalculateSig}{
A logical value which determines whether a significance estimate is to be calculated.
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{chdirprops}{This is a list of properties of the characteristic direction. The first element is \code{chdirprops$chdir}, the vector in expresion space whose direction characerises the differential expression. The second element, \code{chdirprops$pca2d}, is the 2D PCA projection of the data. The third element, \code{chdirprops$chdir_pca2d} is the 2D projection of the characteristic direction vector into PCA space.}
\item{results}{A list with an element corresponding to each of the shrinkage parameter vlues giing the sorted list of genes and their characteristic direction coefficients. If a significance estimate has been made then only the significant genes are returned here.}
\item{plots}{for each value of the shrinkage parameter a 2D PCA projection of the data and the characteristic direction is generated. If a significance estimate is made then the significance curve is also produced. A positive peak indicates that the two classes of samples are significantly different.}
}
\references{
Clark, Neil R., et al. "The characteristic direction: a geometrical approach to identify differentially expressed genes." BMC bioinformatics 15.1 (2014): 79.
}
\author{
Neil R Clark and Avi Ma'ayan
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{chdirSig}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

##################################
#
# An example characteristic direction analysis
#
##################################

# Load the example data

data(example_expression_data)
data(example_sampleclass)
data(example_gammas)

# Examine the expression data
head(example_expression_data)

# Examine the corresponding sample class factor
example_sampleclass

# Run the analysis
chdir_analysis_example <- chdirAnalysis(example_expression_data,example_sampleclass,example_gammas
,CalculateSig=TRUE,nnull=10)

# Examine the results with the first value of the shrinkage parameter (gamma)

# show the first few of the most important genes.

lapply(chdir_analysis_example$results, function(x) x[1:10])

# We can also extract the results of the \code{chdirSig} function
# for example chdir_analysis_example$chdirprops[[1]] gives the whole
# characteristic direction vector for each value of gamma:

lapply(chdir_analysis_example$chdirprops[[1]],head)

# and the estimated number of significant genes can be recovered with

chdir_analysis_example$chdirprops$number_sig_genes


## The function is currently defined as
function (datain, sampleclass, gammas = list(1), nnull = 3, CalculateSig = FALSE) 
{
    if (length(sampleclass) != (length(datain) - 1)) 
        stop("number of elements in sampleclass is inconsistent with input data")
    if (!is.data.frame(datain)) 
        stop("Input data is not in the form of a data frame")
    if (FALSE \%in\% (c("1", "2") \%in\% levels(sampleclass))) 
        stop("sample class does not include '1' and '2'")
    if (length(datain[sampleclass == 1]) < 2) 
        stop("too few controll samples")
    if (length(datain[sampleclass == 2]) < 2) 
        stop("too few samples")
    chdirresults <- chdirSig(datain, sampleclass, gammas, nnull = nnull, 
        CalculateSig = CalculateSig)
    chdirplots(chdirresults, sampleclass, gammas, CalculateSig)
    outAll <- lapply(chdirresults[[1]], function(x) {
        x[sort.list(x^2, decreasing = TRUE), ]
    })
    if (CalculateSig) {
        outSig <- mapply(function(x, ns) {
            x[sort.list(x^2, decreasing = TRUE)[1:ns], ]
        }, chdirresults[[1]], chdirresults[[6]])
        list(chdirprops = chdirresults, results = outSig)
    }
    else {
        list(chdirprops = chdirresults, results = outAll)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{differential}
\keyword{expression}% __ONLY ONE__ keyword per line
\keyword{DE}
\keyword{genes}