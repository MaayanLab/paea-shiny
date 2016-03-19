\name{PAEAAnalysis}
\alias{PAEAAnalysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Function to Evaluate Gene-Set Enrichment Using PAEA.
}
\description{
This function evaluates the enrichmnet of gene sets in expression data using the PAEA method. A characteristic direction (the result of the function \code{chdirAnalysis} and a Gene Matrix Transposed (GMT) file, which is a set of subsets of genes whose enrichmnet is evaluated, and returns a prioritized list of the gene sets. 
}
\usage{
PAEAAnalysis(chdirresults, gmtfile, gammas = c(1), casesensitive = FALSE,
showprogress=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chdirresults}{
This input is the first part of the output from \code{chdirAnalysis}, which has the names \code{$chdirprops}. This contains the characteristic direction which forms the basis for the enrichmnet analysis.
}
  \item{gmtfile}{
A data set composed of a list of lists of genes (a GMT file); each list of genes has, as its first element, a label for the gene list that follows in that line. For example, if the genes are members of a pathway then the list will be of the form: pathway_name Gene1 Gene2 ... GeneN.
}
  \item{gammas}{
The list of shrinkage parameter values as used in the calculation of the characteristic direction.
}
  \item{casesensitive}{
A logical variable which determines whether the gene comparisons should be case sensitive.
}
  \item{showprogress}{show a progress bar.}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{p_values}{A matrix of p values with gene sets down the rows and values of the shrinkage parameter values across the columns.}
\item{principal_angles}{A matrix with rows corresponding to gene sets, columns corresponding to values of the shrinkage parameter  and elements equal to the principal angle between the gene set subspace and the characteristic direction.}
}
\references{
Clark, Neil R., et al. "The characteristic direction: a geometrical approach to identify differentially expressed genes." BMC bioinformatics 15.1 (2014): 79.
}
\author{
Neil R. Clark and Avi Ma'ayan
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.


##################################
#
# An example PAEA analysis
#
##################################
# Load the expression data

data(example_expression_data)
data(example_sampleclass)
data(example_gammas)

#load a gmt file
data(GeneOntology_BP.gmt)

# Run the characteristic direction analysis
chdir_analysis_example <- chdirAnalysis(example_expression_data,example_sampleclass,
example_gammas,CalculateSig=FALSE)

# Run the PAEA analysis

PAEAtest <- PAEAAnalysis(chdir_analysis_example$chdirprops, gmt[1:100], example_gammas)

# Examine the p values

PAEAtest$p_values

# Examine the principal angles

PAEAtest$principal_angles


## The function is currently defined as
function (chdirresults, gmtfile, gammas = c(1), casesensitive = FALSE) 
{
    gmtlinenames <- lapply(gmtfile, function(x) x[[1]])
    gmtlines <- lapply(gmtfile, function(x) x[-1])
    PAEAresults <- lapply(gmtlines, function(x) PAEA(chdirresults[[1]], 
        x, casesensitive = casesensitive))
    gammalabels <- unlist(lapply(gammas, function(x) paste("gamma=", 
        x)))
    pvalues <- lapply(PAEAresults, function(x) x[[2]])
    pvalues <- matrix(unlist(pvalues), ncol = length(gmtlines), 
        dimnames = list(gammalabels, gmtlinenames))
    pavalues <- lapply(PAEAresults, function(x) x[[1]])
    pavalues <- matrix(unlist(pvalues), ncol = length(gmtlines), 
        dimnames = list(gammalabels, gmtlinenames))
    gmtp <- sort.list(pvalues[1, ])
    list(t(pvalues[, gmtp]), t(pavalues[, gmtp]))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ enrichment }
\keyword{ multivariate }% __ONLY ONE__ keyword per line
