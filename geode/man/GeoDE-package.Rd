\name{GeoDE-package}
\alias{GeoDE-package}
\alias{GeoDE}
\docType{package}
\title{
Differential Expression and Enrichment Analysis with 
(Geo)metrical(D)ifferential(E)expression.
}
\description{
This package contains functions for performing multivariate analysis of geneome-wide expression data and also enrichment analysis.
}
\details{
\tabular{ll}{
Package: \tab GeoDE\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-06-06\cr
License: \tab GPL-2\cr
}

Given gene expression data from two classes (e.g. controll verses perturbed samples) with biological replicates in each class, this package can be used to extract the most significant genes and gene-sets.

Differential expression is characterised with a single direction in expression space, which can be interpreted to extract the most signiicant genes: this is achieved with the \code{chdirAnalysis} function.

Once the characeristic direction has been calculated gene-set enrichmnet can be evaluated using the \code{PAEAAnalysis} function. The user is free to use any library of gene-sets, however, included in this package is a broad range of gene-set libraries listed below:

BioCarta_pathways.gmt

Cancer_Cell_Line_Encyclopedia.gmt

ChEA.gmt

Chromosome_location.gmt

CORUM.gmt.gmt

GeneOntology_BP.gmt

GeneOntology_CC.gmt

GeneOntology_MF.gmt

GeneSigDB.gmt

Genome_Browser_PWMs.gmt

HMDB_Metabolites.gmt

Human_Gene_Atlas.gmt

KEA.gmt

KEGG_pathways.gmt

MGI_MP_top3.gmt

MGI_MP_top4.gmt

microRNA.gmt

Mouse_Gene_Atlas.gmt

NCI60.gmt

NURSA-IPMS.gmt

OMIM_disease_genes.gmt

OMIM_Expanded.gmt

Pfam-InterPro-domains.gmt

PPI_Hub_Proteins.gmt

Reactome_pathways.gmt

TF_PPIs.gmt

VirusMINT.gmt

WikiPathways_pathways.gmt

}
\author{
Author: Neil Clark and Avi Ma'ayan

Maintainer: Neil R. Clark <neil.clark@mssm.edu>
}
\references{
Clark, Neil R., et al. "The characteristic direction: a geometrical approach to identify differentially expressed genes." BMC bioinformatics 15.1 (2014): 79.
}

\keyword{ package }
\keyword{ gene }
\keyword{ differential }
\keyword{ expression }
\keyword{ enrichmnet }
\keyword{ multivariate }
\keyword{ characteristic }
\keyword{ direction }

\examples{

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
chdir_analysis_example <- chdirAnalysis(example_expression_data,example_sampleclass,example_gammas
,CalculateSig=FALSE)

# Run the PAEA analysis

PAEAtest <- PAEAAnalysis(chdir_analysis_example$chdirprops, gmt[1:100], example_gammas)

# Examine the p values

PAEAtest$p_values

# Examine the principal angles

PAEAtest$principal_angles

##################################
#
# An example multigmtPAEA analysis
#
##################################
# Load the expression data

data(example_expression_data)
data(example_sampleclass)
data(example_gammas)

#load GMT file names
data(AllGMTfiles)

# Run the characteristic direction analysis
chdir_analysis_example <- chdirAnalysis(example_expression_data,example_sampleclass,example_gammas
,CalculateSig=FALSE)

# Run the PAEA analysis over the first two GMT files in the library

multiPAEAtest <- multigmtPAEAAnalysis(chdir_analysis_example$chdirprops, AllGMTfiles[2:3],
example_gammas)

# To run on all the gmt files

#multiPAEAtestAll <- multigmtPAEAAnalysis(chdir_analysis_example$chdirprops, gammas=example_gammas)



}
