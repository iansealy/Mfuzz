\name{mfuzz}
\alias{mfuzz}
\title{Function for soft clustering based on fuzzy c-means.}

\description{This function is a wrapper function for
  \code{\link[e1071]{cmeans}} of the \code{e1071} package. It performs
  soft clustering of genes based on their expression values using
  the fuzzy c-means algorithm.}

\usage{mfuzz(eset,centers,m,\dots)}

\arguments{\item{eset}{object of the class \dQuote{ExpressionSet}.}
  \item{centers}{number of clusters.}
  \item{m}{fuzzification parameter.}
  \item{\dots}{additional parameters for \code{\link[e1071]{cmeans}}.}
}

\details{This function is the core function for soft clustering.
  It groups genes based on the Euclidean distance and the c-means
  objective function which is a weighted square error function.
  Each gene is assigned a membership value between 0 and 1 for each
  cluster. Hence, genes can be assigned to different clusters in a
  gradual manner. 
  This contrasts hard clustering where each gene can belongs to a single
  cluster.
}

\value{An object of class \code{flcust} (see
  \code{\link[e1071]{cmeans}}) which is a list with components:
  \item{centers}{the final cluster centers.}
   \item{size}{the number of data points in each cluster of the closest hard
          clustering.}
   \item{cluster}{ a vector of integers containing the indices of the clusters
          where the data points are assigned to for the closest hard
          clustering, as obtained by assigning points to the (first)
          class with maximal membership.}
    \item{iter}{the number of iterations performed.}
  \item{membership}{a matrix with the membership values of the data points to
         the clusters.}
   \item{withinerror}{the value of the objective function.}
    \item{call}{the call used to create the object.}
}

\note{Note that the clustering is based soley on the \code{exprs} matrix and 
no information is used from the \code{phenoData}. In particular, 
the ordering of samples (arrays) is the same as the ordering 
of the columns in the \code{exprs} matrix. Also, replicated arrays in the 
\code{exprs} matrix are treated as independent by the \code{mfuzz} function
i.e. they should be averagered prior to clustering or placed into different
distinct \dQuote{ExpressionSet} objects.}


\author{Matthias E. Futschik (\url{http://www.sysbiolab.eu})}

\references{M.E. Futschik and B. Charlisle, Noise robust clustering of
gene expression time-course data, Journal of Bioinformatics and
Computational Biology, 3 (4), 965-988, 2005

L. Kumar and M. Futschik, Mfuzz: a software package for soft clustering of microarray data, Bioinformation, 2(1) 5-7,2007}

\seealso{ \code{\link[e1071]{cmeans}}}

\examples{
if (interactive()){
data(yeast)
# Data pre-processing
yeastF <- filter.NA(yeast)
yeastF <- fill.NA(yeastF) # for illustration only; rather use knn method
yeastF <- standardise(yeastF)

# Soft clustering and visualisation
cl <- mfuzz(yeastF,c=20,m=1.25)
mfuzz.plot(yeastF,cl=cl,mfrow=c(2,2))

# Plotting center of cluster 1 
X11(); plot(cl[[1]][1,],type="l",ylab="Expression") 

# Getting the membership values for the first 10 genes in cluster 1
cl[[4]][1:10,1] 
}
}

\keyword{cluster}
