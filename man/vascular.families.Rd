\name{vascular.families}
\alias{vascular.families}
\docType{data}
\title{Orders and Higher Level Classifications of Vascular Plants}
\description{
This data set lists orders for families of vascular plants (angiosperms, gymnosperms and pteridophytes). For angiosperms, information from orders and higher levels of classification correspond to the fourth update of the Angiosperm Phylogeny Group (APG IV, \doi{10.1111/boj.12385}). Higher levels of classification correspond to names of nodes of the consensus tree (Figure 1 in \doi{10.1111/boj.12385}). Orders for gymnosperms and pteridophytes were obtained from the website of Missouri Botanical Garden.
}

\usage{data(vascular.families)}

\format{
  A data frame with 476 observations on the following 10 variables.
  \describe{
    \item{\code{Group}}{Group.}
    \item{\code{Family.ID}}{Unique ID for each family. For angiosperms, these correspond to APG IV.}
    \item{\code{Family}}{Name of the plant family.}
    \item{\code{Family.taxonID}}{taxonID retrieved from World Flora Online.}
    \item{\code{Order}}{Name of the plant order.}
    \item{\code{Order.taxonID}}{taxonID retrieved from World Flora Online.}
    \item{\code{Node.1}}{Name of the node in the consensus tree.}
    \item{\code{Node.2}}{Name of the node in the consensus tree, with Node.2 nested within Node.1.}
    \item{\code{Node.3}}{Name of the node in the consensus tree, with Node.3 nested within Node.2.}
    \item{\code{Node.4}}{Name of the node in the consensus tree, with Node.4 nested within Node.3.}
  }
}


\references{
The Angiosperm Phylogeny Group, M. W. Chase, M. J. M. Christenhusz, M. F. Fay, J. W. Byng, W. S. Judd, D. E. Soltis, D. J. Mabberley, A. N. Sennikov, P. S. Soltis, P. F. Stevens, An update of the Angiosperm Phylogeny Group classification for the orders and families of flowering plants: APG IV, Botanical Journal of the Linnean Society 181: 1-20. \doi{10.1111/boj.12385}
}

\examples{
data(vascular.families)
}
\keyword{datasets}
