\name{WFO.match}
\alias{WFO.match}

\title{Standardize plant names according to World Flora Online taxonomic backbone}

\description{
This package checks a list of species against the World Flora Online (WFO) taxonomic backbone. The user needs to first download a static copy of the Taxonomic Backbone data from \url{http://www.worldfloraonline.org/downloadData}.
}

\usage{
    WFO.match(spec.data = NULL, WFO.file = NULL, WFO.data = NULL,
    spec.name = "spec.name", Genus = "Genus", Species = "Species", 
    Infraspecific.rank = "Infraspecific.rank", Infraspecific = "Infraspecific",
    acceptedNameUsageID.match = TRUE,
    Fuzzy = 0.001, Fuzzy.shortest = TRUE, Fuzzy.within = FALSE,
    verbose = FALSE, counter = 1000)
}

\arguments{
  \item{spec.data}{ A data.frame containing variables with species names. In case that a character vector is provided, then this vector will be converted to a data.frame }
  \item{WFO.file}{ File name of the static copy of the Taxonomic Backbone. If not \code{NULL}, then data will be reloaded from this file.}
  \item{WFO.data}{ Data set with the static copy of the Taxonomic Backbone. Ignored if \code{WFO.file} is not \code{NULL}. }
  \item{spec.name}{ Name of the column with taxonomic names. In case that a spec.name is provided, then separate genus and species names will be ignored.}
  \item{Genus}{ Name of the column with the genus names. }
  \item{Species}{ Name of the column with the species names. }
  \item{Infraspecific.rank}{ Name of the column with the infraspecific rank (such as subsp., var. or cultivar.). }
  \item{Infraspecific}{ Name of the column with the infraspecific names. }
  \item{acceptedNameUsageID.match}{ If \code{TRUE}, obtain the accepted name and others details from the earlier acceptedNameUsageID. }
  \item{Fuzzy}{ If larger than 0, then attempt fuzzy matching in case an identifical taxonomic name is not found in the World Flora Online. This argument will be used as argument \code{max.distance} in the internally called \code{\link[base]{agrep}}. Note that fuzzy matching is only possible for the spec.name.  }
  \item{Fuzzy.shortest}{ Limit the matching of names to those with the most similar lenght of characters (this feature is expected to eliminate matches at infraspecific levels, see examples)). }
  \item{Fuzzy.within}{ Limit the matching of names to those that contain exactly the submitted plant name (this feature is expected to be useful when submitting plant names that only contain a subset of the first characters of the species name, in order to check for best matches manually afterwards). }
  \item{verbose}{ Give details on the fuzzy matching process. }
  \item{counter}{ Progress on the matching process is reported by multiples of this counter. }
}

\details{
The function matches plant names. Columns retrieved from the World Flora Online are added to the provided input data.frame. In case that there are multiple matches, then rows from the input data.frame are repeated.

Column 'Unique' shows whether there was a unique match (or not match) in the WFO.

Column 'Matched' shows whether there was a match in the WFO.

Column 'Fuzzy' shows whether matching was done by the fuzzy method.

Column 'Subseq' gives different numbers for different matches for the same plant name.

Column 'Hybrid' shows whether there was a hybrid character in the scientificName.

Column 'New.accepted' shows whether the species details correspond to the current accepted name.

Column 'Old.status' gives the taxonomic status of the first match with the non-blank acceptedNameUsageID.

Column 'Old.ID' gives the ID of the first match with the non-blank acceptedNameUsageID.

Column 'Old.name' gives the name of the first match with the non-blank acceptedNameUsageID.


The function was inspired on the \code{Taxonstand} package that matches plant names against The Plant List. Note that The Plant List has been static since 2013, but was used as the starting point for the Taxonomic Backbone of the World Flora Online. 
}

\value{
The function returns a data.set where matched species details from the WFO are provided.
}

\references{
World Flora Online. An Online Flora of All Known Plants.
\url{http://www.worldfloraonline.org}
}   

\author{Roeland Kindt (World Agroforestry)}

\examples{

data(WFO.example)

spec.test <- data.frame(spec.name=c("Faidherbia albida", "Acacia albida", 
    "Omalanthus populneus", "Pygeum afric"))

WFO.match(spec.data=spec.test, WFO.data=WFO.example, counter=1, verbose=TRUE)

# Show all the fuzzy matches, which included those at infraspecifc level
WFO.match(spec.data=spec.test, WFO.data=WFO.example, counter=1, Fuzzy.shortest=FALSE, verbose=TRUE)

# Hybrid species
WFO.match("Arabis divaricarpa", WFO.data=WFO.example)

\dontrun{

# Cross-check with Taxonstand results
library(Taxonstand)
data(bryophytes)

# Give the file with the static copy of the Taxonomic Backbone data ('classification.txt')
# that was downloaded from \url{http://www.worldfloraonline.org/downloadData}.
WFO.file.RK <- file.choose()

# check species name
w1 <- WFO.match(bryophytes[1:20, ], WFO.file=WFO.file.RK, spec.name="Full.name", counter=1)
w1


# check species name from list of names
w1 <- WFO.match(bryophytes$Full.name[1:20], WFO.file=WFO.file.RK, counter=1)

# re-check species names obtained via Taxonstand
# note that Taxonstand did not match some infraspecific names ('Higher.level')
r1 <- TPL(bryophytes$Full.name[1:20], corr = TRUE)
w2 <- WFO.match(r1, WFO.file=WFO.file.RK, Genus="New.Genus", Species="New.Species",
          Infraspecific.rank="New.Infraspecific.rank", Infraspecific="New.Infraspecific", counter=1)
w2

# only check genus and species
# specify different names for infraspecific columns as default to Taxonstand
w3 <- WFO.match(r1, WFO.file=WFO.file.RK, Genus="New.Genus", Species="New.Species", 
          Infraspecific.rank="none", Infraspecific="none", counter=1)

# note that the method above also retrieved infraspecific levels
# to only retrieve at the species level, match infraspecific levels with an empty column
r1$empty <- rep("", nrow(r1))
w4 <- WFO.match(r1, WFO.file=WFO.file.RK, Genus="New.Genus", Species="New.Species", 
          Infraspecific.rank="empty", Infraspecific="empty", counter=1)

# save results to file
# utils::write.table(w4, quote=F, sep="\t", row.names=F, append=FALSE)

# limit the fuzzy matches to those that contain a shortened version of a species name
w5 <- WFO.match("Acacia caes", WFO.file=WFO.file.RK, Fuzzy.within=TRUE, verbose=TRUE)

}
}




