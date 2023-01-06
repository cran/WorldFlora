\name{new.backbone}
\alias{new.backbone}

\title{Develop a User-created Taxonomic Backbone data set}

\description{
Instead of using the taxonomic backbone data set from World Flora Online, it is possible to use matching functions of WorldFlora with alternative taxonomic backbone. The function creates new variables that correspond to key variables in the World Flora Online backbone so that matching functions WFO.match and WFO.one can be applied.
}

\usage{

new.backbone(x,
    taxonID = "taxonID", scientificName = "scientificName",
    scientificNameAuthorship = "scientificNameAuthorship",
    acceptedNameUsageID = NULL, taxonomicStatus = NULL
)

}

\arguments{
    \item{x}{ data.frame with the variables. }
    \item{taxonID}{ name of the variable with the identification }
    \item{scientificName}{ name of the variable with the full taxon name }
    \item{scientificNameAuthorship}{ name of the variable with the naming authors }
    \item{acceptedNameUsageID}{ ID of the record with the current (accepted) name. Should respond to an ID in the 'taxonID' column. In case the taxonomic name is current, then this field should be left blank. This field is used by function WFO.match to find the accepted name of a species.}
    \item{taxonomicStatus}{ Variable that indicates whether the record is for a current name or a synonym. This variable is used by function WFO.one to discriminate situations where best matches include matches with current names and synonyms. }
}

\details{
This function allows a user to create a new taxonomic backbone data set that is understood by WFO.match and WFO.one.

Alternative examples with the Mammal Diversity Database (\url{https://www.mammaldiversity.org/}) and the World Checlist of Vascular Plants (\url{https://powo.science.kew.org/about-wcvp}) are provided in the Kindt 2021a,b RPubs.
}

\value{
The function returns a data.table that can be understood by WFO.match and WFO.one for standardizing taxonomic names.
}

\author{Roeland Kindt (World Agroforestry)}

\seealso{
\code{\link{WFO.match}}, \code{\link{WFO.one}}
}

\references{
Kindt, R. 2021a. Standardizing mammal species names with the Mammal Species Database
    via exact and fuzzy matching functions from the WorldFlora package.
    \url{https://rpubs.com/Roeland-KINDT}

Kindt, R. 2021b. Standardizing GlobalTreeSearch tree species names with
    World Flora Online and the World Checklist of Vascular Plants
    \url{https://rpubs.com/Roeland-KINDT}
}

\examples{

\dontrun{

# load the World Flora Online taxonomic backbone
WFO.remember()

# get a list of Sapotaceae species
Sapotaceae <- WFO.data[WFO.data$family == "Sapotaceae",]
Sapotaceae <- Sapotaceae[Sapotaceae$taxonRank == "SPECIES", ]
Sapotaceae <- Sapotaceae[Sapotaceae$taxonomicStatus == "Accepted", ]
Sapotaceae <- Sapotaceae[, c("scientificName", "scientificNameAuthorship")]
Sapotaceae <- data.frame(ID = c(1:nrow(Sapotaceae)), Sapotaceae)
names(Sapotaceae)[2:3] <- c("species.name", "author")
head(Sapotaceae)

# create a new backbone from the GlobalTreeSearch database,
# after copying locally from https://tools.bgci.org/global_tree_search.php
GTS.dir <- "E://Roeland//R///World Flora Online//2021"
GTS <- read.csv(paste0(GTS.dir, "//global_tree_search.csv"))
GTS <- GTS[, 1:2]
GTS <- data.frame(GTS.ID = paste0("GTS-", c(1:nrow(GTS))), GTS)
nrow(GTS)

# create the new backbone
GTS.data <- new.backbone(GTS,
                         taxonID="GTS.ID",
                         scientificName="TaxonName",
                         scientificNameAuthorship="Author")
head(GTS.data)

# Check and standardize Sapotaceae
Sapotaceae.match <- WFO.one(WFO.match(Sapotaceae,
                                      WFO.data = GTS.data,
                                      spec.name = "species.name",
                                      Authorship = "author"))

nrow(Sapotaceae.match[Sapotaceae.match$Fuzzy == FALSE, ] )
nrow(Sapotaceae.match[Sapotaceae.match$Fuzzy == TRUE &
        Sapotaceae.match$Fuzzy.dist < 4, ] )
Sapotaceae.match[Sapotaceae.match$Fuzzy == TRUE &
        Sapotaceae.match$Fuzzy.dist < 4,
        c("ID", "species.name", "Fuzzy.dist", "scientificName")]

}

}
