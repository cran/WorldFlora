\name{WFO.match}
\alias{WFO.match}
\alias{WFO.url}
\alias{WFO.one}
\alias{WFO.browse}
\alias{WFO.synonyms}
\alias{WFO.family}

\title{Standardize plant names according to World Flora Online taxonomic backbone}

\description{
This package checks a list of taxa (typically species) against the World Flora Online (WFO) taxonomic backbone. The user needs to first download a static copy of the Taxonomic Backbone data from \url{https://www.worldfloraonline.org} or \url{https://zenodo.org/doi/10.5281/zenodo.7460141}  (_DwC_backbone_R.zip).
}

\usage{
    WFO.match(spec.data = NULL, WFO.file = NULL, WFO.data = NULL,
        no.dates = TRUE,
        spec.name = "spec.name", Genus = "Genus", Species = "Species",
        Infraspecific.rank = "Infraspecific.rank", Infraspecific = "Infraspecific",
        Authorship = "Authorship", First.dist = FALSE,
        acceptedNameUsageID.match = TRUE,
        Fuzzy = 0.1, Fuzzy.force = FALSE, Fuzzy.max = 250, Fuzzy.min = TRUE,
        Fuzzy.shortest = FALSE, Fuzzy.within = FALSE,
        Fuzzy.two = TRUE, Fuzzy.one = TRUE,
        squish = TRUE,
        spec.name.tolower = FALSE, spec.name.nonumber = TRUE, spec.name.nobrackets = TRUE,
        exclude.infraspecific = FALSE,
        infraspecific.excluded = c("cultivar.", "f.", "sect.", "subf.", "subg.",
            "subsp.", "subvar.", "var", "var.", "[infraspec.]", "fo.", "forma",
            "nothosubsp.", "nothovar.", "sect."),
        spec.name.sub = TRUE,
        sub.pattern=c(" sp[.] A", " sp[.] B", " sp[.] C", " sp[.]", " spp[.]", " pl[.]",
            " indet[.]", " ind[.]", " gen[.]", " g[.]", " fam[.]", " nov[.]", " prox[.]",
            " cf[.]", " aff[.]", " s[.]s[.]", " s[.]l[.]",
            " p[.]p[.]", " p[.] p[.]", "[?]", " inc[.]", " stet[.]", "Ca[.]",
            "nom[.] cons[.]", "nom[.] dub[.]", " nom[.] err[.]", " nom[.] illeg[.]",
            " nom[.] inval[.]", " nom[.] nov[.]", " nom[.] nud[.]", " nom[.] obl[.]",
            " nom[.] prot[.]", " nom[.] rej[.]", " nom[.] supp[.]", " sensu auct[.]"),
        verbose = TRUE, counter = 1000)

    WFO.url(WFO.result = NULL, browse = FALSE, browse.rows = c(1:1), ...)

    WFO.one(WFO.result = NULL, priority = "Accepted",
        spec.name = NULL, Auth.dist = NULL, Old.author.dist=NULL, First.dist = NULL,
        verbose = TRUE, counter = 1000)

    WFO.browse(taxon, WFO.file = NULL, WFO.data = NULL,
        accepted.only = FALSE, acceptedNameUsageID.match = TRUE, ...)

    WFO.synonyms(taxon, WFO.file = NULL, WFO.data = NULL, ...)

    WFO.family(taxon, WFO.file = NULL, WFO.data = NULL, ...)
}

\arguments{
  \item{spec.data}{ A data.frame containing variables with species names. In case that a character vector is provided, then this vector will be converted to a data.frame }
  \item{WFO.file}{ File name of the static copy of the Taxonomic Backbone. If not \code{NULL}, then data will be reloaded from this file.}
  \item{WFO.data}{ Data set with the static copy of the Taxonomic Backbone. Ignored if \code{WFO.file} is not \code{NULL}. }
  \item{no.dates}{ Speeding up the loading of the WFO.data by not loading fields of 'created' and 'modified'. }
  \item{spec.name}{ Name of the column with taxonomic names. In case that a \code{spec.name} is provided, then separate genus and species names will be ignored. For function \code{WFO.one}, giving the name for this columns results in copying a submitted but unmatched plant name into the \code{scientificName} of the results.}
  \item{Genus}{ Name of the column with the genus names. }
  \item{Species}{ Name of the column with the species names. }
  \item{Infraspecific.rank}{ Name of the column with the infraspecific rank (such as "subsp.", "var." or "cultivar."). }
  \item{Infraspecific}{ Name of the column with the infraspecific names. }
  \item{Authorship}{ Name of the column with the naming authorities. }
  \item{First.dist}{ If \code{TRUE}, then calculate the fuzzy distance between the first words of the submitted and matched names (these are typically the genus names) . }
  \item{acceptedNameUsageID.match}{ If \code{TRUE}, obtain the accepted name and others details from the earlier acceptedNameUsageID. }
  \item{Fuzzy}{ If larger than 0, then attempt fuzzy matching in case an identifical taxonomic name is not found in the World Flora Online. This argument will be used as argument \code{max.distance} in the internally called \code{\link[base]{agrep}}. Note that fuzzy matching is only possible for the \code{spec.name}.  }
  \item{Fuzzy.force}{ If \code{TRUE}, always use the fuzzy matching algorithm, even when the \code{spec.name} was matched exactly. }
  \item{Fuzzy.max}{ Maximum number of fuzzy matches. }
  \item{Fuzzy.min}{ If \code{TRUE}, limit the matching of names to those with the smallest Levenshtein distance, calculated via {\link[utils]{adist}}. }
  \item{Fuzzy.shortest}{ If \code{TRUE}, limit the matching of names to those with the most similar length of characters (this feature is expected to eliminate matches at infraspecific levels, see examples). }
  \item{Fuzzy.within}{ If \code{TRUE}, limit the matching of names to those that contain exactly the submitted plant name (this feature is expected to be useful when submitting plant names that only contain a subset of the first characters of the species name, in order to check for best matches manually afterwards). }
  \item{Fuzzy.two}{ If \code{TRUE}, in case that there were no fuzzy matches, limit the terms to be matched to the first two (these are expected to be genus and species names). }
  \item{Fuzzy.one}{ If \code{TRUE}, in case that there were no fuzzy matches, limit the terms to be matched to the first one (expected to be the genus name). }
  \item{squish}{ If \code{TRUE}, remove repeated whitespace and white space from the start and end of the submitted full name via {\link[stringr]{str_squish}}. }
  \item{spec.name.tolower}{ If \code{TRUE}, then convert all characters of the \code{spec.name} to lower case via {\link[base]{tolower}}. }
  \item{spec.name.nonumber}{ If \code{TRUE}, then submitted \code{spec.name} that contain numbers will be interpreted as genera, only matching the first word. }
  \item{spec.name.nobrackets}{ If \code{TRUE}, then submitted \code{spec.name} then sections of the submitted name after '(' will be removed. Note that this will also remove sections after ')', such as authorities for plant names that are in a separate column of WFO.  }
  \item{exclude.infraspecific}{ If \code{TRUE}, then exclude records that contain the infraspecific levels defined by \code{infraspecific.excluded}. }
  \item{infraspecific.excluded}{ Infraspecific levels (available from column 'verbatimTaxonRank') excluded in the results. Note that levels are excluded both in direct matches and matches with the accepted name. }
  \item{spec.name.sub}{ If \code{TRUE}, then delete sections of the \code{spec.name} that match the \code{sub.pattern}. }
  \item{sub.pattern}{ Sections of the \code{spec.name} to be deleted }
  \item{verbose}{ Give details on the fuzzy matching process. }
  \item{counter}{ Progress on the matching process is reported by multiples of this counter. }

  \item{WFO.result}{ Result obtained via {\link[WorldFlora]{WFO.match}}. }
  \item{browse}{ If \code{TRUE}, then browse urls specified by \code{browse.rows}. }
  \item{browse.rows}{ Indices of row with the urls to be browsed. }

  \item{priority}{ Method of selecting the 1-to-1 matches. Option \code{Accepted} first limits candidates to accepted names, with a possible second step of eliminating accepted names that are synonyms. Option \code{Synonym} first limits candidates to those that are not synonyms, with a possible second step of eliminating names that are not accepted. When the number of matches is larger than one after these steps, a third algorithm picks the candidate with the smallest \code{taxonID}. }
  \item{Auth.dist}{ In case that the name of the variable with the Levenshtein distance between the authorship names is provided, then the algorithm first prioritizes records with the best match between the submitted and matched author names. }
  \item{Old.author.dist}{ In case that the name of the variable with the Levenshtein distance between the authorship names for the synonym matches is provided, then the algorithm first prioritizes records with the best match between the submitted and matched author names. }
  \item{taxon}{ Character string with the name of the taxon for which information will be given (for families, different genera; for genera, different specieds; for species, infraspecific levels). }
  \item{accepted.only}{ If \code{TRUE}, then only provide taxa with accepted names. }

  \item{...}{ Other arguments for {\link[utils]{browseURL}} (\code{WFO.url}) or \code{WFO.match} (\code{WFO.browse}). }
}

\details{
The principal function (\code{WFO.match}) matches plant names. Columns retrieved from the World Flora Online are added to the provided input data.frame. In case that there are multiple matches, then rows from the input data.frame are repeated.

Column 'Unique' shows whether there was a unique match (or not match) in the WFO.

Column 'Matched' shows whether there was a match in the WFO.

Column 'Fuzzy' shows whether matching was done by the fuzzy method.

Column 'Fuzzy.dist' gives the Levenshtein distance calculated between submitted and matched plant names {\link[utils]{adist}}.

Column 'Auth.dist' gives the Levenshtein distance calculated between submitted and matched authorship names, if the former were provided {\link[utils]{adist}}.

Column 'Subseq' gives different numbers for different matches for the same plant name.

Column 'Hybrid' shows whether there was a hybrid character in the scientificName.

Column 'New.accepted' shows whether the species details correspond to the current accepted name.

Column 'Old.status' gives the taxonomic status of the first match with the non-blank acceptedNameUsageID.

Column 'Old.ID' gives the ID of the first match with the non-blank acceptedNameUsageID.

Column 'Old.name' gives the name of the first match with the non-blank acceptedNameUsageID.


The function was inspired on the \code{Taxonstand} package that matches plant names against The Plant List. Note that The Plant List has been static since 2013, but was used as the starting point for the Taxonomic Backbone of the World Flora Online.

Function \code{WFO.one} finds one unique matching name for each submitted name. Via \code{priority = "Accepted"}, it first limits candidates to accepted names, with a possible second step of eliminating accepted names that are synonyms. Via \code{priority = "Synonym"}, it first limits candidates to those that are not synonyms, with a possible second step of eliminating names that are not accepted. When the number of matches is larger than one after these steps, a third algorithm picks the candidate with the smallest \code{taxonID}. When a \code{spec.name} is given to \code{WFO.one}, the original submitted name is inserted for the \code{scientificName}.

When the user specifies the column with the \code{Auth.dist}, documenting the Levenshtein
distance between the submitted and matched authorities, then \code{WFO.one} first prioritizes records with best match between Authorities.

Function \code{WFO.browse} lists all the genera for a family, all species for a genus or all infraspecific levels for a species.

Function \code{WFO.synonyms} gives all records with the acceptedNameUsageID equal to the matched accepted species shown in the first row.

Function \code{WFO.family} provides information on the order of vascular plants, based on information available from {\link[WorldFlora]{vascular.families}}. Based on an internal list of bryophyte families, when the submitted plant name is a bryophyte, the function returns 'bryophyte' instead.
}

\value{
The main function returns a data.set with the matched species details from the WFO.
}

\references{
World Flora Online. An Online Flora of All Known Plants.
    \url{https://www.worldfloraonline.org}

Sigovini M, Keppel E, Tagliapietra. 2016. Open Nomenclature in the biodiversity era.
    Methods in Ecology and Evolution 7: 1217-1225.

Kindt, R. 2020. WorldFlora: An R package for exact and fuzzy matching of plant names against
    the World Flora Online taxonomic backbone data.
    Applications in Plant Sciences 8(9): e11388
}

\author{Roeland Kindt (World Agroforestry, CIFOR-ICRAF)}

\seealso{
\code{\link{WFO.match.fuzzyjoin}}
}

\examples{

data(WFO.example)

spec.test <- data.frame(spec.name=c("Faidherbia albida", "Acacia albida",
    "Omalanthus populneus", "Pygeum afric"))

WFO.match(spec.data=spec.test, WFO.data=WFO.example, counter=1, verbose=TRUE)

# Also calculate the Levenshtein distance for the genus
WFO.match(spec.data=spec.test, WFO.data=WFO.example, First.dist=TRUE,
    counter=1, verbose=TRUE)

# Show all the fuzzy matches, which included those at infraspecifc level
e1 <- WFO.match(spec.data=spec.test, WFO.data=WFO.example, counter=1,
    Fuzzy.min=FALSE, Fuzzy.shortest=FALSE, verbose=TRUE)
e1

# Use function WFO.one for a 1-to-1 match between submitted and matched names
WFO.one(e1)

# Hybrid species
WFO.match("Arabis divaricarpa", WFO.data=WFO.example)
WFO.match("Arabis x divaricarpa", WFO.data=WFO.example)

# Convert capitals to lower case
WFO.match("FAIDHERBIA ALBIDA", WFO.data=WFO.example, spec.name.tolower=TRUE)

# Remove sections of plant names that are equal to ' sp.' or ' indet. '
WFO.match("Prunus sp.", WFO.data=WFO.example, spec.name.sub=TRUE)

# Get urls, but do not open any
e2 <- WFO.match(spec.data=spec.test, WFO.data=WFO.example, counter=1, verbose=TRUE)
WFO.url(e2, browse=FALSE, browse.rows=c(1:nrow(e2)))

# Include input species names where no matches were found
# This happens when the name with original species names is provided to WFO.one
x1 <- WFO.match("World agroforestry", WFO.data=WFO.example)
WFO.one(x1, spec.name="spec.name")

\dontrun{

# Cross-check with Taxonstand results
library(Taxonstand)
data(bryophytes)

# Give the file with the static copy of the Taxonomic Backbone data ('classification.txt')
# that was downloaded from \url{https://www.worldfloraonline.org/downloadData}.
# Possibly first use unzip(file.choose()) for the downloaded WFO_Backbone.zip
WFO.file.RK <- file.choose()

# check species name
w1 <- WFO.match(bryophytes[1:20, ], WFO.file=WFO.file.RK, spec.name="Full.name", counter=1)
w1


# check species name from list of names
w1 <- WFO.match(bryophytes$Full.name[1:20], WFO.file=WFO.file.RK, counter=1)

# re-check species names obtained via Taxonstand
# note that Taxonstand did not match some infraspecific names ('Higher.level')
r1 <- Taxonstand::TPL(bryophytes$Full.name[1:20], corr = TRUE)
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

# as an alternative to the method above, exclude all documented infraspecific levels
# from the results
w5 <- WFO.match(r1, WFO.file=WFO.file.RK, Genus="New.Genus", Species="New.Species",
          exclude.infraspecific=TRUE, counter=1)

# save results to file
# utils::write.table(w4, quote=F, sep="\t", row.names=F, append=FALSE)

# limit the fuzzy matches to those that contain a shortened version of a species name
w6 <- WFO.match("Acacia caes", WFO.file=WFO.file.RK, Fuzzy=0.01, Fuzzy.within=TRUE, verbose=TRUE)

# show all the matches for a genus
spec.test1 <- data.frame(Genus=c("Casimiroa"))
w8 <- WFO.match(spec.test1, WFO.file=WFO.file.RK, exclude.infraspecific=TRUE, verbose=TRUE)

# show all listings at a next hierarchical level
WFO.data1 <- data.table::fread(WFO.file.RK, encoding="UTF-8")

WFO.browse("Pinaceae", WFO.data=WFO.data1)
WFO.browse("Pinaceae", WFO.data=WFO.data1, accepted.only=T)

WFO.browse("Tsuga", WFO.data=WFO.data1)
WFO.browse("Tsuga", WFO.data=WFO.data1, accepted.only=T)

WFO.browse("Olea europaea", WFO.data=WFO.data1)
WFO.browse("Olea europaea", WFO.data=WFO.data1, accepted.only=T)

# browsing only works at family, genus and species levels
# for orders, however, information is given from vascular.families
WFO.browse("Polypodiales", WFO.data=WFO.data1)

# submitting no name results in a list of all families
WFO.browse(, WFO.data=WFO.data1)

# give synonyms
WFO.synonyms("Olea europaea", WFO.data=WFO.data1)

# give order and other higher levels from family
WFO.family("Olea europaea", WFO.data=WFO.data1)
}
}




