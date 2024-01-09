\name{WFO.match.fuzzyjoin}
\alias{WFO.match.fuzzyjoin}

\title{Standardize plant names according to World Flora Online taxonomic backbone}

\description{
An alternative and typically faster method of matching records than {\link[WorldFlora]{WFO.match}} that allows for different methods of calculating the fuzzy distance via {\link[stringdist]{stringdist}}.
}

\usage{
    WFO.match.fuzzyjoin(spec.data = NULL, WFO.file = NULL, WFO.data = NULL,
        no.dates = TRUE,
        spec.name = "spec.name",
        Authorship = "Authorship",
        stringdist.method = "lv", fuzzydist.max = 4,
        Fuzzy.min = TRUE,
        acceptedNameUsageID.match = TRUE,
        squish = TRUE,
        spec.name.tolower = FALSE, spec.name.nonumber = TRUE, spec.name.nobrackets = TRUE,
        spec.name.sub = TRUE,
        sub.pattern=c(" sp[.] A", " sp[.] B", " sp[.] C", " sp[.]", " spp[.]", " pl[.]",
            " indet[.]", " ind[.]", " gen[.]", " g[.]", " fam[.]", " nov[.]", " prox[.]",
            " cf[.]", " aff[.]", " s[.]s[.]", " s[.]l[.]",
            " p[.]p[.]", " p[.] p[.]", "[?]", " inc[.]", " stet[.]", "Ca[.]",
            "nom[.] cons[.]", "nom[.] dub[.]", " nom[.] err[.]", " nom[.] illeg[.]",
            " nom[.] inval[.]", " nom[.] nov[.]", " nom[.] nud[.]", " nom[.] obl[.]",
            " nom[.] prot[.]", " nom[.] rej[.]", " nom[.] supp[.]", " sensu auct[.]"))

}

\arguments{
  \item{spec.data}{ A data.frame containing variables with species names. In case that a character vector is provided, then this vector will be converted to a data.frame }
  \item{WFO.file}{ File name of the static copy of the Taxonomic Backbone. If not \code{NULL}, then data will be reloaded from this file.}
  \item{WFO.data}{ Data set with the static copy of the Taxonomic Backbone. Ignored if \code{WFO.file} is not \code{NULL}. }
  \item{no.dates}{ Speeding up the loading of the WFO.data by not loading fields of 'created' and 'modified'. }
  \item{spec.name}{ Name of the column with taxonomic names. }
  \item{Authorship}{ Name of the column with the naming authorities. }
  \item{stringdist.method}{ Method used to calculate the fuzzy distance as used by in the internally called \code{\link[stringdist]{stringdist}}.   }
  \item{fuzzydist.max}{ Maximum distance used for joining as in \code{\link[fuzzyjoin]{stringdist_join}}.  }
   \item{Fuzzy.min}{ Limit the results of fuzzy matching to those with the smallest distance. }
  \item{acceptedNameUsageID.match}{ If \code{TRUE}, obtain the accepted name and others details from the earlier acceptedNameUsageID. }
  \item{squish}{ If \code{TRUE}, remove repeated whitespace and white space from the start and end of the submitted full name via {\link[stringr]{str_squish}}. }
  \item{spec.name.tolower}{ If \code{TRUE}, then convert all characters of the \code{spec.name} to lower case via {\link[base]{tolower}}. }
  \item{spec.name.nonumber}{ If \code{TRUE}, then submitted \code{spec.name} that contain numbers will be interpreted as genera, only matching the first word. }
  \item{spec.name.nobrackets}{ If \code{TRUE}, then submitted \code{spec.name} then sections of the submitted name after '(' will be removed. Note that this will also remove sections after ')', such as authorities for plant names that are in a separate column of WFO.  }
  \item{spec.name.sub}{ If \code{TRUE}, then delete sections of the \code{spec.name} that match the \code{sub.pattern}. }
  \item{sub.pattern}{ Sections of the \code{spec.name} to be deleted }

}

\details{
This function matches plant names by using the \code{\link[fuzzyjoin]{stringdist_left_join}} function internally. The results are provided in a similar formatto those from \code{\link[WorldFlora]{WFO.match}}; therefore the \code{\link[WorldFlora]{WFO.one}} function can be used in a next step of the analysis.

For large data sets the function may fail due to memory limits. A solution is to analyse
different subsets of large data, as for example shown by Kindt (2023).

Column 'Unique' shows whether there was a unique match (or not match) in the WFO.

Column 'Matched' shows whether there was a match in the WFO.

Column 'Fuzzy' shows whether matching was done by the fuzzy method.

Column 'Fuzzy.dist' gives the fuzzy distance calculated between submitted and matched plant names, calculated internally with {\link[fuzzyjoin]{stringdist_left_join}}.

Column 'Auth.dist' gives the Levenshtein distance calculated between submitted and matched authorship names, if the former were provided. This distance is calculated in the same way as for the {\link[WorldFlora]{WFO.match}} function via {\link[utils]{adist}}.

Column 'Subseq' gives different numbers for different matches for the same plant name.

Column 'Hybrid' shows whether there was a hybrid character in the scientificName.

Column 'New.accepted' shows whether the species details correspond to the current accepted name.

Column 'Old.status' gives the taxonomic status of the first match with the non-blank acceptedNameUsageID.

Column 'Old.ID' gives the ID of the first match with the non-blank acceptedNameUsageID.

Column 'Old.name' gives the name of the first match with the non-blank acceptedNameUsageID.

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

Kindt, R. 2023. Standardizing tree species names of GlobalTreeSearch with WorldFlora while
  testing the faster matching function of WFO.match.fuzzyjoin.
   \url{https://rpubs.com/Roeland-KINDT/996500}

}

\author{Roeland Kindt (World Agroforestry, CIFOR-ICRAF)}

\seealso{
\code{\link{WFO.match}}
}

\examples{

\dontrun{
data(WFO.example)

library(fuzzyjoin)

spec.test <- data.frame(spec.name=c("Faidherbia albida", "Acacia albida",
    "Faidherbia albiad",
    "Omalanthus populneus", "Pygeum afric"))

WFO.match.fuzzyjoin(spec.data=spec.test, WFO.data=WFO.example)

# Using the Damerau-Levenshtein distance
WFO.match.fuzzyjoin(spec.data=spec.test, WFO.data=WFO.example,
    stringdist.method="dl")
}
}




