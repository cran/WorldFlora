\name{WFO.prepare}
\alias{WFO.prepare}

\title{Prepare a data set for analysis with WFO.match}

\description{
The function attempts to split a list of species names with naming authorities in different fields of botanical names and authorities.
}

\usage{
    WFO.prepare(spec.data = NULL, spec.full="spec.full",
    squish=TRUE, 
    trinomial=c("cultivar.", "f.", "sect.", "subf.", "subg.", "subsp.", "subvar.", "var.",
        "CULTIVAR.",  "SECT.", "SUBF.", "SUBG.", "SUBSP.", "SUBVAR.", "VAR."),
    verbose=TRUE, counter=1000)
}

\arguments{
  \item{spec.data}{ A data.frame containing variables with species names. In case that a character vector is provided, then this vector will be converted to a data.frame }
  \item{spec.full}{ Name of the column with full taxonomic names.}
  \item{squish}{ If \code{TRUE}, remove repeated whitespace and white space from the start and end of the submitted full name via {\link[stringr]{str_squish}}. }
  \item{trinomial}{ Descriptors for trinomial names. In case a trinomial name is expected, the species name will be obtained from the first two words and the two words starting with the trinomial descriptor. }
  \item{verbose}{ Give details on the process. }  
  \item{counter}{ Progress on the process is reported by multiples of this counter. }
}

\details{
The function splits submitted names into the botanical name ('spec.name') and the naming authority ('Authorship'). When the submitted name contains section between brackets that are not at the beginning of the naming authority, these sections will be removed.
}

\value{
The function splits names in the botanical name and the naming authority.
}

\author{Roeland Kindt (World Agroforestry)}

\examples{

WFO.prepare("Terminalia superba Engl. & Diels (**) (In review)")
WFO.prepare("Sorbus aucuparia subsp. praemorsa (Guss.) Nyman")

}




