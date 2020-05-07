\name{WFO.prepare}
\alias{WFO.prepare}

\title{Prepare a data set for analysis with WFO.match}

\description{
The function attempts to split a list of species names with naming authorities in different fields of botanical names and authorities.
}

\usage{
    WFO.prepare(spec.data = NULL, spec.full="spec.full",
    squish = TRUE, spec.name.nonumber = TRUE,
        spec.name.sub = TRUE, 
        sub.pattern = c(" sp[.] A", " sp[.] B", " sp[.] C", " sp[.]", " spp[.]", " pl[.]",
            " indet[.]", " ind[.]", " gen[.]", " g[.]", " fam[.]", " nov[.]", " prox[.]", 
            " cf[.]", " aff[.]", " s[.]s[.]", " s[.]l[.]", 
            " p[.]p[.]", " p[.] p[.]", "[?]", " inc[.]", " stet[.]", "Ca[.]", 
            "nom[.] cons[.]", "nom[.] dub[.]", " nom[.] err[.]", " nom[.] illeg[.]", 
            " nom[.] inval[.]", " nom[.] nov[.]", " nom[.] nud[.]", " nom[.] obl[.]", 
            " nom[.] prot[.]", " nom[.] rej[.]", " nom[.] supp[.]", " sensu auct[.]"),  
    genus.2.flag = TRUE, species.2.flag = TRUE, 
    punctuation.flag = TRUE, pointless.flag = TRUE,
    trinomial = c("cultivar.", "f.", "sect.", "subf.", "subg.", 
        "subsp.", "subvar.", "var.",
        "CULTIVAR.",  "SECT.", "SUBF.", "SUBG.", "SUBSP.", "SUBVAR.", "VAR."),
    verbose = TRUE, counter = 1000)
}

\arguments{
  \item{spec.data}{ A data.frame containing variables with species names. In case that a character vector is provided, then this vector will be converted to a data.frame }
  \item{spec.full}{ Name of the column with full taxonomic names.}
  \item{squish}{ If \code{TRUE}, remove repeated whitespace and white space from the start and end of the submitted full name via {\link[stringr]{str_squish}}. }
  \item{spec.name.nonumber}{ If \code{TRUE}, then submitted \code{spec.full} that contain numbers will be interpreted as genera, only matching the first word. }
  \item{spec.name.sub}{ If \code{TRUE}, then delete sections of the \code{spec.full} that match the \code{sub.pattern}. }
  \item{sub.pattern}{ Sections of the \code{spec.full} to be deleted }
  \item{genus.2.flag}{ Flag first part of the names with only 2 characters. }  
  \item{species.2.flag}{ Flag second part of the names with only 2 characters. }  
  \item{punctuation.flag}{ Flag if the retained plant name has punctuation characters. }  
  \item{pointless.flag}{ Flag if the retained plant name has sub.pattern without the point. }  
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
WFO.prepare("Ormosia aff. coarctata Jackson")
WFO.prepare("Ormosia aff coarctata Jackson")
WFO.prepare("Ormosia /coarctata Jackson")
WFO.prepare("Qualea TMG 148 Aubl.")
# Note that the sub.pattern is ' cf.'
WFO.prepare("cf Myrcia M1")

}

