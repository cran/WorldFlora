\name{WFO.prepare}
\alias{WFO.prepare}
\alias{WFO.preprepare}

\title{Prepare a data set for analysis with WFO.match}

\description{
The main function of \code{WFO.prepare} attempts to split a list of species names with naming authorities in different fields of botanical names and authorities.
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
    authors.ending.f=c("Aiton f.", "Baker f.", "Bak. f.", "Burm. f.", 
                       "Cheng f.", "Chrtek f.", 
                       "De Marco f.", "Fang f.", "Ferry f.", "Forsyth f.", 
                       "Forster f.", "Fraser f.", "G.Don f.", "Haller f.",
                       "Hallier f.", "Hook. f.", "Hooker f.", "Hsueh f.", 
                       "J.Kickx f.", "J. Kickx f.", "Keng f.", 
                       "Kickx f.", "Klokov f.", "Koster f.",
                       "Liou f.", "L. f.", "Ma f.", "Mikan f.",
                       "Occhioni f.", "Rchb. f.", 
                       "Schultes f.", "Schult. f.", "Stapf f."),
    verbose = TRUE, counter = 1000)
    
WFO.preprepare(spec.data=NULL, spec.full="spec.full",
    trinomial.first="subsp.", trinomial.second="var.")     
    
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
  \item{authors.ending.f}{ Author names that end with ' f.', not confuse the function about trinomials with 'f.', indicating 'filius' ('son of'). }  
  \item{verbose}{ Give details on the process. }  
  \item{counter}{ Progress on the process is reported by multiples of this counter. }

  \item{trinomial.first}{ Pattern to split species name in different columns. }
  \item{trinomial.second}{ Second pattern to split species name in different columns. }
}

\details{
Function \code{WFO.prepare} splits submitted names into the botanical name ('spec.name') and the naming authority ('Authorship'). When the submitted name contains section between brackets that are not at the beginning of the naming authority, these sections will be removed. Function \code{WFO.preprepare} was designed to deal with situations where author names are given at species and infra-specific levels (see examples).
}

\value{
The function splits names in the botanical name and the naming authority.
}

\author{Roeland Kindt (World Agroforestry)}

\examples{

\dontrun{
WFO.prepare("Terminalia superba Engl. & Diels (**) (In review)")
WFO.prepare("Sorbus aucuparia subsp. praemorsa (Guss.) Nyman")
WFO.prepare("Ormosia aff. coarctata Jackson")
WFO.prepare("Ormosia aff coarctata Jackson")
WFO.prepare("Ormosia /coarctata Jackson")
WFO.prepare("Qualea TMG 148 Aubl.")
# Note that the sub.pattern is ' cf.'
WFO.prepare("cf Myrcia M1")
# Dealing with author names that end with ' f.' ('filius')
WFO.prepare("Malveopsis scabrosum Stapf f.")

# Using preprepare to deal with authorities at multiple levels
WFO.preprepare("Agave deserti Engelm. subsp. simplex Gentry")
WFO.preprepare("Zoysia matrella (L.) Merr. var. pacifica Goudsw.")

test.name <- paste0("Agastache pallidiflora (A. Heller) Rydb.",
  " subsp. neomexicana (Briq.) Lint & Epling",
  " var. havardii (A. Gray) R.W. Sanders")
WFO.preprepare(test.name)
}

}

