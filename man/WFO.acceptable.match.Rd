\name{WFO.acceptable.match}
\alias{WFO.acceptable.match}

\title{Check for fuzzy matches that can be acceptable based on gender notations}

\description{
The function checks whether submitted and match names only differ by ending by -um, -us or -a. An extra check is done to accept differences that result from having 'ii' instead of 'i' in the submitted and matched name. An optional check ignores differences in vowels.
}

\usage{

    WFO.acceptable.match(x, spec.name="spec.name",
      no.vowels=FALSE)

}

\arguments{

    \item{x}{ Output for \code{WFO.match}, \code{WFO.match.fuzzyjoin} or \code{WFO.match.one}. }
    \item{spec.name}{ Name of taxon submitted for matching.}
    \item{no.vowels}{ Accept results if only vowels differ between submitted and matched name. }

}

\details{
The function was initially developed to check for changes in gender notations.

In new versions, also the following differences in species names are judged to be acceptable:

- hybrid and non-hybrid names (eg, Sorbus avonensis - Sorbus xavonensis)

- i vs. j (eg, Syzygium naiadum - Syzygium najadum)

- tt vs. t (eg, Ficus scott-elliottii - Ficus scott-elliotii)

- ll vs. l (eg, Garcinia moseleyana - Garcinia moselleyana)

- rr vs. r (eg, Hymenodictyon perrieri - Hymenodictyon perieri)

- mm vs. m (eg, Monteverdia schummaniana - Monteverdia schumanniana)

- nn vs. n (eg, Pyrus tamamschiannae - Pyrus tamamschianae)

- ff vs. f (eg, Dendropanax langsdorfii - Dendropanax langsdorffii)

- hh vs. h (eg, Gmelina leichardtii - Gmelina leichhardtii)

- dd vs. d (eg, Miconia buddlejoides - Miconia budlejoides)

- is vs. e (eg, Decarydendron ranomafanensis - Decarydendron ranomafanense)

- dt vs. d (eg, Stadtmannia acuminata - Stadmania acuminata)

}

\value{
The function returns a logical vector that indicates whether names could be acceptable.
}

\author{Roeland Kindt (World Agroforestry)}

\examples{

\dontrun{

data(WFO.example)

spec.test <- data.frame(spec.name=c("Faidherbia albida", "Acacia albida",
  "Faidherbia albidum", "Faidherbia albidus",
  "Faidherbia albiida",
  "Prunus africanus", "Prunos africanea",
  "Prunus afrocaneus", "Prunus afrocaneos"))

match1 <- WFO.match.fuzzyjoin(spec.data=spec.test, WFO.data=WFO.example,
  fuzzydist.max = 6)
match1[, c("spec.name", "scientificName")]

# check for gender differences (and ii - i)
WFO.acceptable.match(match1)

# ignore differences in vowels
WFO.acceptable.match(match1, no.vowels=TRUE)

accepted.cases <- WFO.acceptable.match(match1, no.vowels=TRUE)
match1.accepted <- match1[accepted.cases == TRUE, ]
match1.notaccepted <- match1[accepted.cases == FALSE, ]
}

}




