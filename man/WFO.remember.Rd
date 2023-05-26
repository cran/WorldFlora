\name{WFO.remember}
\alias{WFO.remember}
\alias{WFO.download}

\title{Remember the location of the Taxonomic Backbone data set}

\description{
The function remembers where the Taxonomic Backbone data was downloaded to. In case that no arguments are specified, then data.frame \code{WFO.data} will contain the previously specified Taxonomic Backbone data.
}

\usage{
    WFO.download(WFO.url =
      paste0("https://files.worldfloraonline.org/files/WFO_Backbone/",
             "_WFOCompleteBackbone/WFO_Backbone.zip"),
    save.dir = getwd(), WFO.remember = TRUE,
    timeout = 500, ...)

    WFO.remember(WFO.file = NULL, WFO.data = "WFO.data", WFO.pos = 1)
}

\arguments{

    \item{WFO.url}{ Hyperlink to the download from the World Flora Online. }
    \item{save.dir}{ Directory where the file will be downloaded and unzipped.}
    \item{WFO.remember}{ Remember the location of the file for \code{WFO.remember}. }
    \item{timeout}{ Timeout in seconds for some internet operations, to be modified among Options Settings. }
    \item{...}{ Other arguments for {\link[utils]{download.file}}. }

    \item{WFO.file}{ File path to the Taxonomic Backbone data ('classification.txt'). }
    \item{WFO.data}{ Name of data set to be used by other WorldFlora functions.}
    \item{WFO.pos}{ Argument pos as in {\link[base]{assign}}. }

}

\details{
These functions avoid that a user needs to reload and re-specify the location of the Taxonomic Backbone data that was previously downloaded from the World Flora Online website. The location is saved in a text file in the 'etc' directory of the WorldFlora directory.
}

\value{
The function remembers the local location of the Taxonomic Backbone data.
}

\author{Roeland Kindt (World Agroforestry)}

\examples{

\dontrun{

# change the working directory
setwd(choose.dir())

# download the Taxonomic Backbone data
WFO.download()

# remember the previous download and avail the data as 'WFO.data'
WFO.remember()

# check
WFO.match("Faidherbia albida", WFO.data=WFO.data)
}

}




