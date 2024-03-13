WFO.synonyms <- function(
    taxon=NULL, WFO.file=NULL, WFO.data=NULL, ...
)
{
    if (is.null(WFO.data) == TRUE) {
        message(paste("Reading WFO data"))
        WFO.data <- data.table::fread(WFO.file, encoding="UTF-8")
    }

# if nothing provided, then give list of all the families

    WFO.found <- WFO.one(WFO.match(taxon, WFO.file=NULL, WFO.data=WFO.data, ...))
    if (nrow(WFO.found) == 0) {
      message("no matches found")
      return(NULL)
    }
    if (WFO.found$taxonID == "") {
      message("no matches found")
      return(NULL)
    }

    WFO.found <- WFO.found[, c("taxonID", "scientificName", "scientificNameAuthorship", "taxonRank", "taxonomicStatus", "acceptedNameUsageID")]  

    ID.found <- WFO.found$taxonID

    browse.found <- WFO.data[WFO.data$acceptedNameUsageID==ID.found, ]
    browse.found <- browse.found[, c("taxonID", "scientificName", "scientificNameAuthorship", "taxonRank", "taxonomicStatus", "acceptedNameUsageID")]
    browse.found <- browse.found[order(browse.found$scientificName), ]

    result <- rbind(WFO.found, browse.found)

    return(result)
}




