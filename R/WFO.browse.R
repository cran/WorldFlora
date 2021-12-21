WFO.browse <- function(
    taxon=NULL, WFO.file=NULL, WFO.data=NULL,
        accepted.only = FALSE, acceptedNameUsageID.match = TRUE, ...
)
{
    if (is.null(WFO.data) == TRUE) {
        message(paste("Reading WFO data"))
        WFO.data <- data.table::fread(WFO.file, encoding="UTF-8")
    }

# if nothing provided, then give list of all the families

    rank.found1 <- as.logical(0)

    if (is.null(taxon) == TRUE) {
        rank.found <- "all"
        rank.found1 <- as.logical(1)
# changed for World Flora Online DEC 2021 release
        right.level <- (WFO.data$taxonRank == "family" |
                            WFO.data$taxonRank == "Family" |
                            WFO.data$taxonRank == "FAMILY")
        result <- WFO.data[right.level, ]
        result <- result[, c("taxonID", "scientificName", "scientificNameAuthorship", "taxonRank", "taxonomicStatus", "acceptedNameUsageID")]
        result <- result[order(result$scientificName), ]
        cat(paste("Results are a list of all families", "\n", sep=""))

    }else{
        WFO.found <- WFO.one(WFO.match(taxon, WFO.file=NULL, WFO.data=WFO.data, ...))
        if (nrow(WFO.found) == 0) {stop("no matches found")}

        taxon.found <- WFO.found$scientificName
        rank.found <- WFO.found$taxonRank

        cat(paste("Submitted name ", taxon, " was matched with: ", taxon.found, " of taxonRank: ", rank.found, "\n", sep=""))
    }


    if (rank.found %in% c("species", "Species", "SPECIES", "nothospecies")) {
        rank.found1 <- as.logical(1)
        browse.found <- WFO.data[WFO.data$genus==WFO.found$genus & WFO.data$specificEpithet==WFO.found$specificEpithet,]
        right.level <- browse.found$verbatimTaxonRank != ""
        browse.found1 <- browse.found[right.level, ]
        browse.found <- browse.found1[order(browse.found1$scientificName), ]
        result <- browse.found[, c("taxonID", "scientificName", "scientificNameAuthorship", "taxonRank", "taxonomicStatus", "acceptedNameUsageID")]
    }

# changed for World Flora Online DEC 2021 release
    if (rank.found %in% c("genus", "Genus", "GENUS")) {
        rank.found1 <- as.logical(1)
        browse.found <- WFO.data[WFO.data$genus==taxon.found,]
        right.level <- (browse.found$taxonRank == "SPECIES" | browse.found$taxonRank == "Species" | browse.found$taxonRank == "species" | browse.found$taxonRank == "nothospecies")
        browse.found1 <- browse.found[right.level, ]
        browse.found <- browse.found1[order(browse.found1$scientificName), ]
        result <- browse.found[, c("taxonID", "scientificName", "scientificNameAuthorship", "taxonRank", "taxonomicStatus", "acceptedNameUsageID")]
    }

# changed for World Flora Online DEC 2021 release
    if (rank.found %in% c("family", "Family", "FAMILY")) {
        rank.found1 <- as.logical(1)
        browse.found <- WFO.data[WFO.data$family==taxon.found,]
# changed for World Flora Online DEC 2021 release
        right.level <- (browse.found$taxonRank == "genus" |
                            browse.found$taxonRank == "Genus" |
                            browse.found$taxonRank == "GENUS")
        browse.found1 <- browse.found[right.level, ]
        browse.found <- browse.found1[order(browse.found1$scientificName), ]
        result <- browse.found[, c("taxonID", "scientificName", "scientificNameAuthorship", "taxonRank", "taxonomicStatus", "acceptedNameUsageID")]
    }

# changed for World Flora Online DEC 2021 release
    if (rank.found %in% c("order", "ORDER")) {
        .WorldFlora <- new.env()
        utils::data("vascular.families", package="WorldFlora", envir=.WorldFlora)
        WFO.families <- eval(as.name("vascular.families"), envir=.WorldFlora)
        order.match <- WFO.families[WFO.families$Order == taxon.found, ]
        return(order.match)
    }

    if (rank.found1 == FALSE) {stop("Listings are only provided for families, genera and species")}

    if (nrow(result) == 0) {
        message("No taxa at the next level")
        return(NULL)
    }else{
        if (accepted.only == TRUE) {
# changed for World Flora Online DEC 2021 release
            result <- result[result$taxonomicStatus %in% c("Accepted", "ACCEPTED"), ]
        }else{
            if (acceptedNameUsageID.match == TRUE) {
                result$New.name <- rep("", nrow(result))
                for (i in 1:nrow(result)) {
                    UsageID <- as.character(result[i, "acceptedNameUsageID"])
                    if (UsageID != "") {
                        WFO.match1 <- WFO.data[WFO.data$taxonID == UsageID, ]
                        if (nrow(WFO.match1) == 0) {
                            warning(paste("WARNING: no data for ", UsageID, " from ", result[i, "scientificName"]))
                        }else if (nrow(WFO.match1) > 1) {
                            warning(paste("WARNING: more than 1 row of matches for ", UsageID, " from ", result[i, "scientificName"]))
                        }else{
                            result[i, "New.name"] <- WFO.match1[1, "scientificName"]
                        }
                    }
                }
            }
        }
    }

    return(result)

}

