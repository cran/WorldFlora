WFO.match <- function(
    spec.data=NULL, WFO.file=NULL, WFO.data=NULL,
    spec.name="spec.name", Genus="Genus", Species="Species", 
    Infraspecific.rank="Infraspecific.rank", Infraspecific="Infraspecific",
    acceptedNameUsageID.match=TRUE,
    Fuzzy=0.001, Fuzzy.shortest=TRUE, Fuzzy.within=FALSE,
    verbose=FALSE, counter=1000
)
{
    if (class(spec.data) == "character") {spec.data <- data.frame(spec.name = spec.data)}
    if (is.factor(spec.data) == TRUE) {spec.data <- data.frame(spec.name = spec.data)}

    if (! requireNamespace("data.table")) {stop("Please install the data.table package")}

    if (is.null(WFO.data) == TRUE) {
        message(paste("Reading WFO data"))
        WFO.data <- data.table::fread(WFO.file, encoding="UTF-8")
    }

    spec.data$Matched <- spec.data$Unique <- rep(as.logical(1), nrow(spec.data))
    spec.data$Fuzzy <- rep(as.logical(0), nrow(spec.data))
    spec.data$Subseq <- rep(1, nrow(spec.data))
    init.column <- ncol(spec.data)

    if (spec.name %in% names(spec.data)) {spec.data[, spec.name] <- as.character(spec.data[, spec.name])}
    if (Genus %in% names(spec.data)) {spec.data[, Genus] <- as.character(spec.data[, Genus])}
    if (Species %in% names(spec.data)) {spec.data[, Species] <- as.character(spec.data[, Species])}
    if (Infraspecific.rank %in% names(spec.data)) {spec.data[, Infraspecific.rank] <- as.character(spec.data[, Infraspecific.rank])}
    if (Infraspecific %in% names(spec.data)) {spec.data[, Infraspecific] <- as.character(spec.data[, Infraspecific])}

    if (Infraspecific.rank %in% names(spec.data)) {
        for (i in 1:nrow(spec.data)) {
            if (is.na(spec.data[i, Infraspecific.rank]) == TRUE) {spec.data[i, Infraspecific.rank] <- ""}
        }
    }

    if (Infraspecific %in% names(spec.data)) {
        for (i in 1:nrow(spec.data)) {
            if (is.na(spec.data[i, Infraspecific]) == TRUE) {spec.data[i, Infraspecific.rank] <- ""}
        }
    }

    WFO.empty <- WFO.data[1, ]
    for (i in 1:ncol(WFO.empty)) {WFO.empty[, i] <- ""}
#
    for (i in 1:nrow(spec.data)) {

        if (round(i/counter, 0) == i/counter) {message(paste("Reached record # ", i, sep=""))}

        if (spec.name %in% names(spec.data)) {
            WFO.match <- WFO.data[WFO.data$scientificName==spec.data[i, spec.name],]        
            if (nrow(WFO.match) == 0 && Fuzzy > 0) {
                specFuzzy <- agrep(spec.data[i, spec.name], x=WFO.data$scientificName, value=T, max.distance=Fuzzy)
                if (length(specFuzzy) > 0) {
                    spec.data[i, "Fuzzy"] <- as.logical(1)
                    specFuzzy <- unique(specFuzzy)
                    if (verbose == TRUE) {message(paste("Fuzzy matches for ", spec.data[i, spec.name], "were: ", paste(specFuzzy, collapse=", ")))}
                    if (Fuzzy.within == T) {
                        Fuzzy.shortest <- as.logical(0)
                        within.matches <- grepl(spec.data[i, spec.name], x=specFuzzy)
                        specFuzzy <- specFuzzy[within.matches]
                        if (verbose == T) {
                            if (length(specFuzzy) >  0) {
                                message(paste("Matches within for ", spec.data[i, spec.name], "were: ", paste(specFuzzy, collapse=", ")))
                            }else{
                                message(paste("No matches within for ", spec.data[i, spec.name]))
                            }
                        }
                    }
                    if (Fuzzy.shortest == TRUE) {
                        target.l <- nchar(spec.data[i, spec.name])
                        found.l <- nchar(specFuzzy)
                        found.diff <- abs(found.l - target.l)
                        specFuzzy <- specFuzzy[found.diff == min(found.diff)]
                           if (verbose == TRUE) {message(paste("Shortest fuzzy matches for ", spec.data[i, spec.name], "were: ", paste(specFuzzy, collapse=", ")))}

                    }
                    for (j in 1:length(specFuzzy)) {
                        WFO.match1 <- WFO.data[WFO.data$scientificName==specFuzzy[j],]
                        if (j==1) {
                            WFO.match <- WFO.match1
                        }else{
                            WFO.match <- rbind(WFO.match, WFO.match1)
                        }
                }
            }
        }
#
# Only match genus and species separately if specName was not given
        }else{
            if (Infraspecific.rank %in% names(spec.data)) {
                WFO.match <- WFO.data[WFO.data$genus==spec.data[i, Genus] & WFO.data$specificEpithet==spec.data[i, Species] 
                    & WFO.data$verbatimTaxonRank==spec.data[i, Infraspecific.rank] & WFO.data$infraspecificEpithet==spec.data[i, Infraspecific], ]
            }else if (Infraspecific %in% names(spec.data)) {
                WFO.match <- WFO.data[WFO.data$genus==spec.data[i, Genus] & WFO.data$specificEpithet==spec.data[i, Species] 
                    & WFO.data$infraspecificEpithet==spec.data[i, Infraspecific], ]
            }else{
                WFO.match <- WFO.data[WFO.data$genus==spec.data[i, Genus] & WFO.data$specificEpithet==spec.data[i, Species], ]
            }
        }
        if (nrow(WFO.match) > 1) {
            spec.data[i, "Unique"] <- as.logical(0)
            WFO.match2 <- cbind(spec.data[rep(i, nrow(WFO.match)), ], WFO.match)
            WFO.match2$Subseq <- c(1:nrow(WFO.match))
        }else if (nrow(WFO.match) == 1) { 
            WFO.match2 <- cbind(spec.data[i, ], WFO.match)
        }else{
            spec.data[i, "Matched"] <- as.logical(0)
            WFO.match2 <- cbind(spec.data[i, ], WFO.empty)
        }

        if (i==1) {
            WFO.out <- WFO.match2
        }else{
            WFO.out <- rbind(WFO.out, WFO.match2)
        }
    }
# check for hybrids
    WFO.out$Hybrid <- rep("", nrow(WFO.out))
    for (i in 1:nrow(WFO.out)) {
        if (grepl(intToUtf8(215), WFO.out[i, "scientificName"])) {WFO.out[i, "Hybrid"] <- intToUtf8(215)}
    }
    if (acceptedNameUsageID.match==T) {
        message(paste("\n", "Checking new accepted IDs"))
        right.columns <- c((init.column+1) : (ncol(WFO.out)-1))
        WFO.out$New.accepted <- rep(as.logical(0), nrow(WFO.out))
        WFO.out$Old.status <- rep("", nrow(WFO.out))
        WFO.out$Old.ID <- rep("", nrow(WFO.out))
        WFO.out$Old.name <- rep("", nrow(WFO.out))
        for (i in 1:nrow(WFO.out)) {
            if (round(i/counter, 0) == i/counter) {message(paste("Reached record # ", i, sep=""))}
#
            if (WFO.out[i, "acceptedNameUsageID"] != "") {
                WFO.match <- WFO.data[WFO.data$taxonID==WFO.out[i, "acceptedNameUsageID"], ]
                if (nrow(WFO.match) == 0) {
                    warning(paste("WARNING: no data for ", WFO.out[i, "acceptedNameUsageID"], " from ", WFO.out[i, "scientificName"]))
                }else if (nrow(WFO.match) > 1) {
                    warning(paste("WARNING: more than 1 row of matches for ", WFO.out[i, "acceptedNameUsageID"], " from ", WFO.out[i, "scientificName"]))
                }else{
                    WFO.out[i, "New.accepted"] <- as.logical(1)
                    WFO.out[i, "Old.status"] <- WFO.out[i, "taxonomicStatus"]
                    WFO.out[i, "Old.ID"] <- WFO.out[i, "taxonID"]
                    WFO.out[i, "Old.name"] <- WFO.out[i, "scientificName"]
                    WFO.out[i, right.columns] <- WFO.match
                    if (grepl(intToUtf8(215), WFO.out[i, "scientificName"])) {
                        WFO.out[i, "Hybrid"] <- intToUtf8(215)
                    }else{
                        WFO.out[i, "Hybrid"] <- ""
                    }
                }
            }
        }
    }
#    if (is.null(filename) == FALSE) {utils::write.table(WFO.out, file=filename, quote=F, sep="\t", row.names=F, append=append)}
    return(WFO.out)
}



