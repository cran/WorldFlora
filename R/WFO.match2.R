WFO.match.fuzzyjoin <- function(
    spec.data=NULL, WFO.file=NULL, WFO.data=NULL,
    no.dates=TRUE,
    spec.name="spec.name",
    Authorship="Authorship",
    stringdist.method="lv", fuzzydist.max=4,
    Fuzzy.min=TRUE,
    acceptedNameUsageID.match=TRUE,
    squish=TRUE,
    spec.name.tolower=FALSE, spec.name.nonumber=TRUE, spec.name.nobrackets=TRUE,
    spec.name.sub=TRUE,
    sub.pattern=c(" sp[.] A", " sp[.] B", " sp[.] C", " sp[.]", " spp[.]", " pl[.]", " indet[.]", " ind[.]", " gen[.]", " g[.]", " fam[.]",
        " nov[.]", " prox[.]", " cf[.]", " aff[.]", " s[.]s[.]", " s[.]l[.]", " p[.]p[.]", " p[.] p[.]", "[?]", " inc[.]", " stet[.]",
        "Ca[.]", "nom[.] cons[.]", "nom[.] dub[.]", " nom[.] err[.]", " nom[.] illeg[.]", " nom[.] inval[.]", " nom[.] nov[.]",
        " nom[.] nud[.]", " nom[.] obl[.]", " nom[.] prot[.]", " nom[.] rej[.]", " nom[.] supp[.]", " sensu auct[.]")
)
{
# do NOT allow data.table format, DEC 2022
    if (("data.table" %in% class(spec.data)) == TRUE) {spec.data <- data.frame(spec.data)}
    if (("data.frame" %in% class(spec.data)) == FALSE) {spec.data <- data.frame(spec.name = spec.data)}
    if (is.factor(spec.data) == TRUE) {spec.data <- data.frame(spec.name = spec.data)}

    if (! requireNamespace("data.table")) {stop("Please install the data.table package")}

    if (is.null(WFO.data) == TRUE) {
        message(paste("Reading WFO data"))
        WFO.data <- data.table::fread(WFO.file, encoding="UTF-8")
    }else{
        WFO.data <- data.table::data.table(WFO.data)
    }

    if ("created" %in% names(WFO.data)) {data.table::set(WFO.data, j="created", value=NULL)}
    if ("modified" %in% names(WFO.data)) {data.table::set(WFO.data, j="modified", value=NULL)}

    WFO.names <- names(WFO.data)
    WFO.names <- c(WFO.names, "Hybrid")
    if (Authorship %in% names(spec.data)) {WFO.names <- c(WFO.names, "Auth.dist")}
    if (acceptedNameUsageID.match == TRUE) {
        if (Authorship %in% names(spec.data)) {
            WFO.names <- c(WFO.names, "New.accepted", "Old.status", "Old.ID", "Old.name", "Old.author", "Old.author.dist")
        }else{
            WFO.names <- c(WFO.names, "New.accepted", "Old.status", "Old.ID", "Old.name")
        }
    }
    for (i in 1:length(WFO.names)) {
        if (WFO.names[i] %in% names(spec.data)) {
            message(paste("Original data set variable '", WFO.names[i], "' replaced by variable '", WFO.names[i], ".ORIG'", sep=""))
            names(spec.data)[names(spec.data) == WFO.names[i]] <- paste(WFO.names[i], ".ORIG", sep="")
        }
    }

# copying code from WFO.match, always work via spec.name
#
#    if (spec.name %in% names(spec.data)) {
        spec.data[, spec.name] <- as.character(spec.data[, spec.name])
        for (i in 1:nrow(spec.data)) {
            if (is.na(spec.data[i, spec.name]) == TRUE) {spec.data[i, spec.name] <- ""}
        }
        if (squish == TRUE) {
            spec.name.ORIG <- paste(spec.name, ".ORIG", sep="")
            spec.data[, spec.name.ORIG] <- spec.data[, spec.name]
            spec.data[, spec.name] <- stringr::str_squish(spec.data[, spec.name.ORIG])
            spec.data$Squished <- rep(as.logical(0), nrow(spec.data))
            for (i in 1:nrow(spec.data)) {
                if (nchar(spec.data[i, spec.name.ORIG]) > 0) {
                    if (spec.data[i, spec.name.ORIG] != spec.data[i, spec.name]) {spec.data[i, "Squished"] <- as.logical(1)}
                }
            }
        }
        if (spec.name.tolower == TRUE) {
            if (squish == FALSE) {
                spec.name.ORIG <- paste(spec.name, ".ORIG", sep="")
                spec.data[, spec.name.ORIG] <- spec.data[, spec.name]
            }
            spec.data[, spec.name] <- tolower(spec.data[, spec.name.ORIG])
            for (i in 1:nrow(spec.data)) {
                substr(spec.data[i, spec.name], start=1, stop=1) <- toupper(substr(spec.data[i, spec.name], start=1, stop=1))
            }
        }
        if (spec.name.sub == TRUE) {
            if (squish == FALSE && spec.name.tolower == FALSE) {
                spec.name.ORIG <- paste(spec.name, ".ORIG", sep="")
                spec.data[, spec.name.ORIG] <- spec.data[, spec.name]
            }
            for (i in 1:length(sub.pattern)) {
                spec.data[, spec.name] <- gsub(pattern=sub.pattern[i], replacement="", x=spec.data[, spec.name])
            }
        }
        if (spec.name.nobrackets == TRUE) {
            if (squish == FALSE && spec.name.tolower == FALSE && spec.name.sub == FALSE) {
                spec.name.ORIG <- paste(spec.name, ".ORIG", sep="")
                spec.data[, spec.name.ORIG] <- spec.data[, spec.name]
            }
            spec.data$Brackets.detected <- rep(as.logical(0), nrow(spec.data))
            for (i in 1:nrow(spec.data)) {
                species.string <- spec.data[i, spec.name]
                if (grepl(pattern="[(]", x=species.string) == TRUE) {
                    spec.data[i, "Brackets.detected"] <- as.logical(1)
                    brack.place <- as.numeric(unlist(gregexpr(pattern="[(]", text=species.string)))[1]
                    species.new.string <- substr(species.string, start=1, stop=brack.place-1)
                    spec.data[i, spec.name] <- stringr::str_squish(species.new.string)
                }
            }
        }
        if (spec.name.nonumber == TRUE) {
            if (squish == FALSE && spec.name.tolower == FALSE && spec.name.sub == FALSE && spec.name.nobrackets == FALSE) {
                spec.name.ORIG <- paste(spec.name, ".ORIG", sep="")
                spec.data[, spec.name.ORIG] <- spec.data[, spec.name]
            }
            spec.data$Number.detected <- rep(as.logical(0), nrow(spec.data))
            for (i in 1:nrow(spec.data)) {
                species.string <- spec.data[i, spec.name]
                if (grepl("[[:digit:]]", species.string) == TRUE) {
                    spec.data[i, "Number.detected"] <- as.logical(1)
                    species.terms <- unlist(strsplit(species.string, split= " "))
                    species.new.string <- species.terms[1]
                    while (grepl("^[[:digit:]]", substr(species.new.string, start=nchar(species.new.string), stop=nchar(species.new.string))) == TRUE){
                         species.new.string <- substr(species.new.string, start=1, stop=nchar(species.new.string)-1)
                    }
                    spec.data[i, spec.name] <- species.new.string
                }
            }
        }
        if (any(grepl(" x ", spec.data[, spec.name])) == TRUE) {
            message(paste("pattern ' x ' was interpreted as hybrid notation and replaced by ' ", intToUtf8(215), "'", sep=""))
            if (squish == FALSE && spec.name.tolower == FALSE && spec.name.sub == FALSE && spec.name.nonumber == FALSE && spec.name.nobrackets == FALSE) {
                spec.name.ORIG <- paste(spec.name, ".ORIG", sep="")
                spec.data[, spec.name.ORIG] <- spec.data[, spec.name]
            }
            for (i in 1:nrow(spec.data)) {
                species.string <- spec.data[i, spec.name]
                if (grepl(" x ", species.string) == TRUE) {
                    species.new.string <- gsub(pattern=" x ", replacement=paste(" ", intToUtf8(215), sep=""), x=species.string)
                    spec.data[i, spec.name] <- species.new.string
                }
            }
        }
 #   }

    spec.data$Matched <- spec.data$Unique <- rep(as.logical(1), nrow(spec.data))
    spec.data$Fuzzy <- rep(as.logical(0), nrow(spec.data))
    spec.data$Fuzzy.dist <- rep(NA, nrow(spec.data))

#    if (acceptedNameUsageID.match == TRUE) {spec.data$Auth.dist <- rep(Inf, nrow(spec.data))}
    if (Authorship %in% names(spec.data)) {spec.data$Auth.dist <- rep(Inf, nrow(spec.data))}

    spec.data$OriSeq <- c(1: nrow(spec.data))
    spec.data$Subseq <- rep(1, nrow(spec.data))
    init.column <- ncol(spec.data)

## avoid problems with dates (error reported by Lauri Vesa)
##    WFO.empty$created <- as.Date("1000-01-02")
##    WFO.empty$modified <- as.Date("1000-01-02")

# Modified January 2023 to work with smaller data sets when working with fuzzy matches

    WFO.dat <- WFO.data[, c("taxonID", "scientificName")]

# Modified December 2022 to use left_join first for direct matches
    spec.data$spec.link <- spec.data[, spec.name]

    WFO.full <- dplyr::left_join(spec.data,
                                 WFO.dat,
                                 by=c(spec.link ="scientificName"))
    remain.ind <- WFO.full[is.na(WFO.full$taxonID)==TRUE, "OriSeq"]
    WFO.full <- WFO.full[is.na(WFO.full$taxonID)==FALSE, ]

    if (nrow(WFO.full) > 0) {
      full.matches <- TRUE
      seq.unique <- unique(WFO.full$OriSeq)

      for (i in 1:length(seq.unique)) {
        un.r <- nrow(WFO.full[WFO.full$OriSeq == seq.unique[i], ])
        if (un.r > 1) {
          WFO.full[WFO.full$OriSeq == seq.unique[i], "Subseq"] <- c(1:un.r)
          WFO.full[WFO.full$OriSeq == seq.unique[i], "Unique"] <- as.logical(0)
        }
      }

      spec.data <- spec.data[spec.data$OriSeq %in% remain.ind, ]
      n.remain <- nrow(spec.data)

    }else{
      full.matches <- FALSE
      n.remain <- nrow(spec.data)
    }

# Modified verbose options in January 2023

    if (n.remain > 0) {

    message(paste("Checking for fuzzy matches for ", n.remain, " records", sep=""))

    WFO.out <- fuzzyjoin::stringdist_left_join(spec.data,
                                               WFO.dat,
                                               by=c(spec.link ="scientificName"),
                                               method=stringdist.method,
                                               max_dist=fuzzydist.max,
                                               distance_col="Fuzzy.calc")

    WFO.out$Fuzzy.dist <- WFO.out$Fuzzy.calc
    WFO.out$Fuzzy <- as.logical(1)
    WFO.out[is.na(WFO.out$taxonID)==TRUE, "Fuzzy"] <- as.logical(0)
    WFO.out[is.na(WFO.out$taxonID)==TRUE, "Matched"] <- as.logical(0)

    seq.unique <- unique(WFO.out$OriSeq)

    for (i in 1:length(seq.unique)) {
      un.r <- nrow(WFO.out[WFO.out$OriSeq == seq.unique[i], ])
      if (un.r > 1) {
        WFO.out[WFO.out$OriSeq == seq.unique[i], "Subseq"] <- c(1:un.r)
        WFO.out[WFO.out$OriSeq == seq.unique[i], "Unique"] <- as.logical(0)
      }
    }

    if (Fuzzy.min == TRUE) {
      join.cases <- unique(WFO.out$OriSeq)
      for (i in 1:length(seq.unique)) {
        WFO.out.i <- WFO.out[WFO.out$OriSeq == seq.unique[i], ]
        if (nrow(WFO.out.i) > 1) {
          WFO.out.i <- WFO.out.i[which(WFO.out.i$Fuzzy.dist == min(WFO.out.i$Fuzzy.dist)), ]
          if (nrow(WFO.out.i) == 1) {WFO.out.i$Unique <- as.logical(1)}
        }
        if (i==1) {
          WFO.out2 <- WFO.out.i
        }else{
          WFO.out2 <- rbind(WFO.out2, WFO.out.i)
        }
      }
      WFO.out <- WFO.out2
    }

    WFO.out <- WFO.out[, !names(WFO.out) %in% c("scientificName", "Fuzzy.calc")]

    if (full.matches == TRUE) {
      WFO.out <- rbind(WFO.full, WFO.out)
      WFO.out <- WFO.out[order(WFO.out$OriSeq, WFO.out$Subseq), ]
    }

    }else{ # no remaining records
      WFO.out <- WFO.full
    }

    WFO.out <- WFO.out[, !names(WFO.out) %in% c("spec.link")]

# get full WFO data after the loop, January 2023
    WFO.out <- dplyr::left_join(WFO.out,
                                WFO.data,
                                by="taxonID")
#
# check for hybrids
    WFO.out$Hybrid <- rep("", nrow(WFO.out))
    for (i in 1:nrow(WFO.out)) {
        if (grepl(intToUtf8(215), WFO.out[i, "scientificName"])) {WFO.out[i, "Hybrid"] <- intToUtf8(215)}
    }

# find new accepted data

    if (acceptedNameUsageID.match == TRUE) {
        message(paste0("\n", "Checking new accepted IDs"))
        right.columns <- c((init.column+1) : (ncol(WFO.out)-1))
        WFO.out$New.accepted <- rep(as.logical(0), nrow(WFO.out))
        WFO.out$Old.status <- rep("", nrow(WFO.out))
        WFO.out$Old.ID <- rep("", nrow(WFO.out))
        WFO.out$Old.name <- rep("", nrow(WFO.out))
        if (Authorship %in% names(spec.data)) {
            WFO.out$Old.author <- rep("", nrow(WFO.out))
            WFO.out$Old.author.dist <- rep("", nrow(WFO.out))
        }
        for (i in 1:nrow(WFO.out)) {
#            if (round(i/counter, 0) == i/counter) {message(paste("Reached record # ", i, sep=""))}
#
# updated 14-FEB-2020 after bug report from Sandeep Pulla
# second update JAN 2023
            if (is.na(WFO.out[i, "acceptedNameUsageID"]) == TRUE) {WFO.out[i, "acceptedNameUsageID"] <- ""}

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
                    if (Authorship %in% names(spec.data)) {
                        WFO.out[i, "Old.author"] <- WFO.out[i, "scientificNameAuthorship"]
                        Fuzzy.dist <- as.numeric(utils::adist(WFO.out[i, Authorship], y=WFO.out[i, "scientificNameAuthorship"]))
                        if (is.na(Fuzzy.dist) == TRUE) {Fuzzy.dist <- Inf}
                        WFO.out[i, "Old.author.dist"] <- Fuzzy.dist
                    }
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

    if (Authorship %in% names(spec.data)) {
        for (i in 1:nrow(WFO.out)) {
            Fuzzy.dist <- as.numeric(utils::adist(WFO.out[i, Authorship], y=WFO.out[i, "scientificNameAuthorship"]))
            if (is.na(Fuzzy.dist) == TRUE) {Fuzzy.dist <- Inf}
            WFO.out[i, "Auth.dist"] <- Fuzzy.dist
        }
    }

    return(WFO.out)
}

