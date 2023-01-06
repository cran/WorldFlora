WFO.match <- function(
    spec.data=NULL, WFO.file=NULL, WFO.data=NULL,
    no.dates=TRUE,
    spec.name="spec.name", Genus="Genus", Species="Species",
    Infraspecific.rank="Infraspecific.rank", Infraspecific="Infraspecific",
    Authorship="Authorship", First.dist=FALSE,
    acceptedNameUsageID.match=TRUE,
    Fuzzy=0.1, Fuzzy.force=FALSE, Fuzzy.max=250, Fuzzy.min=TRUE, Fuzzy.shortest=FALSE, Fuzzy.within=FALSE,
    Fuzzy.two=TRUE, Fuzzy.one=TRUE,
    squish=TRUE,
    spec.name.tolower=FALSE, spec.name.nonumber=TRUE, spec.name.nobrackets=TRUE,
    exclude.infraspecific=FALSE,
    infraspecific.excluded=c("cultivar.", "f.", "sect.", "subf.", "subg.", "subsp.", "subvar.", "var", "var.",
                             "[infraspec.]", "fo.", "forma", "nothosubsp.", "nothovar.", "sect."),
    spec.name.sub=TRUE,
    sub.pattern=c(" sp[.] A", " sp[.] B", " sp[.] C", " sp[.]", " spp[.]", " pl[.]", " indet[.]", " ind[.]", " gen[.]", " g[.]", " fam[.]",
        " nov[.]", " prox[.]", " cf[.]", " aff[.]", " s[.]s[.]", " s[.]l[.]", " p[.]p[.]", " p[.] p[.]", "[?]", " inc[.]", " stet[.]",
        "Ca[.]", "nom[.] cons[.]", "nom[.] dub[.]", " nom[.] err[.]", " nom[.] illeg[.]", " nom[.] inval[.]", " nom[.] nov[.]",
        " nom[.] nud[.]", " nom[.] obl[.]", " nom[.] prot[.]", " nom[.] rej[.]", " nom[.] supp[.]", " sensu auct[.]"),
    verbose=TRUE, counter=1000
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

## avoid problems with dates (error reported by Lauri Vesa)
##    WFO.data$created <- as.Date(as.character(WFO.data$created))
##    WFO.data$modified <- as.Date(as.character(WFO.data$modified))

##   WFO.data$created[is.na(WFO.data$created)] <- as.Date("1000-01-01")
##   WFO.data$modified[is.na(WFO.data$modified)] <- as.Date("1000-01-01")

## from version 1.9 removed columns of created and modified, as handling these as dates
## takes quite some time (reported by Pavel Pipek in August 2021)

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
    if (spec.name %in% names(spec.data)) {
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
    }

    if (Genus %in% names(spec.data)) {spec.data[, Genus] <- as.character(spec.data[, Genus])}
    if (Species %in% names(spec.data)) {spec.data[, Species] <- as.character(spec.data[, Species])}
    if (Infraspecific.rank %in% names(spec.data)) {spec.data[, Infraspecific.rank] <- as.character(spec.data[, Infraspecific.rank])}
    if (Infraspecific %in% names(spec.data)) {spec.data[, Infraspecific] <- as.character(spec.data[, Infraspecific])}

    spec.data$Matched <- spec.data$Unique <- rep(as.logical(1), nrow(spec.data))
    spec.data$Fuzzy <- rep(as.logical(0), nrow(spec.data))
    spec.data$Fuzzy.toomany <- rep(as.logical(0), nrow(spec.data))
    spec.data$Fuzzy.two <- rep(as.logical(0), nrow(spec.data))
    spec.data$Fuzzy.one <- rep(as.logical(0), nrow(spec.data))
    spec.data$Fuzzy.dist <- rep(NA, nrow(spec.data))

#    if (acceptedNameUsageID.match == TRUE) {spec.data$Auth.dist <- rep(Inf, nrow(spec.data))}
    if (Authorship %in% names(spec.data)) {spec.data$Auth.dist <- rep(Inf, nrow(spec.data))}
    if (First.dist == TRUE) {spec.data$First.dist <- rep(Inf, nrow(spec.data))}

    spec.data$OriSeq <- c(1: nrow(spec.data))
    spec.data$Subseq <- rep(1, nrow(spec.data))
    init.column <- ncol(spec.data)

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
## avoid problems with dates (error reported by Lauri Vesa)
##    WFO.empty$created <- as.Date("1000-01-02")
##    WFO.empty$modified <- as.Date("1000-01-02")

# Modified January 2023 to work with smaller data sets when working with fuzzy matches

    if (spec.name %in% names(spec.data)) {

    WFO.dat <- WFO.data[, c("taxonID", "scientificName")]
    WFO.empt <- WFO.empty[, c("taxonID", "scientificName")]

# Modified December 2022 to use left_join first for direct matches

    spec.data$spec.link <- spec.data[, spec.name]

    if (Fuzzy.force == FALSE) {

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
        if (un.r > 1) {WFO.full[WFO.full$OriSeq == seq.unique[i], "Subseq"] <- c(1:un.r)}
      }

      spec.data <- spec.data[spec.data$OriSeq %in% remain.ind, ]
      n.remain <- nrow(spec.data)

    }else{
      full.matches <- FALSE
      n.remain <- nrow(spec.data)
    }

    }

# Modified verbose options in January 2023

    if (n.remain > 0) {

    message(paste("Checking for fuzzy matches for ", n.remain, " records", sep=""))

    if (verbose==TRUE) {

    for (i in 1:nrow(spec.data)) {

        if (round(i/counter, 0) == i/counter) {message(paste("Reached record # ", i, sep=""))}

        fuzzy.matches <- FALSE

        spec.data.i1 <- spec.data.i <- spec.data[i, spec.name]

        if (spec.data.i %in% c("Compositae", "Leguminosae", "Umbelliferae", "Palmae",
                               "Cruciferae", "Guttiferae", "Labiatae", "Gramineae")) {

        if (spec.data.i == "Compositae") {WFO.match <- WFO.dat[WFO.dat$scientificName=="Asteraceae", ]}
        if (spec.data.i == "Leguminosae") {WFO.match <- WFO.dat[WFO.dat$scientificName=="Fabaceae", ]}
        if (spec.data.i == "Umbelliferae") {WFO.match <- WFO.dat[WFO.dat$scientificName=="Apiaceae", ]}
        if (spec.data.i == "Palmae") {WFO.match <- WFO.dat[WFO.dat$scientificName=="Arecaceae", ]}
        if (spec.data.i == "Cruciferae") {WFO.match <- WFO.dat[WFO.dat$scientificName=="Brassicaceae", ]}
        if (spec.data.i == "Guttiferae") {WFO.match <- WFO.dat[WFO.dat$scientificName=="Clusiaceae", ]}
        if (spec.data.i == "Labiatae") {WFO.match <- WFO.dat[WFO.dat$scientificName=="Lamiaceae", ]}
        if (spec.data.i == "Gramineae") {WFO.match <- WFO.dat[WFO.dat$scientificName=="Poaceae", ]}

        } else if (nchar(spec.data.i) > 0) {
#            WFO.match <- WFO.dat[WFO.dat$scientificName==spec.data.i,]

            if (Fuzzy > 0 || Fuzzy.force == TRUE) {
                specFuzzy <- agrep(spec.data.i, x=WFO.dat$scientificName, value=T, max.distance=Fuzzy)

                if (length(specFuzzy) == 0 && Fuzzy.two == TRUE) {
                    species.string <- spec.data.i
                    species.terms <- unlist(strsplit(species.string, split= " "))
                    if (length(species.terms) > 2) {
                        species.string2 <- paste(species.terms[1], " ", species.terms[2], sep="")
                        specFuzzy <- agrep(species.string2, x=WFO.dat$scientificName, value=T, max.distance=Fuzzy)
                        spec.data[i, "Fuzzy.two"] <- as.logical(1)
                        if (length(specFuzzy) > 0) {message(paste("Fuzzy matches for ", spec.data.i, " were only found for first 2 terms (",
                                                                  species.string2, ")", sep=""))
# Modified in January 2023 to now find best match with species
                          spec.data.i <- species.string2
                          specFuzzy.2 <- NULL
                          for (j in 1:length(specFuzzy)) {
                            species.string3 <- unlist(strsplit(specFuzzy[j], split= " "))
                            if (length(species.string3) < 3) {specFuzzy.2 <- c(specFuzzy.2, specFuzzy[j])}
                          }
                          if (length(specFuzzy.2) > 0) {
                            message(paste("With Fuzzy.two, reduced matches to those of 2 words only"))
                            specFuzzy <- specFuzzy.2
                          }
                        }

                    }
                }

                if (length(specFuzzy) == 0 && Fuzzy.one == TRUE) {
                    species.string <- spec.data.i
                    species.string2 <- unlist(strsplit(species.string, split= " "))[1]
                    if (nchar(species.string2) > 2) {
                        spec.data[i, "Fuzzy.one"] <- as.logical(1)
                        WFO.match <- WFO.dat[WFO.dat$scientificName==species.string2,]
                        if ((nrow(WFO.match) == 0 && Fuzzy > 0) || Fuzzy.force == TRUE) {
                            specFuzzy <- agrep(species.string2, x=WFO.dat$scientificName, value=T, max.distance=Fuzzy)
                            if (length(specFuzzy) > 0) {message(paste("Fuzzy matches for ", spec.data.i, " were only found for first term (",
                                                                      species.string2, ")", sep=""))
# Modified in January 2023 to now find best match with genus
                              spec.data.i <- species.string2
                              specFuzzy.2 <- NULL
                              for (j in 1:length(specFuzzy)) {
                                species.string3 <- unlist(strsplit(specFuzzy[j], split= " "))
                                if (length(species.string3) < 2) {specFuzzy.2 <- c(specFuzzy.2, specFuzzy[j])}
                              }
                              if (length(specFuzzy.2) > 0) {
                                message(paste("With Fuzzy.one, reduced matches to those of 1 word only"))
                                specFuzzy <- specFuzzy.2
                              }
                            }
                        }else{
                            specFuzzy <- NULL
                            spec.data[i, "Fuzzy"] <- as.logical(1)
                            fuzzy.matches <- TRUE
                        }
                    }
                }

                if (length(specFuzzy) > Fuzzy.max) {
                    spec.data[i, "Fuzzy.toomany"] <- length(specFuzzy)
                    message(paste("Too many (", length(specFuzzy), ") fuzzy matches for ", spec.data.i, ", including ", specFuzzy[1], sep=""))
                    specFuzzy <- NULL
                    spec.data[i, "Matched"] <- as.logical(0)
                    WFO.match <- WFO.empt
                }

                if (length(specFuzzy) > 0) {
                    spec.data[i, "Fuzzy"] <- as.logical(1)
                    fuzzy.matches <- TRUE
                    specFuzzy <- unique(specFuzzy)

                    message(paste("Fuzzy matches for ", spec.data.i1, "were: ", paste(specFuzzy, collapse=", ")))

                    if (Fuzzy.within == TRUE) {
                        Fuzzy.shortest <- Fuzzy.min <- FALSE
                        within.matches <- grepl(spec.data.i, x=specFuzzy)
                        specFuzzy <- specFuzzy[within.matches]

                            if (length(specFuzzy) >  0) {
                                message(paste("Matches within for ", spec.data.i1, "were: ", paste(specFuzzy, collapse=", ")))
                            }else{
                                message(paste("No matches within for ", spec.data.i1))
                            }

                    }
                    if (Fuzzy.min == TRUE && length(specFuzzy) > 1) {
                        Fuzzy.shortest <- FALSE
                        Fuzzy.dist <- as.numeric(utils::adist(specFuzzy, y=spec.data.i))
                        target.l <- min(Fuzzy.dist)
                        specFuzzy <- specFuzzy[Fuzzy.dist == target.l]
                        message(paste("Best fuzzy matches for ", spec.data.i1, "were: ", paste(specFuzzy, collapse=", ")))
                    }
                    if (Fuzzy.shortest == TRUE && length(specFuzzy) > 1) {
                        target.l <- nchar(spec.data.i)
                        found.l <- nchar(specFuzzy)
                        found.diff <- abs(found.l - target.l)
                        specFuzzy <- specFuzzy[found.diff == min(found.diff)]
                        message(paste("Shortest fuzzy matches for ", spec.data.i1, "were: ", paste(specFuzzy, collapse=", ")))
                    }
                    for (j in 1:length(specFuzzy)) {
                        WFO.match1 <- WFO.dat[WFO.dat$scientificName==specFuzzy[j],]
                        if (j==1) {
                            WFO.match <- WFO.match1
                        }else{
                            WFO.match <- rbind(WFO.match, WFO.match1)
                        }
                    }
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
              WFO.match2 <- cbind(spec.data[i, ], WFO.empt)
            }

            # Need to calculate distance again as some repetition in scientificName (eg Agave mitis)
            if (fuzzy.matches == TRUE) {
              for (j in 1:nrow(WFO.match2)) {
                WFO.match2[j, "Fuzzy.dist"] <- as.numeric(utils::adist(WFO.match2[j, "scientificName"], y=spec.data.i1))

                if (First.dist == TRUE) {
                  genus.input <- spec.data.i
                  genus.input2 <- unlist(strsplit(genus.input, split= " "))[1]
                  genus.match <- WFO.match2[j, "scientificName"]
                  genus.match2 <- unlist(strsplit(genus.match, split= " "))[1]
                  Fuzzy.dist1 <- as.numeric(utils::adist(genus.input2, y=genus.match2))
                  if (is.na(Fuzzy.dist1) == TRUE) {Fuzzy.dist1 <- Inf}
                  WFO.match2[j, "First.dist"] <- Fuzzy.dist1
                }

              }
            }

        }else{ # avoid empty record
          spec.data[i, "Matched"] <- as.logical(0)
          WFO.match2 <- cbind(spec.data[i, ], WFO.empt)
        }


            if (i==1) {
              WFO.out <- WFO.match2
            }else{
              WFO.out <- rbind(WFO.out, WFO.match2)
            }
        } # finish i loop verbose option

    }else{ # not verbose

      for (i in 1:nrow(spec.data)) {

   #     if (round(i/counter, 0) == i/counter) {message(paste("Reached record # ", i, sep=""))}

        fuzzy.matches <- FALSE
        spec.data.i1 <- spec.data.i <- spec.data[i, spec.name]

        if (spec.data.i %in% c("Compositae", "Leguminosae", "Umbelliferae", "Palmae",
                               "Cruciferae", "Guttiferae", "Labiatae", "Gramineae")) {

          if (spec.data.i == "Compositae") {WFO.match <- WFO.dat[WFO.dat$scientificName=="Asteraceae", ]}
          if (spec.data.i == "Leguminosae") {WFO.match <- WFO.dat[WFO.dat$scientificName=="Fabaceae", ]}
          if (spec.data.i == "Umbelliferae") {WFO.match <- WFO.dat[WFO.dat$scientificName=="Apiaceae", ]}
          if (spec.data.i == "Palmae") {WFO.match <- WFO.dat[WFO.dat$scientificName=="Arecaceae", ]}
          if (spec.data.i == "Cruciferae") {WFO.match <- WFO.dat[WFO.dat$scientificName=="Brassicaceae", ]}
          if (spec.data.i == "Guttiferae") {WFO.match <- WFO.dat[WFO.dat$scientificName=="Clusiaceae", ]}
          if (spec.data.i == "Labiatae") {WFO.match <- WFO.dat[WFO.dat$scientificName=="Lamiaceae", ]}
          if (spec.data.i == "Gramineae") {WFO.match <- WFO.dat[WFO.dat$scientificName=="Poaceae", ]}

        } else if (nchar(spec.data.i) > 0) {

          if (Fuzzy > 0 || Fuzzy.force == TRUE) {
            specFuzzy <- agrep(spec.data.i, x=WFO.dat$scientificName, value=T, max.distance=Fuzzy)

            if (length(specFuzzy) == 0 && Fuzzy.two == TRUE) {
              species.string <- spec.data.i
              species.terms <- unlist(strsplit(species.string, split= " "))
              if (length(species.terms) > 2) {
                species.string2 <- paste(species.terms[1], " ", species.terms[2], sep="")
                specFuzzy <- agrep(species.string2, x=WFO.dat$scientificName, value=T, max.distance=Fuzzy)
                spec.data[i, "Fuzzy.two"] <- as.logical(1)
                if (length(specFuzzy) > 0) {
# Modified in January 2023 to now find best match with species
                  spec.data.i <- species.string2
                  specFuzzy.2 <- NULL
                  for (j in 1:length(specFuzzy)) {
                    species.string3 <- unlist(strsplit(specFuzzy[j], split= " "))
                    if (length(species.string3) < 3) {specFuzzy.2 <- c(specFuzzy.2, specFuzzy[j])}
                  }
                  if (length(specFuzzy.2) > 0) {
                    specFuzzy <- specFuzzy.2
                  }
                }
              }
            }

            if (length(specFuzzy) == 0 && Fuzzy.one == TRUE) {
              species.string <- spec.data.i
              species.string2 <- unlist(strsplit(species.string, split= " "))[1]
              if (nchar(species.string2) > 2) {
                spec.data[i, "Fuzzy.one"] <- as.logical(1)
                WFO.match <- WFO.dat[WFO.dat$scientificName==species.string2,]
                if ((nrow(WFO.match) == 0 && Fuzzy > 0) || Fuzzy.force == TRUE) {
                  specFuzzy <- agrep(species.string2, x=WFO.dat$scientificName, value=T, max.distance=Fuzzy)
                  if (length(specFuzzy) > 0) {
# Modified in January 2023 to now find best match with genus
                    spec.data.i <- species.string2
                    specFuzzy.2 <- NULL
                    for (j in 1:length(specFuzzy)) {
                      species.string3 <- unlist(strsplit(specFuzzy[j], split= " "))
                      if (length(species.string3) < 2) {specFuzzy.2 <- c(specFuzzy.2, specFuzzy[j])}
                    }
                    if (length(specFuzzy.2) > 0) {
                      specFuzzy <- specFuzzy.2
                    }
                  }
                }else{
                  specFuzzy <- NULL
                  spec.data[i, "Fuzzy"] <- as.logical(1)
                  fuzzy.matches <- TRUE
                }
              }
            }

            if (length(specFuzzy) > Fuzzy.max) {
              spec.data[i, "Fuzzy.toomany"] <- length(specFuzzy)
              specFuzzy <- NULL
              spec.data[i, "Matched"] <- as.logical(0)
              WFO.match <- WFO.empt
            }

            if (length(specFuzzy) > 0) {
              spec.data[i, "Fuzzy"] <- as.logical(1)
              fuzzy.matches <- TRUE
              specFuzzy <- unique(specFuzzy)

              if (Fuzzy.within == TRUE) {
                Fuzzy.shortest <- Fuzzy.min <- FALSE
                within.matches <- grepl(spec.data.i, x=specFuzzy)
                specFuzzy <- specFuzzy[within.matches]
              }

              if (Fuzzy.min == TRUE  && length(specFuzzy) > 1) {
                Fuzzy.shortest <- FALSE
                Fuzzy.dist <- as.numeric(utils::adist(specFuzzy, y=spec.data.i))
                target.l <- min(Fuzzy.dist)
                specFuzzy <- specFuzzy[Fuzzy.dist == target.l]
              }

              if (Fuzzy.shortest == TRUE && length(specFuzzy) > 1) {
                target.l <- nchar(spec.data.i)
                found.l <- nchar(specFuzzy)
                found.diff <- abs(found.l - target.l)
                specFuzzy <- specFuzzy[found.diff == min(found.diff)]
              }

              for (j in 1:length(specFuzzy)) {
                WFO.match1 <- WFO.dat[WFO.dat$scientificName==specFuzzy[j],]
                if (j==1) {
                  WFO.match <- WFO.match1
                }else{
                  WFO.match <- rbind(WFO.match, WFO.match1)
                }
              }
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
            WFO.match2 <- cbind(spec.data[i, ], WFO.empt)
          }

          # Need to calculate distance again as some repetition in scientificName (eg Agave mitis)
          if (fuzzy.matches == TRUE) {
            for (j in 1:nrow(WFO.match2)) {
              WFO.match2[j, "Fuzzy.dist"] <- as.numeric(utils::adist(WFO.match2[j, "scientificName"], y=spec.data.i1))

              if (First.dist == TRUE) {
#                genus.input <- spec.data.i
                genus.input2 <- unlist(strsplit(spec.data.i, split= " "))[1]
                genus.match <- WFO.match2[j, "scientificName"]
                genus.match2 <- unlist(strsplit(genus.match, split= " "))[1]
                Fuzzy.dist1 <- as.numeric(utils::adist(genus.input2, y=genus.match2))
                if (is.na(Fuzzy.dist1) == TRUE) {Fuzzy.dist1 <- Inf}
                WFO.match2[j, "First.dist"] <- Fuzzy.dist1
              }

            }
          }

        }else{ # avoid empty record
          spec.data[i, "Matched"] <- as.logical(0)
          WFO.match2 <- cbind(spec.data[i, ], WFO.empt)
        }


        if (i==1) {
          WFO.out <- WFO.match2
        }else{
          WFO.out <- rbind(WFO.out, WFO.match2)
        }
      } # finish i loop non- verbose option

    } # not verbose

    WFO.out <- WFO.out[, !names(WFO.out) %in% c("scientificName")]

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
# Only match genus and species separately if specName was not given
        }else{

          for (i in 1:nrow(spec.data)) {

            if (Infraspecific.rank %in% names(spec.data)) {
                WFO.match <- WFO.data[WFO.data$genus==spec.data[i, Genus] & WFO.data$specificEpithet==spec.data[i, Species]
                    & WFO.data$verbatimTaxonRank==spec.data[i, Infraspecific.rank] & WFO.data$infraspecificEpithet==spec.data[i, Infraspecific], ]
            }else if (Infraspecific %in% names(spec.data)) {
                WFO.match <- WFO.data[WFO.data$genus==spec.data[i, Genus] & WFO.data$specificEpithet==spec.data[i, Species]
                    & WFO.data$infraspecificEpithet==spec.data[i, Infraspecific], ]
            }else if (Species %in% names(spec.data)){
                WFO.match <- WFO.data[WFO.data$genus==spec.data[i, Genus] & WFO.data$specificEpithet==spec.data[i, Species], ]
            }else {
                WFO.match <- WFO.data[WFO.data$genus==spec.data[i, Genus], ]
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
      }  # i loop
    }


# Modified in January 2023 to only exclude in final analysis step
#    if (exclude.infraspecific == TRUE) {
#        keep.rows <- rep(as.logical(1), nrow(WFO.out))
#        for (i in 1:nrow(WFO.out)) {
#            if (WFO.out[i, "verbatimTaxonRank"] %in% infraspecific.excluded) {keep.rows[i] <- as.logical(0)}
#        }
#        WFO.out <- WFO.out[keep.rows, ]
#    }

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

    if (exclude.infraspecific == TRUE) {
        keep.rows <- rep(as.logical(1), nrow(WFO.out))
        for (i in 1:nrow(WFO.out)) {
            if (WFO.out[i, "verbatimTaxonRank"] %in% infraspecific.excluded) {keep.rows[i] <- as.logical(0)}
        }
        WFO.out <- WFO.out[keep.rows, ]
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

