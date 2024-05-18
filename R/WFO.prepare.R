WFO.prepare <- function(
    spec.data=NULL, spec.full="spec.full",
    squish=TRUE, spec.name.nonumber=TRUE,
    spec.name.sub=TRUE,
    sub.pattern=c(" sp[.] A", " sp[.] B", " sp[.] C", " sp[.]", " spp[.]", " pl[.]", " indet[.]", " ind[.]", " gen[.]", " g[.]", " fam[.]",
        " nov[.]", " prox[.]", " cf[.]", " aff[.]", " s[.]s[.]", " s[.]l[.]", " p[.]p[.]", " p[.] p[.]", "[?]", " inc[.]", " stet[.]",
        "Ca[.]", "nom[.] cons[.]", "nom[.] dub[.]", " nom[.] err[.]", " nom[.] illeg[.]", " nom[.] inval[.]", " nom[.] nov[.]",
        " nom[.] nud[.]", " nom[.] obl[.]", " nom[.] prot[.]", " nom[.] rej[.]", " nom[.] supp[.]", " sensu auct[.]"),
    genus.2.flag=TRUE, species.2.flag=TRUE, punctuation.flag=TRUE, pointless.flag=TRUE,
    trinomial=c("cultivar.", "f.", "sect.", "subf.", "subg.", "subsp.", "subvar.", "var.",
                "CULTIVAR.",       "SECT.", "SUBF.", "SUBG.", "SUBSP.", "SUBVAR.", "VAR."),
    authors.ending.f=c("Aiton f.", "Baker f.", "Bak. f.", "Burm. f.", 
                       "Cheng f.", "Chrtek f.", 
                       "De Marco f.", "Fang f.", "Ferry f.", "Forsyth f.", 
                       "Forster f.", "Fraser f.", "G.Don f.", "Haller f.",
                       "Hallier f.", "Hook. f.", "Hooker f.", "Hsueh f.", 
                       "J.Kickx f.", "J. Kickx f.", "Keng f.", 
                       "Kickx f.", "Klokov f.", "Koster f.",
                       "Liou f.", "L. f.", "Ma f.", "Mikan f.",
                       "Occhioni f.", "Rchb. f.", "Schultes f.", 
                       "Schult. f.", "Stapf f."),
    verbose=TRUE, counter=1000
)
{
    if (class(spec.data) %in% c("data.frame") == FALSE) {spec.data <- data.frame(spec.full = spec.data)}
    if (is.factor(spec.data) == TRUE) {spec.data <- data.frame(spec.full = spec.data)}

    WFO.names <- c("spec.name", "Authorship")
    for (i in 1:length(WFO.names)) {
        if (WFO.names[i] %in% names(spec.data)) {
            message(paste("Original data set variable '", WFO.names[i], "' replaced by variable '", WFO.names[i], ".ORIG'", sep=""))
            names(spec.data)[names(spec.data) == WFO.names[i]] <- paste(WFO.names[i], ".ORIG", sep="")
        }
    }

# Deal with author names that end in "f." such as "L. f."
# March 2024
    authors.ending.nof <- gsub(pattern=" f.",
                               replacement=" f_author",
                               x=authors.ending.f)
    
    authors.ending <- data.frame(pattern=authors.ending.f,
                                 replacement=authors.ending.nof)
    
    for (i in 1:nrow(authors.ending)) {
      spec.data[, spec.full] <- gsub(pattern=authors.ending[i, "pattern"],
                                  replacement=authors.ending[i, "replacement"],
                                  x=spec.data[, spec.full])
    }
    
    
    spec.data$spec.name <- rep("", nrow(spec.data))
    spec.data$Authorship <- rep("", nrow(spec.data))
    if (genus.2.flag == TRUE) {spec.data$genus.2 <- spec.data$genus.nchar <- rep("", nrow(spec.data))}
    if (species.2.flag == TRUE) {spec.data$species.2 <- spec.data$species.nchar <- rep("", nrow(spec.data))}
    if (punctuation.flag == TRUE) {spec.data$punctuation <- rep("", nrow(spec.data))}
    if (pointless.flag == TRUE) {spec.data$pointless <- rep("", nrow(spec.data))}

    for (i in 1:nrow(spec.data)) {

        if (round(i/counter, 0) == i/counter) {message(paste("Reached record # ", i, sep=""))}

        spfull <- spec.data[i, spec.full]

        if (squish == TRUE) {
            spfull1 <- stringr::str_squish(spfull)
            if (verbose == TRUE) {if (spfull != spfull1) {message(paste(spfull, " was squished", sep=""))}}
            spfull <- spfull1
        }

        if (spec.name.sub == TRUE) {
            spfull1 <- spfull
            for (j in 1:length(sub.pattern)) {
                spfull1 <- gsub(pattern=sub.pattern[j], replacement="", x=spfull1)
            }
            if (verbose == TRUE) {if (spfull != spfull1) {message(paste("Removed sub.patterns from " , spfull, " resulting in: ", spfull1, sep=""))}}
            spfull <- spfull1
        }

        if (spec.name.nonumber == TRUE) {
            spfull1 <- spfull
            if (grepl("[[:digit:]]", spfull1) == TRUE) {
                species.terms <- unlist(strsplit(spfull1, split= " "))
                species.new.string <- species.terms[1]
                while (grepl("^[[:digit:]]", substr(species.new.string, start=nchar(species.new.string), stop=nchar(species.new.string))) == TRUE){
                    species.new.string <- substr(species.new.string, start=1, stop=nchar(species.new.string)-1)
                }
                spfull1 <- species.new.string
            }
            if (verbose == TRUE) {if (spfull != spfull1) {message(paste("Numbers detected in ", spfull, " and replaced by: ",  spfull1, sep=""))}}
            spfull <- spfull1
        }

        sp.string <- unlist(strsplit(spfull, split= " "))
        auth.add <- 3

        if (length(sp.string) < 3) {
            sp.final <- spfull
        }else{
            sp.final <- paste(sp.string[1], " ", sp.string[2], sep="")

            if (any(sp.string %in% trinomial)) {
                sub.start <- which(sp.string %in% trinomial)

                if (length(sub.start) > 1) {
                    warning(paste("Two trinomial epithets provided for ", spfull, sep=""))
                    auth.add <- max(sub.start) + 2
                    sp.final2 <- paste(sp.string[min(sub.start)], " ", sp.string[min(sub.start)+1], sep="")
                    sp.final <- paste(sp.final, " ", sp.final2, sep="")
                }else{
                    auth.add <- sub.start + 2

                    if (length(sp.string) < sub.start + 1) {
                        warning(paste("No trinomial epithet provided for ", spfull, sep=""))
                        auth.add <- sub.start + 1
                    }else{
                        sp.final2 <- paste(sp.string[sub.start], " ", sp.string[sub.start+1], sep="")
                        sp.final <- paste(sp.final, " ", sp.final2, sep="")
                    }
                }
            }
        }

        auth.final <- ""
        while (auth.add <= length(sp.string)) {
            auth.final <- paste(auth.final, " ", sp.string[auth.add], sep="")
            auth.add <- auth.add + 1
        }
        if (nchar(auth.final) > 1) { auth.final <- substr(auth.final, start=2, stop=nchar(auth.final))}

# remove second set of brackets
        if (nchar(auth.final) > 1) {
            auth.final1 <- substr(auth.final, start=2, stop=nchar(auth.final))
            if (grepl(pattern="[(]", x=auth.final1) == TRUE) {
                if (verbose == TRUE) {message(paste("Second bracket detected for ", auth.final, sep=""))}
                brack.place <- as.numeric(unlist(gregexpr(pattern="[(]", text=auth.final1)))[1] + 1
                auth.final2 <- substr(auth.final, start=1, stop=brack.place-1)
                auth.final <- stringr::str_squish(auth.final2)
            }
        }

        spec.data[i, "spec.name"] <- sp.final
        spec.data[i, "Authorship"] <- auth.final

        if (genus.2.flag==TRUE || species.2.flag==TRUE) {
            sp.string <- unlist(strsplit(sp.final, split= " "))
            if (genus.2.flag == TRUE) {
                spec.data[i, "genus.nchar"] <- nchar(sp.string[1])
                if (nchar(sp.string[1]) < 3) {
                    spec.data[i, "genus.2"] <- sp.string[1]
                    if (verbose == TRUE) {message(paste("Short first part of name detected for: ", sp.final, sep=""))}
                }
            }
            if (species.2.flag == TRUE && length(sp.string) > 1) {
                spec.data[i, "species.nchar"] <- nchar(sp.string[2])
                if (nchar(sp.string[2]) < 3) {
                    spec.data[i, "species.2"] <- sp.string[2]
                    if (verbose == TRUE) {message(paste("Short second part of name detected for: ", sp.final, sep=""))}
                }
            }
        }

        if (punctuation.flag==TRUE) {

# first remove the trinomials since these contain punctuation marks!
            sp.final1 <- tolower(sp.final)
            for (j in 1:length(trinomial)) {
                sp.final1 <- gsub(pattern=trinomial[j], replacement="", x=sp.final1)
            }

            if (grepl(pattern="[[:punct:]]", x=sp.final1) == TRUE) {
                spec.data[i, "punctuation"] <- as.logical(1)
                if (verbose == TRUE) {message(paste("Punctuation mark detected for: ", sp.final, sep=""))}
            }
        }

        if (pointless.flag==TRUE) {
            sp.final1 <- tolower(sp.final)

# first remove the trinomials since these contain punctuation marks!
            for (j in 1:length(trinomial)) {
                if (trinomial[j] != "f.") {sp.final1 <- gsub(pattern=trinomial[j], replacement="", x=sp.final1)}
            }

            for (j in 1:length(sub.pattern)) {
                sub1 <- gsub(pattern="[[:punct:]]", replacement="", x=sub.pattern[j])
                sub2 <- tolower(sub1)
                if (nchar(sub1) > 2 && grepl(pattern=sub2, x=sp.final1) == TRUE) {
                    spec.data[i, "pointless"] <- as.logical(1)
                    if (verbose == TRUE) {message(paste("Subpattern without punctuation mark ('", sub1, "') detected for: ", sp.final, sep=""))}
                }
            }
        }

    }

    for (i in 1:nrow(authors.ending)) {
      spec.data[, spec.full] <- gsub(pattern=authors.ending[i, "replacement"],
                                  replacement=authors.ending[i, "pattern"],
                                  x=spec.data[, spec.full])
      spec.data$spec.name <- gsub(pattern=authors.ending[i, "replacement"],
                                  replacement=authors.ending[i, "pattern"],
                                  x=spec.data$spec.name)
      spec.data$Authorship <- gsub(pattern=authors.ending[i, "replacement"],
                                  replacement=authors.ending[i, "pattern"],
                                  x=spec.data$Authorship)
    }
    
    
    if (all.equal(spec.data$Authorship, rep("", nrow(spec.data))) == TRUE) {
        spec.data <- spec.data[, -which(names(spec.data) == "Authorship")]
    }

    return(spec.data)
}

WFO.preprepare <- function(
    spec.data=NULL, spec.full="spec.full",
    trinomial.first="subsp.",
    trinomial.second="var.") 
{
  if (class(spec.data) %in% c("data.frame") == FALSE) {spec.data <- data.frame(spec.full = spec.data)}
  if (is.factor(spec.data) == TRUE) {spec.data <- data.frame(spec.full = spec.data)}

  infra1 <- trinomial.first
  infra2 <- trinomial.second
  
  WFO.names <- c("spec.name", "Authorship")
  for (i in 1:length(WFO.names)) {
    if (WFO.names[i] %in% names(spec.data)) {
      message(paste("Original data set variable '", WFO.names[i], "' replaced by variable '", WFO.names[i], ".ORIG'", sep=""))
      names(spec.data)[names(spec.data) == WFO.names[i]] <- paste(WFO.names[i], ".ORIG", sep="")
    }
  } 

# inspired also by https://www.youtube.com/watch?v=DiY8EqZDwoI    
  spec.data$first <- stringr::str_detect(spec.data[, spec.full],
                                         pattern=paste0(" ", infra1, " "))

  spec.data$split.before <- spec.data[, spec.full]
  spec.data[spec.data$first == TRUE, "split.before"] <- stringr::str_match(spec.data[spec.data$first == TRUE, "spec.full"],
                                                                                     pattern=paste0("(.*)\\s", infra1, "\\s"))[, 2]
  
  # no need to have the infravar detected, can be empty
  spec.data$split.after <- stringr::str_match(spec.data[, spec.full],
                                              pattern=paste0("\\s", infra1, "\\s(.*)"))[, 2]
  
  # but then need to precede with the infravar pattern if detected
  spec.data[spec.data$first == TRUE, "split.after"] <- paste0(infra1, " ", spec.data[spec.data$first == TRUE, "split.after"])

  # Cases where second infravar in the before part 
  spec.data$only.second <- stringr::str_detect(spec.data$split.before,
                                               pattern=paste0(" ", infra2, " "))  
  
  spec.data[spec.data$only.second == TRUE, "split.before"] <- stringr::str_match(spec.data[spec.data$only.second == TRUE, "spec.full"],
                                                                                 pattern=paste0("(.*)\\s", infra2, "\\s"))[, 2]
  
  spec.data[spec.data$only.second == TRUE, "split.after"] <- stringr::str_match(spec.data[spec.data$only.second == TRUE, "spec.full"],
                                                                                pattern=paste0("\\s", infra2, "\\s(.*)"))[, 2]
  
  spec.data[spec.data$only.second == TRUE, "split.after"] <- paste0(infra2, " ", spec.data[spec.data$only.second == TRUE, "split.after"])
  
  # Cases where second infravar in the after part 
  spec.data$also.second <- stringr::str_detect(spec.data$split.after,
                                               pattern=paste0(" ", infra2, " "))
  
  if (sum(spec.data$also.second ) > 0 ) {
  
  spec.data$second.split.before <- rep("", nrow(spec.data))
  spec.data$second.split.after <- rep("", nrow(spec.data))
  
  spec.data[spec.data$also.second == TRUE, "second.split.before"] <- stringr::str_match(spec.data[spec.data$also.second == TRUE, "split.after"],
                                                                                        pattern=paste0("(.*)\\s", infra2, "\\s"))[, 2]
  
  spec.data[spec.data$also.second == TRUE, "second.split.after"] <- stringr::str_match(spec.data[spec.data$also.second == TRUE, "split.after"],
                                                                                pattern=paste0("\\s", infra2, "\\s(.*)"))[, 2]
  
  spec.data[spec.data$also.second == TRUE, "second.split.after"] <- paste0(infra2, " ", spec.data[spec.data$also.second == TRUE, "second.split.after"])    
 
  }
        
  return(spec.data)
  
}


