WFO.prepare <- function(
    spec.data=NULL, spec.full="spec.full",
    squish=TRUE, 
    trinomial=c("cultivar.", "f.", "sect.", "subf.", "subg.", "subsp.", "subvar.", "var.",
                "CULTIVAR.",       "SECT.", "SUBF.", "SUBG.", "SUBSP.", "SUBVAR.", "VAR."),
    verbose=TRUE, counter=1000
)
{
    if (class(spec.data) == "character") {spec.data <- data.frame(spec.full = spec.data)}
    if (is.factor(spec.data) == TRUE) {spec.data <- data.frame(spec.full = spec.data)}

    WFO.names <- c("spec.name", "Authorship")
    for (i in 1:length(WFO.names)) {
        if (WFO.names[i] %in% names(spec.data)) {
            message(paste("Original data set variable '", WFO.names[i], "' replaced by variable '", WFO.names[i], ".ORIG'", sep=""))
            names(spec.data)[names(spec.data) == WFO.names[i]] <- paste(WFO.names[i], ".ORIG", sep="")
        }
    }
    
    spec.data$spec.name <- rep("", nrow(spec.data))
    spec.data$Authorship <- rep("", nrow(spec.data))

    for (i in 1:nrow(spec.data)) {

        if (round(i/counter, 0) == i/counter) {message(paste("Reached record # ", i, sep=""))}

        spfull <- spec.data[i, spec.full]

        if (squish == TRUE) {
            spfull1 <- stringr::str_squish(spfull)
            if (verbose == TRUE) {if (spfull != spfull1) {message(paste(spfull, " was squished", sep=""))}}
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
    }
    
    if (all.equal(spec.data$Authorship, rep("", nrow(spec.data))) == TRUE) {
        spec.data <- spec.data[, -which(names(spec.data) == "Authorship")]
    }
    
    return(spec.data)
}




   