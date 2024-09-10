WFO.one <- function(
    WFO.result=NULL, priority="Accepted",
    spec.name=NULL, Auth.dist=NULL, Old.author.dist=NULL, First.dist=NULL,
    verbose=TRUE, counter=1000
)
{
  WFO.new <- WFO.result
  
  if ((priority %in% c("Accepted", "Synonym")) == FALSE) {stop("Priority should either be 'Accepted' or 'Synonym'")}
  
  WFO.new$One.Reason <- rep("", nrow(WFO.result))
  
  accepted.select <- function(WFO.c) {
    # changed for World Flora Online DEC 2021 release
    WFO.o <- WFO.c[WFO.c[, "taxonomicStatus"] %in% c("Accepted", "ACCEPTED"), , drop=F]
    return(WFO.o)
  }
  
  synonym.select <- function(WFO.c) {
    WFO.o <- WFO.c[WFO.c[, "New.accepted"] == FALSE, , drop=F]
    return(WFO.o)
  }
  
  smallID.select <- function(WFO.c, verbose=FALSE) {
    if (verbose == TRUE) {
      small.candidates <- sort(as.character(WFO.c[, "scientificName"]))
      message(paste("Smallest ID candidates for ", WFO.c[1, "OriSeq"], "were: ", paste(small.candidates, collapse=", ")))
    }
    WFOID.strings <- WFO.c[, "taxonID"]
    #        WFOID.strings2 <- as.numeric(substr(WFOID.strings, start=5, stop=nchar(WFOID.strings[1])))
    # modified in version 1.9 to deal with data created by new.backbone
    WFOID.strings2 <- as.numeric(gsub("[^0-9]", "", x=WFOID.strings))
    WFO.o <- WFO.c[which.min(WFOID.strings2), , drop=F]
    return(WFO.o)
  }
  
  WFO.cases <- unique(WFO.result[, "OriSeq"])

  for (i in 1:length(WFO.cases)) {
    
    if (round(i/counter, 0) == i/counter) {message(paste("Reached case # ", i, sep=""))}
    
    WFO.case <- WFO.result[WFO.result[, "OriSeq"] == WFO.cases[i], , drop=F]
    WFO.case.orig <- WFO.case
    onereason <- ""
    
    if (nrow(WFO.case) > 1) {
      if (verbose == T) {message(paste("Different candidates for original record # ", WFO.case[1, "OriSeq"],  ", including ", WFO.case[1, "scientificName"], sep=""))}
      
      if (length(Old.author.dist) > 0) {
        # Check for synonym matches first, August 2024              
        if (Old.author.dist %in% names(WFO.result)) {
          suppressWarnings(min.dist <- min(as.numeric(WFO.case[WFO.case$New.accepted == TRUE, Old.author.dist]), na.rm=TRUE))
          suppressWarnings(min.dist.nosyn <- min(as.numeric(WFO.case[WFO.case$New.accepted == FALSE, Auth.dist]), na.rm=TRUE))

          if (is.na(min.dist) == FALSE  && min.dist != Inf  && min.dist < min.dist.nosyn) {
            onereason <- "Authorship best match for synonym"
            WFO.case2 <- WFO.case[WFO.case[, Old.author.dist] == min.dist, , drop=F]
            if (nrow(WFO.case2) == 1) {if (verbose == T) {message(paste("Found unique best Authorship match case for record # ", WFO.case[1, "OriSeq"], sep=""))}}
            WFO.case <- WFO.case2
          }
        }
      }
      
      if (length(Auth.dist) > 0  && nrow(WFO.case) > 1) {          
        
        if (Auth.dist %in% names(WFO.result) && nrow(WFO.case) > 1) {
          suppressWarnings(min.dist <- min(as.numeric(WFO.case[WFO.case$New.accepted == FALSE, Auth.dist]), na.rm=TRUE))
          if (is.na(min.dist) == FALSE && min.dist != Inf) {
            onereason <- "Authorship best match"
            WFO.case2 <- WFO.case[WFO.case[WFO.case$New.accepted == FALSE, Auth.dist] == min.dist, , drop=F]
            if (nrow(WFO.case2) == 1) {if (verbose == T) {message(paste("Found unique best Authorship match case for record # ", WFO.case[1, "OriSeq"], sep=""))}}
            WFO.case <- WFO.case2
          }
        }
      }

      if (length(First.dist) > 0  && nrow(WFO.case) > 1) {
        if (First.dist %in% names(WFO.result)) {
          min.dist <- min(as.numeric(WFO.case[, First.dist]), na.rm=TRUE)
          if (is.na(min.dist) == FALSE) {
            onereason <- "First distance best match"
            WFO.case2 <- WFO.case[WFO.case[, First.dist] == min.dist, , drop=F]
            if (nrow(WFO.case2) == 1) {if (verbose == T) {message(paste("Found unique best first distance match case for record # ", WFO.case[1, "OriSeq"], sep=""))}}
            WFO.case <- WFO.case2
          }
        }
      }
      
      if (priority == "Accepted" && nrow(WFO.case) > 1) {
        WFO.case1 <- accepted.select(WFO.case)
        
        if (nrow(WFO.case1) == 1) {
          if (verbose == T) {message(paste("Found unique Accepted case for record # ", WFO.case[1, "OriSeq"], sep=""))}
          onereason <- "Accepted"
          WFO.case2 <- WFO.case1
        }else if (nrow(WFO.case1) > 1) {
          WFO.case2 <- synonym.select(WFO.case1)
          if (nrow(WFO.case2) == 1) {if (verbose == T) {message(paste("Found unique non-synonym case for record # ", WFO.case[1, "OriSeq"], sep=""))}}
          onereason <- "Not a synonym"
          if (nrow(WFO.case2) == 0) {WFO.case2 <- WFO.case}
        }else{
          WFO.case2 <- synonym.select(WFO.case)
          if (nrow(WFO.case2) == 1) {if (verbose == T) {message(paste("Found unique non-synonym case for record # ", WFO.case[1, "OriSeq"], sep=""))}}
          onereason <- "Not a synonym"
          if (nrow(WFO.case2) == 0) {WFO.case2 <- WFO.case}
        }
      }
      
      if (priority == "Synonym" && nrow(WFO.case) > 1) {
        
        WFO.case1 <- synonym.select(WFO.case)
        
        if (nrow(WFO.case1) == 1) {
          if (verbose == T) {message(paste("Found unique non-synonym case for record # ", WFO.case[1, "OriSeq"], sep=""))}
          onereason <- "Not a synonym"
          WFO.case2 <- WFO.case1
        }else if (nrow(WFO.case1) > 1) {
          WFO.case2 <- accepted.select(WFO.case1)
          if (nrow(WFO.case2) == 1) {if (verbose == T) {message(paste("Found unique Accepted case for record # ", WFO.case[1, "OriSeq"], sep=""))}}
          onereason <- "Accepted"
          if (nrow(WFO.case2) == 0) {WFO.case2 <- WFO.case}
        }else{
          WFO.case2 <- accepted.select(WFO.case)
          if (nrow(WFO.case2) == 1) {if (verbose == T) {message(paste("Found unique Accepted case for record # ", WFO.case[1, "OriSeq"], sep=""))}}
          onereason <- "Accepted"
          if (nrow(WFO.case2) == 0) {WFO.case2 <- WFO.case}
        }
      }
      
      if (nrow(WFO.case2) > 1) {
        WFO.case3 <- smallID.select(WFO.case2, verbose=verbose)
        if (verbose == T) {message(paste("Selected record with smallest ID for record # ", WFO.case[1, "OriSeq"], sep=""))}
        onereason <- "smallest ID"
      }else{
        WFO.case3 <- WFO.case2
      }
      
      for (j in 1:nrow(WFO.case.orig)) {
        if (WFO.case.orig[j, "Subseq"] != WFO.case3[1, "Subseq"]) {
          del1 <- which(WFO.new[, "OriSeq"] == WFO.case.orig[j, "OriSeq"])
          del2 <- which(WFO.new[del1, "Subseq"] == WFO.case.orig[j, "Subseq"])
          del3 <- del1[del2]
          WFO.new <- WFO.new[-del3, ]
        }else{
          WFO.new[which(WFO.new[, "OriSeq"] == WFO.case.orig[j, "OriSeq"]), "One.Reason"] <- onereason
        }
      }
    }
    
  }
  
  if (is.null(spec.name) == FALSE) {
    for (i in 1:nrow(WFO.new)) {
      # update DEC-2022
      if (is.na(WFO.new[i, "scientificName"])){WFO.new[i, "scientificName"] <- ""}
      if (WFO.new[i, "scientificName"] == "") {
        WFO.new[i, "scientificName"] <- WFO.new[i, spec.name]
        WFO.new[i, "One.Reason"] <- "no match found"
      }
    }
  }
  
  return(WFO.new)
}
