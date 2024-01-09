new.backbone <- function(
    x,
    taxonID="taxonID", scientificName="scientificName",
    scientificNameAuthorship="scientificNameAuthorship",
    acceptedNameUsageID=NULL, taxonomicStatus=NULL)
{
  
  x <- data.frame(x)
  for (j in 1:ncol(x)) {x[, j] <- as.character(x[, j])}
  
  out <- data.frame(taxonID = x[, taxonID],
                    scientificName = x[, scientificName],
                    scientificNameAuthorship = x[, scientificNameAuthorship])
  
  if (is.null(acceptedNameUsageID)) {
    out$acceptedNameUsageID <- as.character("", nrow(out))
  }else{
    out <- data.frame(out,
                      acceptedNameUsageID = x[, acceptedNameUsageID])
  }
  
  if (is.null(taxonomicStatus)) {
    out$taxonomicStatus <- as.character("Accepted", nrow(out))
    out[out$acceptedNameUsageID != "", "taxonomicStatus"] <- "Synonym"
  }else{
    out <- data.frame(out,
                      taxonomicStatus = x[, taxonomicStatus])
  }

  keep.columns <- names(x) %in% c("taxonID",
                                  "scientificName", "scientificNameAuthorship",
                                  "acceptedNameUsageID", "taxonomicStatus"
  ) == FALSE
  
  out <- data.table::data.table(out, x[, keep.columns])
  
  # modified January 2024 to deal with self-referencing acceptedNameUsageID
  selfies <- out$taxonID == out$acceptedNameUsageID
  selfies[is.na(selfies)==TRUE] <- FALSE
  n.self <- nrow(out[selfies, ])
  
  if (n.self > 0) {
    message(paste("Self-referencing was detected for", n.self, "records, so emptying acceptedNameUsageID for these."))
    out[selfies, "acceptedNameUsageID"] <- ""
  }
  
  return(out)
  
}
