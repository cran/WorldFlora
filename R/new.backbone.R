new.backbone <- function(
    x, 
    taxonID="taxonID", scientificName="scientificName", 
    scientificNameAuthorship="scientificNameAuthorship",
    acceptedNameUsageID=NULL, taxonomicStatus=NULL,
    created=NULL, modified=NULL)
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

if (is.null(created)) {
  out$created <- as.character("1000-01-01", nrow(out))
}else{
  out <- data.frame(out,
                    created = x[, created])
}

if (is.null(modified)) {
  out$modified <- as.character("1000-01-01", nrow(out))
}else{
  out <- data.frame(out,
                    modified = x[, modified])
}

keep.columns <- names(x) %in% c("taxonID", 
                                "scientificName", "scientificNameAuthorship", 
                                "acceptedNameUsageID", "taxonomicStatus", 
                                "created", "modified") == FALSE

out <- data.table::data.table(out, x[, keep.columns])

return(out)

}
