WFO.url <- function(
    WFO.result=NULL, browse=FALSE, browse.rows=c(1:1), ...
)
{
    if (class(WFO.result) %in% c("data.frame") == FALSE) {WFO.result <- data.frame(taxonID = WFO.result)}
    if (is.factor(WFO.result) == TRUE) {WFO.result <- data.frame(taxonID = WFO.result)}

    result <- as.character(NA, length=nrow(WFO.result))

    for (i in 1:nrow(WFO.result)) {
        if (WFO.result[i, "taxonID"] != "") {
            result[i] <- paste("http://worldfloraonline.org/taxon/", WFO.result[i, "taxonID"], sep="")
        }
    }

    if (browse == TRUE) {
        for (i in browse.rows) {
            if (is.na(result[i]) == FALSE) {utils::browseURL(url=result[i], ...)}
        }
    }

    return(result)
}

