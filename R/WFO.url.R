WFO.url <- function(
    WFO.result=NULL, browse=FALSE, browse.rows=c(1:1), ...
)
{
	# Check that data.table is installed
    if (! requireNamespace("data.table")) {stop("Please install the data.table package")}

    # Convert WFO.result to a data.frame if it is a data.table of if it not a data.frame
    if ((data.table::is.data.table(WFO.result)) & (!is.data.frame(WFO.result))) {WFO.result <- data.frame(taxonID = WFO.result)}
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

