WFO.remember <- function(
    WFO.file=NULL, WFO.data="WFO.data", WFO.pos=1
)
{

    WFO.data.file <- paste(system.file(package="WorldFlora"), "/etc/remember.txt", sep="")
    WFO.data.file <- normalizePath(WFO.data.file)

    if (is.null(WFO.file) == FALSE) {
        data.save <- data.frame(WFO.file=file.path(WFO.file), date=date())
        data.table::fwrite(data.save, file=WFO.data.file, sep="|", row.names=FALSE)
        WFO.file1 <- normalizePath(file.path(WFO.file))
    }else{
        if (file.exists(WFO.data.file) == FALSE) {stop("Location of WFO data was not provided earlier")}
        data.save <- data.table::fread(WFO.data.file, sep="|")
        WFO.file1 <- normalizePath(file.path(as.character(data.save$WFO.file)))
        cat(paste("Data sourced from: ", WFO.file1, " (", data.save$date, ")", "\n", sep=""))
    }

    message(paste("Reading WFO data"))
    assign(WFO.data, data.table::fread(WFO.file1, encoding = "UTF-8"), pos=WFO.pos)

    message(paste("The WFO data is now available from ", WFO.data, sep=""))

}



