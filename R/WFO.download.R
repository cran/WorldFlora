WFO.download <- function(
    WFO.url="http://104.198.143.165/files/WFO_Backbone/_WFOCompleteBackbone/WFO_Backbone.zip",
    save.dir=getwd(), WFO.remember=TRUE, ...
)
{

    save.file <- file.path(paste(save.dir, "//WFO_Backbone.zip", sep=""))
    utils::download.file(WFO.url, destfile=save.file, ...)
    utils::unzip(save.file)

    if (WFO.remember == TRUE) {
        WFO.file1 <- paste(save.dir, "//classification.txt", sep="")
        WFO.remember(WFO.file=WFO.file1)
    }

}
