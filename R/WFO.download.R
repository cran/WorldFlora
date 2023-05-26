WFO.download <- function(
    WFO.url=paste0("https://files.worldfloraonline.org/files/WFO_Backbone/",
                   "_WFOCompleteBackbone/WFO_Backbone.zip"),
    save.dir=getwd(), WFO.remember=TRUE, timeout=500, ...
)
{

    options(timeout = timeout)

    message("Downloading and extracting zip file in ", save.dir)
    save.file <- normalizePath(file.path(paste0(save.dir, "/WFO_Backbone.zip")))
    utils::download.file(WFO.url, destfile=save.file, ...)
# argument exdir added as suggested by Nicolas Casajus 24-MAR-2023    
    utils::unzip(save.file, exdir=save.dir)

    if (WFO.remember == TRUE) {
        WFO.file1 <- paste0(save.dir, "/classification.txt", sep="")
        if (file.exists(WFO.file1) == FALSE) {
          WFO.file1 <- paste0(save.dir, "/classification.csv")
        }
        if (file.exists(WFO.file1) == FALSE) {
          warning("Taxonomic backbone data of 'classification.txt' or 'classification.csv' could not be found")
        }else{
          WFO.remember(WFO.file=WFO.file1)
        }
    }

}
