.onAttach <- function(...) {
      packageStartupMessage("WorldFlora ", utils::packageDescription("WorldFlora", field="Version"),
      ": Use function WFO.match to check plant names; \nFirst you need to download the World Flora Online taxonomic backbone from \nwww.worldfloraonline.org/downloadData")
}

