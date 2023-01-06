.onAttach <- function(...) {
      packageStartupMessage("WorldFlora ", utils::packageDescription("WorldFlora", field="Version"),
      ": Use function WFO.match or WFO.match.fuzzyjoin to check plant names;
      \nFirst you need to download and unzip the World Flora Online taxonomic backbone from \nwww.worldfloraonline.org/downloadData;
      \nUse functions WFO.download and WFO.remember to download and reload the backbone data;
      \nPackage RcmdrPlugin.WorldFlora provides a graphical user interface for this package.")
}

