WFO.acceptable.match <- function(
    x, spec.name="spec.name",
    no.vowels=FALSE
)
{
  x$submitted <- x[, spec.name]
  x$matched <- x$scientificName
  x[x$New.accepted == TRUE, "matched"] <- x[x$New.accepted == TRUE, "Old.name"]

  x$submitted <- stringr::str_replace(x$submitted, pattern="um$", replacement="a")
  x$matched <- stringr::str_replace(x$matched, pattern="um$", replacement="a")

  x$submitted <- stringr::str_replace(x$submitted, pattern="us$", replacement="a")
  x$matched <- stringr::str_replace(x$matched, pattern="us$", replacement="a")

  x$submitted <- stringr::str_replace(x$submitted, pattern="-", replacement="")
  x$matched <- stringr::str_replace(x$matched, pattern="-", replacement="")

  x$submitted <- stringr::str_replace_all(x$submitted, pattern="ii", replacement="i")
  x$matched <- stringr::str_replace_all(x$matched, pattern="ii", replacement="i")

  if (no.vowels == TRUE) {
    x$submitted <- stringr::str_replace_all(x$submitted, pattern="[aeiouy]", replacement="")
    x$matched <- stringr::str_replace_all(x$matched, pattern="[aeiouy]", replacement="")
  }

  return(x$submitted == x$matched)

}
