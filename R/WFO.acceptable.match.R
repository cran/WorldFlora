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

  x$submitted <- stringr::str_replace(x$submitted, pattern="is$", replacement="e")
  x$matched <- stringr::str_replace(x$matched, pattern="is$", replacement="e")

  x$submitted <- stringr::str_replace(x$submitted, pattern="-", replacement="")
  x$matched <- stringr::str_replace(x$matched, pattern="-", replacement="")

  x$submitted <- stringr::str_replace(x$submitted, pattern=intToUtf8(215), replacement="")
  x$matched <- stringr::str_replace(x$matched, pattern=intToUtf8(215), replacement="")

  x$submitted <- stringr::str_replace_all(x$submitted, pattern="j", replacement="i")
  x$matched <- stringr::str_replace_all(x$matched, pattern="j", replacement="i")

  x$submitted <- stringr::str_replace_all(x$submitted, pattern="ii", replacement="i")
  x$matched <- stringr::str_replace_all(x$matched, pattern="ii", replacement="i")

  x$submitted <- stringr::str_replace_all(x$submitted, pattern="tt", replacement="t")
  x$matched <- stringr::str_replace_all(x$matched, pattern="tt", replacement="t")

  x$submitted <- stringr::str_replace_all(x$submitted, pattern="ll", replacement="l")
  x$matched <- stringr::str_replace_all(x$matched, pattern="ll", replacement="l")

  x$submitted <- stringr::str_replace_all(x$submitted, pattern="rr", replacement="r")
  x$matched <- stringr::str_replace_all(x$matched, pattern="rr", replacement="r")

  x$submitted <- stringr::str_replace_all(x$submitted, pattern="mm", replacement="m")
  x$matched <- stringr::str_replace_all(x$matched, pattern="mm", replacement="m")

  x$submitted <- stringr::str_replace_all(x$submitted, pattern="nn", replacement="n")
  x$matched <- stringr::str_replace_all(x$matched, pattern="nn", replacement="n")

  x$submitted <- stringr::str_replace_all(x$submitted, pattern="ff", replacement="f")
  x$matched <- stringr::str_replace_all(x$matched, pattern="ff", replacement="f")

  x$submitted <- stringr::str_replace_all(x$submitted, pattern="hh", replacement="h")
  x$matched <- stringr::str_replace_all(x$matched, pattern="hh", replacement="h")

  x$submitted <- stringr::str_replace_all(x$submitted, pattern="dd", replacement="d")
  x$matched <- stringr::str_replace_all(x$matched, pattern="dd", replacement="d")

  x$submitted <- stringr::str_replace_all(x$submitted, pattern="dt", replacement="d")
  x$matched <- stringr::str_replace_all(x$matched, pattern="dt", replacement="d")

  if (no.vowels == TRUE) {
    x$submitted <- stringr::str_replace_all(x$submitted, pattern="[aeiouy]", replacement="")
    x$matched <- stringr::str_replace_all(x$matched, pattern="[aeiouy]", replacement="")
  }

  return(x$submitted == x$matched)

}
