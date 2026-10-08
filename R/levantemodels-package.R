#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import dplyr
## usethis namespace: end
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("LEVANTE measures are covered under a CC-BY-NC-4.0 license (https://creativecommons.org/licenses/by-nc/4.0/deed.en). Measures adapted from the Rapid Online Assessment of Reading (Language Sounds, Sentence Reading, Word Reading) are covered under a Stanford Academic License (https://github.com/yeatmanlab/roar-mp/blob/main/LICENSE).")
}
