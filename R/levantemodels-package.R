#' @section Options:
#' - `levantemodels.cache_dir`: directory in which to cache the metadata tables
#'   fetched from `levante_metadata_items` and `levante_metadata_scoring`
#'   (`fetch_corpus_items()`, `fetch_scoring_table()`, etc.), so that later
#'   sessions read them from disk instead of downloading them again. Only
#'   released dataset versions are cached. Defaults to `NULL` (no caching).
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import dplyr
## usethis namespace: end
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("LEVANTE measures are covered under a CC-BY-NC-4.0 license (https://creativecommons.org/licenses/by-nc/4.0/deed.en). Measures adapted from the Rapid Online Assessment of Reading (Language Sounds, Sentence Reading, Word Reading) are covered under a Stanford Academic License (https://github.com/yeatmanlab/roar-mp/blob/main/LICENSE).")
}
