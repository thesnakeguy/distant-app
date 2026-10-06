source_coop_base <- "https://data.source.coop/scar/"
source_coop_prefix <- "distant/"

source_coop_index <- function(base = source_coop_base,
                              prefix = source_coop_prefix) {
  endpoint <- paste0(base, prefix, "?list-type=2&max-keys=1000")
  keys <- character(0)
  repeat {
    txt <- paste(readLines(endpoint, warn = FALSE), collapse = "\n")
    hits <- regmatches(txt, gregexpr("<Key>[^<]*</Key>", txt))[[1]]
    keys <- c(keys, sub("</Key>", "", sub("<Key>", "", hits, fixed = TRUE),
                        fixed = TRUE))
    token <- regmatches(txt, regexpr("<NextContinuationToken>[^<]*</NextContinuationToken>",
                                     txt))
    if (!length(token)) break
    token <- sub("</NextContinuationToken>", "",
                 sub("<NextContinuationToken>", "", token, fixed = TRUE),
                 fixed = TRUE)
    endpoint <- paste0(base, prefix,
                       "?list-type=2&max-keys=1000&continuation-token=",
                       utils::URLencode(token, reserved = TRUE))
  }
  keys <- keys[grepl("\\.tif$", keys)]
  stats::setNames(paste0(base, keys), basename(keys))
}

layer_url <- function(index, file) {
  url <- unname(index[file])
  if (length(url) != 1 || is.na(url)) NULL else url
}
