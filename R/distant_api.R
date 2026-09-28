# R/distant_api.R -- Discovery of everything the app needs to know about the
# DistAnt collection: the minimal metadata table, the per-layer COG URLs on
# Source Cooperative, and the publication level Zenodo archives.
#
# All three are fetched once, normalised into a single lookup table, and cached
# for the lifetime of the R process. No per-session network traffic is needed.

# ---------------------------------------------------------------------------
# HTTP helpers
# ---------------------------------------------------------------------------

http_get <- function(url, query = NULL) {
  con <- url(url, open = "rb")
  on.exit(close(con), add = TRUE)
  rawToChar(readBin(con, "raw", n = 8e6))
}

# Retries with linear backoff. Zenodo and Source Cooperative both rate limit
# aggressively enough that a single transient failure should not kill startup.
http_get_retry <- function(url, query = NULL,
                           retries = config$http_retries,
                           timeout_s = config$http_timeout_s) {
  last <- NULL
  for (i in seq_len(retries)) {
    res <- tryCatch(
      {
        con <- url(url, open = "rb", blocking = TRUE)
        on.exit(close(con), add = TRUE)
        txt <- rawToChar(readBin(con, "raw", n = 8e6))
        Encoding(txt) <- "UTF-8"
        txt
      },
      error = function(e) e
    )
    if (!inherits(res, "error")) return(res)
    last <- res
    Sys.sleep(min(2^i, 8))
  }
  stop(sprintf("GET %s failed after %d attempts: %s", url, retries,
               conditionMessage(last)), call. = FALSE)
}

# ---------------------------------------------------------------------------
# On-disk cache
# ---------------------------------------------------------------------------

#' Persist a lookup result so a transient upstream outage costs one request.
#'
#' Only the small, slow-moving index responses go through here. Rendered
#' previews have their own cache with a size cap, because those are large
#' enough to need pruning.
write_file_cache <- function(name, value) {
  path <- file.path(app_paths$cache, name)
  tryCatch({
    saveRDS(value, paste0(path, ".tmp"))
    file.rename(paste0(path, ".tmp"), path)
  }, error = function(e) {
    warning("Could not write cache ", name, ": ", conditionMessage(e))
  })
  invisible(path)
}

#' Read a cached lookup result, treating anything stale as a miss.
#'
#' An empty or unreadable cache counts as a miss, so a bad write upstream can
#' never leave the app permanently without archive links.
read_file_cache <- function(name, ttl_hours) {
  path <- file.path(app_paths$cache, name)
  if (!file.exists(path)) return(NULL)
  age <- as.numeric(difftime(Sys.time(), file.mtime(path), units = "hours"))
  if (is.na(age) || age > ttl_hours) return(NULL)
  value <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(value) || !is.list(value) || is.null(value$files) ||
      !nrow(value$files)) {
    return(NULL)
  }
  value
}

# ---------------------------------------------------------------------------
# Minimal metadata table
# ---------------------------------------------------------------------------

#' Read a DistAnt metadata.csv into a data frame.
#'
#' The upstream file quotes every field except `future_projections`, which is
#' bare TRUE/FALSE, so types are pinned explicitly rather than guessed.
read_metadata_csv <- function(path_or_url) {
  is_url <- grepl("^https?://", path_or_url)
  con <- if (is_url) url(path_or_url, open = "rb") else file(path_or_url, open = "rb")
  on.exit(close(con), add = TRUE)
  txt <- rawToChar(readBin(con, "raw", n = 8e6))
  Encoding(txt) <- "UTF-8"

  d <- utils::read.csv(
    text = txt,
    stringsAsFactors = FALSE,
    colClasses = "character",
    na.strings = c("NA", "")
  )

  # Coerce the two columns that are not plain strings.
  d$future_projections <- tolower(trimws(d$future_projections)) %in% c("true", "t", "1", "yes")
  num_cols <- c("model_performance", "xmin", "xmax", "ymin", "ymax",
                "x_resolution", "y_resolution")
  for (nm in intersect(num_cols, names(d))) {
    d[[nm]] <- suppressWarnings(as.numeric(d[[nm]]))
  }
  d
}

#' Fetch, dedupe and decorate the layer catalogue.
#'
#' The published table carries 484 rows for 435 layers: seven Krueger et al.
#' files are repeated eight times each, byte for byte. Dropping exact duplicate
#' rows is what makes `id` a usable primary key.
load_layers <- function(metadata_url = config$metadata_csv,
                        local_fallback = config$metadata_csv_local) {
  d <- tryCatch(
    read_metadata_csv(metadata_url),
    error = function(e) {
      if (file.exists(local_fallback)) {
        warning("Falling back to bundled metadata.csv: ", conditionMessage(e))
        read_metadata_csv(local_fallback)
      } else {
        stop(conditionMessage(e), call. = FALSE)
      }
    }
  )

  # The published table carries 484 rows for 435 layers: seven Krueger et al.
  # files are repeated eight times each, byte for byte. Dropping exact
  # duplicate rows is what makes `id` a usable primary key.
  d <- d[!duplicated(d), , drop = FALSE]
  rownames(d) <- NULL
  d
}

# ---------------------------------------------------------------------------
# Source Cooperative S3 index
# ---------------------------------------------------------------------------

#' Pull the text content of every `<tag>...</tag>` in a document.
#'
#' Returns one element per occurrence, in document order, with the tags
#' stripped. A delimiter listing puts one `<Key>` per object, so finding only
#' the first would silently truncate a whole page of results.
xml_attr <- function(x, tag) {
  rx <- paste0("<", tag, ">([^<]*)</", tag, ">")
  hits <- regmatches(x, gregexpr(rx, x, perl = TRUE))[[1]]
  if (!length(hits)) return(character())
  sub(rx, "\\1", hits, perl = TRUE)
}

#' List every object under a bucket prefix, following S3 pagination.
#'
#' `delimiter = "/"` at the top level returns the publication directories as
#' `<Prefix>` entries rather than `<Key>`s; each of those is then listed in
#' full. Fabric-Ruiz alone holds 126 objects, so continuation tokens are
#' required rather than optional.
s3_list <- function(endpoint, bucket, prefix, delimiter = NULL,
                    max_keys = 1000L) {
  q <- list(`list-type` = "2", prefix = prefix,
            `max-keys` = as.character(max_keys))
  if (!is.null(delimiter)) q$delimiter <- delimiter

  acc <- NULL
  repeat {
    txt <- http_get_retry(paste0(
      endpoint, "/", bucket, "?",
      paste(names(q), vapply(q, function(v) utils::URLencode(as.character(v),
                                                              reserved = TRUE),
                             character(1)),
            sep = "=", collapse = "&")))
    page <- data.frame(
      key         = xml_attr(txt, "Key"),
      size        = as.numeric(xml_attr(txt, "Size")),
      etag        = xml_attr(txt, "ETag"),
      modified    = xml_attr(txt, "LastModified"),
      prefix_elem = xml_attr(txt, "Prefix"),
      stringsAsFactors = FALSE
    )
    if (nrow(page)) acc <- rbind(acc, page)

    if (!any(grepl("<IsTruncated>true</IsTruncated>", txt, fixed = TRUE))) break
    tok <- xml_attr(txt, "NextContinuationToken")
    if (!length(tok) || !any(nzchar(tok))) break
    q$`continuation-token` <- tok[1]
  }

  if (is.null(acc)) {
    return(data.frame(key = character(), size = numeric(), etag = character(),
                      modified = character(), prefix_elem = character(),
                      pub_dir = character(), stringsAsFactors = FALSE))
  }
  acc <- acc[!is.na(acc$key) & acc$key != prefix, , drop = FALSE]
  acc$pub_dir <- sub("^[^/]+/", "", acc$key)
  acc$pub_dir <- sub("/[^/]*$", "", acc$pub_dir)
  rownames(acc) <- NULL
  acc
}

#' Build a file -> (COG URL, size, publication directory) lookup.
source_coop_index <- function() {
  top <- s3_list(config$s3_endpoint, config$s3_bucket, config$s3_prefix,
                 delimiter = "/")
  # With a delimiter the directories come back as <Prefix>, not <Key>, and
  # each already carries its trailing slash.
  dirs <- unique(sub(paste0("^", config$s3_prefix), "", top$prefix_elem))
  dirs <- dirs[nzchar(dirs)]

  pages <- lapply(dirs, function(d) {
    s3_list(config$s3_endpoint, config$s3_bucket, paste0(config$s3_prefix, d))
  })

  # Only raster layers are of interest; a handful of sidecar files and an
  # unlisted "confidence" layer live alongside them.
  idx <- do.call(rbind, pages)
  idx <- idx[grepl("\\.tif$", idx$key), , drop = FALSE]
  idx$basename <- basename(idx$key)
  idx$url      <- paste0(config$s3_endpoint, "/", config$s3_bucket, "/", idx$key)
  idx
}

# ---------------------------------------------------------------------------
# Zenodo
# ---------------------------------------------------------------------------

#' Shape a record payload into the table the app needs.
#'
#' Accepts either the API's own hit object or the bundled snapshot in
#' `inst/zenodo_record.json`, which carries the same fields.
as_zenodo_record <- function(x) {
  raw <- x$files %||% list()
  keys  <- vapply(raw, function(f) as.character(f$key %||% ""), character(1))
  sizes <- vapply(raw, function(f) as.numeric(f$size %||% NA_real_), numeric(1))
  rec <- as.integer(x$id)
  # Published date, if the record carries one; used only for the BibTeX year.
  published <- as.character(x$published %||% x$created %||% "")

  list(
    record_id = rec,
    record_url = sprintf("%s/records/%d", config$zenodo_api, rec),
    doi     = as.character(x$doi %||% sprintf("10.5281/zenodo.%d", rec)),
    landing = as.character(x$landing %||% sprintf("https://doi.org/10.5281/zenodo.%d", rec)),
    published = published,
    files   = data.frame(
      key   = keys,
      zip   = grepl("\\.zip$", keys),
      size  = sizes,
      # The API's self link points at /api/records/...; users should land on
      # the human-facing /records/... page so the browser downloads the file
      # rather than showing JSON.
      url   = sprintf("https://zenodo.org/records/%d/files/%s/content", rec, keys),
      stringsAsFactors = FALSE
    )
  )
}

#' Read the record snapshot shipped with the app.
#'
#' Zenodo sits behind a CDN that intermittently refuses automated clients, and
#' the institutional server this app is meant for may also sit behind a
#' firewall. A snapshot of the last known record keeps the archive links
#' working in those cases instead of silently dropping them.
zenodo_snapshot <- function() {
  path <- file.path(app_dir, config$zenodo_snapshot)
  if (!file.exists(path)) return(NULL)
  tryCatch(
    as_zenodo_record(jsonlite::fromJSON(path, simplifyVector = FALSE)),
    error = function(e) NULL
  )
}

#' Resolve the concept DOI to the newest concrete record.
#'
#' The concept DOI itself is not addressable by the files API, so the current
#' version's numeric record id has to be looked up. Resolution is best effort:
#' live API, then a cached response, then the bundled snapshot. A failure at
#' every level is reported as a warning and leaves the archive links empty
#' rather than taking the app down with it.
zenodo_latest_record <- function(force = FALSE) {
  if (!force) {
    cached <- read_file_cache(config$zenodo_cache, config$zenodo_cache_ttl_hours)
    if (!is.null(cached)) return(cached)
  }

  concept <- sub("^10\\.5281/zenodo\\.", "", config$zenodo_concept_doi)
  rec <- tryCatch({
    txt <- http_get_retry(sprintf(
      "%s/records?q=conceptrecid:%s&sort=-mostrecent&size=1",
      config$zenodo_api, concept))
    res <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
    # Zenodo has shipped both a flat `hits: [...]` array and a wrapped
    # `hits: {hits: [...], total: n}` envelope. Accept either, and insist on a
    # usable record so a schema change degrades to the snapshot rather than
    # silently resolving to nothing.
    pool <- res$hits
    if (is.list(pool) && !is.null(pool$hits)) pool <- pool$hits
    hit <- pool[[1]]
    if (is.null(hit$id) || !length(hit$files)) {
      stop("Zenodo record payload carried no files", call. = FALSE)
    }
    hit$doi     <- sprintf("10.5281/zenodo.%s", hit$id)
    hit$landing <- sprintf("https://doi.org/10.5281/zenodo.%s", hit$id)
    as_zenodo_record(hit)
  }, error = function(e) e)

  if (inherits(rec, "error")) {
    warning("Zenodo record lookup failed (", conditionMessage(rec),
            "); falling back to cached/bundled snapshot.", call. = FALSE)
    return(zenodo_snapshot())
  }

  write_file_cache(config$zenodo_cache, rec)
  rec
}

#' Map publication directory -> Zenodo archive.
#'
#' Source Cooperative directories and Zenodo archives share the same slug
#' (`fabri-ruiz_et_al-2020` / `fabri-ruiz_et_al-2020.zip`), so no fuzzy citation
#' matching is needed. Publications that have not been archived yet simply do
#' not appear, and the UI reports that instead of offering a dead link.
#' The parts of a Zenodo record the app shows or cites.
#'
#' The whole record is carried, not just the handful of fields one call site
#' happened to need: `published` is where the citation's year comes from, so a
#' narrowed list silently dated every citation "this year". `id` is kept as an
#' alias for `record_id` because that was the name this attribute used to have.
zenodo_record_summary <- function(zrec) {
  if (is.null(zrec)) {
    return(list(id = NA_integer_, doi = config$zenodo_concept_doi,
                landing = config$zenodo_concept_url, published = NA_character_))
  }
  c(list(id = zrec$record_id, landing = zrec$landing), zrec)
}

zenodo_archives <- function(zrec = zenodo_latest_record()) {
  empty <- data.frame(
    pub_dir = character(0), key = character(0),
    size = numeric(0), url = character(0),
    stringsAsFactors = FALSE
  )
  attr(empty, "record") <- zenodo_record_summary(NULL)
  if (is.null(zrec)) return(empty)

  z <- zrec$files[zrec$files$zip, , drop = FALSE]
  if (!nrow(z)) {
    attr(empty, "record") <- zenodo_record_summary(zrec)
    return(empty)
  }
  z$pub_dir <- sub("\\.zip$", "", z$key)
  z <- z[, c("pub_dir", "key", "size", "url")]
  attr(z, "record") <- zenodo_record_summary(zrec)
  z
}

# ---------------------------------------------------------------------------
# Assembly
# ---------------------------------------------------------------------------

#' Recover the publication a layer belongs to when it has no COG of its own.
#'
#' Layer ids are prefixed with the publication's author initials and year
#' (`Wo2023-Multiple_Antarctic_fishes_night`) and often name the authors
#' outright (`Gr2025-green_snow_algae`). Nineteen layers are described in the
#' metadata but were never published as standalone files, so the bucket join
#' cannot reach them and the Zenodo fallback needs a second route.
#'
#' The id prefix is resolved once per publication and shared by every layer
#' carrying it, so `Gr2025-red_snow_algae` inherits `green_et_al-2021` from its
#' sibling `Gr2025-green_snow_algae` rather than having to carry the surname
#' itself. Two signals are accepted:
#'
#'   1. the publication's author surname appears as a whole token in one of the
#'      layer ids, and
#'   2. the id's initials match publication directories whose name, where it
#'      carries a year, was published in that same year.
#'
#' Either signal has to narrow the field to a single directory. Ambiguous or
#' unmatched publications stay NA, and the UI says so rather than offering an
#' archive from the wrong publication.
infer_pub_dir <- function(layers, scoop) {
  hit <- match(layers$file, scoop$basename)
  known <- scoop$pub_dir[hit]

  dirs <- sort(unique(known[!is.na(known)]))

  # Slugs are `<surname><_et_al>[-]<year>`; the surname itself can contain a
  # separator (`el_gabbas`, `cuzin-roudy`, `pinkerton_hayward`).
  surname <- sub("[_-](et_al)?[_-]?[12][0-9]{3}$", "", dirs)
  d_init <- tolower(substr(surname, 1, 2))
  d_year <- rep(NA_character_, length(dirs))
  has_year <- grepl("[12][0-9]{3}", dirs)
  d_year[has_year] <- regmatches(dirs[has_year], regexpr("[12][0-9]{3}", dirs[has_year]))

  code <- sub("^(..[0-9]{4}).*$", "\\1", layers$id)
  out <- known

  for (cd in unique(code[is.na(out)])) {
    rows <- which(is.na(out) & code == cd)
    yr <- substr(cd, 3, 6)

    # 1. Surname as a whole token in any id sharing this publication code. The
    #    boundary matters: Woods' `Electrona` layers contain "el", which would
    #    otherwise match El Gabbas.
    cand <- which(vapply(surname, function(a) {
      nchar(a) >= 4 && any(grepl(
        paste0("(^|[-_])", a, "($|[-_])"), layers$id[rows]))
    }, logical(1)))

    # 2. Initials, with a disagreeing year treated as a hard rejection. Chown
    #    et al. 2012 is the only directory starting "ch", but Chaabani et al.
    #    2019 is a different publication and must not borrow its zip.
    if (length(cand) != 1L) {
      cand <- which(d_init == tolower(substr(cd, 1, 2)))
      cand <- cand[is.na(d_year[cand]) | d_year[cand] == yr]
    }

    if (length(cand) == 1L) out[rows] <- dirs[cand]
  }
  out
}

#' Build the single table the rest of the app queries.
#'
#' Returns one row per layer, carrying its metadata plus every download route
#' that is known to exist. `cog_url` is NA for layers that are described in the
#' metadata but are not published as individual COGs; `zip_url` is NA when no
#' Zenodo archive covers them either.
build_catalogue <- function() {
  layers <- load_layers()

  scoop <- source_coop_index()
  zen   <- zenodo_archives()

  hit <- match(layers$file, scoop$basename)
  layers$cog_url  <- scoop$url[hit]
  layers$cog_size <- scoop$size[hit]
  layers$etag     <- scoop$etag[hit]
  layers$pub_dir  <- infer_pub_dir(layers, scoop)

  zhit <- match(layers$pub_dir, zen$pub_dir)
  layers$zip_url  <- zen$url[zhit]
  layers$zip_size <- zen$size[zhit]
  layers$zip_name <- zen$key[zhit]

  layers$cog_size[is.na(layers$cog_size)] <- NA_real_
  layers$aux_xml_url <- ifelse(
    is.na(layers$cog_url), NA_character_,
    paste0(layers$cog_url, ".aux.xml")
  )

  attr(layers, "zenodo") <- attr(zen, "record")
  layers
}

# ---------------------------------------------------------------------------
# Presentation helpers
# ---------------------------------------------------------------------------

#' Compact label for a long citation string.
#'
#' Reference entries are full citations ("Chaabani S, Lopez-Gonzalez PJ, ...
#' Jerosch K (2019) Ecological niche modelling of ..."). Two are bare URLs.
#' Both shapes are reduced to something that fits a dropdown, and the year is
#' looked for anywhere in the string because it is not always at the front.
short_reference <- function(x) {
  vapply(x, function(s) {
    if (is.na(s) || !nzchar(trimws(s))) return("(unspecified)")
    s <- trimws(s)
    if (grepl("^https?://", s)) return(sub("^https?://([^/]+).*$", "\\1", s))

    m <- regexpr("\\([12][0-9]{3}\\)", s)
    if (m > 0) {
      authors <- trimws(substr(s, 1L, m - 1L))
      authors <- gsub("\\s+", " ", sub("[,;]\\s*$", "", authors))
      n <- length(trimws(strsplit(authors, ",", fixed = TRUE)[[1]]))
      # Ten authors is a lot to read in a dropdown; "et al." carries the same
      # information for the purpose of telling two references apart.
      first <- trimws(strsplit(authors, ",", fixed = TRUE)[[1]])[1]
      return(paste0(if (n > 3) paste0(first, " et al.") else authors, " ",
                    substr(s, m, m + 5L)))
    }
    if (nchar(s) > 48) paste0(substr(s, 1L, 47L), "\u2026") else s
  }, character(1), USE.NAMES = FALSE)
}

#' A BibTeX entry for citing the collection.
collection_bibtex <- function(zen = attr(catalogue(), "zenodo")) {
  # Cite the *concept* DOI, not the version DOI: the concept always resolves to
  # whatever is current, so a citation taken today keeps working next year.
  version <- sub("^10\\.5281/zenodo\\.", "", zen$doi %||% "")
  year    <- if (grepl("^[0-9]{4}", zen$published %||% "")) {
    substr(zen$published, 1L, 4L)
  } else {
    format(Sys.Date(), "%Y")
  }
  sprintf(
    paste0("@misc{SCAR_DistAnt_%s,\n",
           "  author       = {Plasman, Charlie and Van de Putte, Anton and Krueger, Lucas and Merkel, Benjamin},\n",
           "  title        = {SCAR DistAnt Ecological Model Output Repository},\n",
           "  year         = {%s},\n",
           "  doi          = {%s},\n",
           "  url          = {%s},\n",
           "  note         = {Version %s}\n",
           "}"),
    version, year, config$zenodo_concept_doi, config$zenodo_concept_url, version
  )
}

human_size <- function(bytes) {
  ifelse(
    is.na(bytes), NA_character_,
    ifelse(bytes < 1024, sprintf("%d B", bytes),
    ifelse(bytes < 1024^2, sprintf("%.0f kB", bytes / 1024),
    ifelse(bytes < 1024^3, sprintf("%.1f MB", bytes / 1024^2),
                      sprintf("%.2f GB", bytes / 1024^3))))
  )
}

# Backends ------------------------------------------------------------------

#' Process-wide catalogue, fetched at most once per R session.
catalogue <- local({
  cache <- NULL
  function(force = FALSE) {
    if (is.null(cache) || force) {
      init_gdal()
      ensure_dirs()
      cache <<- build_catalogue()
    }
    cache
  }
})

reset_catalogue_cache <- function() {
  catalogue(force = TRUE)
  invisible(TRUE)
}
