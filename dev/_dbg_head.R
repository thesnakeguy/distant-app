Sys.setenv(DISTANT_APP_DIR = normalizePath("."))
for (f in list.files("R", full.names = TRUE, pattern = "[.]R$")) source(f)
ensure_dirs()
lay <- catalogue()[catalogue()$file == "CR2014-Euphausia_superba_cog.tif", ][1, ]
u <- lay$zip_url
cat("url:", u, "\n")

opts <- rownames(curl::curl_options())
cat("options matching nobody/customrequest/head:",
    paste(intersect(opts, c("nobody", "customrequest", "head")), collapse = ", "), "\n\n")

try_it <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) paste("ERROR:", conditionMessage(e)))
  cat(sprintf("%-30s -> %s\n", label, r))
}
try_it("nobody = TRUE", {
  res <- curl::curl_fetch_memory(u, handle = curl::new_handle(nobody = TRUE))
  sprintf("HTTP %s, %d bytes", attr(res, "status_code"), length(res$content))
})
try_it("nobody = 1L", {
  res <- curl::curl_fetch_memory(u, handle = curl::new_handle(nobody = 1L))
  sprintf("HTTP %s, %d bytes", attr(res, "status_code"), length(res$content))
})
try_it("GET whole body", {
  res <- curl::curl_fetch_memory(u)
  sprintf("HTTP %s, %.0f kB", attr(res, "status_code"), length(res$content) / 1024)
})
