# Load commune centroids once per worker, not per session
commune_centroids <- NULL
try({
  rds_path <- "/srv/shiny-server/all_gadm41_centroids_level2.rds"
  if (file.exists(rds_path)) {
    message("📦 Loading commune centroids from ", rds_path)
    commune_centroids <- readRDS(rds_path)
    message("✅ Loaded commune centroids: ", nrow(commune_centroids), " rows")
  } else {
    warning("⚠️ RDS file not found: ", rds_path)
  }
}, silent = TRUE)

# Make it globally available
options(commune_centroids = commune_centroids)


options(shiny.sanitize.errors = FALSE)
options(shiny.fullstacktrace = TRUE)
options(shiny.error = function() {
  cat(paste(Sys.time(), "ERROR\n",
            paste0(utils::capture.output(traceback()), collapse = "\n"), "\n\n"),
      file = "/srv/shiny-server/policymakers/event_log.txt", append = TRUE)
})

