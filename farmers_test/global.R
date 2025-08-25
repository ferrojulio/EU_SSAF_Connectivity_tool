# Load commune centroids once per worker, not per session
# The all_gadm41_centroids_level2.rds file is no longer used as it has been replaced by country-level RDS files.


options(shiny.sanitize.errors = FALSE)
options(shiny.fullstacktrace = TRUE)
options(shiny.error = function() {
  cat(paste(Sys.time(), "ERROR\n",
            paste0(utils::capture.output(traceback()), collapse = "\n"), "\n\n"),
      file = "/srv/shiny-server/policymakers/event_log.txt", append = TRUE)
})

