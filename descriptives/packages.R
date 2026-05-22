# descriptives/packages.R
# Install R dependencies for compute_descriptives.R.
# Used by GitHub Actions to populate the package cache.

pkgs <- c("dplyr", "readr", "e1071", "diptest", "jsonlite", "here")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}
