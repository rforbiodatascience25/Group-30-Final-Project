# cleanup.R
library(here)
library(fs)

# Find alle HTML-filer i R-mappen
html_files <- dir_ls(here("R"), glob = "*.html")

# Flyt hver HTML-fil til results-mappen
for (f in html_files) {
  dest <- here("results", path_file(f))
  file_move(f, dest)
}

