# Test fixtures and shared helpers. Loaded automatically by testthat.

# Source all helper modules under R/ so test_dir() finds them.
# Path is relative to configurator/ (two levels up from tests/testthat/).
helpers <- list.files(
  file.path(dirname(dirname(getwd())), "R"),
  pattern = "\\.R$",
  full.names = TRUE
)
for (f in helpers) source(f, local = FALSE)
