Install_load_package <- function(pkg_name) {
  # Checks to see whether a package is installed and loaded
  # into the current namespace. If not, the package is installed
  # and added to the current namespace.
  #
  # Args:
  #   pkg_name: a character string with the desired package name.
  #
  # Returns:
  #
  if(!require(pkg_name, character.only = TRUE)){install.packages(pkg_name)}
  library(pkg_name, character.only = TRUE)
}