.onLoad <- function(libname, pkgname) {
  if (identical(Sys.getenv("CROWDTANGLE_TOKEN"), "")) {
    if (file.exists(".cttoken")) {
      Sys.setenv(CROWDTANGLE_TOKEN = readLines(".cttoken"))
    }
  }
}
