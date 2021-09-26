#' Helper function to handle authentification.
#'
#' @param token token
#' @importFrom utils menu
#' @export
ct_auth <- function(token = NULL) {

  oldtok <- Sys.getenv("CROWDTANGLE_TOKEN")

  if (identical(oldtok, "")) {

    c <- menu(title = "Where should I store your token between sessions?",
              choices = c("Nowhere (ask each time)",
                          "As environment variables (in '~/.Renviron'",
                          "In the working directory"))

    if (identical(c, 2L)) {
      l <- readLines("~/.Renviron")
      writeLines(c(l, paste0("CROWDTANGLE_TOKEN=\"",
                             token, "\"")), "~/.Renviron")
    } else if (identical(c, 3L)) {
      writeLines(token, ".cttoken")
      Sys.chmod(".cttoken", mode = "0400")
    }

  } else if (!identical(oldtok, token)) {

    c <- menu(title = "Replace saved old token?",
              choices = c("Yes", "No"))

    if (identical(c, 2L)) {
      token <- oldtok
    } else {
      if (file.exists(".cttoken")) {
        writeLines(token, ".cttoken")
        Sys.chmod(".cttoken", mode = "0400")
      } else {
        l <- readLines("~/.Renviron")
        l[grepl("CROWDTANGLE_TOKEN", l)] <-
          paste0("CROWDTANGLE_TOKEN=\"", token, "\"")
        writeLines(l, "~/.Renviron")
      }
    }
  }
  Sys.setenv(CROWDTANGLE_TOKEN = token)

  class(token) <- "ct_token"
  return(token)
}
