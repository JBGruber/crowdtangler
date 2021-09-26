#' Get lists
#'
#' Get all lists in your account.
#'
#' @param token token.
#'
#' @import httr
#' @export
ct_getlists <- function(token = NULL) {

  base_url <- "https://api.crowdtangle.com/"

  if (is.null(token)) {
    token <- Sys.getenv("CROWDTANGLE_TOKEN")
  }
  if (identical(token, "")) {
    stop("You need a token to continue. See ?ct_auth()")
  }
  url <- modify_url(base_url, path = "lists")
  res <- GET(url, add_headers("x-api-token" = token))
  out <- dplyr::bind_rows(content(res)$result$lists)
  return(out)
}
