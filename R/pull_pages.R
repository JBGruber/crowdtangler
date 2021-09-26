#' Internal function to iterate over pages
#' @import httr
#' @noRd
pull_pages <- function(url, pages, token, data) {

  json_files <- character()
  i <- 1
  if (!dir.exists(data)) {
    dir.create(data)
  }

  while (identical(length(url), 1L)) {

    res <- GET(url, add_headers("x-api-token" = token))

    file <- paste0(
      data, "/ct_pull",
      gsub(":|\\s", "_", Sys.time()),
      ".json"
    )
    writeLines(content(res, "text"), file, useBytes = TRUE)
    json_files <- c(json_files, file)

    cont <- content(res)

    if (!is.null(cont[["message"]]) &
        identical(cont[["code"]], 32L)) {
      warning(cont[["message"]])
    }
    if (identical(cont[["code"]], 32L)) {
      message("Rate limit reached. Waiting 1 minute...")
      Sys.sleep(61)
    } else {
      message("Pulled page ", i, "...")
      url <- cont$result$pagination[["nextPage"]]
      i <- i + 1
      pages <- pages - 1
    }
    if (identical(pages, 0)) url <- character()
  }

  return(json_files)
}
