#' Get Leaderboard
#'
#' Retrieves leaderboard data for a certain list or set of accounts.
#'
#' @param ids A list of CrowdTangle accountIds to retrieve leaderboard data for.
#'   These should be provided comma-separated. This and listId are mutually
#'   exclusive; if both are sent, accountIds will be preferred.
#' @param count The number of accounts to return on the leaderboard. Default is
#'   50.
#' @param list The list of the leaderboard to retrieve. Use \link{ct_getlists} to
#'   retrieve a overview of your lists.
#' @param start Start date to create the leaderboard. Format is
#'   “yyyy-mm-ddThh:mm:ss”. Times have to be provided as UTC times. Default is
#'   one day earlier than endDate.
#' @param end The endDate of the leaderboard range. Format is the same as start.
#'   Default is now.
#' @param offset The number of rows to offset (generally used for pagination).
#' @param order "asc" or "desc".
#' @param sort "total_interactions" or "interaction_rate". Default is
#'   "total_interactions".
#' @param token token.
#'
#' @details For details see https://github.com/CrowdTangle/API/wiki/Leaderboard.
#'
#' @import httr
#' @export
ct_leaderboard <- function(ids = NULL,
                           count = NULL,
                           list = NULL,
                           start = NULL,
                           end = NULL,
                           offset = NULL,
                           order = NULL,
                           sort = NULL,
                           token = NULL) {

  base_url <- "https://api.crowdtangle.com/"

  if (!is.null(token)) {
    warning("You provided a token to this function directly. ",
            " Saving it in a script might be insecure. ",
            "Consider using ct_auth() instead.")
  }

  if (is.null(token)) {
    token <- Sys.getenv("CROWDTANGLE_TOKEN")
  }

  if (identical(token, "")) {
    stop("You need a token to continue. See ?ct_auth()")
  }

  if (!is.null(start)) {
    # yyyy-mm-ddThh:mm:ss
  }
  if (!is.null(start)) {
    # yyyy-mm-ddThh:mm:ss
  }
  url <- modify_url(
    base_url,
    path = "leaderboard",
    query = list(
      accountIds = ids,
      count = count,
      endDate = end,
      listId = list,
      offset = offset,
      orderBy = order,
      sortBy	= sort,
      startDate = start
    )
  )
  res <- GET(url, add_headers("x-api-token" = token))
  cont <- content(res)
  if (!is.null(cont[["message"]])) {
    warning(cont[["message"]])
  }
  out <- cont$result$accountStatistics
  out <- purrr::map_df(out, function(a) {
    account <- tibble::as_tibble(
      a$account,
      .name_repair = function(x) paste0("accout_", x)
    )
    summary <- tibble::as_tibble(
      a$summary,
      .name_repair = function(x) paste0("summary_", x)
    )
    subscriber <- tibble::as_tibble(
      a$subscriberData,
      .name_repair = function(x) paste0("subscriber_", x)
    )
    dplyr::bind_cols(account, summary, subscriber)
  })
  attr(out, "response") <- res
  return(out)
}
