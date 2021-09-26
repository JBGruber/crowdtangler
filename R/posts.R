#' Get Posts
#'
#' Retrieves posts from accounts of lists of accounts.
#'
#' @param count The maximum number of posts to return.
#' @param start Start date to create the leaderboard. Format is
#'   “yyyy-mm-ddThh:mm:ss”. Times have to be provided as UTC times. Default is
#'   one day earlier than endDate.
#' @param end The endDate of the leaderboard range. Format is the same as start.
#'   Default is now.
#' @param offset The number of rows to offset (generally used for pagination).
#' @param token token.
#' @param accounts a string corresponding to account handles (ie iamcardib) or
#'   platform IDs (ie 1436859892).
#' @param brandedContent Limits to or excludes posts that have been marked as
#'   Branded Content, either as Publisher or Marketer.
#' @param list The list of the leaderboard to retrieve. Use \link{ct_getlists}
#'   to retrieve a overview of your lists.
#' @param includeHistory Includes timestep data for growth of each post
#'   returned.
#' @param language 2-character code of language.
#' @param minInteractions If set, will exclude posts with total interactions
#'   below this threshold.
#' @param pageAdminTopCountry Not sure.
#' @param searchTerm Returns only posts that match this search term. Terms AND
#'   automatically. Separate with commas for OR, use quotes for phrases. E.g.
#'   CrowdTangle API -> AND. CrowdTangle, API -> OR. "CrowdTangle API" -> AND in
#'   that exact order. You can also use traditional Boolean search with this
#'   parameter.
#' @param sortBy The method by which to filter and order posts: date,
#'   interaction_rate, overperforming, total_interactions, underperforming
#' @param verified Limits to posts where the account has the verified setting
#'   matching the input.
#' @param parse Logical. Should data be parsed?
#' @param data A directory to store json files in.
#'
#' @details For details see https://github.com/CrowdTangle/API/wiki/Posts.
#'
#' @import httr
#' @export
ct_posts <- function(accounts = NULL,
                     brandedContent = NULL,
                     count = 10L,
                     list = NULL,
                     start = NULL,
                     end = NULL,
                     includeHistory = NULL,
                     language = NULL,
                     minInteractions = NULL,
                     offset = NULL,
                     pageAdminTopCountry = NULL,
                     searchTerm = NULL,
                     sortBy = NULL,
                     verified = NULL,
                     parse = TRUE,
                     data = NULL,
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
    if (!grepl("\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}", start)) {
      stop("start must be given in this exact time format: ",
           "yyyy-mm-ddThh:mm:ss")
    }
  }
  if (!is.null(end)) {
    # yyyy-mm-ddThh:mm:ss
    if (!grepl("\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}", start)) {
      stop("end must be given in this exact time format: ",
           "yyyy-mm-ddThh:mm:ss")
    }
  }
  if (is.null(count)) {
    count <- 100L
  }
  pages <- ceiling(count / 100L)
  if (count > 100L) count <- 100L

  if (is.null(data)) {
    data <- tempdir()
    on.exit(unlink(data))
  }

  url <- modify_url(
    base_url,
    path = "posts",
    query = list(
      accounts = accounts,
      brandedContent = brandedContent,
      count = count,
      listIds = list,
      startDate = start,
      endDate = end,
      includeHistory = includeHistory,
      language = language,
      minInteractions = minInteractions,
      offset = offset,
      pageAdminTopCountry = pageAdminTopCountry,
      searchTerm = searchTerm,
      sortBy = sortBy,
      verified = verified
    )
  )

  json_files <- pull_pages(url = url, pages = pages,
                           data = data, token = token)

  if (parse) {
    return(ct_parse_posts(json_files))
  } else {
    invisible(json_files)
  }
}


#' Parse Data from Posts Endpoint
#'
#' Given a file or files from a call to \link{ct_posts}, the function parses
#' the json format into a (mostly) tidy data.frame (tibble). Duplicated entries
#' (resulting from, e.g., multiple downloads of posts from the same list) are
#' automatically removed.
#'
#' @param x files(s) or directory path
#' @param recursive if x is a directory, should the function scan recursively
#'   for more files.
#'
#' @return data.frame (tibble)
#' @export
ct_parse_posts <- function(x, recursive = FALSE) {
  if (dir.exists(x)) {
    x <- list.files(
      path = x,
      pattern = ".json$",
      recursive = recursive,
      full.names = TRUE,
      ignore.case = TRUE
    )
  }
  if (all(file.exists(x))) {
    # also removes entries without results
    json_l <- unlist(purrr::map(x, function(f) {
      out <- jsonlite::read_json(f)
      out[["result"]][["posts"]]
    }), recursive = FALSE)
  } else {
    stop("x should be an existing directory or file(s)")
  }

  pb <- progress::progress_bar$new(
    total = length(json_l),
    format = "parsing [:bar] :percent eta: :eta"
  )

  out <- purrr::map_df(json_l, function(p) {
    pb$tick()
    account <- tibble::as_tibble(
      p$account,
      .name_repair = function(x) paste0("accout_", x)
    )
    statistics <- tibble::as_tibble(
      t(p$statistics),
      .name_repair = function(x) paste0("statistics_", x)
    )

    # needs improvement but does the job
    links <- dplyr::bind_rows(p$expandedLinks)
    if (nrow(links) > 0) {
      links <- tibble::as_tibble(
        purrr::map(links, list),
        .name_repair = function(x) paste0("links_", x)
      )
    } else {
      links <- NULL
    }

    media <- dplyr::bind_rows(p$media)
    if (nrow(media) > 0) {
      media <- tibble::as_tibble(
        purrr::map(media, list),
        .name_repair = function(x) paste0("media_", x)
      )
    } else {
      media <- NULL
    }

    p <- tibble::as_tibble(
      p[setdiff(names(p), c("account", "statistics",
                            "media", "expandedLinks"))]
    )

    return(dplyr::bind_cols(
      account, p, links, media
    ))
  })
  out[!duplicated(out$id), ]
}
