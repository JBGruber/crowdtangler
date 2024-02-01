# test_that("posts", {
#   expect_equal(
#     {
#       out <- ct_getlists()
#       c(class(out), ncol(out), nrow(out) > 0)
#     },
#     c("tbl_df", "tbl", "data.frame", "3", "TRUE")
#   )
# })
#
# test_that("leaderboard", {
#   expect_equal(
#     {
#       out <- ct_leaderboard(count = 1)
#       c(class(out), ncol(out), nrow(out))
#     },
#     c("tbl_df", "tbl", "data.frame", "29", "1")
#   )
# })
#
# test_that("posts", {
#   expect_equal(
#     {
#       out <- ct_posts(count = 1)
#       c(class(out), ncol(out) >= 34, nrow(out))
#     },
#     c("tbl_df", "tbl", "data.frame", "TRUE", "1")
#   )
# })
