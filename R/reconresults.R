#' @title Compares two SQL resultsets from two database environments
#' @description Takes a SQL statement as an input and executes the SQL in two database environments, then compares the SQL resultset for differences.
#'
#' @param query SQL Query.
#' @param env1 Environment string to the first database.
#' @param env2 Environment string to the second database.
#' @return Returns a tibble of differences
#'
ReconResults <-
  function(query,
           env1,
           env2) {
    x <- OraRun(query, env1)
    y <- OraRun(query, env2)
    if (length(x) != length(y))
      warning("Column count between x and y are not the same")
    if (!identical(colnames(x), colnames(y)))
      warning("Column names between x and y are not the same")
    if (!identical(sapply(x, class), sapply(y, class)))
      warning("Column types between x and y are not the same")

    lhs <- x
    rhs <- y
    lhs$x <- T
    rhs$y <- T
    z <- merge(lhs, rhs, all = T) %>%
      as_data_frame()

    cat(
      sprintf(
        "# x: %s \U00D7 %s, y: %s \U00D7 %s, x \U2229 y : %s \U00D7 %s, x \U22C3 y: %s \U00D7 %s, x \U2212 y: %s \U00D7 %s, y \U2212 x: %s \U00D7 %s\r\n",

        nrow(x),
        length(x),
        nrow(y),
        length(x),
        nrow(dplyr::intersect(x, y)),
        length(dplyr::intersect(x, y)),
        nrow(z),
        length(z) - 2,
        nrow(z[z$y != T, ]),
        length(z[z$y != T, ]) - 2,
        nrow(z[z$x != T, ]),
        length(z[z$x != T, ]) - 2
      )
    )
    z
  }
