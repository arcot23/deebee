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
        "# %s: %s \U00D7 %s, %s: %s \U00D7 %s, %s \U2229 %s : %s \U00D7 %s, %s \U22C3 %s: %s \U00D7 %s, %s \U2212 %s: %s \U00D7 %s, %s \U2212 %s: %s \U00D7 %s\r\n",
        env1,
        nrow(x),
        length(x),
        env2,
        nrow(y),
        length(x),
        env1,
        env2,
        nrow(dplyr::intersect(x, y)),
        length(dplyr::intersect(x, y)),
        env1,
        env2,
        nrow(z),
        length(z) - 2,
        env1,
        env2,
        nrow(z[z$y != T, ]),
        length(z[z$y != T, ]) - 2,
        env2,
        env1,
        nrow(z[z$x != T, ]),
        length(z[z$x != T, ]) - 2
      )
    )
    z
  }
