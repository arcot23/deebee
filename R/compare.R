#' @title Compares two SQL resultsets from two database environments
#' @description Takes a SQL statement as an input and executes the SQL in two database environments, then compares the SQL resultset for differences.
#'
#' @param query SQL Query.
#' @param env1 Environment string to the first database.
#' @param env2 Environment string to the second database.
#' @param inXbutNotY If True (default) informs to find differences in X and not in Y. If False infors to find differences in Y and not in X.
#' @return Returns a tibble of differences
#'
CompareOraDataset <-
  function(query,
           env1,
           env2,
           inXbutNotY = T) {
    if (inXbutNotY)
      dplyr::setdiff(OraRun(query, env1),
                     OraRun(query, env2))
      else
        dplyr::setdiff(OraRun(query, env2),
                       OraRun(query, env1))
  }
