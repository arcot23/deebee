#' @title Compares two SQL resultsets from two database environments
#' @description Takes a SQL statement as an input and executes the SQL in two database environments, then compares the SQL resultset for differences.
#'
#' @param query SQL Query.
#' @param env1 Environment string to the first database.
#' @param env2 Environment string to the second database.
#' @return Returns a tibble of differences
#'
CompareOraDataset <-
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

    x$x = T   
    y$y = T   
    z <- merge(x, y, all = T) %>%     
      as_data_frame()       

    cat(sprintf(
      "# x: %s, y: %s, xUy: %s, x-y: %s, y-x: %s;",
      nrow(x),
      nrow(y),
      nrow(z),
      nrow(z[z$y != T,]),
      nrow(z[z$x != T,])
    ))       
    z
  }
