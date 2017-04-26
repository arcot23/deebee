library(RODBC)


#' @title Runs a SQL query using ODBC driver
#' @description Runs a SQL query in a database using the ODBC driver and returns a resultset. Before hand a DSN has to be created
#' under Control Panel -> Administrative tools.
#' @param query SQL Query to execute.
#' @param dsn Data source name.
#' @return SQL resultset in a dataframe
#' @examples
#' ODBCRun("SELECT * FROM ALL_TABLES", "myDSN")
ODBCRun <- function (query, dsn = "mydsn") {
  #open the connection
  myconn <- RODBC::odbcConnect(dsn)
  #dataset <- sqlFetch(myconn, "seg_accounts_attr")

  #fetch the data
  dataset <- RODBC::sqlQuery(myconn, query)

  #close the connection
  RODBC::close(myconn)

  #return the dataset
  dataset
}
