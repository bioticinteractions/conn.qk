#' function to integrate db connections
#' @param query The query, as a string
#' @param db The database type: 'redshift' or 'ratt'.
#' @param table The type of data returned. Default is to return object as dataframe.
#'   The options are: 'df' (dataframe) or 'dt' (data.table). More to come.
#' @return these functions will return a query
#' @importFrom RPostgreSQL PostgreSQL
#' @importFrom RMySQL MySQL
#' @importFrom dplyr tbl
#' @importFrom data.table setDT
#' @import DBI
#' @export
# create connection function
conn_qk_1 <- function(query, db, table) {
  if (missing(query)) {
    stop('you need to specify your query')
  }
  if (db == 'ratt') {
    temp_conn = DBI::dbConnect(
      RMySQL::MySQL(),
      user = 'USERNAME',
      password = 'PASSWORD',
      dbname = 'DBNAME',
      host = 'HOST'
    )
  } else if (db == 'redshift') {
    temp_conn =   DBI::dbConnect(
      RPostgreSQL::PostgreSQL(),
      user = 'USERNAME',
      password = 'PASSWORD',
      dbname = 'DBNAME',
      host = 'HOST',
      port = 'PORT'
    )
  } else {
    stop('check your db parameters ', db, ' is not a valid option')
  }

  temp_q = DBI::dbGetQuery(conn = temp_conn, statement = query)

  if (table == 'dt') {
    data.table::setDT(temp_q)
  } else if (missing(table) | table == 'df') {
    temp_q = temp_q
  } else {
    temp_q = temp_q
  }

  return(temp_q)
}