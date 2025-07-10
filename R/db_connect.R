#' Connect to Metrics Database
#'
#' @param env_file path to environment file containing keys
#'
#' @export
get_metrics_db_conn <- function(env_file = ".env") {
  db_user <- get_env_var("METRICDB-USERNAME", env_file = env_file)
  db_key <- get_env_var("METRICDB-PASSWORD")
  db_host <- get_env_var("METRICDB-HOST")

  con <- DBI::dbConnect(RPostgres::Postgres(), host=db_host, dbname="metrics",user=db_user,password=db_key)
  return(con)
}
