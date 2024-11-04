#' Connect to Metrics Database
#'
#' @export
get_metrics_db_conn <- function(env_file = ".env") {
  db_user <- get_env_var("DB_USERNAME", env_file = env_file)
  db_key <- get_env_var("DB_SECRET_KEY")
  db_host <- get_env_var("DB_HOST")

  con <- DBI::dbConnect(RPostgres::Postgres(), host=db_host, dbname="metrics",user=db_user,password=db_key)
  return(con)
}
