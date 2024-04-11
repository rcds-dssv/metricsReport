close_if_open <- function(conn) {
  tryCatch({
    close(conn)
  },
  error = function(e) {
    invisible()
  })
}
