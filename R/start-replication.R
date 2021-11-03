#' @rdname run_replication
#' @param folder the folder name for restoring the relication r scrips
#' @examples run_replication()
#' @title  Run Repl


run_replication <- function(folder = "replication-code"){
  list.files(folder, full.names = TRUE) %>%
  map(source)}