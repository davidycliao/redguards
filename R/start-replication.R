#' @export run_replication
#' @rdname run_replication
#' @param folder the folder name for restoring the relication r scrips
#' @importFrom emIRT poisIRT
#' @importFrom udpipe cooccurrence
#' @importFrom textrank textrank_keywords
#' @importFrom tibble as_tibble 
#' @importFrom purrr map
#' @importFrom parallel detectCores
#' @importFrom foreach foreach
#' @importFrom quanteda dfm docnames
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster 
#' @importFrom igraph graph_from_data_frame
#' @importFrom ggraph ggraph geom_edge_arc label_rect geom_node_label geom_node_text
#' @importFrom ggplot2 aes labs theme
#' @importFrom grid arrow
#' @import dplyr
#' @examples run_replication()
#' @title  Run replication


run_replication <- function(folder = "replication-code"){
  lapply(list.files( "replication-code", full.names = TRUE), source)}



