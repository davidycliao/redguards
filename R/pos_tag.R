#' First Step: Parsing Simplified Chinese Text with udpipe in R and 2.5 Chinese-GSDSIM Model
#'
#' This function call works on udpipe packages and parallel computing packages in R.
#'
#' 
#'
#' @param dataframe A \code{dataframe} at least should contain \code{text variable} and \code{doc_id}. 
#' 
#' @param self_language_model False 
#'
#'
#' @param model_name You can provide your trained model but you need to locate 
#' 
#' @references 
#'    
#'   
#'   
#' @export

pos_tag = function(dataframe, self_language_model = FALSE, model_name,...) {
  if (isTRUE(self_language_model)){
    model_name = paste(getwd(), model_name, sep = "/")
    ud_model = udpipe_load_model(model_name)
    udpipe_splits = function(x, file) {ud_model = udpipe_load_model(file)
    x <- as.data.table(udpipe_annotate(ud_modesl, 
                                       x = x$content, 
                                       doc_id = x$Groups))
    return(x)}}
  ud_model = udpipe_download_model(language = "chinese-gsdsimp", model_dir = getwd())
  udpipe_splits = function(x, file){
    ud_model = udpipe_load_model(file)
    x = as.data.table(udpipe_annotate(ud_model, x = x$content, doc_id = x$Groups))
    return(x)
  } 
  
  split_to_list = split(dataframe , seq(1, nrow(dataframe), by =  round(nrow(dataframe) / detectCores())))
  doFuture::registerDoFuture()
  future::plan("multiprocess", workers = parallel::detectCores()-1)
  x = future.apply::future_lapply(split_to_list, 
                                  udpipe_splits, 
                                  file = ud_model$file, future.seed = TRUE) %>%
    data.table::rbindlist()
  return(x)
} 




