#' @rdname annotate_splits
#' @param x  text variable
#' @param file provide the directory path to udpipe model
#' @examples future_lapply(X = corpus_splitted, FUN = annotate_splits, file = ud_model$file, future.seed = TRUE) %
#' @title  split the text document to run the model parallelly 


annotate_splits <- function(x, file, individuals = TRUE) {
  ud_model <- udpipe_load_model(file)
  if (isTRUE(individuals)) {
    x = as.data.table(udpipe_annotate(ud_model,
                                      x = x$content,
                                      doc_id = x$id_doc))}
  else if (isFALSE(individuals)) {
    x = as.data.table(udpipe_annotate(ud_model, x = x$content,
                                      doc_id = x$incident_index))}
  else{ break }
  return(x)
} 


#' @rdname pos_tagging
#' @param df text document object
#' @examples pos_tagging(incindent)
#' @title  part-of-speech at individual level or incident level
#' @details This function requires pre-trianed model (chinese-gsdsimp-ud-2.5-191206.udpipe) which can be downloaded at Pre-trained models: chinese-gsdsimp-ud-2.5-191206.udpipe. 
#' @note This is limited to used for specific data frame from replicaiton dataset


pos_tagging <- function(df, individuals = TRUE) {
  # to evaluate whether the data frame is correct
  if (any(sum(colnames(df) %in% c("id_doc","content", "incident_index"))>2)){ 
    if (isTRUE(individuals)) { 
      msg = "Tokenization is done at individual level"
      annotate_splits <- function(x, file){ 
        ud_model = udpipe_load_model(file)
        x = as.data.table(udpipe_annotate(ud_model, 
                                          x = x$content,
                                          doc_id = x$id_doc))}
      } 
    else if(isFALSE(individuals)) {
      msg = "Tokenization is done at incident level"
      annotate_splits <- function(x, file){ 
        ud_model = udpipe_load_model(file)
        x = as.data.table(udpipe_annotate(ud_model, 
                                          x = x$content,
                                          doc_id = x$incident_index))}}
  } else { stop("Tokenization breaks due to wrong replication data")  }  
  timer_parser <- system.time({
    ud_model <- udpipe_load_model("chinese-gsdsimp-ud-2.5-191206.udpipe")
    doFuture::registerDoFuture()
    future::plan("multisession", workers = detectCores()-1)
    corpus_splitted <- split(df, seq(1, nrow(df), by =  round(nrow(df) / detectCores()-1)))
    ud <- future_lapply(X = corpus_splitted, 
                        FUN = annotate_splits, 
                        file = ud_model$file, future.seed = TRUE) %>%
      rbindlist() 
    ud <- as.data.table(ud)})      
  message(msg)
  print(timer_parser)    
  return(ud)
}



#' @rdname get_dictionary
#' @param folder a data frame or matrix
#' @examples get_dictionary(mat)
#' @title  Generate dictionary object via a matrix or data frame
get_dictionary <- function(df) {
  dic = as.data.frame(t(colnames(df)))
  colnames(dic) = colnames(df)
  dic  = dictionary(as.list(dic))
  return(dic)
}



#' @rdname dtm_wfm
#' @param dtm_object input requires dtm object from Quanteda
#' @examples dtm_wfm(dtm)
#' @title  Turning DTM to WFM
dtm_wfm <- function(dtm_object){
  # transpose & rename of row and colnames
  dataframe = quanteda::convert(dtm_object, to = "data.frame")
  transposed_df = transpose(dataframe)
  transposed_df = transposed_df[-1, ]
  # get row and colnames in order
  colnames(transposed_df) = dataframe$doc_id
  rownames(transposed_df) = colnames(dataframe[-1])
  # transform WTM-formated dataframe from character to numeric form 
  transposed_df[colnames(transposed_df)] = sapply(transposed_df[colnames(transposed_df)], as.numeric)
  # transform into word-frequency-matrix
  output = austin::wfm(transposed_df)
  return(output)
}


#' @rdname create_start
#' @param wfm_matrix wfm object
#' @examples create_start(wfm)
#' @title  create start point for poisIRT() 
create_start <- function(wfm_matrix){
  s = list(alpha ={matrix(rnorm(ncol(wfm_matrix) * 1) * 1, nrow = ncol(wfm_matrix), ncol = 1)}, 
           x = {matrix(rnorm(nrow(wfm_matrix) * 1) * 1, nrow = ncol(wfm_matrix), ncol = 1)},    
           psi = {matrix(rnorm(ncol(wfm_matrix) * 1) * 1, nrow =  nrow(wfm_matrix), ncol = 1)},  
           beta = {matrix(rnorm(ncol(wfm_matrix) * 1) * 1, nrow = nrow(wfm_matrix), ncol = 1)})
  return(s)
}




#' @rdname get_estimates
#' @param df the estimate object from poisIR 
#' @examples get_estimates(poisIRT_object)
#' @title  retrieve estimates from poisIRT class
get_estimates <- function(df){
  if (class(df)[1] =="poisIRT"){
    df <- data.frame(x = df$means$x,
                     sd = sqrt(df$vars$x),
                     id_doc = as.numeric(rownames(df$means$psi)))  %>%
    mutate(lower = x - 1.96*sd, upper = x + 1.96*sd)}
  else{
    stop("David's reminder: This is not poisIRT object, please check it again!!!" ) 
    }
  return(df)
  cat("Estimation from", class(df)[2])
}


#' @rdname to_integer
#' @examples scale_y_continuous(breaks = function(x) to_integer(x, n = 10)) 
#' @title  make float breakpoints to integer
to_integer <- function(x, n = 5) {
  l <- pretty(x, n)
  l[abs(l %% 1) < .Machine$double.eps ^ 0.5] 
}

