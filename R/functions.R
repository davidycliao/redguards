#' redgaurds: A package for replicating  estimates and findings in the article of Factionalism and the 
#' Red Guards under Mao's China: Ideal Point Estimation Using Text Data.
#' 
#' @docType package
#' @name redgaurds

#' @export .onAttach 
.onAttach <- function(...) {
  crearted_date <- date()
  x <- regexpr("[0-9]{4}", crearted_date)
  this.year <- substr(crearted_date, x[1], x[1] + attr(x, "match.length") - 1)
  packageStartupMessage("## David Yen-Chieh Liao, Yi-Nung Tsai, Daniel Tene and Dechun Zhang")
  packageStartupMessage("## https://github.com/davidycliao/redguards")
  packageStartupMessage("## 2020 - ", this.year)
  packageStartupMessage("## redgaurds: A package for replicating estimates and findings in the article of")
  packageStartupMessage("## “Factionalism and the Red Guards under Mao's China: Ideal Point Estimation Using Text Data.”")
}


#' @export pos_tagging
#' @rdname pos_tagging
#' @param df text document object
#' @importFrom doFuture registerDoFuture
#' @importFrom data.table rbindlist 
#' @importFrom future.apply future_lapply  
#' @title  Part-of-speech at individual level or incident level
#' @details This function requires pre-trianed model (chinese-gsdsimp-ud-2.5-191206.udpipe) which can be downloaded at Pre-trained models: chinese-gsdsimp-ud-2.5-191206.udpipe. 
#' @note This is limited to use for specific data frame from replication dataset
pos_tagging <- function(df, individuals = TRUE) {
  # to evaluate whether the data frame is correct
  if (any(sum(colnames(df) %in% c("id_doc","content", "incident_index"))>2)){ 
    if (isTRUE(individuals)) { 
    msg = "Tokenization is done at individual level"
    annotate_splits <- function(x, file){ 
    ud_model =  udpipe::udpipe_load_model(file)
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


#' @export get_dictionary
#' @rdname get_dictionary
#' @param folder a data frame or matrix
#' @importFrom quanteda dictionary
#' @title  Generate dictionary object via a matrix or data frame
get_dictionary <- function(df) {
  dic = as.data.frame(t(colnames(df)))
  colnames(dic) = colnames(df)
  dic  = quanteda::dictionary(as.list(dic))
  return(dic)
  }


#' @export dtm_wfm
#' @rdname dtm_wfm
#' @param x input requires dtm object from quanteda or tm pacakge
#' @importFrom austin wfm 
#' @importFrom quanteda convert
#' @importFrom data.table transpose
#' @title  Turning DTM to WFM
dtm_wfm <- function(x){
  if (class(x)[1] == "dfm") {
    # transpose & rename of row and columns
    dataframe = quanteda::convert(x, to = "data.frame")
    transposed_df = transpose(dataframe)
    transposed_df = transposed_df[-1, ]
    # get row and colnames in order
    colnames(transposed_df) = dataframe$doc_id
    rownames(transposed_df) = colnames(dataframe[-1])
    # transform WTM-formated dataframe from character to numeric form 
    transposed_df[colnames(transposed_df)] = sapply(transposed_df[colnames(transposed_df)], as.numeric)
    # transform into word-frequency-matrix
    output = austin::wfm(transposed_df)}
  else{stop("This is not dtm object made by quanteda, make sure you put document term matrix in dtm_wfm()" )}
  class(output) = c("wfm", "redguards")
  return(output)
}


#' @export create_start
#' @rdname create_start
#' @param x wfm object
#' @param set.seed default is 1234 
#' @param verbose default is FALSE
#' @title  create start point for poisIRT() 
#' @importFrom stats runif

create_start <- function(x, set.seed = 1234, verbose = FALSE){
  set.seed(set.seed)
  if(isTRUE(verbose)) {
    cat("seed is set to", set.seed, "\n")}
  if (class(x)[1] == "wfm"){
    set.seed(set.seed)
    J = nrow(x)
    K = ncol(x)
    s = list(alpha = matrix(stats::runif(K, -1, 1)),
             x =  matrix(stats::runif(K, -1, 1)),
             psi = matrix(stats::runif(J, 0, 1)),
             beta = matrix(stats::runif(J, 0, 1))) }
  else { stop("This is not wfm object, please make sure you put document term matrix in create_start()" ) }
  return(s)
  }

# create_start <- function(wfm_matrix){
#   s = list(alpha ={matrix(rnorm(ncol(wfm_matrix) * 1) * 1, nrow = ncol(wfm_matrix), ncol = 1)}, 
#            x = {matrix(rnorm(nrow(wfm_matrix) * 1) * 1, nrow = ncol(wfm_matrix), ncol = 1)},    
#            psi = {matrix(rnorm(ncol(wfm_matrix) * 1) * 1, nrow =  nrow(wfm_matrix), ncol = 1)},  
#            beta = {matrix(rnorm(ncol(wfm_matrix) * 1) * 1, nrow = nrow(wfm_matrix), ncol = 1)})
#   return(s)
# }





#' @export create_prior
#' @rdname create_prior
#' @param  mu prior mean for x_i, β_j, ψ_k and ψ_k. 
#' @param sigma2 prior variance for x_i, β_j, ψ_k and ψ_k. 
#' @examples create_prior(mu = 0, sigma2 = 100)
#' @title  create prior variance and meean  for poisIRT
create_prior <- function(mu = 0, sigma2 = 100,...){
  p = list(psi = list(mu = mu,  sigma2 = sigma2),      
           psi = list(mu = mu,  sigma2 = sigma2),         
           alpha = list(mu = mu, sigma2 = sigma2),    
           beta = list(mu = mu, sigma2 = sigma2),
           x = list(mu = mu, sigma2 = sigma2)  )   
  return(p)
}


#' @export get_estimates
#' @rdname get_estimates
#' @param emIRT.out the estimate object from poisIR 
#' @title  retrieve estimates from poisIRT class
get_estimates <- function(emIRT.out, id_doc =id_doc ){
  if (class(emIRT.out)[1] =="poisIRT"){
    df = data.frame(id_doc = as.numeric(id_doc),
                    x = emIRT.out$means$x,
                    sd = sqrt(emIRT.out$vars$x),
                    lower = emIRT.out$means$x - 1.96*sqrt(emIRT.out$vars$x),
                    upper = emIRT.out$means$x + 1.96*sqrt(emIRT.out$vars$x) ) 
    # cat("Estimated object is", class(emIRT.out)[1], "\n\t")
  }
  else{
    stop("This is not poisIRT, please check it again!!!" ) 
  }
  return(df)
}

#' @export get_wordfeatures
#' @rdname get_wordfeatures
#' @param df the estimate object from poisIR 
#' @title  retrieve word features from poisIRT class
get_wordfeatures <- function(df,...){
  if (class(df)[1] == "poisIRT"){
    df <-  data.frame(feature = rownames(df$means$beta), 
                      psi = df$means$alpha,
                      beta = df$means$beta,
                      alpha = df$means$alpha) }
  else{
    stop("This is not poisIRT object, please check it again!!!" ) 
  }
  return(df)
  cat("Estimation from", class(df)[2], "via emIRT.")
}


#' @export get_keywords
#' @rdname get_keywords
#' @param df the estimate object should be textrank_keywords class from textrank  
#' @importFrom utils head
#' @importFrom utils tail
#' @title  retrieve the most/less frequent keywords from textrank_keywords class
get_keywords <- function(df, n = 10, head = TRUE, ...){
  if (class(df)[1] == "textrank_keywords"){
    if(isTRUE(head)) {
      key_words = head(df$keywords[nchar(df$keywords$keyword)>2,], n = n)
      }
    else if (isFALSE(head)) {    
      key_words = tail(df$keywords[nchar(df$keywords$keyword)>2,], n = n)
      } 
    }
  else{
    stop("This is not textrank_keywords object, please check it again!!!" ) 
  }
  return(key_words)
  print("Estimates from", class(df)[2], "via textrank")
}


#' @export to_integer
#' @rdname to_integer
#' @title  make float breakpoints to integer
to_integer <- function(x, n = 5) {
  l <- pretty(x, n)
  l[abs(l %% 1) < .Machine$double.eps ^ 0.5] 
}


#' Pre-processed textual files parsed on CoNLL-U format 
#'
#' A dataset containing the prices and other attributes of almost 54,000
#'  diamonds. The variables are as follows:
#'
# \itemize{
#   \item doc_id. 
#   \item paragraph_id. 
#   \item sentence_id.  
#   \item token_id.
#   \item token.
#   \item lemma.
#   \item upos.
#   \item xpos.
#   \item feats. 
#   \item head_token_id
#   \item dep_rel
#   \item misc 
#   \item keyword_doc_id
# }
#'
#' @docType data
#' @keywords CoNLL 
#' @name conll
#' @usage data(conll)
#' @format A data frame with 831,639 rows and 14 variables
NULL

#' @docType data
#' @keywords dfm_individual_list  
#' @name dfm_individual_list
#' @usage data(dfm_individual_list)
#' @format A data frame with 831,639 rows and 14 variables
NULL

#' @docType data
#' @keywords dfm_list  
#' @name dfm_list
#' @usage data(dfm_list)
#' @format A data frame with 831,639 rows and 14 variables
NULL



#' @docType data
#' @keywords incident  
#' @name incident
#' @usage data(incident)
#' @format A data frame with 831,639 rows and 14 variables
NULL


#' @docType data
#' @keywords dfm_list  
#' @name dfm_list
#' @usage data(dfm_list)
#' @format A data frame with 831,639 rows and 14 variables
NULL

#' @docType data
#' @keywords incident_list  
#' @name incident_list
#' @usage data(incident_list)
#' @format A data frame with 831,639 rows and 14 variables
NULL

#' @docType data
#' @keywords individual_idea_point  
#' @name individual_idea_point
#' @usage data(individual_idea_point)
#' @format A data frame with 831,639 rows and 14 variables
NULL


#' @docType data
#' @keywords individual_list  
#' @name individual_list
#' @usage data(individual_list)
#' @format A data frame with 831,639 rows and 14 variables
NULL


#' @docType data
#' @keywords keyw_list  
#' @name keyw_list
#' @usage data(keyw_list)
#' @format A data frame with 831,639 rows and 14 variables
NULL

#' @docType data
#' @keywords keyword  
#' @name keyword
#' @usage data(keyword)
#' @format A data frame with 831,639 rows and 14 variables
NULL



#' @docType data
#' @keywords incident_list  
#' @name incident_list
#' @usage data(incident_list)
#' @format A data frame with 831,639 rows and 14 variables
NULL


#' @docType data
#' @keywords dict  
#' @name dict
#' @usage data(dict)
#' @format A data frame with 831,639 rows and 14 variables
NULL

#' @docType data
#' @keywords redgaurds_dfm  
#' @name redgaurds_dfm
#' @usage data(redgaurds_dfm)
#' @format A data frame with 831,639 rows and 14 variables
NULL

#' @docType data
#' @keywords redguard_estimates  
#' @name redguard_estimates
#' @usage data(redguard_estimates)
#' @format A data frame with 831,639 rows and 14 variables
NULL

#' @docType data
#' @keywords redgaurds_wfm  
#' @name redgaurds_wfm
#' @usage data(redgaurds_wfm)
#' @format A data frame with 831,639 rows and 14 variables
NULL


