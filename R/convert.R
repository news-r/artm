#' Preprocess
#' 
#' @name preproc
#' @export
preprocess.character <- function(text, id = NULL, count = TRUE, file = NULL){
  assertthat(!missing(text), msg = "Missing `text`")

  if(is.null(id))
    id <- seq(1, length(text))  
} 

preprocess.data.frame <- function(data, text, id = NULL, count = TRUE, file = NULL){
  assertthat(!missing(data), msg = "Missing `data`")
  assertthat(!missing(text), msg = "Missing `text`")
  if(is.null(file))
    file <- tempfile(fileext = ".txt")
  
  # select data
  text_enquo <- rlang::enquo(text)
  id_enquo <- rlang::enquo(id)

  selected <- dplyr::select(data, text = !!text_enquo, id = !!id_enquo)
  if("id" %in% names(selected))
    selected <- dplyr::mutate(selected, id = 1:dplyr::n())

  # tokenize

  write(tokenized, file = temp)
}

#' @rdname preproc
#' @export
as_preprocessed <- function(file){
  .as_preprocessed(file, temp = FALSE)
}
