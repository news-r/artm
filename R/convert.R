#' Preprocess
#' 
#' Convert text to \href{https://github.com/VowpalWabbit/vowpal_wabbit/wiki/Input-format}{Vowpal Wabbit} format.
#' 
#' @param data A \code{data.frame} containing \code{text} and (optionally) \code{id} columns.
#' @param ... Any other argument.
#' @param text Text to preprocess.
#' @param id Unique Document id.
#' @param count Whether to count term frequency.
#' @param file Path to \code{.txt} file where to save results. If omitted results are saved to a temp file.
#'
#' @import assertthat
#'
#' @name preproc
#' @export
preprocess <- function(data, ...) UseMethod("preprocess")

#' @rdname preproc
#' @method preprocess character
#' @export
preprocess.character <- function(data, id = NULL, count = TRUE, file = NULL, ...){
  assert_that(!missing(data), msg = "Missing `text`")

  if(is.null(id))
    id <- seq(1, length(data))  

  print(...)
} 

#' @rdname preproc
#' @method preprocess data.frame
#' @export
preprocess.data.frame <- function(data, text, id = NULL, count = TRUE, file = NULL, ...){
  assert_that(!missing(data), msg = "Missing `data`")
  assert_that(!missing(text), msg = "Missing `text`")
  if(is.null(file))
    file <- tempfile(fileext = ".txt")
  
  # select data
  text_enquo <- rlang::enquo(text)
  id_enquo <- rlang::enquo(id)

  selected <- dplyr::select(data, text = !!text_enquo, id = !!id_enquo, ...)
  if("id" %in% names(selected))
    selected <- dplyr::mutate(selected, id = 1:dplyr::n())

  # tokenize
  tokenized <- selected %>%  
    tidytext::unnest_tokens(word, text) 
  
  if(count)
    tokenized <- tokenized %>% 
      dplyr::count(id, word) %>% 
      dplyr::arrange(dplyr::desc(id))
  else
    tokenized <- tokenized %>% 
      dplyr::distinct(id, word) %>% 
      dplyr::arrange(dplyr::desc(id))

  out <- .as_preprocessed(file)

  write(tokenized, file = file)
  invisible(out)
}

#' @rdname preproc
#' @export
as_preprocessed <- function(file){
  assert_that(!missing(file), msg = "Missing `file`")
  .as_preprocessed(file, temp = FALSE)
}

.as_preprocessed <- function(file, temp = TRUE){
  x <- structure(list(file = file, temp = temp), class = c(class(file), "preprocess"))
  invisible(x)
}

#' @export
print.preprocess <- function(x){
  tick_cross <- ifelse(x$temp, crayon::green(cli::symbol$tick), crayon::red(cli::symbol$cross))
  cat(
    crayon::blue(cli::symbol$info), "Path:", x$file, "\n",
    tick_cross, "Temp file", "\n"
  )
}