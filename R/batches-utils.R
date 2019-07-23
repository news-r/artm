#' Batch Vectorizer
#' 
#' @export
batch_vectorizer <- function(preprocessed_file, batches = NULL, collection_name = NULL, 
  data_format = c("vowpal_wabbit", "batches", "bow_uci", "bow_n_wd"), target_folder = NULL, 
  batch_size = 1000, batch_name_type = "code", 
  data_weight = 1.0, n_wd = NULL, vocabulary = NULL, gather_dictionary = TRUE, 
  class_ids = NULL, process_in_memory_model = NULL){

  assert_that(!missing(preprocessed_file), msg = "Missing `preprocessed_file`")
  assert_that(inherits(preprocessed_file, "preprocessed"), msg = "`preprocessed_file` is not of class `preprocessed`, see `as_preprocessed`")

  output <- artm$BatchVectorizer(
    batches = batches, 
    collection_name = collection_name, 
    data_path = preprocessed_file$file, 
    data_format = match.arg(data_format), 
    target_folder = target_folder, 
    batch_size = batch_size, 
    batch_name_type = batch_name_type, 
    data_weight = data_weight, 
    n_wd = n_wd, 
    vocabulary = vocabulary, 
    gather_dictionary = gather_dictionary, 
    class_ids = class_ids, 
    process_in_memory_model = process_in_memory_model
  )

  if(file$temp)
    unlink(file$file, recursive = TRUE)

  return(output)
}