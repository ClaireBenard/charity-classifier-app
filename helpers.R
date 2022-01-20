# Note: this script creates functions to generate classifications of a charity
# based on its charitable object

# load accuracy of models
acc_per_cat <- read_csv(here::here("./data/acc_per_cat.csv"))

## load models
final_trained_models <- list()

for (i in seq(1:4)){
  final_trained_models[[i]] <- readRDS(paste0(here::here(), "/../charity-classifier/models", "/model", i, ".rds"))
}

predict_wf <- function(this_model, data_to_predict){
  
  engine <- this_model$fit$actions$model$spec$engine
  
  predict(this_model, data_to_predict) %>%
    bind_cols(data_to_predict %>%
                select(registered_charity_number) %>%
                mutate(model_name = engine))
}

voting_wf <- function(data_to_predict){
  
  x <- data_to_predict
  list_to_predict <- list(x, x, x, x) # hacky way to create a DF of DFs
  
  final_trained_models %>%
    map2_dfr(., list_to_predict, predict_wf) %>%
    inner_join(acc_per_cat, by = c("model_name", ".pred_class" = "classification_description")) %>%
    group_by(registered_charity_number) %>%
    top_n(1, wt = accuracy_per_cat) %>%
    ungroup() %>%
    select(.pred_class)
  
}