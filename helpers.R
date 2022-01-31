# Note: this script creates functions to generate classifications of a charity
# based on its charitable object

# Import accuracy of models
acc_per_cat <- read_csv(here::here("./data/acc_per_cat.csv"))

# Import trained models
final_trained_models <- list("kernlab" = NULL, 
                             "kknn" = NULL,
                             "nnet" = NULL,
                             "ranger" = NULL)

for (i in c("kernlab", "kknn", "nnet", "ranger")){
  print(paste0("Loading /model_", i, ".rds at ", Sys.time()))
  final_trained_models[[i]] <- readRDS(paste0(here::here(), "/models/model_", i, ".rds"))
}

# Voting function
vote <- . %>%
  inner_join(acc_per_cat, by = c('.pred_class' = 'classification_description',
                                 'model_name')) %>%
  group_by(registered_charity_number, .pred_class) %>% 
  mutate(votes = n()) %>%
  ungroup() %>%
  group_by(registered_charity_number) %>%
  slice_max(order_by = votes) %>%
  slice_max(order_by = accuracy_per_cat) %>%
  summarise_all(first) %>% # edge case when 2 models have the same accuracy
  ungroup()

# predict flow
predict_wf <- function(this_model, data_to_predict){
  
  engine <- this_model$fit$actions$model$spec$engine
  
  predict(this_model, data_to_predict) %>%
    bind_cols(data_to_predict %>%
                select(registered_charity_number) %>%
                mutate(model_name = engine))
}

# Execute prediction and voting
voting_wf <- function(data_to_predict){
  
  x <- data_to_predict
  list_to_predict <- list(x, x, x, x) # hacky way to create a DF of DFs
  
  final_trained_models %>%
    map2_dfr(., list_to_predict, predict_wf) %>%
    vote()
  
} 

