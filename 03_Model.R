#' Our assignment is to build a model that predicts whether an application
#' for Conventional First Liens will be approved or declined. First we will
#' assess the univariate AUC statistics for our candidate variables
candidate_vars <- c("loan_amount",
                    # "property_value_num",
                    # "interest_rate_num",
                    "income_num",
                    "LTV_bin_10",
                    "LTV_bin_20",
                    "LTV_bin_30",
                    "LTV_bin_40",
                    "LTV_bin_50",
                    "LTV_bin_60",
                    "LTV_bin_70",
                    "LTV_bin_80",
                    "LTV_bin_90",
                    "LTV_bin_100",
                    "LTV_bin_gte100")

df_HMDA_filtered <- df_HMDA %>%
  dplyr::select(approved,all_of(candidate_vars))

model <- glm(approved ~ loan_amount + interest_rate_num + income_num + LTV_bin_80, data=df_HMDA_filtered)
df_HMDA_filtered$preds <- predict(model,df_HMDA_filtered)
pROC::auc(df_HMDA_filtered$approved,df_HMDA_filtered$preds)

model <- glm(approved ~ property_value_num+income_num+LTV_bin_80+LTV_bin_90+LTV_bin_100+LTV_bin_gte100, data=df_HMDA_filtered)
df_HMDA_filtered$preds <- predict(model,df_HMDA_filtered)
pROC::auc(df_HMDA_filtered$approved,df_HMDA_filtered$preds)

full.model <- glm(approved ~ ., data=df_HMDA_filtered)
step.model <- stepAIC(full.model, direction = "backward", 
                      trace = FALSE)
df_HMDA_filtered$preds <- predict(step.model,df_HMDA_filtered)
pROC::auc(df_HMDA_filtered$approved,df_HMDA_filtered$preds)