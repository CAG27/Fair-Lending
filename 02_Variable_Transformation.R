#' Our assignment is to build a model that predicts whether an application
#' for Conventional First Liens will be approved or declined. First we need
#' to filter our data just to those applications that were for these types
#' of loans. Per the data dictionary, we need to filter using the variable
#' derived_loan_product_type
df_HMDA <- df_HMDA %>%
  dplyr::filter(derived_loan_product_type == "Conventional:First Lien")

#' This filtering leaves us with 45497 applications for Conventional First Lien
#' loans in our population of interest. Of these, 1621+38609=40230 of them
#' were approved, IE had action_taken equal to 1 or 2:
df_HMDA %>% 
  dplyr::group_by(action_taken) %>% 
  dplyr::summarise(count=dplyr::n())

#' Create a new binary variable for whether an application was approved,
#' regardless of if it actually originated
df_HMDA <- df_HMDA %>%
  dplyr::mutate(approved = dplyr::case_when(action_taken %in% c(1,2) ~ 1,
                                            TRUE ~ 0))

#' Bin LTV ratio into buckets of 10%. First we need to convert it to
#' remove NAs and other character values and convert it to numeric.
df_HMDA <- df_HMDA %>%
  dplyr::mutate(LTV = tidyr::replace_na(loan_to_value_ratio,"0")) %>%
  dplyr::mutate(LTV = as.numeric(LTV)) %>%
  dplyr::mutate(LTV = tidyr::replace_na(LTV,0))

hist(df_HMDA$LTV)
quantile(df_HMDA$LTV,c(0.25,0.5,0.75))

df_HMDA <- df_HMDA %>%
  dplyr::mutate(LTV_bin_10 = 
                  dplyr::case_when(
                    (LTV < 10) ~ 1,
                    TRUE ~ 0),
                LTV_bin_20 = 
                  dplyr::case_when(
                    ((LTV >= 10) & (LTV < 20)) ~ 1,
                    TRUE ~ 0),
                LTV_bin_30 = 
                  dplyr::case_when(
                    ((LTV >= 20) & (LTV < 30)) ~ 1,
                    TRUE ~ 0),
                LTV_bin_40 = 
                  dplyr::case_when(
                    ((LTV >= 30) & (LTV < 40)) ~ 1,
                    TRUE ~ 0),
                LTV_bin_50 = 
                  dplyr::case_when(
                    ((LTV >= 40) & (LTV < 50)) ~ 1,
                    TRUE ~ 0),
                LTV_bin_60 = 
                  dplyr::case_when(
                    ((LTV >= 50) & (LTV < 60)) ~ 1,
                    TRUE ~ 0),
                LTV_bin_70 = 
                  dplyr::case_when(
                    ((LTV >= 60) & (LTV < 70)) ~ 1,
                    TRUE ~ 0),
                LTV_bin_80 = 
                  dplyr::case_when(
                    ((LTV >= 70) & (LTV < 80)) ~ 1,
                    TRUE ~ 0),
                LTV_bin_90 = 
                  dplyr::case_when(
                    ((LTV >= 80) & (LTV < 90)) ~ 1,
                    TRUE ~ 0),
                LTV_bin_100 = 
                  dplyr::case_when(
                    ((LTV >= 90) & (LTV < 100)) ~ 1,
                    TRUE ~ 0),
                LTV_bin_gte100 = 
                  dplyr::case_when(
                    ((LTV >= 10) & (LTV < 20)) ~ 1,
                    TRUE ~ 0))

df_HMDA <- df_HMDA %>%
  dplyr::mutate(property_value_num = tidyr::replace_na(as.numeric(tidyr::replace_na(property_value,"0")),0),
                interest_rate_num = tidyr::replace_na(as.numeric(tidyr::replace_na(interest_rate,"0")),0),
                income_num = as.numeric(tidyr::replace_na(income,0)))