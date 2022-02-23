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
