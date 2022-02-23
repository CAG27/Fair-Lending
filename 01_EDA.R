#' 1. OVERVIEW ####
#' The purpose of this script is to demonstrate essential data science skills
#' using publicly-available HMDA data. The Home Mortgage Disclosure Act, aka
#' HMDA, requires that qualifying financial institutions submit data on 
#' real estate secured products such as mortgages, HELOCs, etc. 

#' 2. CONFIGURE ENVIRONMENT & READ IN DATA ####
# Attach required packages
library(dplyr)
library(tidyr)
library(MASS)
library(lubridate)

# Read in the data (I saved it to the project directory)
df_HMDA <- read.csv("msamd_46060_actions_taken_1-2-3.csv")

#' 3. EXPLORATORY DATA ANALYSIS ####
# The data contain 69360 records and 99 variables
str(df_HMDA)

# Quality check our data and make sure it represents the desired population,
# which is the Tucson, AZ MSA for the year 2020, and only applications which
# were originated, approved not originated, or declined. Per the data dictionary
# found at https://ffiec.cfpb.gov/documentation/2018/lar-data-fields/, we know
# that the variable action_taken value of 1 means the loan was originated, 2
# means the loan was approved but not originated, and 3 means the loan was
# declined. 
unique(df_HMDA$activity_year) == 2020 # TRUE
unique(df_HMDA$derived_msa.md) == 46060 # TRUE
unique(df_HMDA$action_taken) %in% c(1,2,3) # all TRUE

# Explore whether we have any missing data
for (i in 1:ncol(df_HMDA)){
  if (nrow(df_HMDA %>% dplyr::filter(is.na(df_HMDA[i]))) > 0){
    print(
      paste0(
        colnames(df_HMDA)[i],
        " has ",
        nrow(df_HMDA %>% dplyr::filter(is.na(df_HMDA[i]))),
        ", or ~",
        round(nrow(df_HMDA %>% dplyr::filter(is.na(df_HMDA[i])))/nrow(df_HMDA),4)*100,
        "% records with missing values."
        )
      )
  }
}

#' We can see that a number of variables have a large number of missing values:
#' We will want to exclude the following variables from our model due to having
#' >90% missing values: total_points_and_fees, prepayment_penalty_term,
#' intro_rate_period, multifamily_affordable_units, applicant_ethnicity.2, 
#' applicant_ethnicity.3, applicant_ethnicity.4, applicant_ethnicity.5,
#' co.applicant_race.2, co.applicant_race.3, co.applicant_race.4, 
#' co.applicant_race.5, aus.2, aus.3, aus.4, aus.5, denial_reason.2,
#' denial_reason.3, denial_reason.4.

#' Assess distributions and frequencies. This will help us determine if any
#' variables that make it into our final model may need to be transformed
#' due to skewness
for (i in 1:ncol(df_HMDA)){
  if (is.numeric(df_HMDA[[i]])){
    hist(df_HMDA[[i]])
  } else {
    print(df_HMDA %>% dplyr::group_by(df_HMDA[i]) %>% dplyr::summarise(dplyr::n()))
  }
}

