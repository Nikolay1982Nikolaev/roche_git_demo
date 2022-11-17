#' @title CHECK COMPLIANCE
#' @description This function receive:  
#' @input
#' - df               = data frame 
#' - gdsr_publication = NULL default parameter ...
#' - core_vars        = TRUE default parameter: if the curator are interested only of core variable or all variable 
#' - tot_missing      = TRUE default parameter for total missing values in df by variable and from entire data frame
#'
#' @return   A new data frame: with variable DOMAIN, COMPLIANCE 
#'           Where the last row is TOTAL for entire daта frame if it is required       
#'
#' @export
#' @import     dplyr
#' @importFrom dciRefsys create_metadata
#' @importFrom assertthat assert_that
#'



rm(list = ls(pattern="[^MH_SDTM]"))

library(dciRefsys)
library(assertthat)
library(dplyr)


df <- data.frame(
  DOMAIN  = c("LB", "LB","LB", "LB","LB", "LB","LB", "LB","LB", "LB"),
  LBORRES  = c("12.3", NA_character_, "12.3", NA_character_,"12.3", NA_character_,"12.3", 
               NA_character_,"12.3", NA_character_),
  LBDY     = c(".", "none",".", "none","20", "none","20", "none","20", "none"),
  LBSPEC   = c("CSF", "NONE","CSF", "NONE","CSF", "NONE","CSF", "NONE","CSF", "NONE"),
  RocheMeasurementName = c("URINE_PROTEIN", "", "NA", "","URINE_PROTEIN", "",
                           "NA", "","URINE_PROTEIN", ""),
  stringsAsFactors = FALSE)
df
################################ CHECK COMPLIANCE DATA ##################################
check_compliance_data <- function(df,
                             gdsr_publication = NULL,
                             core_vars        = TRUE){

  # Assert input parameter                       
  assert_that(is.data.frame(df), msg = "DF: inserted obejct is not dataframe")
  assert_that(core_vars %in% c(TRUE, FALSE), 
              msg = "CORE_VARS: inserted parameter in not TRUE or FALSE")
  
  df <- data.frame(lapply(df, as.character))
  
  # create the metadatga
  domain_meta    <- create_metadata(df, publication_name = gdsr_publication)
  df_domain_all  <- domain_meta %>% filter(toupper(.data$systems) != "NOT FOUND")
  df_domain_core <-  filter(df_domain_all, core_variable == "Y")
  
  if (core_vars == TRUE){
    required_domein <- df_domain_core %>% pull("variable")
  } else {
    required_domein <- df_domain_all  %>% pull("variable")
  }
  
  # processing   
  df[df == "NONE"] <- ""
  df[df == "none"] <- ""
  df[df == "<NA>"] <- ""
  df[df == "NAN"] <- ""
  df[df == "NA"]   <- ""
  df[df == "."]    <- ""
  
  df <- df %>% mutate(row_no = row_number()) 
  df <- admiral::convert_blanks_to_na(df) %>% mutate_all(as.character) 
  df <- df %>% select(row_no, all_of(required_domein))
  
  
  
  nrow_df <- dim(df)[1]
  Missing <- colSums(is.na(df)) 
  Variable <- names(df)
  
  df <- data.frame(Variable,  Missing) %>%
    mutate(
      Percent_missing   = round(100 - ((nrow_df - Missing)    / nrow_df) * 100, digits = 1),
      Compliance = 100 - Percent_missing  ) %>%
    filter(Variable !='row_no' 
    ) 
  
  rownames(df) <-NULL
  
  df <- df[c("Variable", "Compliance")]

  return(df)  
}


df <- check_compliance_data(df, 
                            gdsr_publication = NULL, 
                            core_vars        = TRUE)

df

