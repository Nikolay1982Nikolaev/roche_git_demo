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


gdsr_publication = NULL
core_vars         = TRUE
  
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
  df[df == "NA"] <- ""
  df[df == "."] <- ""
  df

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
  df




####################################################################################################
library(dplyr)
df <- data.frame(
  DOMAIN   = c("LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB", "LB"),
  LBORRES  = c("12.3", NA_character_, "12.3", NA_character_,"12.3", NA_character_,"12.3", 
               NA_character_,"12.3", NA_character_),
  LBORRESU = c("ug/L", "mg/L","ug/L", "mg/L","ug/L", "mg/L","ug/L", "mg/L","ug/L", "mg/L"),
  LBDY     = c("20", "none","20", "none","20", "none","20", "none","20", "none"),
  LBSTRESN = c("300", "five thousand","300", "five thousand","300", "five thousand","300", 
               "five thousand","300", "five thousand"),
  LBMETHOD = c("ELISA", "ELISA","NA", "ELISA","ELISA", "ELISA","ELISA", "ELISA","ELISA", 
               "ELISA"),
  LBSPEC   = c("CSF", "NONE","CSF", "NONE","CSF", "NONE","CSF", "NONE","CSF", "NONE"),
  RocheMeasurementName = c("URINE_PROTEIN", "", "NA", "","URINE_PROTEIN", "",
                           "NA", "","URINE_PROTEIN", ""),
  stringsAsFactors = FALSE)
#check_compliance_data(df)
####################################################################################################
df

df[df %in% c("NONE", "none")] <- ""
df



