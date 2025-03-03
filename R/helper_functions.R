utils::globalVariables(c("accession", "name", "all_names", "secondary_accessions",    "all_HMDBs", "chebi_id", "kegg_id", "label_processed",    "inchikey_processed", "inchikey_skeleton",    "inchikey_skeleton_in_reference_INCHIKEY",    "inchikey_processed_in_reference_INCHIKEY_skeleton","all_names_in_reference_NAME","HMDB_id_list", "HMDB_name_list", "HMDB_label"))



#' @title Remove Letters Between Parentheses
#'
#' @description Removes any letters enclosed within parentheses from a given text string.
#'     It leaves the parentheses empty or removes them entirely if they contain only letters.
#'
#' @param text A character string containing text where letters within parentheses will be removed.
#'
#' @return A character string with letters inside parentheses removed. The parentheses themselves are also removed.
#'
#' @details
#' - The function uses a regular expression to match any sequence of letters inside parentheses and replace them with empty strings.
#' - It ensures that parentheses without letters are also cleaned, resulting in no empty parentheses remaining.
#'
#'@keywords internal
#'
remove_letters_between_parentheses <- function(text) {
# Use regular expression to match letters between parentheses and replace them with empty strings
modified_text <- gsub("\\(([A-Za-z]+)\\)", "()", text)
modified_text <- gsub("\\(\\)", "",  modified_text)
return(modified_text)
}




#' @title Process Labels
#'
#' @description Cleans and standardizes labels by removing spaces, dashes, uppercase letters,
#'     special characters (such as single and double quotes, asterisks), and numbers in parentheses at the end of the label.
#'
#' @param df A data frame containing the labels to be processed.
#' @param col_name The name of the column containing the labels to be processed.
#' @param reference Boolean, if equal=TRUE the function will not remove the letters between parenthesis
#'
#' @return A data frame with a new column `label_processed`, which contains the cleaned labels.
#'
#' @details
#' - The function performs several text cleaning operations:
#'   - It removes spaces and dashes.
#'   - It converts the text to lowercase.
#'   - It removes single and double quotes, and asterisks.
#'   - It uses an external function `remove_letters_between_parentheses` to remove shorter names within parentheses.
#'   - It also removes numeric values in parentheses at the end of the label (e.g., "(1)", "(2)").
#' - The cleaned labels are stored in a new column `label_processed`.
#'
#'@keywords internal
process_labels <- function(df, col_name, reference) {

  # Remove spaces
  df$label_processed <- gsub(" ", "", df[[col_name]])

  # Remove "-"
  df$label_processed <- gsub("[ -]", "", df$label_processed)

  # Remove upper cases
  df$label_processed <- tolower(df$label_processed)

  # Remove '
  df$label_processed <- gsub("'", "", df$label_processed)

  # Remove "
  df$label_processed <- gsub("\"", "", df$label_processed)

  #Remove *
  df$label_processed <- gsub("\\*", "", df$label_processed)


  if(reference==FALSE){

  df$label_processed <- gsub("^(\\(R\\)|\\(L\\))", "", df$label_processed)

  #when they use shorter names
  df$label_processed <-remove_letters_between_parentheses(df$label_processed)

  #remove (1), (2) ..(n) where n is a number in a parenthesis ending the caracter
  df$label_processed <- gsub("\\s*\\(\\d+\\)$", "",  df$label_processed)
  }

  return(df)
}



#' @title Process InChIKey
#'
#' @description Processes a specified column containing InChIKey strings by extracting
#'     the first `N_skeleton` characters to create a skeleton InChIKey and the first `N_inchikey` characters
#'     to create a processed InChIKey.
#'
#' @param df A data frame containing the column with InChIKey strings.
#' @param col_name The name of the column containing the InChIKey strings to be processed.
#' @param N_skeleton An integer specifying the number of characters to extract for the skeleton InChIKey.
#'     Default is 14.
#' @param N_inchikey An integer specifying the number of characters to extract for the processed InChIKey.
#'     Default is 23.
#'
#' @return A data frame with two new columns:
#'   - `inchikey_skeleton`: A new column containing the first `N_skeleton` characters of the specified InChIKey.
#'   - `inchikey_processed`: A new column containing the first `N_inchikey` characters of the specified InChIKey.
#'
#' @details
#' - This function takes a column with InChIKey strings and extracts a skeleton version (first 14 characters)
#'   and a processed version (first 23 characters) of each InChIKey.
#' - It returns the data frame with these new columns added.
#'
#'
#'@keywords internal
#'
process_inchikey <- function(df, col_name, N_skeleton=14, N_inchikey=23) {
  # Create a new column 'inchikey_skeleton' by extracting the first 14 characters of the specified column.
  df$inchikey_skeleton <- substr(df[[col_name]], 1, N_skeleton)

  # Create another new column 'inchikey_processed' by extracting the first 23 characters of the specified column.
  df$inchikey_processed <- substr(df[[col_name]], 1, N_inchikey)

  return(df)
}






#' @title Match Identifier
#'
#' @description Matches metabolite identifiers from a cohort data set with a reference database,
#'     annotating the matches and providing additional information.
#'
#' @param df_cohort A data frame containing cohort data with metabolite identifiers to be mapped.
#' @param col_cohort A character string specifying the column in `df_cohort` containing the identifier to be mapped.
#' @param df_reference A data frame containing reference data with metabolite identifiers and corresponding metadata.
#' @param identifier A character string specifying the type of identifier to map.
#'     Supported values are `"INCHIKEY"`, `"NAME"`, `"HMDB"`, `"CHEBI"`, and `"KEGG"`.
#'
#' @return A data frame that merges the cohort and reference data frames based on the specified identifier.Includes an additional column indicating whether a match was found.
#'
#' @details
#' - Counts and prints the number of available identifiers in the cohort data set before matching.
#' - Renames columns in the reference data frame by appending `"_in_reference_"` and the `identifier`
#'     to avoid naming conflicts.
#' - Merges the cohort and reference data frames based on the specified identifier columns.
#' - Adds a new logical column (`mapped_to_<col_reference>`) to indicate if a match was found.
#'
#'@importFrom dplyr %>%
#'@importFrom dplyr mutate
#'@importFrom rlang sym :=

#'@keywords internal


match_identifier <- function(df_cohort, col_cohort,df_reference, col_reference, identifier){



  nb_av_ids <-  sum(!is.na(df_cohort[[col_cohort]]) & df_cohort[[col_cohort]] != "NA" & df_cohort[[col_cohort]] != "")
  print(paste("Number of identifier available in your dataset: ", nb_av_ids))
  #select column of interest from the database
  colnames(df_reference)<-paste0(colnames(df_reference),"_in_reference_",identifier)

  df_cohort <- df_cohort[!is.na(df_cohort[[col_cohort]]), ]
  df_cohort <- df_cohort[!grepl("^\\s*$", df_cohort[[col_cohort]]), ]
  #merge based the cohort with the database based on the identifier
  df_cohort<-merge(df_cohort,df_reference,by.x=col_cohort,by.y=paste0(col_reference,"_in_reference_",identifier),all.x=T)

  # Annotate if there is a match between the cohort and the database based on the identifier or not
  new_column <- paste0("mapped_to_", col_reference)
  mapped_column <- paste0("accession_in_reference_", identifier)
  df_cohort <- df_cohort %>%dplyr::mutate(!!new_column := !is.na(!!sym(mapped_column)))

  return(df_cohort)
}



#' @title Map Unique Identifiers
#'
#' @description Maps a given metabolite identifier type in a cohort data frame to reference identifiers
#'     (e.g., INCHIKEY, NAME, HMDB, CHEBI, KEGG) by performing a series of processing, matching,
#'     and reporting steps.
#'
#' @param df_cohort A data frame containing cohort data with metabolite identifiers to be mapped.
#' @param col_cohort A character string specifying the column in `df_cohort` containing the identifier to be mapped.
#' @param df_reference A data frame containing reference data with metabolite identifiers and corresponding metadata.
#' @param identifier A character string specifying the type of identifier to map.
#'     Supported values are `"INCHIKEY"`, `"NAME"`, `"HMDB"`, `"CHEBI"`, and `"KEGG"`.
#'
#' @return A data frame with updated mapping information, including matched identifiers
#'
#' @details
#' - For `INCHIKEY`, both the full and skeleton INCHIKEY identifiers are mapped.
#' - For `NAME`, metabolite names are processed and matched to all known labels, inclusing synonyms.
#' - For `HMDB`, `CHEBI`, and `KEGG`, the corresponding identifiers are matched directly to reference identifiers.
#'
#' @importFrom dplyr %>%
#' @keywords internal

map_unique_identifier <- function(df_cohort, col_cohort,df_reference, identifier) {


  df_cohort_temp<-df_cohort%>%dplyr::select(col_cohort)



  if(identifier=="INCHIKEY"){

    df_reference_inchikey<-df_reference%>%dplyr::select(accession,name,inchikey_skeleton, inchikey_processed)

    df_cohort<-process_inchikey(df_cohort, col_cohort, N_skeleton=14, N_inchikey=23)
    print("Mapping INCHIKEY")
    df_cohort<-match_identifier(df_cohort, df_reference_inchikey, col_cohort="inchikey_processed", col_reference="inchikey_processed", identifier="INCHIKEY")
    mapped_inchikey<-sum(df_cohort$mapped_to_inchikey_processed, na.rm = T)
    print(paste(mapped_inchikey, " metabolites mapped to HMDB Ids based on INCHIKEY."))

    print("Mapping INCHIKEY skeleton")
    df_cohort<-match_identifier(df_cohort, df_reference_inchikey, col_cohort="inchikey_skeleton", col_reference="inchikey_skeleton", identifier="INCHIKEY_skeleton")
    mapped_inchikey_skeleton<-sum(df_cohort$mapped_to_inchikey_skeleton, na.rm = T)
    print(paste(mapped_inchikey_skeleton, " metabolites mapped to HMDB Ids based on INCHIKEY skeleton."))
    df_cohort<-df_cohort%>%dplyr::select(-inchikey_processed,-inchikey_skeleton_in_reference_INCHIKEY,-inchikey_processed_in_reference_INCHIKEY_skeleton)

  }

  else if(identifier=="NAME"){
    print("Mapping the names")
    df_reference_label<-df_reference%>%dplyr::select(accession,name,all_names)
    df_reference_label <- df_reference_label %>% separate_rows(all_names, sep = "\\|", convert = TRUE)
    df_reference_label<-process_labels(df_reference_label, "all_names", reference=TRUE)
    df_reference_label<-distinct(df_reference_label,accession,label_processed, .keep_all = T)
    df_cohort<-process_labels(df_cohort, col_cohort, reference=FALSE)
    df_cohort<-match_identifier(df_cohort, df_reference_label, col_cohort="label_processed", col_reference="label_processed", identifier="NAME")
    mapped_labels<-sum(df_cohort$mapped_to_label_processed, na.rm = T)
    df_cohort<-df_cohort%>%dplyr::rename("mapped_to_name"="mapped_to_label_processed")
    print(paste(mapped_labels, " metabolites mapped to HMDB Ids based on their labels."))

    df_cohort<-df_cohort%>%dplyr::select(- all_names_in_reference_NAME)
    #df_cohort<-merge(df_cohort_temp,df_cohort,by=col_cohort,all.x=T)


  } else if(identifier=="HMDB"){

    df_reference_HMDB<-df_reference%>%dplyr::select(accession,name,all_HMDBs)
    df_reference_HMDB <- df_reference_HMDB %>% separate_rows(all_HMDBs, sep = "\\|", convert = TRUE)
    df_reference_HMDB<-distinct(df_reference_HMDB,accession,name,all_HMDBs, .keep_all = T)
    df_cohort<-match_identifier(df_cohort, col_cohort=col_cohort, df_reference_HMDB , col_reference="all_HMDBs", identifier="HMDB")

    mapped_HMDB<-sum(df_cohort$mapped_to_all_HMDBs, na.rm = T)
    df_cohort<-df_cohort%>%dplyr::rename("mapped_to_HMDB_id"="mapped_to_all_HMDBs")
    print(paste(mapped_HMDB, " metabolites mapped to HMDB Ids based on their HMDB identifiers."))

  } else if(identifier=="CHEBI"){

    df_reference_chebi<-df_reference%>%dplyr::select(accession,name,chebi_id)
    df_cohort<-match_identifier(df_cohort, col_cohort=col_cohort, df_reference_chebi , col_reference="chebi_id", identifier="CHEBI")
    mapped_chebi<-sum(df_cohort$mapped_to_chebi_id, na.rm = T)
    print(paste(mapped_chebi, " metabolites mapped to HMDB Ids based on their CHEBI identifiers."))
    #df_cohort<-merge(df_cohort_temp,df_cohort,by=col_cohort,all.x=T)

  } else if(identifier=="KEGG"){

    df_reference_kegg<-df_reference%>%dplyr::select(accession,name,kegg_id)
    df_cohort<-match_identifier(df_cohort, col_cohort=col_cohort, df_reference_kegg , col_reference="kegg_id", identifier="KEGG")
    mapped_kegg<-sum(df_cohort$mapped_to_kegg_id, na.rm = T)
    print(paste(mapped_kegg, " metabolites mapped to HMDB Ids based on their KEGG identifiers."))
    #df_cohort<-merge(df_cohort_temp,df_cohort,by=col_cohort,all.x=T)
  }



  return(df_cohort)
  print("Mapping done")
}













#' @title Extract Highest Star
#'
#' @description Identifies and extracts the substring(s) with the highest number
#'     of trailing asterisks (`*`) from a delimited input string.
#'
#' @param s A string where items are delimited by `//`, each possibly ending with
#'     one or more trailing asterisks (`*`).
#'
#' @return A single string containing the substring(s) with the highest number
#'     of trailing asterisks, separated by `/` if there are ties.
#'
#' @details
#' - The input string is split into individual items based on the `//` delimiter.
#' - Each item is checked for trailing asterisks using a regular expression.
#' - The function identifies the item(s) with the maximum number of trailing asterisks.
#' - If multiple items tie for the highest count, they are concatenated using `/`.
#'

#'@keywords internal

extract_highest_star <- function(s) {
  # Split the input string by '/'
  items <- unlist(strsplit(s, '//'))

  # Initialize variables to keep track of the highest star count and corresponding items
  highest_star_count <- (Inf * -1)
  highest_star_items <- list()  # To store all items with the highest star count

  # Iterate through items
  for (item in items) {
    # Count the number of stars in the current item
    g <- gregexpr("\\*+$", item)
    star_count <- attr(g[[1]], 'match.length')

    # Update logic to store items with the maximum star count
    if (!is.na(star_count) && star_count > highest_star_count) {
      highest_star_count <- star_count
      highest_star_items <- list(item)  # Reset the list with the new highest star item
    } else if (star_count == highest_star_count) {
      highest_star_items <- c(highest_star_items, item)  # Append tied item
    }
  }

  # If no stars were found, return NA instead of an empty string
  if (length(highest_star_items) == 0) {
    return(NA_character_)
  }
  # Combine all highest star items into a single string separated by '|'
  return(paste(unlist(unique(highest_star_items)), collapse = "|"))
}








#' @title Mapper Confidence Level
#'
#' @description Based on which the mapping strategy, give a confidence level for the metabolite annotation
#'
#' @param data A data frame containing metabolite identifiers to be updated with confidence score.
#'
#' @return A modified data frame where selected identifier columns with corresponding confidence level.
#'
#' @details
#' - Columns ending with `_HMDB`, `_inchikey`, `_NAME`, `_KEGG`, or `_CHEBI` are appended with `***`.
#' - Columns ending with `_skeleton` are appended with `**`.
#' - Non-character columns matching the patterns are skipped with a warning.
#'
#' modified_df <- mapper_confidence_level(df)
#'
#'@keywords internal
#'@importFrom dplyr %>%
#'@importFrom dplyr mutate across everything

mapper_confidence_level <- function(data) {

  # Get the names of all columns in the data frame
  columnNames <- colnames(data)

  # Loop through each column name
  for (columnName in columnNames) {
    # Check if the column name ends with "_HMDB", "_inchikey_processed", "_labels", or "_skeleton"
    if (grepl("_HMDB$|_inchikey$|_NAME$|_skeleton$|_KEGG$|_CHEBI$", columnName)) {
      # Get the column using the $ operator
      data[[columnName]]<-as.character(data[[columnName]])
      column <- data[[columnName]]

      # Check if the column is character
      if (is.character(column)) {
        # Modify the column based on the specific condition
        if (grepl("_HMDB$", columnName)) {
          data[[columnName]] <- paste0(column, "***")
        } else if (grepl("_inchikey_processed$", columnName)) {
          data[[columnName]] <- paste0(column, "***")
        }
        else if (grepl("_NAME$", columnName)) {
          data[[columnName]] <- paste0(column, "***")
        } else if (grepl("_skeleton$", columnName)) {
          data[[columnName]] <- paste0(column, "**")
        } else if (grepl("_KEGG$", columnName)) {
          data[[columnName]] <- paste0(column, "***")
        }else if (grepl("_CHEBI$", columnName)) {
          data[[columnName]] <- paste0(column, "***")
        }
      } else {
        warning(paste("Column", columnName, "is not character and will not be modified."))
      }
    }
  }

  # Iterate through all columns and apply the modification
  #data <- data %>% mutate(across(everything(), ~ add_1_stars_after_na(.)))

  return(data)
}


#' Remove Stars After "NA"
#'
#' This function removes all asterisks (`*`) that appear immediately after the string "NA"
#' in a given column of data. It uses a regular expression to identify and replace
#' such patterns.
#'
#' @param column A character vector representing a column of data. This can be a single
#' column from a dataframe or a standalone vector.
#'
#' @return A character vector with all occurrences of "NA" followed by one or more
#' asterisks (`*`) replaced with "NA".
#'
#' @keywords internal
remove_stars_after_NA <- function(column) {
  gsub("NA\\*+", NA, column)
}









#' @title Generate a Final Labeled Dataframe
#'
#' @description This function processes an input dataset  by combining and selecting
#' relevant columns (identifiers with highest confidence levels)
#'
#' @param data_input A data frame containing input data with columns for accession and name information.
#' @param details Logical, indicating whether to include all details in the output. If `TRUE`, the entire dataframe is returned
#' with renamed columns for clarity. If `FALSE`, only selected columns are returned.
#' @param study_cols A column name or a vector of column names in `df_study` corresponding to identifiers to be mapped.
#' @param original_colname the name of the column containing the original metabolites label
#'
#' @details
#' The function performs the following steps:
#' 1. Identifies columns starting with `"accession_in_reference"` and `"name_in_reference"`.
#' 2. Creates new columns `HMDB_id_list` and `HMDB_name_list` by combining unique, non-NA values from the identified columns.
#' 3. Extracts the highest confidence level (based on the number of stars) for `HMDB_id` and `HMDB_label` using the
#' `extract_highest_star` function.
#' 4. Calculates the number of stars (`Num_Stars`) for the label and maps star counts to corresponding confidence levels.
#' 5. Depending on the `details` parameter, either the full processed dataframe or a subset with selected columns is returned.
#'
#' @return A data frame with the final processed results:
#'   - If `details = TRUE`, all processed columns are returned with renamed columns for clarity.
#'   - If `details = FALSE`, a subset with columns for the original study name, HMDB label, HMDB identifier, and confidence level
#' is returned.
#'
#' @import dplyr
#' @importFrom stats na.omit
#' @keywords internal
#'
final_label_dataframe <- function(data_input,study_cols, details=FALSE, original_colname) {

  # Create vectors of column names starting with "accession_in_synonyms" and "name_in_synonyms"
  accession_columns <- grep("^accession_in_reference", names(data_input), value = TRUE)
  name_columns <- grep("^name_in_reference", names(data_input), value = TRUE)


  data_input <- data_input %>%
    mutate(across(all_of(accession_columns), remove_stars_after_NA)) %>%  # Clean accession columns
    rowwise() %>%  # Rowwise operation to process each row independently
    mutate(
      HMDB_id_list = {
        # Get unique non-NA values from the accession columns
        values <- unique(na.omit(c_across(all_of(accession_columns))))
        non_empty_values <- values[values != ""]

        # Concatenate non-empty values or return NA if no valid values
        if (length(non_empty_values) > 0) {
          paste(non_empty_values, collapse = "//")
        } else {
          NA_character_
        }
      }
    ) %>%
    ungroup() %>%  # Remove rowwise grouping after creation of HMDB_id_list
    group_by_at(original_colname) %>%  # Group by the column specified in original_colname
    mutate(
      HMDB_id_list = paste(unique(na.omit(HMDB_id_list)), collapse = "//")  # Concatenate values within each group
    )


  data_input <- data_input %>%
    mutate(across(all_of(name_columns), remove_stars_after_NA)) %>%      # Clean name columns
    rowwise() %>%  # Rowwise operation to process each row independently
    mutate(
      HMDB_name_list = {
        # Get unique non-NA values from the accession columns
        values <- unique(na.omit(c_across(all_of(name_columns))))
        non_empty_values <- values[values != ""]

        # Concatenate non-empty values or return NA if no valid values
        if (length(non_empty_values) > 0) {
          paste(non_empty_values, collapse = "//")
        } else {
          NA_character_
        }
      }
    ) %>%
    ungroup() %>%  # Remove rowwise grouping after creation of HMDB_name_list
    group_by_at(original_colname) %>%  # Group by the column specified in original_colname
    mutate(
      HMDB_name_list = paste(unique(na.omit(HMDB_name_list)), collapse = "//")  # Concatenate values within each group
    )


  data_input <- data_input %>%
    mutate(HMDB_id = sapply(HMDB_id_list, function(x) ifelse(is.na(x), NA_character_, extract_highest_star(x))),
           HMDB_label = sapply(HMDB_name_list, function(x) ifelse(is.na(x), NA_character_, extract_highest_star(x))))



  # Add a new column for the number of stars
  data_input <- data_input %>%
    mutate(Num_Stars = sapply(HMDB_label, function(x) {
    if (is.na(x)) {
      return(0) # Handle NA values explicitly
    }
    matches <- gregexpr("\\*+", x)[[1]]
    if (all(matches == -1)) { # If no stars are found
      return(0)
    } else {
      return(max(attr(matches, "match.length"), na.rm = TRUE))
    }
  }))


  data_input <- data_input %>%
    mutate(HMDB_label = gsub("\\*", "", HMDB_label))%>%
    mutate(HMDB_id = gsub("\\*", "", HMDB_id))


  if (details==TRUE){
    # Extract column names starting with "mapped_to"
    mapped_columns <- grep("^mapped_to", colnames(data_input), value = TRUE)
    data_input_selected <- data_input%>%dplyr::select(original_colname,study_cols,mapped_columns, "HMDB_label", "HMDB_id", "Num_Stars")%>%
      mutate(HMDB_label = gsub("\\*+$", "", HMDB_label))%>%
      mutate(HMDB_id = gsub("\\*+$", "", HMDB_id))
    data_input_selected<-data_input_selected%>%rename("Confidence Level"="Num_Stars")
    data_input_selected <- data_input_selected %>%
       mutate(across(mapped_columns, ~ replace(., is.na(.) | . == "NA", FALSE)))


  }else{

    data_input_selected <- data_input%>%dplyr::select(original_colname,study_cols, "HMDB_label", "HMDB_id", "Num_Stars")%>%
      mutate(HMDB_label = gsub("\\*+$", "", HMDB_label))%>%
      mutate(HMDB_id = gsub("\\*+$", "", HMDB_id))
    data_input_selected<-data_input_selected%>%rename("Confidence Level"="Num_Stars")

  }


  data_input_selected <- dplyr::distinct(data_input_selected, .data[[original_colname]], .keep_all = TRUE)



  data_input_selected <- data_input_selected %>%
    mutate(across(ends_with(c("HMDB", "KEGG", "CHEBI","_skeleton","_processed")), ~ gsub("\\*+$", "", .)))


  return(data_input_selected)
}













