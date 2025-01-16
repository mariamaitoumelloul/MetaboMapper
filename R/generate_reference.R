utils::globalVariables(c(
  "df_hmdb",
  "synonyms"
))



#' @title safe_extract
#'
#' @description Extracts the text content from an XML node using the specified XPath expression and namespace. Returns "NA" if an error occurs during extraction.
#'
#' @param xml_node An XML node object from which to extract content.
#' @param xpath A character string specifying the XPath expression to locate the desired element within the XML node.
#' @param ns A named list of namespace definitions to use when resolving the XPath expression.
#'
#' @return A character string containing the extracted text content. Returns "NA" if the specified XPath cannot be resolved or an error occurs.
#'
#'
#'
#' @importFrom xml2 xml_text xml_find_first
#' @keywords internal
#'
safe_extract <- function(xml_node, xpath, ns) {

  #before test
  # tryCatch(
  #   xml_text(xml_find_first(xml_node, xpath, ns), trim = TRUE),
  #   error = function(e) "NA"
  # )

  tryCatch(
    {
      # Ensure that xml_find_first handles the namespace correctly
      result <- xml_find_first(xml_node, xpath, ns)
      # Check if the result is NULL, and return "NA" if so
      if (is.null(result)) return("NA")
      xml_text(result, trim = TRUE)
    },
    error = function(e) "NA"  # Return "NA" in case of error
  )
}



#' @title Extract metabolite information from HMDB XML File
#'
#' @description Parses an HMDB XML file to extract metabolite-related information, organizing it into a structured data frame that will be used as the HMDB reference for the next steps.
#'
#' @param in_file A character string specifying the path to the HMDB XML file.
#'
#' @return A data frame containing extracted metabolite information with the following columns:
#' \itemize{
#'   \item \code{accession}: Primary accession ID.
#'   \item \code{secondary_accessions}: Secondary accession IDs, concatenated into a single string.
#'   \item \code{name}: Name of the metabolite.
#'   \item \code{description}: Description of the metabolite.
#'   \item \code{synonyms}: Synonyms for the metabolite, concatenated into a single string.
#'   \item \code{InChIKey}: InChIKey of the metabolite.
#'   \item \code{description_taxonomy}: Description of the taxonomy.
#'   \item \code{kingdom}: Taxonomic kingdom classification.
#'   \item \code{super_class}: Taxonomic super class.
#'   \item \code{class}: Taxonomic class.
#'   \item \code{sub_class}: Taxonomic subclass.
#'   \item \code{direct_parent}: Taxonomic direct parent classification.
#'   \item \code{chebi_id}: ChEBI ID of the metabolite.
#'   \item \code{kegg_id}: KEGG ID of the metabolite.
#' }
#'
#' @details This function parses an HMDB XML file (XML can be downloaded from the "Downloads" section here :https://hmdb.ca). It iterates through each metabolite
#' node, extracts relevant fields, and returns the information as a data frame. The output will be used for the next steps as the reference.
#'
#' @examples
#' \dontrun{
#'   # Example usage
#'   extracted_data <- hmdbextract("path_to_hmdb.xml")
#'   head(extracted_data)
#' }
#'
#' @importFrom xml2 read_xml xml_find_all
#' @importFrom dplyr bind_rows
#' @importFrom pbapply pboptions pblapply
#' @export
#'
hmdbextract <- function(in_file) {

  print("Reading XML")
  doc <- read_xml(in_file)

  # Define the namespace
  print("Defining a namespace")
  ns <- c(hmdb = 'http://www.hmdb.ca')

  # Parse each metabolite
  metabolites <- xml_find_all(doc, ".//hmdb:metabolite", ns = ns)


  print("Extracting information for each metabolite")
  # Initialize a list to store metabolite data
  pboptions(type = "timer")
  data_list <- pblapply(metabolites, function(metabolite) {
    # Record Information
    accession <- xml_text(xml_find_first(metabolite, "hmdb:accession", ns = ns))
    secondary_accessions <- xml_text(xml_find_all(metabolite, "hmdb:secondary_accessions/hmdb:accession", ns = ns))
    if (length(secondary_accessions) == 0) secondary_accessions <- NA

    # Metabolite Identification
    name <- xml_text(xml_find_first(metabolite, "hmdb:name", ns = ns))
    if (is.na(name)) name <- NA
    description <- xml_text(xml_find_first(metabolite, "hmdb:description", ns = ns))
    if (is.na(description)) description <- NA
    synonyms <- xml_text(xml_find_all(metabolite, "hmdb:synonyms/hmdb:synonym", ns = ns))
    if (length(synonyms) == 0) synonyms <- NA
    inchikey <- xml_text(xml_find_first(metabolite, "hmdb:inchikey", ns = ns))
    if (is.na(inchikey)) inchikey <- NA

    # Taxonomy
    description_taxonomy <- xml_text(xml_find_first(metabolite, "hmdb:taxonomy/hmdb:description", ns = ns))
    if (is.na(description_taxonomy)) description_taxonomy <- NA
    kingdom <- xml_text(xml_find_first(metabolite, "hmdb:taxonomy/hmdb:kingdom", ns = ns))
    if (is.na(kingdom)) kingdom <- NA
    super_class <- xml_text(xml_find_first(metabolite, "hmdb:taxonomy/hmdb:super_class", ns = ns))
    if (is.na(super_class)) super_class <- NA
    classorg <- xml_text(xml_find_first(metabolite, "hmdb:taxonomy/hmdb:class", ns = ns))
    if (is.na(classorg)) classorg <- NA
    sub_class <- xml_text(xml_find_first(metabolite, "hmdb:taxonomy/hmdb:sub_class", ns = ns))
    if (is.na(sub_class)) sub_class <- NA
    direct_parent <- xml_text(xml_find_first(metabolite, "hmdb:taxonomy/hmdb:direct_parent", ns = ns))
    if (is.na(direct_parent)) direct_parent <- NA

    # External Links
    chebi_id <- xml_text(xml_find_first(metabolite, "hmdb:chebi_id", ns = ns))
    if (is.na(chebi_id)) chebi_id <- NA
    kegg_id <- xml_text(xml_find_first(metabolite, "hmdb:kegg_id", ns = ns))
    if (is.na(kegg_id)) kegg_id <- NA

    # Return a named list
    list(
      accession = accession,
      secondary_accessions = paste(secondary_accessions, collapse = ";"),
      name = name,
      description = description,
      synonyms = paste(synonyms, collapse = ";"),
      InChIKey = inchikey,
      description_taxonomy = description_taxonomy,
      kingdom = kingdom,
      super_class = super_class,
      class = classorg,
      sub_class = sub_class,
      direct_parent = direct_parent,
      chebi_id = chebi_id,
      kegg_id = kegg_id
    )

  })

  # Convert the list into a dataframe

  df_hmdb <-bind_rows(data_list)
  print("Start processing the columns for the mapping")
  df_hmdb<-clean_hmdb_data(df_hmdb)

  print("Reference dataframe processed and ready to be used.")
  return(df_hmdb)
}







#' @title Clean and Process HMDB Data
#'
#' @description Cleans and prepare the HMDB dataset by converting all columns to character, replacing NAs with empty strings,
#' removing unnecessary characters, and creating needed columns for the next steps.
#'
#' @param df_hmdb Dataframe generated by the first steps of hmdbextract function
#'
#' @return Processed dataframe for the next steps.
#'
#' @importFrom tidyr separate_rows
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate group_by ungroup distinct

#' @keywords internal
#'
clean_hmdb_data <- function(df_hmdb) {



  # Ensure all columns are character type and replace NA with empty strings
  df_hmdb <- as.data.frame(lapply(df_hmdb, as.character), stringsAsFactors = FALSE)
  df_hmdb[is.na(df_hmdb)] <- ""


  # Remove unnecessary columns
  df_hmdb$description_taxonomy <- NULL

  # Separate rows for 'secondary_accessions' by commas
  df_hmdb <- df_hmdb %>%tidyr::separate_rows(secondary_accessions, sep = ";", convert = TRUE)



  df_hmdb <- df_hmdb %>%
    group_by(accession) %>%
    mutate(all_HMDBs = paste(secondary_accessions, collapse = "|")) %>%
    ungroup() %>%  # Ungroup the data to avoid unintended side effects
    mutate(all_HMDBs = paste0(accession, "|", all_HMDBs))
  df_hmdb<-distinct(df_hmdb,accession,name, .keep_all = T)
  df_hmdb$secondary_accessions<-NULL

  df_hmdb <- df_hmdb %>%tidyr::separate_rows(synonyms, sep = ";", convert = TRUE)
  df_hmdb <- df_hmdb %>%
    group_by(accession) %>%
    mutate(all_names = paste(synonyms, collapse = "|")) %>%
    ungroup() %>%  # Ungroup the data to avoid unintended side effects
    mutate(all_names = paste0(name, "|", all_names))

  df_hmdb<-distinct(df_hmdb,accession,name, .keep_all = T)

  df_hmdb <- process_labels(df_hmdb, "all_names")
  df_hmdb <- process_inchikey(df_hmdb, "InChIKey")


  return(df_hmdb)
}


