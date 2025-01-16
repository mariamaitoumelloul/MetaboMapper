library(testthat)
library(MetaboMapper)




# # Unit test for hmdbextract function
# test_that("hmdbextract correctly parses and processes the HMDB XML file", {
#
#   # Path to an example test XML file (use a valid example file for actual testing)
#   test_file <- system.file("extdata", "test_hmdb.xml", package = "your_package") # replace with your test XML file
#
#   # Check that the file exists
#   expect_true(file.exists(test_file))
#
#   # Run the hmdbextract function
#   extracted_data <- hmdbextract(test_file)
#
#   # Check that the extracted data is a data frame
#   expect_true(is.data.frame(extracted_data))
#
#   # Check that the expected columns are present
#   expected_columns <- c("accession", "secondary_accessions", "name", "description", "synonyms", "InChIKey",
#                         "description_taxonomy", "kingdom", "super_class", "class", "sub_class", "direct_parent",
#                         "chebi_id", "kegg_id")
#   expect_true(all(expected_columns %in% colnames(extracted_data)))
#
#   # Verify that some data exists (i.e., no empty data frame)
#   expect_true(nrow(extracted_data) > 0)
#
#   # Test if 'secondary_accessions' is a concatenated string (not a list or NA)
#   expect_true(all(grepl(";", extracted_data$secondary_accessions) | extracted_data$secondary_accessions == ""))
#
#   # Test if 'synonyms' is properly concatenated and separated by "|"
#   expect_true(all(grepl("\\|", extracted_data$all_names) | extracted_data$all_names == ""))
#
#   # Check if 'InChIKey' column is present and contains non-empty values
#   expect_true(all(nchar(extracted_data$InChIKey) > 0 | extracted_data$InChIKey == ""))
#
#   # Ensure that the cleaning process was successful (check 'all_names' and 'all_HMDBs' columns)
#   expect_true("all_names" %in% colnames(extracted_data))
#   expect_true("all_HMDBs" %in% colnames(extracted_data))
#
#   # Check if 'all_names' contains concatenated synonyms (e.g., "synonym1|synonym2")
#   expect_true(all(grepl("\\|", extracted_data$all_names) | extracted_data$all_names == ""))
#
#   # Check if 'all_HMDBs' contains the expected format (e.g., "accession|secondary1|secondary2")
#   expect_true(all(grepl("\\|", extracted_data$all_HMDBs) | extracted_data$all_HMDBs == ""))
#
#   # Ensure that the cleaned data frame does not contain unnecessary NA values
#   expect_true(all(!is.na(extracted_data$name)))
#   expect_true(all(!is.na(extracted_data$InChIKey)))
#   expect_true(all(!is.na(extracted_data$secondary_accessions) | extracted_data$secondary_accessions == ""))
#
# })
