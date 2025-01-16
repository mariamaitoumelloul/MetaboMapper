library(testthat)
library(MetaboMapper)



# # Unit test for match_identifier function
# test_that("match_identifier correctly matches identifiers", {
#   # Example cohort data (df_cohort) with identifiers
#   df_cohort <- data.frame(metabolite_id = c("INCHIKEY12345", "INCHIKEY67890", "INCHIKEY54321", "INCHIKEY00000"))
#
#   # Example reference data (df_reference) with identifiers to match
#   df_reference <- data.frame(accession = c("HMDB0001", "HMDB0002", "HMDB0003"),
#                              inchikey_skeleton_in_reference_INCHIKEY = c("INCHIKEY12345", "INCHIKEY67890", "INCHIKEY99999"),
#                              inchikey_processed_in_reference_INCHIKEY = c("INCHIKEY1234567890", "INCHIKEY6789012345", "INCHIKEY9999900000"))
#
#   # Apply match_identifier function
#   result <- match_identifier(df_cohort, "metabolite_id", df_reference, "inchikey_skeleton", "INCHIKEY")
#
#   # Check that the new column 'mapped_to_inchikey_skeleton' is created
#   expect_true("mapped_to_inchikey_skeleton" %in% colnames(result))
#
#   # # Check that the merged dataset has the expected number of rows (df_cohort's rows + new reference columns)
#   # expect_equal(nrow(result), nrow(df_cohort))
#   #
#   # # Check that 'mapped_to_inchikey_skeleton' is correctly assigned based on matching identifiers
#   # expect_true(all(result$mapped_to_inchikey_skeleton[c(1, 2)] == TRUE))  # Expect TRUE for the first and second rows (matched)
#   # expect_true(all(result$mapped_to_inchikey_skeleton[3] == FALSE))  # Expect FALSE for the third row (no match)
#   # expect_true(all(result$mapped_to_inchikey_skeleton[4] == FALSE))  # Expect FALSE for the fourth row (no match)
#   #
#   # # Test with processed INCHIKEY column
#   # result <- match_identifier(df_cohort, "metabolite_id", df_reference, "inchikey_processed", "INCHIKEY")
#   #
#   # # Check that the new column 'mapped_to_inchikey_processed' is created
#   # expect_true("mapped_to_inchikey_processed" %in% colnames(result))
#   #
#   # # Check the mapping correctness for the processed INCHIKEY
#   # expect_true(all(result$mapped_to_inchikey_processed[c(1, 2)] == TRUE))  # Expect TRUE for the first and second rows (matched)
#   # expect_true(all(result$mapped_to_inchikey_processed[3] == FALSE))  # Expect FALSE for the third row (no match)
#   # expect_true(all(result$mapped_to_inchikey_processed[4] == FALSE))  # Expect FALSE for the fourth row (no match)
#   #
#   # # Ensure the column names in reference are properly modified by the identifier
#   # expect_true("inchikey_skeleton_in_reference_INCHIKEY" %in% colnames(result))
#   # expect_true("inchikey_processed_in_reference_INCHIKEY" %in% colnames(result))
# })
#
# # # Run the tests
# # test_that("match_identifier unit test", {
# #   # Example cohort data
# #   df_cohort <- data.frame(metabolite_id = c("INCHIKEY12345", "INCHIKEY67890", "INCHIKEY54321", "INCHIKEY00000"))
# #
# #   # Example reference data
# #   df_reference <- data.frame(accession = c("HMDB0001", "HMDB0002", "HMDB0003"),
# #                              inchikey_skeleton_in_reference_INCHIKEY = c("INCHIKEY12345", "INCHIKEY67890", "INCHIKEY99999"),
# #                              inchikey_processed_in_reference_INCHIKEY = c("INCHIKEY1234567890", "INCHIKEY6789012345", "INCHIKEY9999900000"))
# #
# #   result <- match_identifier(df_cohort, "metabolite_id", df_reference, "inchikey_skeleton", "INCHIKEY")
# #
# #   # Check if the new column 'mapped_to_inchikey_skeleton' is present
# #   expect_true("mapped_to_inchikey_skeleton" %in% colnames(result))
# #
# #   # Check for correct mapping in the 'mapped_to_inchikey_skeleton' column
# #   expect_equal(sum(result$mapped_to_inchikey_skeleton, na.rm = TRUE), 2)  # Should match 2 identifiers
# #
# #   result <- match_identifier(df_cohort, "metabolite_id", df_reference, "inchikey_processed", "INCHIKEY")
# #
# #   # Check if the new column 'mapped_to_inchikey_processed' is present
# #   expect_true("mapped_to_inchikey_processed" %in% colnames(result))
# #
# #   # Check for correct mapping in the 'mapped_to_inchikey_processed' column
# #   expect_equal(sum(result$mapped_to_inchikey_processed, na.rm = TRUE), 2)  # Should match 2 identifiers
# # })
