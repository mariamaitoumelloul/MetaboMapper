# library(testthat)
# library(MetaboMapper)


# # Example input data for tests
# df_study <- data.frame(
#   metabolite_name = c("1-Methylhistidine", "2-Ketobutyric acid"),
#   HMDB_id = c("HMDB0000001", "HMDB0000005")
# )
#
# df_reference <- data.frame(
#   all_HMDBs = c(
#     "HMDB0000001|HMDB00001|HMDB0004935",
#     "HMDB0000002|HMDB00002",
#     "HMDB0000005|HMDB00005|HMDB0006544"
#   ),
#   all_names = c(
#     "1-Methylhistidine|Pi-methylhistidine",
#     "1,3-Diaminopropane|1,3-Propanediamine|1,3-Propylenediamine",
#     "2-Ketobutyric acid|2-Ketobutanoic acid|2-Oxobutyric acid"
#   )
# )
#
#
# # Unit tests
#
# test_that("MetaboMap throws error for mismatched identifier lengths", {
#   expect_error(
#     MetaboMap(
#       df_study = df_study,
#       study_cols = c("metabolite_name", "HMDB_id"),
#       identifiers = c("NAME"), # Only one identifier instead of two
#       df_reference = df_reference,
#       details = FALSE,
#       original_colname = "metabolite_name"
#     ),
#     "Identifiers and study_cols must have the same length!"
#   )
# })
#
# test_that("MetaboMap throws error for invalid identifiers", {
#   expect_error(
#     MetaboMap(
#       df_study = df_study,
#       study_cols = c("metabolite_name", "HMDB_id"),
#       identifiers = c("INVALID", "HMDB"), # INVALID is not a valid identifier
#       df_reference = df_reference,
#       details = FALSE,
#       original_colname = "metabolite_name"
#     ),
#     "The following identifiers are invalid: INVALID"
#   )
# })
#
# test_that("MetaboMap throws error for missing study columns", {
#   expect_error(
#     MetaboMap(
#       df_study = df_study,
#       study_cols = c("metabolite_name", "NON_EXISTING_COLUMN"), # NON_EXISTING_COLUMN does not exist in df_study
#       identifiers = c("NAME", "HMDB"),
#       df_reference = df_reference,
#       details = FALSE,
#       original_colname = "metabolite_name"
#     ),
#     "The following columns are missing in the study data frame: NON_EXISTING_COLUMN"
#   )
# })
#
# test_that("MetaboMap throws error when reference data frame is missing", {
#   expect_error(
#     MetaboMap(
#       df_study = df_study,
#       study_cols = c("metabolite_name", "HMDB_id"),
#       identifiers = c("NAME", "HMDB"),
#       df_reference = NULL, # df_reference is NULL
#       details = FALSE,
#       original_colname = "metabolite_name"
#     ),
#     "Reference data frame is missing or NULL. To produce the reference data frame, please run hmdbextract function to generate it."
#   )
# })
#
#
#
#
