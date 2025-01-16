library(testthat)
library(MetaboMapper)



test_that("remove_letters_between_parentheses works correctly", {
  # Test cases with various input strings
  expect_equal(remove_letters_between_parentheses("This is a test (abc)."), "This is a test .")
  expect_equal(remove_letters_between_parentheses("No parentheses here."), "No parentheses here.")
  expect_equal(remove_letters_between_parentheses("Text (abc) more text (xyz)."), "Text  more text .")
  expect_equal(remove_letters_between_parentheses("Test (1)."), "Test (1).")
  expect_equal(remove_letters_between_parentheses("Multiple (a) (b) (c)."), "Multiple   .")
})


test_that("process_labels works correctly", {
  # Sample data frame to test the function
  df <- data.frame(label = c("Label 1 (short)", "Example-Label (2)", "Another Example (abc)", "Test Label (xyz) 123"), stringsAsFactors = FALSE)

  # Expected output after cleaning
  expected_df <- data.frame(label = c("Label 1 (short)", "Example-Label (2)", "Another Example (abc)", "Test Label (xyz) 123"),
                            label_processed = c("label1", "examplelabel", "anotherexample", "testlabel123"))

  # Apply the process_labels function
  result_df <- process_labels(df, "label")

  # Check if the processed column matches the expected result
  expect_equal(result_df$label_processed, expected_df$label_processed)
})

test_that("process_inchikey works correctly", {
  # Sample data frame with InChIKey
  df <- data.frame(inchikey = c("XCVBNM1234567890ABCDEFGHIJKLMN", "ABCD1234567890XYZLMNOPQRSTUVWXYZ"), stringsAsFactors = FALSE)

  # Expected output with skeleton and processed InChIKeys
  expected_df <- data.frame(inchikey = c("XCVBNM1234567890ABCDEFGHIJKLMN", "ABCD1234567890XYZLMNOPQRSTUVWXYZ"),
                            inchikey_skeleton = c("XCVBNM12345678", "ABCD1234567890"),
                            inchikey_processed = c("XCVBNM1234567890ABCDEFG", "ABCD1234567890XYZLMNOPQ"))

  # Apply the process_inchikey function
  result_df <- process_inchikey(df, "inchikey", N_skeleton = 14, N_inchikey = 23)

  # Check if the skeleton and processed InChIKeys are correct
  expect_equal(result_df$inchikey_skeleton, expected_df$inchikey_skeleton)
  expect_equal(result_df$inchikey_processed, expected_df$inchikey_processed)
})






