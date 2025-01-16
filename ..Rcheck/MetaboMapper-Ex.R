pkgname <- "MetaboMapper"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('MetaboMapper')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("MetaboMap")
### * MetaboMap

flush(stderr()); flush(stdout())

### Name: MetaboMap
### Title: MetaboMap
### Aliases: MetaboMap

### ** Examples

study_data <- data.frame(
  metabolite_name = c("1-Methylhistidine", "2-Ketobutyric acid"),
  HMDB_id = c("HMDB0000001", "HMDB0000005")
)

reference_data <- data.frame(
  accession = c("HMDB0000001", "HMDB0000002", "HMDB0000005"),
  name = c("1-Methylhistidine", "1,3-Diaminopropane", "2-Ketobutyric acid"),
  all_HMDBs = c(
    "HMDB0000001|HMDB00001|HMDB0004935",
    "HMDB0000002|HMDB00002",
    "HMDB0000005|HMDB00005|HMDB0006544"
  ),
  all_names = c(
    "1-Methylhistidine|Pi-methylhistidine|",
    "1,3-Diaminopropane|1,3-Propanediamine|1,3-Propylenediamine",
    "2-Ketobutyric acid|2-Ketobutanoic acid|2-Oxobutyric acid"
  )
)

result <- MetaboMap(
  df_study = study_data,
  study_cols = c("metabolite_name", "HMDB_id"),
  identifiers = c("NAME", "HMDB"),
  df_reference = reference_data,
  details = FALSE,
  original_colname = "metabolite_name"
)



cleanEx()
nameEx("hmdbextract")
### * hmdbextract

flush(stderr()); flush(stdout())

### Name: hmdbextract
### Title: Extract metabolite information from HMDB XML File
### Aliases: hmdbextract

### ** Examples

## Not run: 
##D   # Example usage
##D   extracted_data <- hmdbextract("path_to_hmdb.xml")
##D   head(extracted_data)
## End(Not run)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
