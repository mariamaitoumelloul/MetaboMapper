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

# Example usage:
study_data <- data.frame(metabolite_name=c("M1","M2"),id1 = c(1, 2), id2 = c("A", "B"))
reference_data <- data.frame(ref_id = c(1, 2), name = c("X", "Y"))
result <- MetaboMap(
  df_study = study_data,
  study_cols = c("id1", "id2"),
  identifiers = c("ref_id", "name"),
  df_reference = reference_data,
  details = FALSE,
  original_name = "metabolite_name"
)



cleanEx()
nameEx("hmdbextract")
### * hmdbextract

flush(stderr()); flush(stdout())

### Name: hmdbextract
### Title: Extract Information of interest from HMDB XML File
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
