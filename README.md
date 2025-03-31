# MetaboMapper Documentation

MetaMapper is an R package developed to connect metabolite names and IDs from various metabolomic studies to HMDB identifiers, enabling cross-study comparisons. The package currently supports mapping the following types of identifiers to HMDB:

- Metabolite name  
- HMDB ID  
- INCHIKEY  
- KEGG ID  
- CHEBI ID


# Installation

```{r}
#Install devtools in R
install.packages("devtools")

#Install MetaboMapper package
install_github("mariamaitoumelloul/MetaboMapper")
```

# Building the Reference from HMDB

To build the reference file, you first need to download the metabolites dataset in XML format from the [HMDB official website](https://hmdb.ca/downloads). We recommend downloading the "All metabolites" dataset.

Once downloaded, you can build the reference dataset by running the `hmdbextract` function provided by the package as follows:

```{r}
# Replace 'path_to_xml' with the actual path to your XML file downloaded from the HMDB database
path_to_xml <- "data/all_metabolites.xml"

# Run the function to create the reference dataset
df_reference <- hmdbextract(path_to_xml)
```
This step needs to be done only once for each new XML document. Once the reference file is generated, it can be reused across different studies or projects.


# Examples

Now that you have the reference dataset, you can use it to map any of the following identifiers to HMDB identifiers:

- **Name**: The function compares the metabolite name from your study with the names and synonyms listed in the HMDB database to find matches.
- **HMDB**: A metabolite may have multiple HMDB IDs (secondary accessions). The function ensures that the HMDB ID in your study is mapped to all versions of the HMDB IDs in the database.
- **INCHIKEY**: The INCHIKEY in your study is processed to extract both the skeleton (base) and the complete key (without the version number), which are then matched to the INCHIKEY in the HMDB database. Note that matching the INCHIKEY skeleton has a lower confidence score than the full version, as it ignores various forms of isomerism (stereo, tautomeric, etc.).
- **Other IDs**: The function can also map KEGG and CHEBI IDs to those available in the HMDB database.

### Input Requirements

To perform the mapping, the following inputs are required:
1. **Reference DataFrame**: The reference dataset generated in the previous step. For more details, refer to the [Building the Reference from HMDB](#building-the-reference-from-hmdb) section.
2. **Study DataFrame**: Your study data, which must include at least one column with metabolite names. Additional columns with related identifiers can also be included.
3. **Identifiers**: This parameter specifies which type of identifier you want to use for mapping. Acceptable values are: `NAME`, `INCHIKEY`, `HMDB`, `KEGG`, or `CHEBI`.

Notes
The details parameter in the function controls the level of information returned:
- If details = `TRUE`, additional information about the mapping process (e.g., synonyms matched, intermediate steps) will be included in the output.
- If details = `FALSE`, only the essential mapped data is returned.
Always ensure that your df_reference and df_study DataFrames are formatted correctly before running the MetaboMap function to avoid errors.

The function may take a few seconds (<2 minutes) to run, especially when using "NAME" as the identifier to map.

### Example: Map One Identifier at a Time

Here is how to map a single identifier to HMDB:

```{r}
# Example when only the metabolite names are provided
df_study <- data.frame(
  metabolite_name = c("1-Methylhistidine", "2-Ketobutyric acid")
)

# Running the MetaboMap function to map based on 'NAME'
df_study_hmdb_matched <- MetaboMap(
  df_study = df_study,
  study_cols = c("metabolite_name"),
  identifiers = c("NAME"), 
  df_reference = df_reference,
  details = FALSE,
  original_colname = "metabolite_name"
)

```{r}
# Example to match based on HMDB ID
df_study <- data.frame(
  metabolite_name = c("1-Methylhistidine", "2-Ketobutyric acid"),
  HMDB_id = c("HMDB0000001", "HMDB0000005")
)

# Running the MetaboMap function to map based on 'HMDB'
df_study_hmdb_matched <- MetaboMap(
  df_study = df_study,
  study_cols = c("HMDB_id"),
  identifiers = c("HMDB"), 
  df_reference = df_reference,
  details = FALSE,
  original_colname = "metabolite_name"
)
```

### Expected Output
The resulting DataFrame will include the following columns:

- Original metabolite name: The metabolite name as provided in your study.
- HMDB_label and HMDB_id: These columns contain the HMDB identifiers mapped from your input data. These can be used as references in future steps for matching with other studies.
- Confidence level: A confidence level is assigned to each match, depending on the identifier used:
For most identifiers (NAME, HMDB, KEGG, CHEBI), the confidence level is 3.
For INCHIKEY skeleton matches, the confidence level is 2, as this type of match ignores stereochemistry and other forms of isomerism.

### Example: Map Multiple Identifiers at a Time

If you need to map multiple identifiers simultaneously, ensure the order of study_cols and identifiers is consistent:

```{r}
# Example to map both HMDB ID and metabolite name
df_study <- data.frame(
  metabolite_name = c("1-Methylhistidine", "2-Ketobutyric acid"),
  HMDB_id = c("HMDB0000001", "HMDB0000005")
)

# Running the MetaboMap function to map based on both 'HMDB' and 'NAME'
df_study_hmdb_matched <- MetaboMap(
  df_study = df_study,
  study_cols = c("HMDB_id", "metabolite_name"),
  identifiers = c("HMDB", "NAME"), 
  df_reference = df_reference,
  details = FALSE,
  original_colname = "metabolite_name"
)
```

