# MetaMapper Documentation

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


# Building the reference from HMDB

This step needs to bedone once per new XML document. Once generated, you can use the same reference for different studies/project.
To build the reference file, you need to download metabolite in XML format from the HMDB official website [dowload here]([http://www.example.com](https://hmdb.ca/downloads). We recommend loading the "All metabolites" dataset. 
You can build the reference dataset by running the function hmbdexract provided by the package as follows:


```{r}
#replace path_to_xml by the corresponding path to your xml loaded from the hmdb database
path_to_xml<-"data/all_metabolites.xml"

#run the function
df_reference <- hmdbextract(path_to_xm)
```
This function extract metabolite information from the HMDB XML file, organizing it into a structured data frame that will be used as the HMDB reference for the next steps. It should take a few minutes to run (< 5min). 


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
