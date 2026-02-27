# openESM Database Cleaning Code

This repository contains the cleaning scripts and metadata for the [openESM Database](https://openesmdata.org), a collection of harmonized Experience Sampling Method (ESM) datasets. Due to data sharing restrictions, raw and cleaned data files are not included; only scripts and metadata are published here. The CC-BY-4.0 license applies to the cleaning scripts, utility functions, and metadata files created by the openESM project. Individual datasets are subject to their original licenses, documented in each dataset's metadata JSON under the license field.


## Repository structure

```
scripts/          Cleaning scripts (one per dataset) + shared utility functions
data/metadata/    Harmonized metadata as JSON (one file per dataset)
data/schema/      JSON Schema for metadata validation
data/clean/       Cleaned data files (not in repo)
data/raw/         Raw data files (not in repo, but links are available in metadata)
```

## Workflow

Each dataset goes through the following steps:

1. **Download** raw data from its original source (OSF, GitHub, figshare, etc.)
2. **Clean** the data in a dedicated `scripts/clean_XXXX_name.R` script — renaming columns to a standard scheme, adding required columns (`id`, `day`, `beep`), and removing or transforming variables as needed
3. **Validate** the cleaned data with `check_data()`, which enforces structural requirements
4. **Annotate variables** by filling a Google Sheets coding file with variable-level metadata (description, construct, response scale, etc.)
5. **Generate metadata** with `create_metadata_json()`, which combines the Google Sheets annotations with dataset-level information into a standardized JSON file

## Key functions (`scripts/functions_data.R`)

| Function | Purpose |
|---|---|
| `check_data(df)` | Validates that a cleaned data frame meets structural requirements |
| `create_metadata_json(dataset_id)` | Builds the metadata JSON from Google Sheets data |
| `list_variable_construct_pairs(folder)` | Extracts all variable–construct mappings across datasets |
| `annotate_constructs(...)` | Propagates construct labels to unannotated datasets with matching variable names |
| `extract_construct_vocabulary(folder)` | Returns a frequency table of all constructs used across datasets |
| `validate_metadata_json(path)` | Checks a metadata JSON against the openESM schema |

## Experimental: AI-assisted annotation (`scripts/functions_annotation.R`)

`generate_annotation_prompt()` assembles a structured prompt from the cleaned data frame, an optional codebook (PDF or XLSX), the cleaning script, and the existing construct vocabulary. The prompt can be passed to an LLM to pre-fill variable metadata, which is then reviewed and converted to an xlsx coding sheet via `annotation_json_to_xlsx()`.

## For further questions

Please contact the openESM team or open an issue on GitHub.




