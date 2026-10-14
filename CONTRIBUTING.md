# Contributing to openESM

Steps 1–11 happen in `openesm-cleaning`; steps 12–15 involve `openesm-metadata` and the website.

### Terms
- Metadata JSON: `data/metadata/XXXX_name_metadata.json`, the source of truth for dataset-level and variable-level metadata; edited directly
- Tracker: private Google Sheet for intake status, author contact, and permissions; not read by any code
- The former curation sheet and coding files are frozen and no longer used

### Conditional steps
- Step 6 applies to every dataset, but is especially relevant if cross-sectional data exist
- Static and sensor files only exist for some datasets

---

## 1. Submission & intake

### How datasets enter the pipeline
- Literature review or community submission via GitHub issue
- Required upfront: citation, data link, brief study description

### Assigning a dataset ID
- Sequential four-digit ID (e.g., `0076`)
- Used as prefix for all associated files: cleaning script, metadata JSON
- Add a row to the tracker

---

## 2. Overlap check

- Check for shared study populations, overlapping time windows, or reuse of the same raw data
- Decision: proceed / merge with existing / flag for discussion
- Document decision in the GitHub issue

---

## 3. Dataset-level metadata

### Fields
- Field definitions: see the [data documentation](https://openesmdata.org/docs/data/)
- General information: `first_author`, `dataset_id`, `year`, `reference_a`, `reference_b`, `dataset_version`, `changelog`
- Code and data access: `zenodo_doi`, `paper_doi`, `link_to_data`, `link_to_code`, `link_to_codebook`, `license`
- Design and participants: `n_participants`, `n_time_points`, `n_days`, `n_beeps_per_day`, `sampling_scheme`, `participants`, `topics`, `passive_data_available`, `cross_sectional_available`, `implicit_missingness`, `raw_time_stamp`
- Record country, language, and types of passive data where available (not yet exported to the JSON)

### Practical notes
- `n_participants`: check in the actual ESM data
- `n_time_points`, `n_days`: check in the paper and the actual ESM data
- `zenodo_doi`: can only be filled in step 12
- `dataset_version`: initialize with `1.0.0`
- `changelog`: newest entry first

### Where it lives
- Metadata JSON, created in step 5 with `new_metadata_json()`

---

## 4. Raw data access

### Locating and obtaining the data
- Sources: OSF, figshare, GitHub, direct author transfer
- Stored locally in `data/raw/` (never committed)
- Use `download_if_missing()` / `osf_download_if_missing()` in the cleaning script

### Reviewing the codebook
- Review original variable naming and response scales
- Note variables requiring transformation, recoding, or special handling

---

## 5. Cleaning script

### Creating the script
- Copy template `scripts/clean_0000x_name.R`, rename to `clean_XXXX_name.R`

### Required output columns
- `id`: participant identifier
- `day`: integer, starting at 1
- `beep`: integer within day, starting at 1

### Cleaning steps
- Rename columns to lowercase snake_case
- Recode missing values with `recode_missing()`
- Check for duplicate id, day, beep combinations
- Add required columns if absent
- Document all transformations

### Validation
- Once the columns are final, create the metadata JSON with `new_metadata_json("XXXX", author, df)`; it never overwrites an existing file
- Run `check_data(df, metadata)` with `metadata <- read_metadata("XXXX")` — errors abort saving; warnings require review
- Save cleaned data as TSV to `data/clean/XXXX_name_ts.tsv`

---

## 6. Anonymity review

### Cross-sectional and time-series data
- Review whether participants can be re-identified from cross-sectional variables
- Check time series data for potentially identifying information
- Consider timestamps, locations, routines, rare event sequences, free text, and combinations of variables
- Consider whether the data could be linked with public or external information
- `sdcMicro` may help assess disclosure risk and apply statistical disclosure-control methods. This is particularly important for future cross-sectional data
- When in doubt, do not include

### Guidelines to consider
- First identify which guidance applies to the dataset, jurisdiction, data access model, and intended release
- Consider the ICO guidance on anonymisation, identifiability, linkage risk, and the motivated intruder test
- Consider the UK Anonymisation Network's guidance and decision-making framework
- Both of these are UK-based, but still very usefully explained
- Consider relevant data protection requirements, including GDPR Recital 26 where applicable

### Decision
- Include / hold / upload separately


---

## 7. Variable annotation

### Where it lives
- `features` array in the metadata JSON, edited directly
- One entry per variable

### Fields
- Field definitions: see the [data documentation](https://openesmdata.org/docs/data/)
- `name`: must match the column name in the cleaned TSV
- `description`: short neutral label (e.g., "Positive affect")
- `details`: exact item wording
- `labels`: one response option per line, format `1 = Label`

### Scope
- Administrative variables (`id`, `day`, `beep`, timestamps): no construct needed
- Technical/contextual variables not measuring a psychological construct: note as out of scope

---

## 8. Similarity computation

### When to run
- After descriptions and details are filled in for all rating-scale items

### Running the script
```bash
python scripts/annotation/compute_similarity.py
```
- Updates `data/similarity/similar_items.json`
- Caches embeddings; only new items are re-embedded

### Output
- Top-10 cross-dataset neighbors per item, ranked by cosine similarity
- Each neighbor shows variable description and assigned construct

---

## 9. Construct coding

### Using similarity suggestions
```bash
Rscript scripts/annotation/suggest_constructs.R 0076
```
- Prints neighbor constructs per item
- Writes full table to `data/construct_suggestions_0076.csv`

### Assigning constructs
- Comma-separated string (e.g., `"positive affect, affect"`)
- Prefer existing vocabulary; most specific label first
- Do not only rely on similarity assessment. Also try to search for similar items across datasets, e.g. in `data\variables_constructs.rds`
- Novel constructs: flag explicitly for taxonomy review

### Independent validation
- Second coder validates all construct assignments
- Disagreements resolved by discussion

---

## 10. Metadata finalization

- Re-run cleaning script from scratch
- Resolve all `check_data()` warnings about mismatches between data and metadata JSON

---

## 11. Repository integration

- Open PR with cleaning script and metadata JSON
- PR description: summary of dataset content, any unusual cleaning/annotation decisions
- Second team member reviews before merge

---

## 12. Zenodo archival
- Goal: upload all data, including codebooks, to Zenodo
- Each dataset is a separate object in the [openESM Zenodo community](https://zenodo.org/communities/openesm/)
- If necessary, test the workflow in [Zenodo Sandbox](https://sandbox.zenodo.org/communities/openesm/); sandbox uploads are not permanently public

### Upload workflow
- Create a Zenodo account if you haven't done so
- Open the openESM community and click **New Upload**
- Upload only the cleaned time series data and codebooks
- Remove irrelevant files or sheets before publishing
- Reserve a DOI by selecting **I need one**

### Dataset fields
- Resource type: **Dataset**
- Title: use the `id_name` value, e.g., `0001_fried`
- Creator: add the organization **openESM**
- Version: `1.0.0`
- Description: `This dataset was taken from [Data origin LINK] and cleaned and harmonized for the openESM project (https://openesmdata.org). Please cite the original authors [DOI] and the openESM project when re-using this dataset and follow the license conditions. For more citation information, please visit our website.`
- License: use the `license` field in the metadata
- If the license is unclear, stop and ask before publishing

### DOI
- Publish the Zenodo record and copy the DOI
- Enter the DOI in the metadata JSON under `zenodo_doi`
- Push the updated JSON to `main` (the JSON is pushed twice: before and after the DOI)

---

## 13. Metadata sync

```r
# In the openesm-metadata repo
source("copy_metadata.R")   # pull latest JSONs from openesm-cleaning
source("bundle_metadata.R") # regenerate datasets.json
```

- `copy_metadata.R` pulls from `main` on GitHub, so push all metadata changes first

---

## 14. Descriptives & similarity update

```bash
Rscript descriptives/compute_descriptives.R 0076
python scripts/annotation/compute_similarity.py
```

- Commit updated output files
- Trigger **Push descriptives to openesm website** workflow from Actions tab

---

## 15. Release

- Bump version in `openesm-metadata`
- Create versioned GitHub release on `openesm-metadata`
- Automated website redeployment follows

---

## Open items

### Roles
- Who approves submissions and assigns IDs
- Who performs the overlap check
- Who performs the anonymity review and decides on inclusion
- Who second-codes constructs and resolves disagreements
- Who reviews PRs and gives final approval before publication
- Who triggers workflows and creates releases

### Infrastructure
- Who may edit the tracker
- Metadata validation (function and CI) is being reworked
- Whether static and sensor files are part of the standard pipeline

### Definitions
- Allowed values for `variable_type` and `assessment_type`
- `year`: publication year (schema) or data collection year (website)?
- Whether to add country, language, and types of passive data to the JSON
- Authoritative construct vocabulary and what counts as a novel construct

### Anonymity
- Acceptable level of identification risk
- When `sdcMicro` is required
- How to assess disclosure risk in time series
- Where to document the risk assessment and decision

### Zenodo
- Reserve the DOI before or after merging the PR
- Whether the Zenodo version always matches `dataset_version`
