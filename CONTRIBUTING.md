# Contributing to openESM

Steps 1–11 happen in `openesm-cleaning`; steps 12–15 involve `openesm-metadata` and the website.

---

## 1. Submission & intake

### How datasets enter the pipeline
- Literature review or community submission via GitHub issue
- Required upfront: citation, data link, brief study description

### Assigning a dataset ID
- Sequential four-digit ID (e.g., `0076`)
- Used as prefix for all associated files: cleaning script, metadata JSON, coding file

---

## 2. Overlap check

- Check for shared study populations, overlapping time windows, or reuse of the same raw data
- Decision: proceed / merge with existing / flag for discussion
- Document decision in the GitHub issue

---

## 3. Dataset-level metadata

### Fields
- Citation (BibTeX), paper DOI, data link
- `n_participants`, n days, beeps per day, total time points
- Country, language, participant description
- Sampling scheme, passive data availability, cross-sectional data availability
- License
- Implicit missingness, raw timestamp availability

### Where it lives
- Central curation sheet → written to metadata JSON by `write_metadata()`

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
- Add required columns if absent
- Document all transformations

### Validation
- Run `check_data(df, dataset_info, variable_data)` — errors abort saving; warnings require review
- Save cleaned data as TSV to `data/clean/XXXX_name_ts.tsv`

---

## 6. Cross-sectional data check

### Anonymity review
- Review whether participants can be re-identified from cross-sectional variables
- When in doubt, do not include

### Decision
- Include / hold / upload separately
- Document under `cross_sectional_available` and `additional_comments` in metadata

---

## 7. Variable annotation

### Creating the coding file
- Dedicated Google Sheets coding file per dataset
- URL recorded in curation sheet under "Coding File URL"
- One row per variable

### Fields
- `name`: variable name in cleaned TSV
- `description`: short neutral label (e.g., "Positive affect")
- `variable_type`: `rating_scale`, `binary`, `categorical`, `numeric`, `PosixCt`, `other`
- `details`: exact item wording or precise measurement description
- `labels`: response option labels (one per line, format: `1. Label`)
- `answer_categories`: number of response options
- `source`: original scale or instrument
- `assessment_type`: `ESM`, `Daily`, or `Other`

### Scope
- Administrative variables (`id`, `day`, `beep`, timestamps): no construct needed
- Technical/contextual variables not measuring a psychological construct: note as out of scope

---

## 8. Similarity computation

### When to run
- After descriptions and details are filled in for all rating-scale items

### Running the script
```bash
python scripts/compute_similarity.py
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
Rscript scripts/suggest_constructs.R 0076
```
- Prints neighbor constructs per item
- Writes full table to `data/construct_suggestions_0076.csv`

### Assigning constructs
- Comma-separated string (e.g., `"positive affect, affect"`)
- Prefer existing vocabulary; most specific label first
- Novel constructs: flag explicitly for taxonomy review

### Independent validation
- Second coder validates all construct assignments
- Disagreements resolved by discussion

---

## 10. Metadata finalization

- Re-run cleaning script from scratch
- Call `write_metadata()` to generate the metadata JSON
- Run `validate_metadata_json()` for schema validation
- Run `scripts/diagnose_check_data.R` — review all warnings
- Resolve issues or document them in `data/check_data_warnings.csv`

---

## 11. Repository integration

- Open PR with cleaning script and metadata JSON
- PR description: summary of dataset content, any unusual cleaning/annotation decisions
- CI: schema validation must pass
- Second team member reviews before merge

---

## 12. Zenodo archival

### What to upload
- Cleaned TSV, metadata JSON, cleaning script

### DOI
- Register DOI on Zenodo
- Write back to `zenodo_doi` in metadata JSON
- Re-run `write_metadata()` to regenerate JSON

---

## 13. Metadata sync

```r
# In the openesm-metadata repo
source("copy_metadata.R")   # pull latest JSONs from openesm-cleaning
source("bundle_metadata.R") # regenerate datasets.json
```

- Verify with `Rscript scripts/diff_metadata_repos.R <path-to-openesm-metadata>`

---

## 14. Descriptives & similarity update

```bash
Rscript descriptives/compute_descriptives.R 0076
python scripts/compute_similarity.py
```

- Commit updated output files
- Trigger **Push descriptives to openesm website** workflow from Actions tab

---

## 15. Release

- Bump version in `openesm-metadata`
- Create versioned GitHub release on `openesm-metadata`
- Automated website redeployment follows
