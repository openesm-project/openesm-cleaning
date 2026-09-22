# Construct Annotation Harness and Review Workbench

## Purpose

Design a provider-neutral, human-in-the-loop workflow that helps curate
psychological and behavioral construct labels for openESM variables. The system
must be useful for annotating new datasets soon, while also exposing probable
inconsistencies in historical annotations. This document is a proposal for
review before implementation.

The production metadata format remains backward compatible: a feature has one
comma-separated `construct` string. The annotation system may use richer
internal fields, but reviewed decisions are exported back to that existing
field.

## Current State and Constraints

- `data/metadata/` is the source of historical feature annotations.
- Existing annotations are useful precedents, not error-free ground truth.
- `data/variables_constructs.xlsx` currently stores `dataset_id`,
  `variable_name`, and a comma-separated construct string. It encodes no formal
  hierarchy and existing strings range from one to eight labels.
- `scripts/compute_similarity.py` embeds rating-scale item descriptions and
  details with `BAAI/bge-large-en-v1.5`, then writes cross-dataset nearest
  neighbors to `data/similarity/similar_items.json`.
- The current experimental functions generate a broad, manual LLM prompt, but
  do not provide reliable retrieval, structured review, persistence,
  evaluation, or provider independence.
- Annotation applies only to psychological or behavioral variables. Technical,
  administrative, identifier, date/time, and purely contextual variables must
  be explicitly classified as out of scope rather than forced into a construct.
- External model APIs are acceptable, but the workflow must also support local
  models and a manual copy/paste route.

## Goals

1. Help a reviewer make construct decisions faster without obscuring evidence.
2. Produce ranked suggestions: one primary construct plus zero or more
   additional constructs.
3. Preserve specific labels when they are useful, alongside broader labels when
   appropriate. Do not require a fixed number of hierarchy levels.
4. Allow new constructs, but make them explicit review candidates rather than
   silently treating them as established vocabulary.
5. Support evaluation across prompts, models, providers, and retrieval settings.
6. Persist enough provenance to reproduce a reviewed decision and investigate a
   poor suggestion later.
7. Generate useful inconsistency candidates from the current historical corpus.

## Non-Goals for the First Release

- Replacing the current Google Sheets curation workflow.
- Changing the public JSON schema or requiring a three-column construct
  hierarchy.
- Automatically writing unreviewed suggestions to a coding sheet or metadata
  JSON.
- Fully reconciling all historical construct annotations before supporting new
  datasets.
- Building accounts, collaboration roles, adjudication workflows, or a hosted
  multi-user service.
- Depending on any single model vendor, structured-output feature, or embedding
  provider.

## Product Shape

Build a local annotation harness with two connected modes.

### 1. Review Workbench

The main workflow for annotating individual variables. A reviewer selects an
uncoded item, inspects source context and retrieved precedents, obtains one or
more model suggestions, edits the construct set, and saves a decision.

Each item view should show:

- Dataset ID, variable name, variable type, assessment type, and current
  construct string.
- Description, exact wording/details, source, labels, answer categories, and
  transformation when available.
- Codebook evidence when it can be linked to the item, with missing evidence
  shown clearly.
- Exact-name matches from historical data, separately from semantic matches.
- Top semantic neighbors, including their text, dataset ID, construct string,
  and similarity score.
- The retrieved evidence packet supplied to the model.
- The model's proposal, confidence, rationale, and cited example IDs.
- Editable primary and additional construct controls.
- Actions to accept, save edited, mark out of scope, defer, or flag for later
  taxonomy review.

The reviewer must be able to see both all retrieved neighbors and the subset
the model cited. A model's rationale is evidence navigation, not proof.

### 2. Evaluation and Audit Harness

The same retrieval and generation pipeline runs against a fixed benchmark and
produces diagnostics. It serves two purposes:

- Compare model/provider/prompt/retrieval configurations before using them
  routinely.
- Surface suspicious historical mappings for human review.

This must share the exact item representation, retrieval implementation, and
output validation used by the workbench. The harness should not be a separate,
unrepresentative experimental script.

## Canonical Data Model

Use provider-neutral records internally. The proposal below is illustrative;
field names can change during implementation, but the separation of concerns
should remain.

### Item Record

```json
{
  "item_id": "0075:gaming_autonomy",
  "dataset_id": "0075",
  "variable_name": "gaming_autonomy",
  "variable_type": "rating_scale",
  "assessment_type": "ESM",
  "description": "Played Way I Wanted",
  "details": "I could play in the way I wanted.",
  "labels": "1 = Strongly disagree; 7 = Strongly agree",
  "answer_categories": "7",
  "source": "BANGS",
  "transformation": "",
  "current_construct": "video games, autonomy, self-determination",
  "codebook_excerpt": null,
  "eligible_for_construct_annotation": true
}
```

`item_id` must be stable across runs. It should not include the selected model
or current construct value.

### Retrieval Evidence Record

```json
{
  "evidence_id": "0042:autonomy_item",
  "retrieval_method": "semantic",
  "rank": 1,
  "similarity": 0.8621,
  "dataset_id": "0042",
  "variable_name": "autonomy_item",
  "item_text": "...",
  "construct": "autonomy, self-determination",
  "annotation_status": "historical"
}
```

Evidence must retain the exact text and construct labels that were available at
the time of generation. Later edits to metadata must not silently rewrite the
record of why a prior recommendation was made.

### Normalized Model Proposal

```json
{
  "in_scope": true,
  "primary_construct": "autonomy",
  "additional_constructs": ["self-determination"],
  "new_construct_candidates": [],
  "confidence": "medium",
  "cited_evidence_ids": ["0042:autonomy_item"],
  "rationale": "The item asks whether the participant could act as desired...",
  "abstain_reason": null
}
```

For out-of-scope items, `in_scope` is `false`, all construct arrays are empty,
and `abstain_reason` explains the classification. The application validates this
contract locally regardless of provider capabilities.

### Review Decision

```json
{
  "decision_id": "uuid",
  "item_id": "0075:gaming_autonomy",
  "status": "accepted_edited",
  "final_primary_construct": "autonomy",
  "final_additional_constructs": ["self-determination", "video games"],
  "final_construct_string": "autonomy, self-determination, video games",
  "reviewer_note": null,
  "created_at": "2026-07-24T12:00:00Z"
}
```

Each review decision references immutable snapshots of the item, retrieval
evidence, prompt/task version, normalized proposal, raw response, and provider
configuration. The reviewer decision, not the model proposal, is exportable.

## Retrieval Design

### Initial Sources

1. Semantic neighbors from `data/similarity/similar_items.json`.
2. Exact cleaned-variable-name matches across datasets.
3. Optional direct text matches or source-instrument matches, if available.

The first implementation should use the existing similarity output as a
retrieval source rather than replace it. It should, however, load the source
metadata alongside it so the evidence record has the full text and current
construct string.

### Eligibility and Filtering

- Retrieve from historically annotated psychological/behavioral items only when
  such a filter is available.
- Exclude the target item itself and items from the same dataset by default.
- Keep exact-name results even when semantic similarity is low, but label them
  clearly as weaker evidence.
- Retain the top 5 to 10 semantic neighbors, subject to a configurable
  similarity threshold.
- Do not hide conflicting labels. Disagreement among close neighbors is useful
  evidence for uncertainty and inconsistency review.

### Later Retrieval Improvements

- Rebuild or extend the index to cover additional eligible variable types once
  their item text is sufficiently documented.
- Index codebook excerpts where reliable item-level matching is possible.
- Use hybrid retrieval combining embeddings, lexical matching, and source/
  instrument metadata.
- Prefer previously reviewed annotations over unreviewed historical labels if a
  curated reference subset becomes available.

## Provider-Neutral Model Layer

The application owns the canonical request and proposal contracts. Provider
adapters translate between those contracts and vendor-specific APIs.

### Required Adapters

- `manual`: writes a review packet and accepts pasted raw model output. This
  supports web-chat subscriptions, including Claude Pro, without requiring API
  credentials.
- `anthropic_api`: Anthropic Messages API using a separate API credential.
- `openai_compatible`: OpenAI API and local OpenAI-compatible endpoints such as
  Ollama, LM Studio, or vLLM.

Gemini or other providers can be added as adapters without changing retrieval,
review, persistence, exports, or evaluation.

### Adapter Interface

Every adapter should implement the conceptual operations below:

```text
capabilities() -> provider features and limits
generate(request, settings) -> raw response and normalized proposal attempt
```

The core must:

1. Build a versioned, provider-neutral task packet.
2. Ask an adapter to generate a response.
3. Store the unmodified raw response.
4. Parse and validate the result against the normalized proposal contract.
5. Offer a repair or retry workflow for malformed output.
6. Never export a model proposal without a reviewer decision.

Use structured output or JSON Schema response modes when available, but treat
them as optional optimizations. The local validator remains authoritative.

### Configuration and Secrets

- Keep model selection, endpoint URLs, temperature, maximum tokens, and retry
  policy in a local configuration file or environment variables.
- Do not commit API keys, raw provider responses, or local reviewer databases.
- Save a non-secret configuration fingerprint with each run, such as provider,
  model identifier, task version, retrieval version, and relevant generation
  settings.
- The manual adapter must produce the same task packet and accept the same
  normalized response schema as API adapters.

## Prompt and Task Contract

The task should be deliberately narrow: classify scope and suggest construct
labels from provided evidence. It must not ask the model to regenerate all
variable metadata.

Task requirements:

- Evaluate whether the item is psychological or behavioral.
- If in scope, propose a ranked primary construct plus optional additional
  constructs.
- Prefer labels that appear in retrieved precedents.
- Allow a new label only when existing labels do not capture the construct;
  list it in `new_construct_candidates` rather than presenting it as settled.
- Cite evidence IDs used for the suggestion.
- State uncertainty with `low`, `medium`, or `high` confidence.
- Abstain when the item wording/evidence is insufficient.
- Return only the normalized response object.

Store task templates in versioned files. The task packet should include the
target item, all retrieval evidence, clear instructions, and the expected
response schema. It should not include the entire flattened historical
vocabulary by default.

## Review Experience

A local browser interface is preferred over spreadsheet-first review because
the core action is inspecting evidence while making many small edits. Streamlit
is a pragmatic first candidate, though another local UI is acceptable if it
preserves the requirements below.

### Required Interaction Flow

1. Choose a queue: uncoded eligible items, deferred items, low-confidence
   proposals, or inconsistency candidates.
2. Select an individual item.
3. Inspect source context and retrieved evidence before or alongside generation.
4. Generate a proposal through the selected adapter, or load a prior proposal.
5. Edit primary and additional labels directly.
6. Accept, save edited, mark out of scope, defer, or flag for taxonomy review.
7. Move to the next queue item without losing the saved audit trail.

### Label Editing

- Use controlled suggestions drawn from known labels, with search and frequency
  information.
- Permit free-text additions only through an explicit `new candidate` action.
- Keep the primary label distinct from additional labels internally.
- Export the accepted primary then additional labels as the existing
  comma-separated construct string.
- Do not treat label order in historical strings as a mandatory hierarchy.

### Persistence and Export

Use a local SQLite database initially. It is simple to query for queues,
supports transactions, and avoids committing experimental data. A portable
export such as JSONL or CSV should also be available for backup.

Initial exports should produce a review-ready CSV/XLSX keyed by dataset ID and
variable name, containing final constructs and reviewer status. A later step
can add a guarded Google Sheets update that writes only explicitly selected,
reviewed decisions and always generates a dry-run report first.

## Evaluation Harness

### Benchmark

Create a fixed stratified benchmark of 50 to 100 historically annotated,
eligible items. A human reviews these items to establish a cleaner reference
set, including items with simple common constructs, specific constructs,
multiple labels, ambiguous wording, and insufficient evidence.

Do not call all existing labels ground truth. The benchmark is a reviewed
comparison set; unrevised historical annotations remain retrieval precedents
and audit candidates.

### Experimental Grid

Compare combinations of:

- Provider and model.
- Prompt/task version.
- Number and kind of retrieved neighbors.
- Similarity threshold.
- Whether exact-name evidence is included.
- Optional local versus external model configuration.

Use deterministic settings where provider support permits, and record settings
for every run.

### Metrics

Report both automated agreement and human-effort measures:

- In-scope classification agreement.
- Primary construct agreement.
- Additional-label precision, recall, and set overlap.
- Abstention quality: whether low-evidence items are appropriately deferred.
- New-label rate and how often new candidates are accepted.
- Evidence citation validity: citations must refer to retrieved evidence.
- Malformed-output and repair rates.
- Reviewer action distribution: accepted unchanged, accepted edited, rejected,
  deferred, and out of scope.
- Reviewer edit burden: labels added/removed or time per item, if practical.

The most decision-relevant outcome is whether the system reduces review effort
without producing hard-to-detect errors. Raw agreement with inconsistent legacy
labels is insufficient.

## Historical Inconsistency Queue

Generate candidates for human review without requiring a full historical
standardization project. Initial heuristics:

- Highly similar items with divergent construct sets.
- Exact cleaned-variable-name matches with different construct sets.
- Labels that appear on semantically heterogeneous items.
- Semantically close unlabeled items whose neighbors have a consistent label.
- Very rare labels, especially when near-common labels appear in similar items.

Each queue entry should show the same evidence view as a new annotation. The
output is a review candidate, never an automatic correction.

## Phased Delivery

### Phase 0: Design Validation

- Review this plan with another AI and/or a human collaborator.
- Decide the runtime stack, local storage location, and which providers to
  support first.
- Select a representative 50 to 100 item benchmark sample.
- Define an initial eligibility rule for psychological/behavioral items.

### Phase 1: Data and Retrieval Foundation

- Build canonical item records from metadata and similarity artifacts.
- Implement exact-name and semantic retrieval with evidence snapshots.
- Build a command-line inspection mode that prints an item and its evidence.
- Add tests for stable item IDs, exclusions, rankings, and evidence contents.

### Phase 2: Provider-Neutral Generation

- Define versioned task and response schemas.
- Implement the manual adapter and local validation first.
- Add Anthropic API and OpenAI-compatible adapters.
- Persist proposals, raw responses, failures, and configuration fingerprints.
- Run the benchmark in batch mode before building a polished UI.

### Phase 3: Local Review Workbench

- Add the item queue, evidence view, label editor, and decision persistence.
- Support accepts, edits, out-of-scope classification, deferral, and flags.
- Export reviewed decisions to a spreadsheet-compatible artifact.
- Track reviewer edit burden and model usefulness.

### Phase 4: Audit and Integration

- Add inconsistency queues based on retrieval and historical labels.
- Add a dry-run Google Sheets update workflow for explicitly reviewed exports.
- Decide whether a guarded write-back is beneficial after real use.

## Acceptance Criteria for a Useful First Release

The workbench is ready for real trial use when it can:

1. Load individual eligible items from the existing metadata corpus.
2. Show source context plus cross-dataset semantic and exact-name evidence.
3. Generate and validate structured proposals through manual and at least one
   API or local endpoint adapter.
4. Let a reviewer change every proposed decision without leaving the evidence
   view.
5. Persist enough information to reproduce an annotation decision.
6. Export only reviewed construct decisions in a format compatible with the
   current curation sheet workflow.
7. Run the same configuration against a reviewed benchmark and report the
   metrics above.
8. Produce an inconsistency queue with explainable evidence.

## Risks and Mitigations

| Risk | Mitigation |
| --- | --- |
| Historical annotations encode inconsistent practice | Treat them as retrieval evidence, create a reviewed benchmark, and surface inconsistency candidates rather than assuming ground truth. |
| The model gives plausible but unsupported rationale | Require cited evidence IDs and show uncited retrieved neighbors to the reviewer. |
| Provider-specific JSON support creates lock-in | Validate locally and make structured-output modes optional adapter features. |
| An external API is unavailable or unaffordable | Keep manual and OpenAI-compatible local adapters as first-class paths. |
| Semantic similarity is weak for short or poorly documented items | Surface insufficient evidence, support abstention, and preserve manual review. |
| A new UI becomes a maintenance burden | Keep it local, single-user, and centered on a small stable data contract. |
| Automatic write-back corrupts curation data | Export reviewed decisions first; add write-back only with dry-run and explicit selection. |

## Decisions Already Made

- Preserve backward compatibility with the current comma-separated `construct`
  string.
- Do not require exactly three construct levels.
- Support a ranked primary construct and optional additional constructs.
- Restrict annotation to psychological and behavioral variables.
- Use existing historical annotations as precedents while expecting
  inconsistencies.
- Use embedding-based semantic similarity as core evidence.
- Keep a human reviewer in control and show concrete item-level evidence.
- Support external APIs, local providers, and manual copy/paste use.
- Keep experimental review records local initially; commit only stable artifacts
  such as benchmark definitions and reviewed exports.
- Prefer a local browser workbench over spreadsheet-first review.

## Open Questions for Review

1. What rule should determine eligibility for psychological/behavioral
   annotation in the initial corpus? Manual flags, variable-type filters,
   keywords, or a reviewed classifier?
2. Should annotation suggestions initially be restricted to rating-scale items,
   matching the current similarity index, or include other well-documented item
   types from the start?
3. How should codebook-to-item matching work when raw variable names and
   cleaned names differ? Is a per-dataset mapping configuration acceptable?
4. What constitutes a reviewed benchmark label for multi-label items: a set of
   equally valid labels, or a primary-plus-additional ordering?
5. Which provider/model combinations are realistic to support in the first
   evaluation, and what local runtime is acceptable on the intended machine?
6. Should reviewer decisions be able to create a new construct immediately, or
   should new candidates require a separate taxonomy approval queue?
7. Is SQLite acceptable as a local, ignored workbench database, with optional
   JSONL/CSV backups?
8. Which evaluation result would justify moving from trial use to a guarded
   Google Sheets write-back integration?
