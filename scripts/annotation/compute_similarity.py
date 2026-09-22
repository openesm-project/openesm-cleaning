"""
Compute semantic similarity between ESM items across datasets.

Reads all data/metadata/*.json files, embeds rating_scale items using
a state of the art transformer model, and writes pre-computed top-10
cross-dataset nearest neighbors to data/similarity/similar_items.json.

Embeddings are cached so only new items are re-embedded on subsequent runs.

Usage:
    python scripts/compute_similarity.py
"""

import json
import os
import sys
from datetime import datetime, timezone
from pathlib import Path

import numpy as np

ROOT = Path(__file__).parent.parent
METADATA_DIR = ROOT / "data" / "metadata"
OUTPUT_DIR = ROOT / "data" / "similarity"
SIMILAR_ITEMS_PATH = OUTPUT_DIR / "similar_items.json"
CACHE_PATH = OUTPUT_DIR / "embeddings_cache.json"
META_PATH = OUTPUT_DIR / "metadata.json"

MODEL_NAME = "BAAI/bge-large-en-v1.5"
TOP_K = 10


# ---------------------------------------------------------------------------
# Data loading
# ---------------------------------------------------------------------------

def build_item_text(feature: dict) -> str:
    """Combine description and details into a single embedding text."""
    parts = [feature.get("description", ""), feature.get("details", "")]
    return ". ".join(p.strip() for p in parts if p and p.strip())


def load_items() -> list[dict]:
    """Load all rating_scale items from metadata JSON files."""
    items = []
    for path in sorted(METADATA_DIR.glob("*_metadata.json")):
        with open(path, encoding="utf-8") as f:
            meta = json.load(f)
        dataset_id = meta.get("dataset_id", path.name[:4])
        for feature in meta.get("features", []):
            if feature.get("variable_type") != "rating_scale":
                continue
            text = build_item_text(feature)
            if not text:
                continue
            items.append({
                "key": f"{MODEL_NAME}::{dataset_id}_{feature['name']}",
                "dataset_id": dataset_id,
                "variable_name": feature["name"],
                "description": feature.get("description", ""),
                "text": text,
            })
    return items


# ---------------------------------------------------------------------------
# Embedding cache
# ---------------------------------------------------------------------------

def load_cache(path: Path) -> dict:
    if path.exists():
        with open(path, encoding="utf-8") as f:
            return json.load(f)
    return {}


def save_cache(cache: dict, path: Path) -> None:
    with open(path, "w", encoding="utf-8") as f:
        json.dump(cache, f)


# ---------------------------------------------------------------------------
# Embedding
# ---------------------------------------------------------------------------

def embed_items(items: list[dict], cache: dict) -> np.ndarray:
    """Return embedding matrix for all items, using cache where possible."""
    new_items = [item for item in items if item["key"] not in cache]

    if new_items:
        try:
            from sentence_transformers import SentenceTransformer
        except ImportError:
            print("ERROR: sentence-transformers not installed.")
            print("Run: pip install sentence-transformers")
            sys.exit(1)

        print(f"Loading model {MODEL_NAME!r} ...")
        model = SentenceTransformer(MODEL_NAME)

        print(f"Embedding {len(new_items)} new items (cached: {len(items) - len(new_items)}) ...")
        texts = [item["text"] for item in new_items]
        new_embeddings = model.encode(
            texts,
            normalize_embeddings=True,
            show_progress_bar=True,
            batch_size=64,
        )
        for item, emb in zip(new_items, new_embeddings):
            cache[item["key"]] = emb.tolist()
    else:
        print(f"All {len(items)} items found in cache.")

    return np.array([cache[item["key"]] for item in items], dtype=np.float32)


# ---------------------------------------------------------------------------
# Similarity + k-NN
# ---------------------------------------------------------------------------

def compute_neighbors(items: list[dict], embeddings: np.ndarray, k: int) -> dict:
    """Compute top-k cross-dataset neighbors for each item."""
    # Cosine similarity matrix (embeddings already normalized)
    sim_matrix = embeddings @ embeddings.T  # shape: (N, N)

    result = {}
    for i, item in enumerate(items):
        sims = sim_matrix[i].copy()

        # Mask same-dataset items and self by simply setting 
        # their similarity to -1.0
        for j, other in enumerate(items):
            if other["dataset_id"] == item["dataset_id"] or j == i:
                sims[j] = -1.0

        top_indices = np.argsort(sims)[::-1][:k]
        neighbors = [
            {
                "dataset_id": items[j]["dataset_id"],
                "variable_name": items[j]["variable_name"],
                "description": items[j]["description"],
                "similarity": round(float(sims[j]), 4),
            }
            for j in top_indices
            if sims[j] > -1.0  # skip if all same-dataset (edge case)
        ]

        lookup_key = f"{item['dataset_id']}_{item['variable_name']}"
        result[lookup_key] = {
            "dataset_id": item["dataset_id"],
            "variable_name": item["variable_name"],
            "description": item["description"],
            "neighbors": neighbors,
        }

    return result


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    print("Loading items from metadata files ...")
    items = load_items()
    print(f"Found {len(items)} rating_scale items across datasets.")

    if not items:
        print("No items found. Exiting.")
        sys.exit(1)

    cache = load_cache(CACHE_PATH)

    # prune cache entries for items no longer present
    current_keys = {item["key"] for item in items}
    removed = [k for k in cache if k not in current_keys]
    if removed:
        print(f"Pruning {len(removed)} stale cache entries ...")
        for k in removed:
            del cache[k]

    embeddings = embed_items(items, cache)
    save_cache(cache, CACHE_PATH)

    print("Computing cross-dataset nearest neighbors ...")
    similar_items = compute_neighbors(items, embeddings, k=TOP_K)

    print(f"Writing {SIMILAR_ITEMS_PATH} ...")
    with open(SIMILAR_ITEMS_PATH, "w", encoding="utf-8") as f:
        json.dump(similar_items, f, indent=2, ensure_ascii=False)

    n_datasets = len({item["dataset_id"] for item in items})
    meta = {
        "model": MODEL_NAME,
        "top_k": TOP_K,
        "n_items": len(items),
        "n_datasets": n_datasets,
        "generated_at": datetime.now(timezone.utc).isoformat(),
    }
    with open(META_PATH, "w", encoding="utf-8") as f:
        json.dump(meta, f, indent=2)

    print(f"Done. {len(similar_items)} items indexed across {n_datasets} datasets.")
    print(f"Output: {OUTPUT_DIR}")


if __name__ == "__main__":
    main()
