"""Format metadata JSONs consistently: 2-space indent, UTF-8, key order kept.

Usage (from repo root):
  python scripts/metadata/format_metadata.py          # rewrite files in place
  python scripts/metadata/format_metadata.py --check  # exit 1 if any file would change (CI)
"""
import json
import sys
from pathlib import Path


def formatted(text):
    return json.dumps(json.loads(text), indent=2, ensure_ascii=False) + "\n"


def main():
    check = "--check" in sys.argv
    changed = []
    for path in sorted(Path("data/metadata").glob("*_metadata.json")):
        text = path.read_bytes().decode("utf-8").replace("\r\n", "\n")
        new = formatted(text)
        if new != text:
            changed.append(path.name)
            if not check:
                path.write_bytes(new.encode("utf-8"))
    if check and changed:
        print("Not formatted (run scripts/metadata/format_metadata.py):")
        print("\n".join(f"  {name}" for name in changed))
        sys.exit(1)
    print(f"{len(changed)} file(s) {'need formatting' if check else 'reformatted'}")


if __name__ == "__main__":
    main()
