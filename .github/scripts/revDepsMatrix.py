#!/usr/bin/env python3

import json
import os
import sys
from pathlib import Path


VALID_REF_MODES = {"head", "release", "both"}


def read_lines() -> list[str]:
    input_repos = os.environ.get("INPUT_REPOS", "")
    if input_repos.strip():
        lines = input_repos.splitlines()
    else:
        path = Path("extras/revDeps.txt")
        if not path.exists():
            raise FileNotFoundError(
                "No reverse-dep list found. Provide INPUT_REPOS or extras/revDeps.txt"
            )
        lines = path.read_text(encoding="utf-8").splitlines()

    cleaned = []
    for line in lines:
        stripped = line.strip()
        if stripped and not stripped.startswith("#"):
            cleaned.append(stripped)

    if not cleaned:
        raise ValueError("Reverse-dep list is empty.")

    return cleaned


def parse_line(line: str) -> list[dict[str, str]]:
    parts = line.split()
    repo = parts[0]
    filter_arg = None
    ref_mode = None

    if len(parts) > 1:
        second = parts[1].lower()
        if second in VALID_REF_MODES:
            ref_mode = second
        else:
            filter_arg = parts[1]
            if len(parts) > 2:
                ref_mode = parts[2].lower()

    ref_mode = ref_mode or "head"
    if ref_mode not in VALID_REF_MODES:
        raise ValueError(
            f"Unsupported ref_mode '{ref_mode}' in reverse dependency config line: {line}"
        )

    ref_modes = ["release", "head"] if ref_mode == "both" else [ref_mode]
    entries = []
    for expanded_mode in ref_modes:
        spec_parts = [repo]
        if filter_arg:
            spec_parts.append(filter_arg)
        spec_parts.append(expanded_mode)
        slug = f"{repo.replace('/', '_')}__{expanded_mode}"
        entries.append(
            {
                "repo": repo,
                "filter": filter_arg or "",
                "ref_mode": expanded_mode,
                "slug": slug,
                "spec": " ".join(spec_parts),
            }
        )
    return entries


def main() -> None:
    include = []
    for line in read_lines():
        include.extend(parse_line(line))

    json.dump({"include": include}, sys.stdout, separators=(",", ":"))


if __name__ == "__main__":
    main()
