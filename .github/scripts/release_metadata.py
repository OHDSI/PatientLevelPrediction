#!/usr/bin/env python3

import re
import subprocess
import sys
from pathlib import Path


VERSION = re.compile(r"^(\d+)\.(\d+)\.(\d+)$")


def parse_version(value):
    match = VERSION.fullmatch(value)
    return tuple(map(int, match.groups())) if match else None


def main(output_path, notes_path):
    fields = dict(
        line.split(":", 1)
        for line in Path("DESCRIPTION").read_text().splitlines()
        if ":" in line and not line[0].isspace()
    )
    package = fields["Package"].strip()
    version = fields["Version"].strip()
    current = parse_version(version)

    tags = subprocess.check_output(["git", "tag", "--list"], text=True).splitlines()
    releases = {
        tag: parse_version(tag[1:])
        for tag in tags
        if tag.startswith("v") and parse_version(tag[1:]) is not None
    }
    latest = max(releases.values(), default=None)
    release_tag = f"v{version}" if current and (
        f"v{version}" in releases or latest is None or current > latest
    ) else ""

    if release_tag:
        lines = Path("NEWS.md").read_text().splitlines()
        heading = f"{package} {version}"
        headings = [index for index, line in enumerate(lines) if line == heading]
        if len(headings) != 1:
            raise ValueError(f"Expected exactly one NEWS heading named: {heading}")
        start = headings[0] + 1
        if start < len(lines) and re.fullmatch(r"=+", lines[start]):
            start += 1
        end = next(
            (index for index in range(start, len(lines))
             if re.fullmatch(rf"{re.escape(package)} \d+\.\d+\.\d+", lines[index])),
            len(lines),
        )
        notes = "\n".join(lines[start:end]).strip()
        if not notes:
            raise ValueError(f"The NEWS section for {version} is empty")
        Path(notes_path).write_text(f"{notes}\n")

    with Path(output_path).open("a") as output:
        output.write(f"package_version={version}\n")
        output.write(f"latest_version={'.'.join(map(str, latest)) if latest else ''}\n")
        output.write(f"release_tag={release_tag}\n")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
