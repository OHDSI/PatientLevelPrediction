#!/usr/bin/env python3

import re
import subprocess
import sys
import tempfile
from pathlib import Path


RELEASE_VERSION = re.compile(r"^(\d+)\.(\d+)\.(\d+)$")
VERSION_TAG = re.compile(r"^v(\d+)\.(\d+)\.(\d+)$")


def version_tuple(pattern, value):
    match = pattern.fullmatch(value)
    return tuple(map(int, match.groups())) if match else None


def release_metadata(description="DESCRIPTION", news="NEWS.md", tags=None):
    fields = dict(
        line.split(":", 1)
        for line in Path(description).read_text().splitlines()
        if ":" in line and not line[0].isspace()
    )
    package = fields["Package"].strip()
    version = fields["Version"].strip()

    if tags is None:
        tags = subprocess.check_output(["git", "tag", "--list"], text=True).splitlines()
    versions = [
        (version_tuple(VERSION_TAG, tag), tag)
        for tag in tags
        if VERSION_TAG.fullmatch(tag)
    ]
    if not versions:
        raise ValueError("No tags matching v<major>.<minor>.<patch> were found")
    latest_tuple, latest_tag = max(versions)
    latest_version = latest_tag[1:]

    current = version_tuple(RELEASE_VERSION, version)
    new_version = f"v{version}" if current and current > latest_tuple else ""

    lines = Path(news).read_text(encoding="utf-8").splitlines()
    heading = f"{package} {version}"
    matches = [index for index, line in enumerate(lines) if line == heading]
    if len(matches) != 1:
        raise ValueError(f"Expected exactly one NEWS heading named: {heading}")

    start = matches[0] + 1
    if start < len(lines) and re.fullmatch(r"=+", lines[start]):
        start += 1
    end = next(
        (
            index
            for index in range(start, len(lines))
            if re.fullmatch(
                rf"{re.escape(package)} \d+\.\d+\.\d+(?:\.\d+)?", lines[index]
            )
        ),
        len(lines),
    )
    notes = "\n".join(lines[start:end]).strip()
    if not notes:
        raise ValueError(f"The NEWS section for {version} is empty")

    return version, latest_version, new_version, notes


def self_test():
    with tempfile.TemporaryDirectory() as directory:
        root = Path(directory)
        description = root / "DESCRIPTION"
        news = root / "NEWS.md"
        def write(version, notes="- New release"):
            description.write_text(f"Package: ExamplePackage\nVersion: {version}\n")
            news.write_text(f"ExamplePackage {version}\n====================\n\n{notes}\n")

        write("6.7.0")
        result = release_metadata(
            description, news, ["not-a-version", "v6.6.9", "v6.5.12"]
        )
        assert result[:3] == ("6.7.0", "6.6.9", "v6.7.0")
        assert result[3] == "- New release"

        # A larger patch number cannot make an older minor version newer.
        result = release_metadata(description, news, ["v6.10.0"])
        assert result[2] == ""

        write("6.7.0.9999", "- Development version")
        result = release_metadata(description, news, ["v6.7.0"])
        assert result[2] == ""


def main():
    if sys.argv[1:] == ["--self-test"]:
        self_test()
        return

    output = Path(sys.argv[1]) if len(sys.argv) > 1 else None
    notes_path = Path(sys.argv[2]) if len(sys.argv) > 2 else Path("release-notes.md")
    version, latest, new_version, notes = release_metadata()
    notes_path.write_text(f"{notes}\n", encoding="utf-8")
    if output:
        with output.open("a", encoding="utf-8") as stream:
            stream.write(f"package_version={version}\n")
            stream.write(f"latest_version={latest}\n")
            stream.write(f"new_version={new_version}\n")
            stream.write(f"notes_file={notes_path}\n")
    release = f"release {new_version}" if new_version else "no release"
    print(f"Package version {version}; latest tag {latest}; {release}")


if __name__ == "__main__":
    main()
