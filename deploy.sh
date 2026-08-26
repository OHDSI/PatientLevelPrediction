#!/bin/bash
set -o errexit -o nounset -o pipefail

if [[ -z "${GH_TOKEN:-}" ]]; then
  echo "GH_TOKEN is required to publish to OHDSI/drat" >&2
  exit 1
fi

shopt -s nullglob
tarballs=(dist/PatientLevelPrediction_*.tar.gz)
if [[ ${#tarballs[@]} -ne 1 ]]; then
  echo "Expected exactly one PatientLevelPrediction source tarball in dist" >&2
  exit 1
fi

package_tarball="${tarballs[0]}"
package_filename=$(basename "$package_tarball")
temp_dir=$(mktemp -d)
trap 'rm -rf "$temp_dir"' EXIT

gh repo clone OHDSI/drat "$temp_dir/drat" -- --depth 1 --branch gh-pages
target="$temp_dir/drat/src/contrib/$package_filename"
if [[ -f "$target" ]]; then
  if cmp --silent "$package_tarball" "$target"; then
    echo "$package_filename is already present in drat with identical contents"
    exit 0
  fi
  echo "$package_filename already exists in drat with different contents" >&2
  exit 1
fi

git -C "$temp_dir/drat" config user.name "github-actions[bot]"
git -C "$temp_dir/drat" config user.email "41898282+github-actions[bot]@users.noreply.github.com"
Rscript -e "drat::insertPackage('$PWD/$package_tarball', repodir = '$temp_dir/drat', commit = FALSE)"
git -C "$temp_dir/drat" add .
git -C "$temp_dir/drat" commit -m "Release $package_filename from run $GITHUB_RUN_ID"
git -C "$temp_dir/drat" push origin gh-pages
