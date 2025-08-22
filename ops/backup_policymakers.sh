#!/usr/bin/env bash
set -euo pipefail

# Run from repo root; or cd to the script’s dir then up if you place it elsewhere
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

TS="$(date +%Y_%m_%d_%H%M)"
OUT="policymakers_backup_${TS}.tar.gz"

# Core app + translations + scoring
POLICY_ITEMS=(
  "policymakers/app.R"
  "policymakers/PolicymakersScoring.R"
  "policymakers/Policymakers_translations.rds"
  "policymakers/www"
)

# Shared deps used by the app
SHARED_ITEMS=(
  "utils.R"
  "all_gadm41_centroids_level2.rds"
  "renv.lock"
  "LICENSE"
  "README.md"
  "deployment_notes.md"
)

# Build the tar.gz
tar -czf "$OUT" \
  --exclude='*.tar.gz' \
  --exclude='*:Zone.Identifier' \
  --exclude='*.sqlite' \
  --transform='s,^,basset_policymakers/,' \
  "${POLICY_ITEMS[@]}" \
  "${SHARED_ITEMS[@]}"

# Quick checks
echo "Wrote: $OUT"
du -h "$OUT" | awk '{print "Size:", $1}'
echo "Archive contents (top 30):"
tar -tzf "$OUT" | head -n 30
