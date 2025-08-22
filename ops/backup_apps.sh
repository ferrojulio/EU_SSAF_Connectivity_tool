#!/bin/bash

# Create a timestamped backup folder
TIMESTAMP=$(date +%Y%m%d_%H%M)
BACKUP_DIR="backup_${TIMESTAMP}"
mkdir -p "$BACKUP_DIR/farmers/www"
mkdir -p "$BACKUP_DIR/policymakers/www"

# Shared files
cp utils.R "$BACKUP_DIR/"
cp all_gadm41_centroids_level2.rds "$BACKUP_DIR/"

# Farmers app
cp farmers/app.R "$BACKUP_DIR/farmers/"
cp farmers/Farmers_translations.rds "$BACKUP_DIR/farmers/"
cp farmers/Farmers_translations.csv "$BACKUP_DIR/farmers/"
cp farmers/farmersScoring.R "$BACKUP_DIR/farmers/"
cp farmers/TestDatabase.sqlite "$BACKUP_DIR/farmers/"
cp farmers/www/* "$BACKUP_DIR/farmers/www/"

# Policymakers app
cp policymakers/app.R "$BACKUP_DIR/policymakers/"
cp policymakers/Policymakers_translations.rds "$BACKUP_DIR/policymakers/"
cp policymakers/Policymakers_translations.xlsx "$BACKUP_DIR/policymakers/"
cp policymakers/PolicymakersScoring.R "$BACKUP_DIR/policymakers/"
cp policymakers/www/* "$BACKUP_DIR/policymakers/www/"

# Create compressed archive
tar -czf "${BACKUP_DIR}.tar.gz" "$BACKUP_DIR"

# Optional cleanup: remove raw folder after compression
rm -rf "$BACKUP_DIR"

echo "✅ Created archive: ${BACKUP_DIR}.tar.gz"

