#!/bin/bash

# Extract all layer information in a structured format
process_json() {
  local file=$1
  local file_label=$2
  
  echo ""
  echo "=========================================="
  echo "$file_label"
  echo "=========================================="
  
  # Extract layers section and parse
  # Get each layer as a distinct JSON object
  
  # Count unique source-layers
  echo "Unique source-layers:"
  grep -o '"source-layer":"[^"]*"' "$file" | sed 's/"source-layer":"//;s/"//' | sort -u | nl
  
  echo ""
  echo "Sample layer details:"
  
  # Extract just Counties to show structure
  grep '"source-layer":"Counties"' "$file" | head -3 | while read line; do
    echo "$line" | grep -o '"id":"[^"]*"' | head -1
    echo "$line" | grep -o '"type":"[^"]*"' | head -1
    echo "$line" | grep -o '"minzoom":[0-9]*' | head -1
    echo "$line" | grep -o '"maxzoom":[0-9]*' | head -1
    echo "---"
  done
}

process_json "root-LightBase.json" "LIGHTBASE.JSON"
process_json "root-LiteLabels.json" "LITELABELS.JSON"
process_json "root-VectorHillshade.json" "VECTORHILLSHADE.JSON"

