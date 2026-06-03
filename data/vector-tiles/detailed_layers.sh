#!/bin/bash

extract_source_layer_details() {
  local file=$1
  local source_layer=$2
  
  echo ""
  echo "=== SOURCE-LAYER: $source_layer ==="
  
  # Find all layers using this source-layer
  grep "\"source-layer\":\"$source_layer\"" "$file" | 
  sed 's/{/\n{/g' | 
  grep "\"source-layer\":\"$source_layer\"" |
  while read line; do
    # Extract key properties
    echo "$line" | grep -o '"id":"[^"]*"' | head -1 | sed 's/.*://;s/"//g' | xargs echo "  ID:"
    echo "$line" | grep -o '"type":"[^"]*"' | head -1 | sed 's/.*://;s/"//g' | xargs echo "  Type:"
    echo "$line" | grep -o '"minzoom":[0-9]*' | head -1 | sed 's/.*://g' | xargs echo "  MinZoom:"
    echo "$line" | grep -o '"maxzoom":[0-9]*' | head -1 | sed 's/.*://g' | xargs echo "  MaxZoom:"
    echo "$line" | grep -o '"filter":\[[^]]*\]' | head -1 | sed 's/.*://g' | xargs echo "  Filter:"
    echo ""
  done | head -100
}

# Extract details for Roads layers from LightBase
extract_source_layer_details "root-LightBase.json" "Roads - white version"
extract_source_layer_details "root-LightBase.json" "Counties"

# Extract from Hillshade
extract_source_layer_details "root-VectorHillshade.json" "ShadePolygons_10Meter"

