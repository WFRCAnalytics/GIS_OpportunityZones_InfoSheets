#!/bin/bash

# Function to extract JSON objects by ID
extract_by_source_layer() {
  local file=$1
  local layer_name=$2
  
  grep "\"source-layer\":\"${layer_name}\"" "$file" | head -50
}

echo "=============================="
echo "SOURCE-LAYER: Roads - white version (LightBase)"
echo "=============================="
grep -A 30 "\"source-layer\":\"Roads - white version\"" root-LightBase.json | head -100

