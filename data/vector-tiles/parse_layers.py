import json
import sys

def analyze_file(filepath, file_label):
    print(f"\n{'='*80}")
    print(f"{file_label}")
    print(f"{'='*80}\n")
    
    with open(filepath, 'r') as f:
        data = json.load(f)
    
    layers = data.get('layers', [])
    print(f"Total Layers: {len(layers)}\n")
    
    # Group by source-layer
    by_source = {}
    for layer in layers:
        src = layer.get('source-layer', 'UNKNOWN')
        if src not in by_source:
            by_source[src] = []
        by_source[src].append(layer)
    
    # Print details
    for src in sorted(by_source.keys()):
        layers_list = by_source[src]
        print(f"SOURCE-LAYER: {src}")
        print(f"  Layer Count: {len(layers_list)}")
        
        # Sample first layer
        if len(layers_list) > 0:
            sample = layers_list[0]
            print(f"  Sample ID: {sample.get('id')}")
            print(f"  Type: {sample.get('type')}")
            print(f"  MinZoom: {sample.get('minzoom', 'N/A')}")
            print(f"  MaxZoom: {sample.get('maxzoom', 'N/A')}")
            if 'filter' in sample:
                print(f"  Filter: {sample['filter']}")
            
            # Check paint properties
            paint = sample.get('paint', {})
            if paint:
                print(f"  Paint keys: {list(paint.keys())}")
            
            # Check layout properties
            layout = sample.get('layout', {})
            if layout:
                print(f"  Layout keys: {list(layout.keys())}")
        
        # If multiple symbol values, show them
        if len(layers_list) > 1:
            filters = set()
            for lyr in layers_list:
                if 'filter' in lyr:
                    filters.add(str(lyr['filter']))
            if filters:
                print(f"  All filters: {list(filters)[:3]}")
        
        print()

analyze_file('root-LightBase.json', 'LIGHTBASE.JSON')
analyze_file('root-LiteLabels.json', 'LITELABELS.JSON')
analyze_file('root-VectorHillshade.json', 'VECTORHILLSHADE.JSON')

