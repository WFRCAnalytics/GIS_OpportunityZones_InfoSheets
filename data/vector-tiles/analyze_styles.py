import json
import sys

files = {
    'root-LightBase.json': 'LightBase',
    'root-LiteLabels.json': 'LiteLabels',
    'root-VectorHillshade.json': 'VectorHillshade'
}

for filepath, label in files.items():
    try:
        with open(filepath, 'r') as f:
            data = json.load(f)
        
        print(f"\n{'='*80}")
        print(f"{label.upper()}")
        print(f"{'='*80}")
        
        layers = data.get('layers', [])
        print(f"Total layers: {len(layers)}\n")
        
        # Collect unique source-layers
        source_layers = {}
        for layer in layers:
            src_layer = layer.get('source-layer', 'UNKNOWN')
            if src_layer not in source_layers:
                source_layers[src_layer] = []
            source_layers[src_layer].append(layer)
        
        for src_layer in sorted(source_layers.keys()):
            print(f"\nSOURCE-LAYER: {src_layer}")
            layer_list = source_layers[src_layer]
            for layer in layer_list:
                layer_id = layer.get('id', 'NO_ID')
                layer_type = layer.get('type', 'NO_TYPE')
                minzoom = layer.get('minzoom', '-')
                maxzoom = layer.get('maxzoom', '-')
                filter_expr = layer.get('filter', None)
                
                print(f"  - ID: {layer_id}")
                print(f"    Type: {layer_type}, Zoom: {minzoom}-{maxzoom}")
                if filter_expr:
                    print(f"    Filter: {filter_expr}")
    except Exception as e:
        print(f"ERROR processing {filepath}: {e}")

