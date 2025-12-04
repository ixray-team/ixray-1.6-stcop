# Plugins
> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimal version**: 1.3

The SDK supports plugins in two languages:
* Lua
* Python

Place plugins in the root `plugins` folder. Subfolders are allowed; plugins are discovered recursively.

## Lua plugins
Lua plugins have limited access to the game scripting API, including base classes:
* ini_file
* reader
* fvector
* fmatrix
* net_packet
* rtoken_list
* token
* token_list
* [Global namespace and base libs](https://github.com/ixray-team/ixray-1.6-stcop/wiki/Lua:-%D0%91%D0%B0%D0%B7%D0%BE%D0%B2%D0%BE%D0%B5-%D0%BF%D1%80%D0%BE%D1%81%D1%82%D1%80%D0%B0%D0%BD%D1%81%D1%82%D0%B2%D0%BE-%D0%B8%D0%BC%D1%91%D0%BD)

### Header
Add to the top of the file:
```lua
-- desc: Description
-- input: [arg, Description], [arg, Description]
```
> `desc` — plugin description

> `input` — description of input args for the plugin. Optional; if omitted or empty, the plugin receives `level` (string path of the currently opened level), same as Lua plugins.

### Plugin example
```lua
-- desc: Find `gaz66_01`
local ini_scene

function main(level)
	ini_scene = ini_file("", 0, 1, 0, level.."scene_object.part")
	ini_scene:section_for_each(test_section)
end

function test_section(name)
	if ini_scene:line_exist(name, "reference_name") then
		local object = ini_scene:r_string(name, "reference_name")
		if (object == "statics\\vehicles\\cars\\gaz66_01") then
			SemiLog("found gaz66_01!")
		end
	end

	return false
end
```

## Python plugins
Unlike Lua, Python plugins can use the full Python API but do not have access to X-Ray internals.

### Header
Add to the top of the file:
```py
# desc: Description
# input: [arg, Description], [arg, Description]
```

> `desc` — plugin description

> `input` — description of input args. Optional; if omitted or empty, the plugin receives `level` (path to current level), same as Lua plugins.

* As a description you can specify a path from `fs.ltx`, e.g. `[import_dir, $import$]`; the SDK will auto-generate the argument and pass it in.

### Plugin example
```python
# desc: Add paddings to trees textures
# input: [path, Textures directory], [paddins, Paddins size]

import os
import numpy as np
import cv2
import argparse
import imageio.v3 as iio
from scipy.ndimage import distance_transform_edt

def add_padding_dds(input_path, output_path, padding_size=10):
    """Add padding to a texture with alpha channel."""
    # Load DDS texture
    img_array = iio.imread(input_path)

    # Ensure alpha channel exists
    if img_array.shape[-1] != 4:
        return None  # Skip if no alpha channel

    # Split channels
    b, g, r, a = cv2.split(img_array)

    # Mask of transparent pixels
    mask = a == 0

    # Fill transparent areas with nearest colored pixels
    for channel in [b, g, r]:
        dist, index = distance_transform_edt(mask, return_indices=True)
        channel[mask] = channel[index[0][mask], index[1][mask]]

    # Merge back
    new_image = cv2.merge((b, g, r, a))

    # Save back to DDS (DXT3)
    iio.imwrite(output_path, new_image, extension=".dds")
    return True  # File was modified

def process_folder(folder_path, padding_size=10):
    """Process all textures in the folder."""
    for file_name in os.listdir(folder_path):
        # Skip bump maps
        if file_name.lower().endswith(".dds") and not file_name.lower().endswith(("_bump.dds", "_bump#.dds")):
            input_path = os.path.join(folder_path, file_name)
            output_path = input_path  # Save with same name
            print(f"Processing: {file_name}")
            try:
                # Process and delete if unchanged
                result = add_padding_dds(input_path, output_path, padding_size)
                if result is None:
                    os.remove(input_path)  # Delete if not changed/no alpha
                    print(f"{file_name} has no alpha or unchanged, removed.")
            except Exception as e:
                print(f"Error processing {file_name}: {e}")

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-path", type=str, required=True, help="Folder")
    parser.add_argument("-paddins", type=int, required=True, help="Padding size")

    args = parser.parse_args()
    process_folder(args.path, args.paddins)

if __name__ == "__main__":
    main()
```
