# Compressor
## Unpacking resources
> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimal version**: 1.1

Command:

`xrCompress.exe -unpack fsgame.ltx <unpack_dir>`

## Packing resources
> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimal version**: 1.0

Command:
`xrCompress.exe <data_dir> -ltx <file_name.ltx> [options]`

Available options:

* **-diff /?** - info about created diffs
* **-fast** - fast compression
* **-store** - store files without compression
* **-ltx <file_name.ltx>** - config with paths for packaged files

## Generating diffs (patch creation)

Command:
` xrCompress.exe -diff <new_data> <old_data> -out <diff_resulf> [options]`

Parameters **<new_data>**, **<old_data>**, **<diff_resulf>** are directory names.

Available options:

* **-nofileage** - skip file date check
* **-crc** - skip CRC32 check
* **-nobinary** - skip binary check
* **-nosize** - skip size check
