# xrCompress resource tool

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.0 for packing, 1.1 for unpacking

## Overview

The tool packs game resources into archives, unpacks mounted archives, and copies changed files from a newer data directory to prepare a patch.

## Syntax

```text
xrCompress.exe <data_dir> [-ltx <config.ltx>] [-fast] [-store]
xrCompress.exe <data_dir> -header <header.ltx>
xrCompress.exe -unpack <fsgame.ltx> <output_dir>
xrCompress.exe -diff <new_data> <old_data> -out <output_dir> [options]
```

| Argument | Effect |
| --- | --- |
| `-ltx <config.ltx>` | Packs files according to the configuration. |
| `-header <header.ltx>` | Specifies the archive header configuration when packing a directory without `-ltx`. |
| `-fast` | Enables fast compression. |
| `-store` | Stores files without compression. |
| `-nofileage` | In diff mode, skips file timestamp comparison. |
| `-nocrc` | In diff mode, skips CRC32 comparison. |
| `-nobinary` | In diff mode, skips byte-for-byte comparison. |
| `-nosize` | In diff mode, skips file size comparison. |

## Examples

Command:

```shell
xrCompress.exe -unpack fsgame.ltx "unpack_dir"
```

```text
xrCompress.exe gamedata -ltx datapack.ltx -fast
xrCompress.exe -diff new_data old_data -out patch_data -nofileage
```

## Recommendations and limitations

- Use paths without spaces for diff directories and for `-ltx` and `-header` values. A separate parser reads those arguments.
- The `-diff /?` output lists `-crc` for disabling CRC32 checks. The implementation accepts `-nocrc`.

## See also

- [Application launch arguments](../main/launch-keys.md)
- [Addons](../addon-system/general-information.md)
