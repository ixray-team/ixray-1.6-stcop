# Notes for modders

## First section of an ltx file is not read (UTF-8 BOM)

**Symptom:** the game does not see the first section of a file (e.g. it is missing from `g_spawn`), while every section after it works.

**Cause:** the file is saved as "UTF-8 with BOM". The invisible bytes `EF BB BF` precede the first line, so it actually reads `<BOM>[section]`, and the ltx parser does not recognise it as a section header.

**Check:**

```bash
head -c 16 file.ltx | od -c
```

If the output starts with `357 273 277`, the file has a BOM.

**Fix:** re-save the file without BOM:

- Notepad++: "Encoding → Convert to UTF-8" (without BOM) or ANSI/windows-1251;
- VS Code: encoding in the status bar → "Save with Encoding" → "UTF-8".

Workaround: leave the first line of the file empty or make it a `;` comment.

See also: [File encoding](/en/configs/file-encoding).
