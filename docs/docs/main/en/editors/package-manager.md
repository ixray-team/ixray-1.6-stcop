# SDK Package Manager

Open **Packages**, next to **Plugins** in the editor menu. The library is shared by projects using this SDK installation.

Click **+ Add package** and choose **Install from GitHub...**, **Install from folder...**, or **Install from archive...**.

For GitHub, enter an HTTPS repository URL such as `https://github.com/owner/repository`, optionally select a branch or tag, and click **Add package** (or **Download package** for assets). An empty revision follows the repository's default branch. Git must be available in `PATH`. Catalog synchronization also uses curl for bounded HTTPS reads. Public GitHub repositories are supported. Local installation does not require Git.

Navigate **All packages** (the whole catalog plus custom installed packages), **My packages** (installed only), **Official**, and **Community** (catalog channels). Plugins and assets share these lists. Search and sort in the middle, then use **Install package** for available entries or **Update package / Remove package** for installed ones. Local data reloads automatically after package installation, update or removal. **Sync catalog** refreshes the remote catalog.

Select an installed package to see its version, source and location. **Update package** downloads the recorded branch/tag or rereads the original folder/archive. **Remove package** removes its managed directory. Local edits and additional files block update/removal; back up your changes and restore the original package first.

Package changes are journaled. After an interrupted operation, the SDK restores the previous installation or finishes cleanup before scanning plugins. Concurrent writes from another editor are rejected while package storage is busy. Cancellation is checked during file enumeration, copying and comparison; once a change has committed, it remains installed.

## Package card

The right pane shows the package name, ID, version, type, author and license. Source, Documentation, Website and Release notes links are optional and appear only when a valid URL is available. Source can point to GitLab, GitHub or another HTTPS site. Links open in your browser through HTTPS.

Use **Overview**, **Changelog** and **License** to inspect a package. Overview combines the short description and README on one page; packages without a README show the short description. README and Changelog support headings, lists and fenced code blocks; other Markdown remains text. HTML and remote images are not rendered. Missing documents do not block installation.

Installation and updates capture README, changelog and license text in `.ixray-packages/installed/<id>/documents.json`. The preview works offline and remains available after the original folder or archive is removed. It is stored separately from the plugin/asset payload. Existing installations without this cache can use same-version catalog previews; update the package to capture its own documents. Documentation for a newer catalog version is not shown as the installed version's documentation.

Before installation, **Sync catalog** downloads `package.json` and root README, CHANGELOG and LICENSE previews from `raw.githubusercontent.com` at the same exact commit. It does not use the GitHub REST API or clone the package payload. Selecting a card reads the cached documents without a network request. Synchronization reuses complete cached package metadata and documents when the resolved commit is unchanged; incomplete previews are retried. Installation captures documents from the downloaded package.

Place `README.md`, `CHANGELOG.md` and `LICENSE.md` at the repository root for previews before installation. Raw discovery also tries `readme.md`, `README.txt`, `README`, `changelog.md`, `CHANGES.md`, `CHANGELOG`, `LICENSE`, `LICENSE.txt`, `license.md` and `COPYING`. Documents are optional; missing files do not block installation. Previews use UTF-8 (a BOM is allowed) and are limited to 32 KiB each; raw downloads over 64 KiB are skipped with a notice. Installed and local packages also search the content directory, use case-insensitive matching and recognize additional HISTORY/COPYING variants. Links and special local files are skipped.

## Local packages

For a folder, enter the path to its directory or its `package.json`. **Browse...** opens the folder picker: double-click directories, use **Up** to navigate, then choose **Use this folder** at the package root. Paths can also be pasted directly.

A ZIP or 7z archive must contain `package.json` at its root or inside one outer directory. Choose it through **Browse...** in the system file dialog, or paste its path. Local packages require the manifest to declare their type and content; the examples below work for folders, ZIP and 7z archives.

The manager installs a separate copy and preserves the original folder/archive. Edit that folder or replace the archive at the same path, then click **Update package**. A moved/deleted source stops the update without changing the installed copy. Local sources must be outside managed SDK roots `packages`, `plugins/packages`, `.ixray-packages`, and must not contain those storage roots.

Archive extraction checks integrity and rejects links, unsafe paths, duplicate names and encrypted entries. Limits: 100000 entries, 2 GiB per file, and 16 GiB total expanded size. ZIP supports Store and Deflate; 7z supports Copy, LZMA, LZMA2, PPMd and their supported filters, including solid archives. Native decoding is built into the editor; no 7-Zip installation or extra runtime DLL is required. Password-protected, multipart and other archive formats are not supported. Existing ZIP installation records remain readable.

## Package types

- SDK plugins: `plugins/packages/<id>`, containing Lua or Python plugins. The Plugins menu refreshes after the operation; close any open Plugin Run dialogs first.
- Asset libraries: **Download** stores the package in `packages/<id>` inside the shared SDK library. **Import** copies selected files into the current SDK using its filesystem paths. Downloading does not import files. **Download update**, **Reload download**, and **Remove download** only affect the library copy; imported files remain independent. Scripts inside asset packages are not executed by the manager.


The SDK's `.ixray-packages` directory stores metadata and reference copies used to detect local changes. Keep it with the SDK.

## Importing assets

After downloading an asset package, click **Import**. The resizable dialog shows a folder tree with file and folder checkboxes, destination paths, and reasons why files cannot be imported. Use **Select all**, **Select none**, or individual checkboxes, then click **Import selected**. The preview compares contents and distinguishes missing, identical, different and blocked files. Missing files are selected by default. Identical files need no copy. Different files remain unchanged unless you enable **Allow replacing different files (originals are backed up)** and select them. Directories and files without a safe recognized destination cannot be selected. Reopen Import to refresh the preview after changing files.

Inside the manifest's `content` directory, preserve the standard SDK layout. For example, with `"content": "assets"`:

```text
assets/
  rawdata/
    objects/example/house.object
    objects/example/house.thm
    levels/example.level
    levels/example/scene_object.part
  gamedata/
    textures/example/brick.dds
  import/
    source_texture.tga
```

Destinations come from the active `fs.ltx` aliases, not hardcoded disk paths. `rawdata/objects` uses `$objects$`, `rawdata/levels` uses `$maps$`, `rawdata/groups` uses `$groups$`, and source textures/sounds use `$textures$`/`$sounds$`. Standard `gamedata` sections use their matching `$game_...$` aliases; `import` uses `$import$`. Other `rawdata` and `gamedata` subdirectories use `$server_data_root$` and `$game_data$`. The most specific section wins; a missing alias is shown in the preview. Relative paths inside each section are preserved. Files cannot be imported into managed package storage.

Include the dependencies your assets need: levels require their `.part` directory, referenced models, textures and other resources. The dependency check reports supported references but does not download dependencies or rewrite embedded references. Import copies prepared resources; placing source files in `import` does not automatically run converters or compilers.

Dependency checking is optional. Open **Details** and click **Check dependencies** when needed; after changing the file selection, run it again to refresh the results. The list distinguishes **Missing**, **Not selected** (available in this package), **In SDK**, and **Selected** resources, and shows which scene part or model requires them. Kept SDK files are checked using their existing contents; selected replacements use package contents. The check runs in the background and supports cancellation. **Import** copies the selected files without running a dependency check, and dependency warnings do not block copying.

Use **Select dependencies** to add required files available in this package to the selection. The action checks the chosen levels again and repeats after selecting metadata, until no additional files can be selected. Existing SDK files are not selected for replacement, and manual selections are preserved. Missing resources and unreadable files remain in the report. The action runs in the background; cancellation or an error leaves the file selection unchanged. Files are copied only after **Import**.

Supported references include editable objects in LTX and supported binary scene parts, nested groups, detail models, surface overrides, glow/light textures, and editable object surface formats `0x0905`–`0x0908`, including shared material textures from `SharedMaterials.ltx`. Terrain templates are identified by their `B_BmmD` class in the active `shaders.xr`; their implicit `<texture>_mask.dds` is also checked. Texture bindings resolve `.ogm`, `.avi` and `.seq` before a plain `.dds`, following renderer priority. DDS frames are checked under the active `$level$` and `$game_textures$`; bump DDS uses the level override only when its global DDS exists, following the renderer's fallback rule. Scene and model data are read without opening a level, loading render resources or resaving models. Unreadable, malformed and compressed dependency sections produce an incomplete-check warning instead of a clean result. This is not full level validation: other shader-generated textures, detail paint base maps and other game resources are not resolved.

Bump/detail references come from `textures.ltx`, then active texture `.thm` files under `$game_textures$` and `$level$`. THM specifications replace earlier bump settings, while only an enabled THM detail association replaces an earlier association. Enabled bump maps require both the named image and its `#` companion. Detail bump maps are checked only when bump-detail usage is enabled. The report identifies the texture or sequence requiring each image. Source thumbnails under `rawdata/textures` are not runtime settings. Unselected optional metadata is shown as **Not selected**, and its settings are not applied to the preview.

For `.seq`, the check reads the optional `cycled` header, validates the frame rate (1–1000 FPS) and checks every listed frame as a plain DDS image. A sequence does not require a same-name base DDS. Limits: 1 MiB per sequence, 10000 frames, 254 bytes per sequence line, matching the renderer buffer. UTF-8 BOM and padded `cycled` headers are rejected because the runtime loader does not accept them. Malformed data, unsafe frame paths or unsupported metadata produce an incomplete-check warning. Video texture files are checked for availability; media decoding is outside this check.

Import runs in the background with progress and cancellation. Sources and destinations are rechecked against the preview, including same-size content changes. A selected replacement first receives a verified backup; exclusive file creation protects files added after the preview. Cancellation and reported errors remove new copies and restore replaced originals. If restoration fails, the report identifies the affected file and retains its backup. Successful imports refresh the filesystem and open Content Browsers.

After a successful import, the Package Manager shows a short imported-file count. Use **Last import report** to open the full report while the Package Manager remains open. Failed imports open the report automatically. The report retains all outcome totals; identical files are omitted from its table. Reports and original files are retained in `.ixray-packages/import-history/import-<id>/`; `report.json` records destinations and backup paths. Each verified backup also has a small `restore-<index>.json` mapping. Reopening Import rescans the SDK, so unchanged imported files appear as identical and edited files can be explicitly selected for replacement.

Forced termination can leave an incomplete import. Import does not have automatic startup recovery: inspect the saved report and restore records and recover originals from the retained backups if needed. Downloaded packages remain unchanged.

## Repository format

Place `package.json` at the package root. Local folders and archives require it; it is recommended for GitHub:

```json
{
  "id": "example.shared-assets",
  "name": "Example Asset Library",
  "version": "1.0.0",
  "description": "Reusable assets for SDK projects",
  "author": "Example Author",
  "type": "assets"
}
```

The supported types are `sdk-plugin` and `assets`. Optional `name`, `version`, `description`, and `author` fields are strings. IDs use lowercase ASCII letters, digits, dots, hyphens and underscores. Updates cannot change a package's ID, type or repository.

`content` is optional. Omit it when the files are next to `package.json`: the package root is used. Set it only when the files are inside another directory, for example `"content": "content"` or `"content": "editors"`. The name is arbitrary; the SDK uses the specified relative path and reports an error if it is missing. An explicitly configured directory is never silently replaced by the root.

| Package layout next to `package.json` | `content` |
| --- | --- |
| `rawdata/`, `gamedata/`, `import/` | Omit the field |
| `content/rawdata/`, `content/gamedata/` | `"content": "content"` |
| `editors/rawdata/`, `editors/gamedata/` | `"content": "editors"` |
| Plugin script directories `core/`, `src/` | Omit the field to include both |

Folder structure inside the chosen directory is preserved. For asset Import to resolve destinations through `fs.ltx`, `rawdata/`, `gamedata/` and `import/` must be directly inside that directory; include only the folders the package needs. Plugin scripts are discovered recursively, so scripts in both `core/` and `src/` are found. Each `.lua` or `.py` file becomes a separate Plugins command.

A `sdk-plugin` package groups one or more scripts for installation, updates and removal. It uses the existing plugin loader and does not select a package entry point through `package.json`. For example, a package containing `core/tool-a.lua` and `src/tool-b.py` installs both directories and adds two commands. Command names come from script filenames; descriptions and inputs use the existing `desc` and `input` comments. Every Lua/Python file in the installed content must be intended as a plugin command; helper modules are also discovered by the current loader.

Optional card fields are `license` (a publisher-supplied name/identifier, up to 256 UTF-8 bytes), `sourceUrl`, `websiteUrl`, `documentationUrl` and `changelogUrl` (up to 2048 bytes each). URLs must use HTTPS with a DNS hostname and no credentials or custom port. Old manifests can omit every card field. `sourceUrl` is the optional source-code link, independent of the installation source. If omitted, a known GitHub repository URL is used when available. An absent documentation URL hides Documentation; packages without any source link hide Source. The License tab shows the included license file; the manager does not infer legal terms from a package type.

Without a manifest, the ID is derived from `owner.repository` and the add dialog supplies the type. Plugins use the `plugins` directory when present, otherwise the repository root, and require a `.lua` or `.py` file. Asset packages use the repository root.


## Official / Community catalog

The published `sdk-packages.json` contains only `official` and `community` arrays of repository links. There is no `schemaVersion`, duplicated description or type field:

```json
{
  "official": ["https://github.com/example/sdk-tools"],
  "community": []
}
```

This URL is an example: replace it with a real public repository containing `package.json`. For a specific branch or tag, use `{"url":"https://github.com/example/sdk-tools","ref":"main"}` instead of the string. An ordinary link follows the default branch. The old full-entry catalog format is not supported.

Package ID, name, type, version, description, author, category and optional card links come from that repository's `package.json`. Optional `readme`, `changelog` and `licenseText` strings are fallback previews when repository documents are absent (up to 32 KiB each, within the 64 KiB manifest limit). Raw documents are cached separately and do not count toward the manifest size. Keep the ID and type stable across updates.

**Sync catalog** uses the catalog source configured in the SDK and reads `sdk-packages.json` from its default branch. No catalog files need to be shipped with the SDK. It resolves package revisions and downloads their manifests over HTTPS in the background. Git and curl must be in PATH. Catalog installation uses the exact commit shown in the card; Update follows the saved branch/tag. A fixed tag stays fixed.

The public repository contains the link index and instructions. Example packages live in their own repositories and are not bundled with the engine. Before the first synchronization, a fresh SDK shows an empty catalog.

To override the source, create `package-manager.json` under `$app_data_root$` (normally `_appdata_ixray_`):

```json
{
  "repository": "https://github.com/example/sdk-package-catalog",
  "ref": "",
  "file": "sdk-packages.json"
}
```

Replace the example URL with your catalog repository URL. Each field is optional; an empty `ref` follows the repository default branch. The settings file is read on every synchronization and is not created automatically. Removing it restores the built-in source. Invalid explicit settings report an error. The old `package-catalog/registry.json` is no longer used.

The SDK's private `.ixray-packages/catalog.json` snapshot stores the index, fetched manifests and document previews for offline display. Opening the window and reloading local data after package operations perform no network requests. If one repository is unavailable, its valid cached card is retained and marked stale; without cached metadata it appears unavailable and cannot be installed. Invalid indexes and cancellation preserve the previous snapshot. Old full-entry caches are ignored. Installed packages remain independent of the remote index.

Official/Community are publisher-maintained channels, not signature verification. Limits: 128 references per channel, 64 KiB for the index or an individual downloaded manifest, 32 MiB for the private snapshot. A repository/revision can appear only once. Duplicate manifest IDs are marked as unavailable conflicts. An unrelated installed source with the same ID remains in My packages and cannot be overwritten by the catalog.

## Limitations

GitHub sources must be public. Private repository authentication, automatic package dependency installation, Git LFS downloads and submodule checkout are not implemented.

Installing or updating a package does not execute its plugin commands or installation scripts. Installed plugins are available in the **Plugins** menu. Symbolic links are rejected.

Network operations support cancellation. Git clone/fetch commands have a 180-second timeout; revision lookups have a 20-second timeout. HTTP requests for metadata and documents have a 15-second total timeout and a 5-second connection timeout. These limits apply separately to each command or request; a complete installation or catalog synchronization can take longer.
