# SDK Package Manager

Open **Packages**, next to **Plugins** in the editor menu. The library is shared by projects using this SDK installation.

Click **+ Add package** and choose **Install from GitHub...**, **Install from folder...**, or **Install from ZIP...**.

For GitHub, enter an HTTPS repository URL such as `https://github.com/owner/repository`, optionally select a branch or tag, and click **Install package**. An empty revision follows the repository's default branch. Git must be available in `PATH`. Catalog synchronization also uses curl for bounded HTTPS reads. Public GitHub repositories are supported. Local installation does not require Git.

Navigate **All packages** (the whole catalog plus custom installed packages), **My packages** (installed only), **Official**, and **Community** (catalog channels). Plugins and assets share these lists. Search and sort in the middle, then use **Install package** for available entries or **Update package / Remove package** for installed ones. Local data reloads automatically after package installation, update or removal. **Sync catalog** refreshes the remote catalog.

Select an installed package to see its version, source and location. **Update package** downloads the recorded branch/tag or rereads the original folder/ZIP. **Remove package** removes its managed directory. Local edits and additional files block update/removal; back up your changes and restore the original package first.

Package changes are journaled. After an interrupted operation, the SDK restores the previous installation or finishes cleanup before scanning plugins. Concurrent writes from another editor are rejected while package storage is busy. Cancellation is checked during file enumeration, copying and comparison; once a change has committed, it remains installed.

## Package card

The right pane shows the package name, ID, version, type, author and license. Source, Documentation, Website and Release notes links are optional and appear only when a valid URL is available. Source can point to GitLab, GitHub or another HTTPS site. Links open in your browser through HTTPS.

Use **Overview**, **Changelog** and **License** to inspect a package. Overview combines the short description and README on one page; packages without a README show the short description. README and Changelog support headings, lists and fenced code blocks; other Markdown remains text. HTML and remote images are not rendered. Missing documents do not block installation.

Installation and updates capture README, changelog and license text in `.ixray-packages/installed/<id>/documents.json`. The preview works offline and remains available after the original folder or ZIP is removed. It is stored separately from the plugin/asset payload. Existing installations without this cache can use same-version catalog previews; update the package to capture its own documents. Documentation for a newer catalog version is not shown as the installed version's documentation.

Before installation, **Sync catalog** downloads `package.json` and root README, CHANGELOG and LICENSE previews from `raw.githubusercontent.com` at the same exact commit. It does not use the GitHub REST API or clone the package payload. Selecting a card reads the cached documents without a network request. Synchronization reuses complete cached package metadata and documents when the resolved commit is unchanged; incomplete previews are retried. Installation captures documents from the downloaded package.

Place `README.md`, `CHANGELOG.md` and `LICENSE.md` at the repository root for previews before installation. Raw discovery also tries `readme.md`, `README.txt`, `README`, `changelog.md`, `CHANGES.md`, `CHANGELOG`, `LICENSE`, `LICENSE.txt`, `license.md` and `COPYING`. Documents are optional; missing files do not block installation. Previews use UTF-8 (a BOM is allowed) and are limited to 32 KiB each; raw downloads over 64 KiB are skipped with a notice. Installed and local packages also search the content directory, use case-insensitive matching and recognize additional HISTORY/COPYING variants. Links and special local files are skipped.

## Local packages

For a folder, enter the path to its directory or its `package.json`. **Browse...** opens the folder picker: double-click directories, use **Up** to navigate, then choose **Use this folder** at the package root. Paths can also be pasted directly.

A ZIP must contain `package.json` at its root or inside one outer directory. Choose it through **Browse... в†’ Use ZIP archive** or paste its path. Local packages require the manifest to declare their type and content; the examples below work for folders and ZIPs.

The manager installs a separate copy and preserves the original folder/archive. Edit that folder or replace the ZIP at the same path, then click **Update package**. A moved/deleted source stops the update without changing the installed copy. Local sources must be outside managed SDK roots `packages`, `plugins/packages`, `.ixray-packages`, and must not contain those storage roots.

ZIP extraction checks CRC and rejects links, unsafe paths, duplicate names and encrypted entries. Limits: 100000 entries, 2 GiB per file, and 16 GiB total expanded size. Stored and Deflate ZIP entries are supported; tarball is not supported.

## Package types

- SDK plugins: `plugins/packages/<id>`, containing Lua or Python plugins. The Plugins menu refreshes after the operation; close any open Plugin Run dialogs first.
- Asset libraries: `packages/<id>` inside the SDK, containing models, textures, sounds, or other files. At least one payload file is required. Packages are stored in the shared library; automatic Content Browser mounting and project import are not implemented yet. Scripts inside asset packages are not executed.


The SDK's `.ixray-packages` directory stores metadata and reference copies used to detect local changes. Keep it with the SDK.

## Repository format

Place `package.json` at the package root. Local folders and ZIPs require it; it is recommended for GitHub:

```json
{
  "id": "example.shared-assets",
  "name": "Example Asset Library",
  "version": "1.0.0",
  "description": "Reusable assets for SDK projects",
  "author": "Example Author",
  "type": "assets",
  "content": "assets"
}
```

The supported types are `sdk-plugin` and `assets`. Optional `name`, `version`, `description`, and `author` fields are strings. `content` is a relative directory inside the repository, defaulting to its root. IDs use lowercase ASCII letters, digits, dots, hyphens and underscores. Updates cannot change a package's ID, type or repository.

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
