# IX-Ray

<!-- markdownlint-disable MD033 -->
<div align="center">
  <p>
    <a href="https://github.com/ixray-team">
      <img src="https://github.com/ixray-team/ixray/raw/default/logo.png" alt="IX-Ray Team" width="150" height="150" />
    </a>
  </p>

  <p>
    <a href="https://github.com/ixray-team/ixray-1.6/releases/tag/r1">
      <img src="https://img.shields.io/github/v/release/ixray-team/ixray-1.6?include_prereleases" alt="Latest release" />
    </a>
  </p>
</div>
<!-- markdownlint-enable MD033 -->

Stable repository of the modernized *X-Ray* 1.6 game engine

## Requirements

- Visual Studio 2022 Community Edition
- Windows SDK 10.0.19041.0
- Borland C++ Builder 6

## Building

Download the repository:

```console
git clone https://github.com/ixray-team/ixray-1.6.git
```

Download needed components:

- DirectX SDK March 2009
- DirectX SDK June 2010

Unpack a contents to the root agreeing to merge

### Engine

- Run `DownloadDependencies.bat`
- Build `XRay.Engine.VS2017.sln` except projects from `utils`

### Editors

- Run `InstallElpack.bat` and `InstallOther.bat` with administrator permissions
- Build `XRay.Engine.VS2017.sln` on `Mixed` except projects from `engine_game`
- Run `ConvertLibraries.bat`
- Build `XRay.Editors.BCB6.bpg`

## Changelog

All significant changes to this repository are documented in [this](CHANGELOG.md) file

## License

See [this](LICENSE.md) file for details
