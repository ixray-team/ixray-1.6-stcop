# IX-Ray

<!-- markdownlint-disable MD033 -->
<div align="center">
  <p>
    <a href="https://github.com/ixray-team">
      <img src="https://github.com/ixray-team/ixray/raw/default/logo.png" alt="IX-Ray Team" width="150" height="150" />
    </a>
  </p>

  <p>
    <a href="https://github.com/ixray-team/ixray-1.6-stcop/releases/tag/r0.3">
      <img src="https://img.shields.io/github/v/release/ixray-team/ixray-1.6-stcop?include_prereleases" alt="Latest release" />
    </a>
    <a href="https://github.com/ixray-team/ixray-1.6-stcop/actions/workflows/building.yml">
      <img src="https://img.shields.io/github/workflow/status/ixray-team/ixray-1.6-stcop/Building" alt="Building status" />
    </a>
  </p>
</div>
<!-- markdownlint-enable MD033 -->

Stable repository of the modernized *X-Ray* 1.6 game engine

## Requirements

- Visual Studio 2022 Community Edition
  - MFC
  - C++/CLI
  - Windows SDK 10.0.19041.0

## Building

Download the repository:

```console
git clone https://github.com/ixray-team/ixray-1.6-stcop.git
```

### Engine

- Run `download-dependencies.ps1`
- Build `XRay.Engine.sln` on `Win32` without projects from `utils`

## Changelog

All significant changes to this repository are documented in [this](CHANGELOG.md) file

## License

See [this](LICENSE.md) file for details
