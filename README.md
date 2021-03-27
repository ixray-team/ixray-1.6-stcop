# IX-Ray

Improved *X-Ray* 1.6 game engine

## Requirements

* Visual Studio 2013 Community Edition
* Borland C++ Builder 6

## Building

Download the repository:

```console
git clone https://github.com/ixray-team/ixray-1.6.git
```

Download needed components:

* DirectX SDK March 2009
* DirectX SDK June 2010

Unpack a contents to the root agreeing to merge

If need to build editors run `InstallElpack.bat` and `InstallOther.bat` with administrator permissions

### Engine

#### Debug

* Build `XRay.Dependencies.VS2013.sln` on `Debug`
* Build `XRay.Engine.VS2013.sln` on `Debug` except projects from `utils`

#### Mixed or Release

* Build `XRay.Dependencies.VS2013.sln` on `Release`
* Build `XRay.Engine.VS2013.sln` on `Mixed` or `Release` except projects from `utils`

### Editors

* Build `XRay.Dependencies.VS2013.sln` on `Release`
* Build `XRay.Engine.VS2013.sln` on `Mixed` except projects from `engine_core` and `engine_game`
* Run `ConvertLibraries.bat`
* Build `XRay.Editors.BCB6.bpg`

## License

See [this](LICENSE.md) file for details
