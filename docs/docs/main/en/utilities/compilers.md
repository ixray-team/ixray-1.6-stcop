# Compilers
> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimal version**: 1.4

![image](https://github.com/user-attachments/assets/ef90d5a5-d2c9-4ff1-a539-a3a8e2cf5a10)

* Fixed thread count initialization
* * Number created equals CPU cores
* Added support for **IntelEmbree** and **CUDA** tracing, greatly increasing speed
* Output of all missing textures and thm
* Removed `net` compilation
* BC7 format support
* Support for splitting `level.cform` and `level.geom` into several files by size
* Ability to override compilation quality without launching `Level Editor`

## Launch

`xrLevelBuilder.exe` opens the compiler UI. Pass `-fsltx <file>` to select the project file system. The argument ends at the first space. For a project path with spaces, launch the utility from the project directory and pass only the file name.

```shell
xrLevelBuilder.exe -fsltx fsgame.ltx
```

The tool loads and saves LC, DO, and AI settings in `xrlevelbuilder.json`. Select the stages and the level in the UI. In AI mode, `-clear_temp_files` deletes the input `build.aimap` after reading it in the AI stage. Keep it if you need to rebuild without exporting the map again.

## LC
* Skip `invalid faces`
* New LightMaps saving algorithm
* * Bakes into selected texture size with faster sampling
* Fixed original bug where sometimes MU models became black
* Ability to skip geometry tessellation
* Ability to skip subdivide stage
* Optimized `geometry merge` stage
* Multithreaded sector compilation on location
* Removed QSlim usage for CForm optimization (Temporary solution)
* Removed old dx9mesh geometry optimizer
* * Now uses modern DirectXMesh library
* LightMap saving modes:
* * No compression `RGBA`
* * DX11 Only `BC7`
* * Vanilla `DXT3`

## DO
* Grass saved in DXT3

## AI
* Reimplemented A* algorithm
* 25-bit grid support
* * Compiler is based on `build.aimap` format for output format `level.aimap`
* Automatic `.spawn` file assembly for **FreeMP**
