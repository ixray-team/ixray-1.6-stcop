# Compilers
> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimal version**: 1.3

![image](https://github.com/user-attachments/assets/a5b56345-ff52-4b07-91c0-3cd242e966a6)

* Fixed thread count initialization
* * Creates count equal to CPU cores
* Added **IntelEmbree** tracing support, greatly speeding up builds
* * Legacy OPCODE build also sped up
* Prints all missing textures and thm
* Removed `net` compilation
* BC7 format support

## LC
* Skip `invalid faces`
* New LightMaps save algorithm
* * Bakes to chosen texture size with faster lookup
* Fixed original bug where MU models could turn black
* Option to skip geometry tessellation
* Option to skip subdivide stage
* Optimized `geometry merge` stage
* Multithreaded sector compilation
* Removed QSlim for CForm optimization
* * Now uses MeshOptimizer for better results
* Removed old dx9mesh geometry optimizer
* * Now uses modern DirectXMesh
* LightMap without compression supported
* * Keeps original texture quality

## DO
* Grass saved as DXT3

## AI
* Reimplemented AStart algorithm
* AI Map compiled in 25-bit format
* * Input supports original SDK data and 25-bit format
