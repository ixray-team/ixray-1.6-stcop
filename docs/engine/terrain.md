# Terrain

Source inspection: 2026-10-03. No terrain bake or render test was run. This describes the current source, including incomplete paths. See [rendering.md](rendering.md) for R4 frame and lighting modes.

Terrain is authored as a heightmap in LevelEditor, converted to an editable mesh and compiled into ordinary static level geometry. Runtime loads those meshes and their textures, not the editor heightmap. This path has no dedicated runtime heightfield, clipmap or terrain quadtree.

`CTerrain / SHeightMap` → `CEditableObject` → `build.prj` → `xrLC` → level geometry, collision and baked textures → R4 scene graph.

Current export passes a null scene-object owner into code that dereferences it. The compiler flow below describes the intended export and rendering of already compiled levels, not a verified successful bake from the current editor.

## Authoring

[CTerrain](../../src/Editors/LevelEditor/Editor/Entry/Terrain/Terrain.h) owns the floating-point `SHeightMap`, a generated `TerrainObject` (`CEditableObject`), vertical multiplier and surface properties. The surface template survives rebuilds because `CTerrain` owns it.

| Property | Default |
| --- | --- |
| Runtime shader | `levels\zaton_earth` |
| xrLC shader | `default` |
| Physical material | `materials\earth` |
| Base texture | `terrain\terrain_mp_atp` |
| Vertical multiplier, `ScaleY` | 50 |

[UITerrainTool](../../src/Editors/LevelEditor/UI/Tools/UITerrainTool.cpp) creates 129 × 129, 257 × 257 and 513 × 513 maps. The separate `Create 1024` button creates 1025 × 1025 samples. [ESceneTerrainTool](../../src/Editors/LevelEditor/Editor/Tools/Terrain/ESceneTerrainTools.cpp) provides raise, lower, flatten and smooth brushes with smoothstep radial falloff, clamping edited heights to `[0,1]`. Flatten captures the starting height; smooth averages a 3 × 3 neighborhood. Sculpting marks the heightmap cache dirty without rebuilding the editable mesh on each stroke.

`GenerateHeightmapByMesh` samples a 512 × 512 grid with downward rays and writes raw 16-bit `.r16` data plus a PNG under `$server_data_root$/terrain/`. It extracts geometry heights, not diffuse textures or material masks. Reverse import wiring has an inconsistency described below.

## Generated mesh

[HeightmapUtils.cpp](../../src/Editors/LevelEditor/Editor/Terrain/HeightmapUtils.cpp), `GenerateMeshByHeightmap`:

- Creates one vertex per sample and two triangles per grid cell.
- Uses positive `Heightmap.Size.x/z` as sample spacing, falling back to 1.
- Centers X/Z and mirrors X. Local Y is `(height - 0.5) * ScaleY * Heightmap.Size.y`.
- Omits a whole cell if any corner has height `<= 0`; zero represents a hole.
- Assigns UVs `u = 1 - x / (width - 1)`, `v = 1 - z / (height - 1)` across one texture domain.
- Creates one surface and generates face normals, vertex normals and adjacency.

Without holes, counts are `width * height` vertices and `2 * (width - 1) * (height - 1)` triangles. A 513 × 513 map produces 263,169 vertices and 524,288 triangles before compilation. These are derived counts, not measured runtime totals.

`RebuildMesh` replaces the editable object and queues the old object's eviction/deletion for the next editor frame. Rebuilds occur when enabling Preview, changing the height multiplier with Preview enabled, leaving Terrain mode and exporting terrain.

## Editor rendering and picking

[Terrain.cpp](../../src/Editors/LevelEditor/Editor/Entry/Terrain/Terrain.cpp), `CTerrain::Render`, selects two paths:

| Condition | Draw path |
| --- | --- |
| Terrain target active, Preview disabled | `HMap.Draw(ScaleY, 1.f)` at priority 1, non-strict order |
| Preview enabled or another target active | `TerrainObject->Render(this, _Transform(), priority, strictB2F)` |

[HeightMap.cpp](../../src/Editors/LevelEditor/Editor/Terrain/HeightMap.cpp) divides the editing representation into 32-cell chunks. Position/color triangle-list vertices live in a persistent vertex buffer, updated when dirty. Draw tests chunk bounds against the view frustum and issues one draw per visible chunk using the editor wire shader. Colors represent relative height. The active `PrecacheRenderData` path calculates flatness but still emits full cell triangles; its `IsFlat` flag does not simplify active draw geometry.

Generated-mesh preview calls [CEditableObject::Render](../../src/Editors/xrECore/Editor/EditObjectEditor.cpp) and [CEditableMesh::Render](../../src/Editors/xrECore/Editor/EditMeshRender.cpp), which set a surface shader and issue immediate buffer draws. Using R4 shader code does not establish deferred editor visual submission for `CTerrain`.

General terrain-target picking uses `HMap.RayPick`; outside that target it uses the editable mesh. The sculpt tool's `PickTerrain` uses the editable mesh directly, so brush picking can use an older mesh until a rebuild. Editor chunks are separate from compiler subdivisions and runtime collision.

## Level export and geometry bake

[BuilderRemote.cpp](../../src/Editors/LevelEditor/Editor/Builder/BuilderRemote.cpp), `ParseStaticObjects`, rebuilds `OBJCLASS_TERRAIN` and calls `BuildEditableObject` with its transform and a null `CSceneObject` owner. The common static builder exports world-space vertices, faces, UVs, smoothing groups, sector/material references and lights to `build.prj`.

[Build.cpp](../../src/utils/xrLC/Build.cpp), `CBuild::Run`, performs pre-optimization, junction correction, adaptive tessellation, collision generation, lighting and conversion to OGF visuals. Lighting resolves materials, calculates normals/tangent basis, builds ordinary lightmap UVs where needed, subdivides geometry, and runs implicit, atlas and vertex lighting paths.

[xrPhase_Subdivide.cpp](../../src/utils/xrLC/xrPhase_Subdivide.cpp) partitions geometry by bounds and face count. Defaults in `b_globals.h` are 32 meters, 2,048 faces and a 64-face minimum partition size. These are compiler targets subject to splitting/merging rules, not guaranteed terrain tile dimensions or hard output limits.

`Flex2OGF` optimizes partitions and attempts progressive-mesh generation. [OGF::MakeProgressive](../../src/utils/xrLC/OGF_Face.cpp) skips small meshes, draft builds, disabled progressive output and oversized meshes; simplification must meet its metric threshold. This is generic mesh LOD, not heightmap-specific LOD or seam stitching.

[xrSaveOGF.cpp](../../src/utils/xrLC/xrSaveOGF.cpp) and `BuildCForm` write:

| Output | Contents |
| --- | --- |
| `level` | Visual descriptions, shader/texture lists, sectors, portals and other level data |
| `level.geom` | Main vertex/index buffers and slide-window data |
| `level.geomx` | Alternate fast geometry, used by the dynamic renderer where available |
| `level.cform` | Static collision geometry, subject to xrLC collision flags |

Editor collision also includes terrain through `CFormBuilder` and `LEPhysics.cpp`. Physical material is separate from RGBA texture-layer blending. Grass/detail objects use `CDetailManager` and `level.details`; they are separate from terrain triangles and the four texture layers.

## Implicit lighting bake

The expected terrain lighting layout is selected by texture metadata. `DataFace::hasImplicitLighting` checks the rendering flag and base texture `.thm` flag `STextureParams::flImplicitLighted`. [ETextureParams.cpp](../../src/xrEngine/ETextureParams.cpp), `OnTypeChange`, enables this flag for the Terrain texture type. Selecting a landscape runtime shader alone does not enable this bake.

[xrPhase_UVmap.cpp](../../src/utils/xrLC/xrPhase_UVmap.cpp) skips ordinary lightmap-atlas UV generation for implicitly lit faces. [xrLight_Implicit.cpp](../../src/utils/xrLC_Light/xrLight_Implicit.cpp), `ImplicitLightingExec`, groups faces by base texture and bakes at that texture's width and height.

For each jittered texel sample, the CPU path queries triangles in base UV space, finds a containing triangle, reconstructs world position/normal with barycentric coordinates and evaluates lighting. Samples are averaged and scaled by 0.5 before packing. CPU ray tracing uses Embree; CUDA is optional when built and selected. Compiler flags can skip RGB or sun lighting.

[xrLight_ImplicitDeflector.cpp](../../src/utils/xrLC_Light/xrLight_ImplicitDeflector.cpp), `SaveTextures`, fills borders and writes into the compiled level directory:

| Texture | RGB | Alpha | UVs |
| --- | --- | --- | --- |
| `<base>.dds` | Original base color | Baked hemisphere lighting | Base UVs |
| `<base>_lm.dds` | Baked RGB lighting | Baked sun visibility | Base UVs |

`lm_layer::Pack` packs RGB and sun into `_lm`; base texture alpha is replaced with hemisphere lighting. Output disables mipmap generation and uses the selected lightmap format: RGBA, BC7, or the `FORMAT_BC5` option, which currently selects DXT5 here. With `LC_SkipStaticMap`, `_lm` is instead a 4 × 4 black texture with alpha 255.

`Flex2OGF` adds `<base>_lm.dds` as the second texture. Implicit initialization also copies base UVs into a second channel for general mesh-format compatibility. Bake resolution follows the base texture, not terrain sample count or ordinary lightmap density.

The bake groups by texture and selects the first containing triangle per UV sample. Differently placed meshes sharing the same base texture and overlapping UVs therefore do not get independent baked maps.

## Runtime landscape material

[r4_loader.cpp](../../src/Layers/xrRenderPC_R4/r4_loader.cpp) loads shaders, buffers, visuals and sectors. [dx11Texture.cpp](../../src/Layers/xrRenderPC_R4/dx11Texture.cpp), `texture_load`, searches the level directory before global textures for ordinary DDS assets, allowing baked base textures to override global originals.

Terrain uses ordinary sector/portal traversal, frustum and HOM tests, scene-graph sorting and indexed mesh draws. Where progressive data exists, `FProgressive` chooses a compiled slide window. Runtime does not reconstruct the authoring grid or use the editor's 32-cell chunks.

[Blender_BmmD.cpp](../../src/Layers/xrRenderPC_R4/Blender_BmmD.cpp) implements the landscape material. Shader names retain `R1`/`R2` terminology, but both lighting modes run in `xrRender_R4`.

### Dynamic lighting mode

Normal passes use `deffer_base` and [deffer_impl.ps.hlsl](../../gamedata/shaders/d3d11/deffer_impl.ps.hlsl). HQ enables `USE_4_BUMP` and binds:

- Base texture and baked `_lm` texture.
- `<base>_mask`, whose normalized RGBA weights select four tiled detail materials.
- Four diffuse textures, `_bump` and `_bump#` textures, plus `_spec` textures where present.

Detail coordinates are base UVs multiplied by `dt_params.xy`. The shader blends layer color and normal/material data and modulates base color by detail color. The PBR branch also blends metalness, roughness, subsurface scattering, ambient occlusion and specular data.

LQ uses a single detail texture and its bump data. `CRender::rimp_select_sh_static` chooses HQ/LQ using camera distance minus the bounding-sphere radius against `r_dtex_range`. This is material quality selection, separate from geometry LOD.

The normal terrain pass reads base alpha as hemisphere lighting and `_lm` alpha as sun visibility, then writes material data and a terrain material ID to the G-buffer. Baked sun reaches packed output when the static-sun permutation uses it; otherwise R4 supplies dynamic sun shadows. `_lm` RGB is not added by the ordinary deferred terrain pass, but is explicitly used by the reflection variant.

The shadow element uses `shadow_base` and can draw compiled fast geometry. `USE_TERRAIN_PARALLAX` exists in the shader, but the inspected tree has no producer enabling the define. Do not describe parallax as active by default. `CBlender_BmmD` does not select a tessellation method.

### Static lighting mode

Forward `impl_dt` shaders combine ambient light, baked `_lm` RGB, hemisphere light modulated by base alpha and sun light modulated by `_lm` alpha. Base/detail colors modulate the result, followed by fog.

HQ uses four-layer mask blending when `r1_use_terrain_mask` is enabled; otherwise it uses one detail texture. LQ uses one detail texture. Landscape visuals are exempt from some distance/size optimization gates through `bLandscape` in this mode; ordinary visibility tests still apply.

## Current defects and verification limits

These findings follow from source inspection; failure scenarios were not exercised. They are not fixes or historical completion claims.

| Finding | Source and consequence |
| --- | --- |
| Null owner during export | `ParseStaticObjects` passes `nullptr` to `BuildEditableObject`; `BuildMesh` unconditionally iterates `obj->m_Surfaces`. Generated terrain reaches a null dereference before export completes. |
| Counts taken before rebuild | `CompileStatic` allocates using `GetStaticDesc`, which counts the existing mesh. Terrain rebuild happens later. Restoring hole cells can increase the face count beyond the allocated face buffer and trigger its bounds assertion. |
| Height scale not persisted | `CTerrain::SaveStream` saves samples, surface strings and inherited transforms, but not `ScaleY`. Load reconstructs with the default multiplier. |
| Dimensions inferred as square | `SHeightMap::SaveSteam` writes only 16-bit samples. `LoadSteam` sets both dimensions from the square root of the byte count; rectangular dimensions cannot round-trip. |
| Scale applied twice | `CTerrain::OnFrame`/`Scale` copy object scale into `HMap.Size`; generation applies it, then preview/export apply `_Transform`, which includes scale again. Non-unit scale can disagree with intended dimensions. |
| Raw import calls scene loader | `ESceneTerrainTool::_AppendObject` opens `.r16` and calls `CTerrain::LoadStream`, which expects heightmap/asset and inherited scene-object chunks. Model extraction writes raw samples. `SHeightMap::LoadRAW` has no caller in this import path. |

Runtime output, bake success, performance and editor/game agreement remain unverified. Render verification needs a loaded level. Use the existing LevelEditor headless autotest for editor rendering and the engine autotest in [lifecycle.md](lifecycle.md) for compiled levels. Relevant cases are non-unit transforms, edited height scale after save/reload, holes restored before export, and the HQ/LQ material transition.

