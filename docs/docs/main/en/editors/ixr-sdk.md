# IXR SDK

> [!IMPORTANT]  
> **Status**: WIP <br>
> **Minimal version**: 2.0  
> This page lists changes versus the original 0.7 SDK.

Included here:
* 0.8 SDK changes by RedPanda  
* OMP SDK changes (merged)  
* Hybrid SDK changes (merged)  
* B.O.R.S.C.H.T SDK changes (merged)  
* TSMP SDK changes (merged)  
* Our own changes  

**See the changelog for exact authorship.**

## Various fixes
* Added GPU Skinning for skeletal models without performance loss
* 127 bones supported for dynamic models
* * First 75 bones are processed on GPU in __Editor__ mode
***
* Shader Editor: Step Sounds entries increased to 16 _(IWP support)_
***
* Actor Editor: 32-bit animation support
* Actor Editor: Smoothing groups mode from SDK 0.4
* Actor Editor: Smoothing groups by **Vertex Normals**
::: details Actor Editor: Link bone with static mesh binding
<Video url="https://www.youtube.com/watch?v=ibvCIYcw6Jc"/>
:::
***
* Level Editor: Create shapes via `RMB -> Create -> Shape`
* Level Editor: 30 **SubMaps** per sector
* Level Editor: 32 **rpoint** _(OMP support)_
* Level Editor: **LOD** map up to 4096x4096
* Level Editor: **Details** count up to 512
* Level Editor: **Details** saved as DXT5
* Level Editor: Wallmark limit removed
* Level Editor: Small polygon culling removed
* Level Editor: Restored missing-model message on level load
* Level Editor: Fixed name output in `EParticlesObject: '' not found in library`
* Level Editor: Can skip errors for missing assets
* Level Editor: Added user.ltx and shader cache support
* Level Editor: Fixed `Ignore Materials` in **AIMap Tools**
* Level Editor: Fixed .thm reading for group objects (read from `rawdata\group`)
* Level Editor: Fixed sector reset when replacing `Scene Object` via `Reference`
::: details Level Editor: Added `Multi Replace` with sector restoration
<Video url="https://www.youtube.com/watch?v=1UCjDdH6BNg"/>
:::
* Level Editor: Open `temp\*.tmp` files
* Level Editor: Grass rendering moved to GPU
* Level Editor: Fixed `.thm` load/save for GroupObject; `.thm` stored with objects
* Level Editor: Optimized `Graph Point` rendering
* Level Editor: [Plugin system support](https://github.com/ixray-team/ixray-1.6-stcop/wiki/SDK:-Plugins)
* Level Editor: Restored simulation mode for `Sound Src` (from 0.4 SDK)
* Level Editor: Fixed crash on `Reload Object` with sector rendering enabled
* Level Editor: Particle rendering for `CCustomZone` and heirs in `Edit` mode (bonfires, anomalies, etc.)
* Level Editor: AI grid stored/built in 25-bit by default; old AI maps auto-convert
* Level Editor: `Scene Objects` validation runs multithreaded
***
* Particles Editor/Level Editor: import level/particles.xr from 0.4 SDK
* Particles Editor: no longer deletes old files in `rawdata/particles`; overwrites duplicates
* Particles Editor: save with skipping invalid particles
***
* Post Process Editor: integrated into Actor Editor
* Dialog Editor: [node-based implementation](https://github.com/ixray-team/ixray-1.6-stcop/wiki/Dialog-Editor)

## Extended features

::: details Height Map (Terrain)

![image](https://github.com/user-attachments/assets/caaa9d22-6803-4b82-bc42-193b1e907c2d)

* Height map support in `r16` format
* Drag from `Content Browser` onto the scene
* Can be extracted from a model

**More:** https://youtu.be/InNlBHp4VwQ
:::

::: details Random Append

Load/save brush settings for **"Random Append"**

![image](https://github.com/user-attachments/assets/535bd6fc-bb38-4a6a-935f-673aadc9a379)

D&D support from **Content Browser**

![ezgif-73c713f1518b07](https://github.com/user-attachments/assets/63630b81-e933-4300-822c-1edd571c70b0)
:::

::: details Validation: Skip stages

![image](https://github.com/user-attachments/assets/958cd86b-0d6c-496e-acd2-8d3313c90769)

* Skip validation during `Make All`
* Skip LOD texture validation
* Skip duplicate name validation
:::

::: details Detail Object List

* D&D from __Content Browser__

![image](https://github.com/user-attachments/assets/dd0f771e-cb3a-4e3f-94ce-4997bdbc6db2)

* Detail mask preview
* Append Color Index via eyedropper on mask preview

![image](https://github.com/user-attachments/assets/ba58b231-8cf6-4fd1-bdb7-04893f512ddc)
:::

::: details Lock Object

Restored __Lock Object__ from SDK 0.5/0.6

![image](https://github.com/user-attachments/assets/0304126a-3d92-43b8-b529-7d8101595153)
:::

::: details Thumbnail View

**Thumbnail View** — quick `.thm` editor without loading the resource (`.tga`, `.object`, etc.)

![image](https://github.com/user-attachments/assets/ce8cf625-f92d-4335-95f2-6ce952a69d7d)

1. Enable `.thm` display in __Content Browser__
2. Pick a file (left click)
3. Save changes or close/open another file to discard
:::

::: details Image Editor

* BC7 support

![image](https://github.com/user-attachments/assets/09c7ded8-7ab6-43c3-a548-a5ec45cb472a)

* Unsupported MIP filters:
> Gaussian, Sinc, Bessel, Hanning, Hamming, Blackman
:::

::: details Library Editor

* LWO export removed
* Added dedicated viewport for objects

![image](https://github.com/user-attachments/assets/f73299f6-4326-4777-958f-dbd7a2211f38)
:::

::: details Particles Editor

* Fixed Distort rendering

![image](https://github.com/user-attachments/assets/8557389e-86fe-47f9-9519-77365ffdb7f2)
:::

::: details Cubic env_mod

* Added cubic env_mod support

![image](https://github.com/user-attachments/assets/1c4b5a2b-70af-40b9-93de-c9c29b924a24)
:::

::: details Minimap Editor

![image](https://github.com/user-attachments/assets/12f1450d-a278-495c-a69e-890e6e62a99a)

Fully functional UI map editor
:::

::: details Viewbox

![image](https://github.com/user-attachments/assets/41d39c8e-9424-4c8b-a1d2-628aee2fa897)

Viewbox shows camera direction relative to 0.0.0
:::

::: details Compilation

Compiler launch from Level Editor.

![image](https://github.com/user-attachments/assets/67a4c4be-6ea7-4b25-b482-6ad2be7b3285)

* Compiler paths can also be set in settings:

![image](https://github.com/user-attachments/assets/db08d18b-0248-44e3-ae78-b324662c6aa3)
:::

::: details Gizmo

Full object manipulation via [ImGuizmo](https://github.com/CedricGuillemet/ImGuizmo/)

![image](https://github.com/user-attachments/assets/f1d94803-8f95-45f5-a2ef-a9fbf45b8b94)

* Old control scheme available in SDK settings: **"Preference -> Viewport -> Buttons"**

![image](https://github.com/user-attachments/assets/b3369494-15df-49c2-ab67-2a8848359c90)

* Box scale for static objects and shapes

![image](https://github.com/user-attachments/assets/4a2359e4-a071-4a7b-936c-c0e4e4a9bc06)

* Spherical shapes scale by radius

![image](https://github.com/user-attachments/assets/a9b41216-8b9c-450a-bb0d-80076ba3521c)

* Local/World mode support
![image](https://github.com/user-attachments/assets/ba287233-9924-496c-98e3-014454c50de6)
:::

::: details Interface

### Docking
Docking support to customize window layouts.

![image](https://github.com/user-attachments/assets/ffc0eefe-bf84-48b4-a1bc-91b8d29452e2)

### Theme
![image](https://github.com/user-attachments/assets/d6a06646-0e67-448b-8c24-dca66ceb8214)

In **"Windows -> Theme"** open the UI theme editor to tweak colors:

![image](https://github.com/user-attachments/assets/8a6df6e3-a0ab-49e2-8fa1-6d3972932134)
:::

::: details Actor Editor: Skip Optimization

Option to skip optimization for dynamic meshes

![image](https://github.com/user-attachments/assets/63cfa5a5-d259-4693-b1a4-97fec0d59130)
:::

::: details Dialogs

Legacy Windows dialogs replaced with Win7+ versions

![image](https://github.com/user-attachments/assets/7bfa8457-b674-4403-9be2-45bef1a6ed2a)
:::

::: details World Properties

Location parameters in a separate window: **"Scene -> World Properties"**:

![image](https://github.com/user-attachments/assets/47cf2f38-12de-4165-b4c1-24b4d2bbadf0)

![image](https://github.com/user-attachments/assets/6b1f1045-66f1-430a-a935-ed69ed6241cb)
:::

::: details Object Reference

**Object Reference** lets you tweak object parameters on a level without changing the base (.object).

![image](https://github.com/user-attachments/assets/97b2b381-ea8f-4149-be57-84bd4d8110f6)
:::

::: details Content Browser

![image](https://github.com/user-attachments/assets/4d619049-933d-4a36-ba1d-592d9e253807)

**Content Browser** — work window for objects. Place **objects/groups/spawn items** on scene, convert .tga to .dds, delete files. **(In development)**

__Current features:__
* Convert TGA <-> PNG
* Convert DDS -> TGA
* Convert DDS -> PNG
* Open levels
* Delete/Copy/Move files (with thm)
* Open TGA to convert to DDS
* Search files/spawn items
* Drag-n-Drop files/spawn items into viewport.
<Video url="https://www.youtube.com/watch?v=wAazMqGHhxo"/>
:::

::: details Play in Editor (PIE)

PIE — run simulation inside the editor. Requires compiled CForm, AI Map, Spawn Elements. Validation runs automatically; you can also run "Make Game" (Spawn Elements) or use buttons below:

![image](https://github.com/user-attachments/assets/554b4bbf-af25-42e5-a8f7-c1c4702b54bc)

Currently implemented:
* A-Life
* Weather
* Dynamic Light
 <Video url="https://www.youtube.com/watch?v=EI3NBB-dfb0"/>
* Sound Environments
 <Video url="https://www.youtube.com/watch?v=-r738Zd1zlE"/>
* Particles (+ Distort)
* Cut-Scene preview

![image](https://github.com/user-attachments/assets/052b801c-2888-4de2-82d6-575876c6e0ab)

Settings:
* Validate `Space Restrictors`
* Move actor to editor camera position
* Enable `build_artefact_spawn_pos`

![image](https://github.com/user-attachments/assets/998fe370-ea21-40b6-a779-9363fbd533e4)
:::

::: details PostProcess Editor

* Moved to **Actor Editor -> Windows -> Post Process**

![image](https://github.com/user-attachments/assets/75b3fd37-80ea-4163-939f-d06aa245ee0f)
:::

::: details Misc buttons

### Recalculate Portals
Deletes existing portals and recreates them.

![image](https://github.com/user-attachments/assets/9f267fc6-f1ca-4acb-af4b-89f71a275828)

### Hot-Key: Duplicate
Creates a copy of the selected object in place (fast copy-paste).

![image](https://github.com/user-attachments/assets/cbe0c71a-ed13-4b6b-9fd8-d613f947a1c8)
:::

::: details Level Type: FreeMP

![image](https://github.com/user-attachments/assets/eec33ede-4625-4e23-ad97-fe650f3b911b)

`FreeMP` — level type for **Free MP** mode (open multiplayer). Compatible with OMP.
:::

::: details Puddles

Dynamic puddles appearing in rain. Compile via **"Compile -> Make -> Make Puddles"**

![image](https://github.com/user-attachments/assets/706028ce-1bea-4a1b-882a-a8fff206230e)
:::

::: details Level Type: Macro Editor

* Macro editor rewritten on the node system

![image](https://github.com/user-attachments/assets/63153dec-3820-41f3-80b6-f1cd7ba67e7c)
:::

::: details Texture Viewer
![image](https://github.com/user-attachments/assets/1a4fdd66-5872-45fb-b7a0-6f2fd1b5abc2)</br>
View textures by channels and in GrayScale. Navigate via **Content Browser**
:::

::: details Weather Properties

**Weather Properties** — separate window for quick weather tuning [**'Options/Menu -> Environment -> Weather properties'**]:
![2](https://github.com/user-attachments/assets/54ef6690-d4eb-4600-8f71-76b25ef908f5)

**You can set:**
* Current weather cycle
* Current time of day
* Time factor or pause time
* Disable raindrop collision (useful when rain tanks FPS)
* Toggle roof rain sound
* Plus standard options — 'Fog/Real Time/Mute Sounds/Stats/Draw Grid'
![3](https://github.com/user-attachments/assets/c3c4979d-80d6-4fb5-a0e6-53d556c22e72)
![4](https://github.com/user-attachments/assets/3d2fb5d7-7127-452d-b3f5-7f2fd1b5abc2)
![5](https://github.com/user-attachments/assets/35015a9c-650e-4880-818a-d97634b0dbf7)
:::
