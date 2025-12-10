# Shader options
> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimal version**: 1.3

# General
Optional shader settings live in [gamedata/configs/engine_external.ltx](https://github.com/ixray-team/ixray-1.6-stcop/blob/default/gamedata/configs/engine_external.ltx). 

Default preset:
```ini

[shaders_options]
USE_LEGACY_LIGHT = 1

; Hozar: Use brga skycolor format
; USE_BGRA_SKYCOLOR = 1;

; Hozar: Use original GSC format for sky ToneMapping
; USE_LEGACY_SKY_TONEMAP = 1

; Hozar: Use normal on Hemi calc
; USE_NORMAL_HEMI_DISTRIBUTION = 1

; Hozar: For PBS pipeline
; IBL_FAKE_IRRADANCE = 1
; IBL_REMAP_IRRADANCE = 1
; IBL_REMAP_REFLECTIONS = 1
; IBL_REMAP_POSITIVE_Y = 1
; IBL_MAX_LOD = 10
; USE_FULL_SKY_SPHERE = 1
```

# Parameters
### USE_LEGACY_LIGHT
* Original lighting model (Diffuse + Specular)

### USE_BGRA_SKYCOLOR 
* Sky color from weather configs using `BRGA` channel order  
> When disabled, uses `RGBA`

### USE_LEGACY_SKY_TONEMAP
* Use the original sky tonemapping algorithm

## IBL / PBS options
Available defines and their impact on rendering.

### USE_NORMAL_HEMI_DISTRIBUTION
* Apply surface normal to hemisphere lighting (ambient occlusion style shading).  

### IBL_FAKE_IRRADANCE
**(PBS only)**  
* Hack for PBS when sky textures lack mip levels.  

### IBL_REMAP_IRRADANCE
* Remap **Diffuse Irradiance** texture.  
> More physically correct lighting; visuals may vary.  

### IBL_REMAP_REFLECTIONS
* Same remap for **Specular Reflections** (PBS only).  

### IBL_REMAP_POSITIVE_Y
* Used for **specular reflections**.  
> For GSC skyboxes: ignores fog in reflections.  

### IBL_MAX_LOD
* _Optional_ number of **mip levels** in skyboxes.  

## USE_FULL_SKY_SPHERE
* Use classic **sky cubes** without stretching.  
