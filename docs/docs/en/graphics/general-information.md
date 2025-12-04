# General Information
# DLSS & FSR & XeSS

![image](https://github.com/user-attachments/assets/9c4b5508-8f0a-4d95-9329-a15f004746a6)

* vid_scale
* vid_scale_mode
* vid_scale_preset

## Upscale

**Upscale (DLSS/FSR2/XeSS)** renders the game at a lower resolution and reconstructs it to a higher one, increasing FPS.

## TAA

DLSS anti-aliasing uses temporal upscaling with resolution reconstruction and aggressive edge smoothing. It blends multiple frames for a sharper, smoother image without large detail loss.

## CAS

**CAS (Contrast Adaptive Sharpening)** is a sharpening filter that boosts contrast on object edges for a crisper image, without amplifying noise or flat-surface artifacts.
![image](https://github.com/user-attachments/assets/0e80b5e8-65b6-4b0f-ac41-1465f8cd9552)

## PBR

**PBR (Physically Based Rendering)** is a rendering approach based on physical light/material interaction. It produces realistic surfaces (metal, wood, skin, etc.) and maintains consistent look across different light sources.

![image](https://github.com/user-attachments/assets/d876cf35-6588-4b5a-8b25-f7806982b738)
![image](https://github.com/user-attachments/assets/fcd47c1b-7e23-4627-a567-24182acb9ca3)

## Chromatic Aberration

**Chromatic aberration** slightly splits colors at object edges, adding subtle “color outlines” to mimic lens distortion for extra realism.
![image](https://github.com/user-attachments/assets/7becc363-fea9-4df1-b848-433593757546)

## Vignette

**Vignette** darkens the image edges toward the corners, focusing attention on the center and adding a cinematic feel.
![screenshot_1](https://github.com/user-attachments/assets/9597e6f1-2ae0-40c4-81af-e5bea946d555)

## Saturation

**Saturation** controls color intensity: high saturation makes colors bright and punchy, low saturation makes them pale or nearly gray.

![image](https://github.com/user-attachments/assets/256bffdf-86d8-4a9b-a5be-ff42cb4068b8)

## Reflections

Water reflections simulate a mirror of the environment on the surface, accounting for ripples, waves, and transparency to keep scenes believable.

![screenshot_2](https://github.com/user-attachments/assets/0e1adeca-6f0d-4e06-acb0-16245fa6f11e)

## Ambient Occlusion

**Ambient Occlusion** darkens creases and corners where light is occluded.

### GTAO

![screenshot_3](https://github.com/user-attachments/assets/17422786-1ae5-4524-a26f-79319bda63f3)
**GTAO** is more accurate and physically grounded, modeling light distribution for softer, more realistic small-scale shadows.

### SSAO

![screenshot_4](https://github.com/user-attachments/assets/73d9b277-6b66-4536-a773-92da541656c7)
**SSAO** is a fast screen-space method; it approximates darkening from depth/normals and can show minor artifacts.

## Hashed Alpha Test

**Hashed Alpha Test** discards transparent pixels using (pseudo)random hashing to smooth edges and reduce jaggies on alpha textures, especially vegetation and meshes.

## Snow Mask

**Snow Mask** dynamically lays snow on outdoor objects (static and dynamic) based on their normal maps. 
![image](https://github.com/user-attachments/assets/1c19bdac-94d1-4e69-8a7d-6fc0dbbc8399)

* `engine_external.ltx`
* * `UseDynamicSnowMask = true`
