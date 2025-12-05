# Physically Based Rendering (PBR)
> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimal version**: 1.3

# PBS lighting model
Enable **PBS** in **engine_external.ltx** by removing/commenting the first line or setting it to `0`.

![image](https://github.com/user-attachments/assets/51dfacd3-f01a-48ae-be84-376d36c8f822)

Description of other macros:

![image](https://github.com/user-attachments/assets/ba1932ab-245f-4aec-a628-bfc75a1b041a)

To enable a PBS material for a texture, choose the lighting model when creating the **THM** in **IXR** editors
* or via [THM Editor](https://disk.yandex.ru/d/AkZWYx3Xohdd-g) (Mortan’s build).

![image](https://github.com/user-attachments/assets/bf1c7cdc-4071-4c09-b070-dd3cad1c02e6)


***
# PBS material packing

## **BUMP** — Texture 2D — geometry data (classic BC3N packing)
- **BUMP R** — Height map for parallax/tessellation (optional) — leave empty by default (BC3N writes garbage)
- **BUMP G** — Normal map green channel (OpenGL format)
- **BUMP B** — Blue channel (optional) — reserved; leave empty by default (BC3N writes garbage)
- **BUMP A** — Red channel of the original normal 

![image](https://github.com/user-attachments/assets/05ae4ef1-98f3-402e-8c28-33a5e5002c67)

## **BUMP#** — Texture 2D — material packing
- **BUMP# R** — Metalness map
- **BUMP# G** — Roughness map
- **BUMP# B** — Translucency (SSS) for vegetation — black by default
- **BUMP# A** — AO map

![image](https://github.com/user-attachments/assets/9f003bbc-675f-404b-9c1c-e6baf943e8f3)

***
# IBL lighting

For correct **IBL** you need baked mip levels on environment textures (skyboxes).

![image](https://github.com/user-attachments/assets/cb8cbcac-f474-472c-8e7b-d83527250eeb)

The diffuse irradiance map (_small sky_) does not need high resolution. Spherical harmonics for diffuse may be supported later.

![image](https://github.com/user-attachments/assets/66a7e7fd-66f5-449b-8186-a14a993b9796)
