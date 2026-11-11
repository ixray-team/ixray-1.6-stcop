# Blurred texture shadows

`CUIStatic` can cast a soft shadow from its current texture alpha onto a virtual UI plane. The feature is opt-in and uses the R4/D3D11 renderer. Other renderers keep drawing the static normally and omit the soft shadow. Existing `<shadows>` outlines remain independent.

## Lua

```lua
-- enabled, Gaussian sigma, opacity [0..1], radius, light X, light Y
static:SetBlurredTextureShadow(true, 3, 0.5, 9, 512, 0)

-- Optional: element height above the receiving plane, point-light height.
-- Defaults: 4 and 300. Height units must match each other.
static:SetBlurredTextureShadowHeight(4, 300)

-- Optional finite emitter radius (root UI units). 0 preserves fixed-filter legacy mode.
static:SetBlurredTextureShadowLightRadius(400)

-- Optional density response: 0 = linear coverage, positive = exponential density.
static:SetBlurredTextureShadowFalloff(3)

-- Explicit opacity interval. Maximum is also the base shadow opacity.
static:SetBlurredTextureShadowOpacityRange(0.0, 0.7)

-- Optional: color; its alpha multiplies the opacity and texture tint alpha.
static:SetBlurredTextureShadowColor(GetARGB(255, 0, 0, 0))

-- Disable the pass; no shadow shader is created or drawn while disabled.
static:SetBlurredTextureShadow(false, 3, 0.5, 9, 512, 0)
```

`GetBlurredTextureShadowEnabled()` reads the enable flag. Sigma and radius use UI control units. Sigma controls Gaussian softness, while radius limits sampling support and padded geometry. Sigma 0 or radius 0 produces a sharp projected silhouette. Opacity 0 skips the shadow draw entirely. Parameters are sanitized for non-finite values and bounded to avoid invalid GPU inputs.

## XML

Add the following child node to a static:

```xml
<blurred_shadow enabled="1" blur="3" opacity="0.5" radius="9"
    light_x="512" light_y="0" height="4" light_height="300" light_radius="400"
    falloff="3" opacity_min="0" opacity_max="0.7"
    r="0" g="0" b="0" a="255" />
```

Omitting the node leaves the effect disabled. Named colors use the same `color` attribute as other UI color nodes.

The opacity profile follows the projected silhouette and preserves the Gaussian convolution result without post-convolution sharpening, contour renormalization, or a steep density curve. Maximum opacity scales the Gaussian mask. Minimum opacity identifies the beginning of its faint outer tail: `tail = smoothstep(0, minimum / maximum, coverage)`, and output opacity is `maximum * coverage * pow(tail, max(falloff / 3, 1))`. This leaves the visible Gaussian body unchanged and only fades the weakest tail smoothly to transparent. There is no nonzero opacity floor at the support boundary.

Maximum 0 skips the pass entirely. Falloff is sanitized to 0–12; the opacity range to 0–1, with minimum clamped at maximum. Legacy `opacity` remains the XML fallback when `opacity_max` is absent.

## Point-light projection

Light XY is in root UI coordinates, 1024×768, independently of the static's parent. A source point `P` at height `h` and light `L` at height `H` projects onto the UI plane at:

```text
S = L.xy + (P.xy - L.xy) * H / (H - h)
```

The full silhouette is projected, so the shadow grows and moves away from the light. Projection uses current coordinates, heading, texture offset, and parent transforms on every draw. Moving a static or its parent, changing its texture/atlas rectangle, or updating light coordinates therefore updates the shadow immediately. No stale per-widget position cache is used. A light at or below the element height produces no shadow; zero-height elements project directly beneath themselves. Singular parent transforms skip the effect.

This is a planar UI shadow model: it does not trace scene geometry or model bent pages, and does not illuminate the source texture. Draw order determines which UI layers cover the shadow. A parent's clipping region may intentionally cut off a child's shadow.

## Finite-source penumbra

A positive `light_radius` enables a local finite-source approximation. At caster height `h`, emitter height `H`, and emitter radius `r`, the transverse receiving-plane kernel radius is `r*h/(H-h)`. Its Gaussian sigma is the configured `blur/radius` fraction of that support. For a local ray with horizontal displacement `d` and vertical depth `H-h`, the kernel stretches along the ray by `sqrt(1 + |d|²/(H-h)²)` and retains its transverse width. This is the covariance of a locally projected volumetric emitter; it is evaluated per fragment, not once at the widget center.

The source texture's UV-to-root-UI basis includes widget heading, atlas coordinates and parent transforms. The shader transforms the directional kernel back into this UV basis. Geometry padding uses a conservative bound over all caster corners so changing light angle does not cut off the penumbra. A zero-height caster has zero penumbra. Changing light XY, light height, caster height or emitter size changes the projected filter. The original plane projection still controls the silhouette's displacement and scale.

This is a planar UI approximation, not scene ray tracing or self-occlusion of curved pages. At grazing angles the penumbra can become wider than the former fixed-radius setting. `light_radius=0` retains the legacy fixed-filter behavior and its old radius semantics. Effects remain fully opt-in and disabled widgets issue no shadow draw.

## Rendering and resources

The feature draws one expanded quad before the original texture. `ui_texture_shadow.ps.hlsl` samples only source alpha, evaluates a normalized 17×17 Gaussian for the projected silhouette, and applies the dense-contact opacity profile. Sampling is restricted to the source atlas rectangle with half-texel-safe clamping and transparent border coverage, so neighboring atlas entries cannot leak into the blur. The Gaussian weights taper smoothly to zero over the final quarter of each kernel axis; this prevents a hard truncation at the edge. The quad includes transparent padding equal to the blur radius and follows the light projection.

Shader variants are allocated lazily alongside the source UI shader, then reused; new source textures invalidate those variants through normal shader creation. Per-frame light, sigma, radius, atlas bounds, and opacity are supplied without generating new image assets. Disabled effects add no shadow pass, shader binding, Gaussian work, or shadow resource creation. Enabled effects cost one additional draw and up to 289 alpha samples per covered pixel.

Runtime shader files live in the main game data, not the menu addon:

```text
gamedata/shaders/d3d11/ui_texture_shadow.ps.hlsl
gamedata/shaders/d3d11/hud_ui_texture_shadow.lua
gamedata/shaders/d3d11/hud_ui_texture_shadow_world.lua
```

Matching versioned sources are kept in the engine repository's `gamedata` tree. Build and deploy the engine modules together because `CUIStatic` and the render interface changed.
