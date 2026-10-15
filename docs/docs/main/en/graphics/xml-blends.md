# XML Blends
> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimal version**: 1.2

## Overview

Shader blends of the D3D11 renderer are XML files in the shader folder (`shaders\d3d11`). A blend is found by its shader name: `effects\water` is `effects_water.xml`.

> [!WARNING]
> Lua blends (`.lua`) are no longer supported. The engine ignores them and writes `! Lua shader blend '<file>' is not supported` to the log. Convert them to XML version 2: the table [Lua to XML](#lua-to-xml) lists the equivalents.

When a shader name has no XML blend, the engine uses the C++ blender of that name, then `stub_default.xml`.

## Version 2

```xml
<shader version="2" r1_static="1">
    <normal>
        <pass vs="model_distort" ps="model_scope_gauss"/>
        <fog value="1"/>
        <zb test="1" write="0"/>
        <blend enable="1" src="srcalpha" dest="invsrcalpha"/>
        <aref enable="1" value="0"/>
        <sorting priority="2" strict="1"/>
        <texture name="s_base" value="{base}"/>
        <texture name="s_image" value="$user$accum"/>
        <sampler name="smp_base"/>
        <sampler name="smp_rtlinear"/>
    </normal>
    <l_special>
        <pass vs="scope_lense_mask" ps="scope_lense_mask"/>
        ...
    </l_special>
</shader>
```

### Root **shader**

* `version` - `2`
* `r1_static` - `1`: with static lighting the blend is used instead of the C++ blender of the same name (0/1, default 0)

### Elements

An element is one shader element. Its operations run in the order they are written.

| Element | Used for |
| --- | --- |
| `normal_hq` | Main element when the base texture has a detail texture |
| `normal` | Main element otherwise; also the second element |
| `l_point` | Point light |
| `l_spot` | Spot light |
| `l_special` | Special pass (emissive, scope mask) |
| `normal_hud` | HUD element; not used with static lighting |
| `normal_ui` | UI element |

Instead of named elements, a blend may fill slots directly with `<element index="N" detail="0|1">` (N = 0..5, `detail` enables the detail texture lookup). A blend uses either named or slot elements.

* `option` - `NAME` or `!NAME`: the element is an alternative used when the shader option `NAME` is set (or not set). Write alternatives of one name one after another; the first one that matches is used.

```xml
<normal option="ALLOW_WBOIT_TRANSPARENCY">
    ...
</normal>
<normal>
    ...
</normal>
```

### Tag **static**

Holds a complete set of elements used with static lighting (`renderer_r4_static`) instead of the elements outside it.

### Textures

`value` of `texture` may contain:

* `{base}` - first texture of the material
* `{second}` - second texture
* `{detail}` - third texture, else the detail texture of the base, else `null`

`{base}_nm` gives the bump texture of the base.

### Operations

| Node | Attributes |
| --- | --- |
| `pass` | `vs`, `ps`, `gs` (optional). Starts a new pass |
| `pass_full` | `vs`, `ps`, `gs` (optional), `fog`, `zread`, `zwrite`. Pass with fog and depth settings |
| `uber_deffer` | Deferred model pass (version 1 `uber_deffer`) |
| `sorting` | `priority` (number), `strict` (0/1) |
| `emissive` | `value` (0/1) |
| `scope` | `value` (0/1) |
| `scope_postprocess` | `value` (0/1) |
| `distort` | `value` (0/1) |
| `wmark` | `value` (0/1) |
| `fog` | `value` (0/1) |
| `zb` | `test` (0/1), `write` (0/1) |
| `blend` | `enable` (0/1), `src`, `dest` (blend) |
| `iblend` | `index` (render target), `enable` (0/1), `src`, `dest` (blend) |
| `aref` | `enable` (0/1), `value` (number) |
| `color_write` | `r`, `g`, `b`, `a` (0/1) |
| `texture` | `name`, `value` |
| `sampler` | `name` |
| `stencil` | `enable` (0/1), `func` (compare), `mask`, `write_mask` (number), `fail`, `pass`, `zfail` (stencil) |
| `stencil_ref` | `value` (number) |
| `zfunc` | `func` (compare) |
| `cull_mode` | `mode` (`none`, `cw`, `ccw`) |
| `alpha_to_coverage` | `value` (0/1) |
| `shader_option` | `name`, `value`. Adds a define to the next shaders |
| `clear_shader_options` | Removes the defines added by `shader_option` |

* blend: `zero`, `one`, `srccolor`, `invsrccolor`, `srcalpha`, `invsrcalpha`, `destalpha`, `invdestalpha`, `destcolor`, `invdestcolor`, `srcalphasat`
* compare: `never`, `less`, `equal`, `lessequal`, `notequal`, `greater`, `greaterequal`, `always`
* stencil: `keep`, `zero`, `replace`, `incrsat`, `decrsat`, `invert`, `incr`, `decr`

### Lua to XML

| Lua | XML |
| --- | --- |
| `function normal(shader, t_base, t_second, t_detail)` | `<normal>` (same for other functions) |
| `shader:begin(vs, ps)` / `shader:begin(vs, gs, ps)` | `<pass vs="" ps=""/>` / `<pass vs="" gs="" ps=""/>` |
| `:sorting(2, false)` | `<sorting priority="2" strict="0"/>` |
| `:emissive(true)`, `:distort(true)`, `:wmark(true)`, `:fog(true)` | `<emissive value="1"/>`, `<distort value="1"/>`, ... |
| `:zb(true, false)` | `<zb test="1" write="0"/>` |
| `:blend(true, blend.one, blend.one)` | `<blend enable="1" src="one" dest="one"/>` |
| `:iblend(1, true, blend.srcalpha, blend.invsrcalpha)` | `<iblend index="1" enable="1" src="srcalpha" dest="invsrcalpha"/>` |
| `:aref(true, 0)` | `<aref enable="1" value="0"/>` |
| `:color_write_enable(true, true, true, false)` | `<color_write r="1" g="1" b="1" a="0"/>` |
| `shader:dx10texture("s_base", t_base)` | `<texture name="s_base" value="{base}"/>` |
| `shader:dx10sampler("smp_base")` | `<sampler name="smp_base"/>` |
| `:dx10stencil(...)`, `:dx10stencil_ref(n)`, `:dx10zfunc(cmp.less)` | `<stencil .../>`, `<stencil_ref value="n"/>`, `<zfunc func="less"/>` |
| `AddShaderOption(name, value)`, `ClearAllShaderOptions()` | `<shader_option name="" value=""/>`, `<clear_shader_options/>` |
| `if GetShaderOption("NAME") then ... else ... end` inside a function | Two elements: `option="NAME"`, then the one without it |
| `if GetShaderOption("USE_R1_STATIC_LIGHTING")` at file level | `<static>` set |
| `r1_static = true` | `r1_static="1"` |

Texture names are written with a single `\`: `fx\fx_noise`.

## Version 1

The older format without `version` is still supported.

```xml
<w>
    <element_0 ps="accum_volumetric" vs="accum_volumetric" fog="0" zread="1" zwrite="0">
        <blend status="1" src="one" dest="one" />
        <sort status="0" count="2" />

        <texture name="s_lmap" rt="t_base" />
        <texture name="s_smap" rt="null" />
        <texture name="s_noise" rt="fx\\fx_noise" />

        <sampler name="smp_rtlinear" />
        <sampler name="smp_linear" />
        <sampler name="smp_smap" />
    </element_0>
</w>
```

### Tag **element_[id]**

* `ps` - pixel shader file
* `vs` - vertex shader file
* `gs` - geometry shader file

* `fog` - fog (0/1)
* `zread` - zbuffer read (0/1)
* `zwrite` - zbuffer write (0/1)

### Tag **blend**

* `status` - enable/disable (0/1)
* `src` - input type (`zero`/`one`/`srccolor`/`invsrccolor`/`srcalpha`/`destalpha`/`invdestalpha`/`destcolor`/`invdestcolor`/`srcalphasat`)
* `dest` - output type (`zero`/`one`/`srccolor`/`invsrccolor`/`srcalpha`/`destalpha`/`invdestalpha`/`destcolor`/`invdestcolor`/`srcalphasat`)

### Tag **sort**

* `status` - enable/disable (0/1)
* `count` - priority (number)

### Tag **flags**

* `dist` - enable/disable distort (0/1)
* `emissive` - enable/disable emissive (0/1)
* `wmark` - enable/disable wmark (0/1)

### Tag **atoc**

* `status` - enable/disable (0/1)

### Tag **aref**

* `status` - enable/disable (0/1)
* `count` - count (number)

### Tag **color**

* `r` - enable/disable r-channel write (0/1)
* `g` - enable/disable g-channel write (0/1)
* `b` - enable/disable b-channel write (0/1)
* `a` - enable/disable a-channel write (0/1)

### Tag **stencil**

* `status` - enable/disable (0/1)
* `cmp` - (`never`, `less`, `equal`, `lessequal`, `notequal`, `greater`, `greaterequal`, `always`)
* `mask` - count (number)
* `wmask` - count (number)
* `fail` - (`keep`, `zero`, `replace`, `incrsat`, `decrsat`, `invert`, `incr`, `decr`)
* `pass` - (`keep`, `zero`, `replace`, `incrsat`, `decrsat`, `invert`, `incr`, `decr`)
* `zfail` - (`keep`, `zero`, `replace`, `incrsat`, `decrsat`, `invert`, `incr`, `decr`)

#### Tag **ref**

* `value` - value (number)

### Tag **texture**

* `name` - texture name

### Tag **sampler**

* `name` - sampler name

### Tag **cull_mode**

* `mode` - (`none`, `cw`, `ccw`)
