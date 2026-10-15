# XML Blends
> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.2

## Общее

Бленды шейдеров D3D11-рендера — это XML-файлы в папке шейдеров (`shaders\d3d11`). Бленд ищется по имени шейдера: `effects\water` — это `effects_water.xml`.

> [!WARNING]
> Lua-бленды (`.lua`) больше не поддерживаются. Движок их не загружает и пишет в лог `! Lua shader blend '<файл>' is not supported`. Переведите их в XML версии 2: соответствия приведены в таблице «Lua → XML» ниже.

Если для имени шейдера нет XML-бленда, используется C++-блендер с тем же именем, а затем `stub_default.xml`.

## Версия 2

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

### Корень **shader**

* `version` - `2`
* `r1_static` - `1`: при статическом освещении используется этот бленд, а не C++-блендер с тем же именем (0/1, по умолчанию 0)

### Элементы

Элемент — это один элемент шейдера. Его операции выполняются в порядке записи.

| Элемент | Назначение |
| --- | --- |
| `normal_hq` | Основной элемент, если у базовой текстуры есть detail-текстура |
| `normal` | Основной элемент в остальных случаях; также второй элемент |
| `l_point` | Точечный свет |
| `l_spot` | Прожектор |
| `l_special` | Специальный проход (emissive, маска прицела) |
| `normal_hud` | Элемент HUD; не используется при статическом освещении |
| `normal_ui` | Элемент UI |

Вместо именованных элементов можно заполнять слоты напрямую: `<element index="N" detail="0|1">` (N = 0..5, `detail` включает поиск detail-текстуры). Бленд использует либо именованные элементы, либо слоты.

* `option` - `NAME` или `!NAME`: элемент — альтернатива, которая используется, когда опция шейдера `NAME` задана (или не задана). Альтернативы одного имени пишутся подряд; используется первая подходящая.

```xml
<normal option="ALLOW_WBOIT_TRANSPARENCY">
    ...
</normal>
<normal>
    ...
</normal>
```

### Тег **static**

Содержит полный набор элементов, который используется при статическом освещении (`renderer_r4_static`) вместо элементов вне него.

### Текстуры

`value` у `texture` может содержать:

* `{base}` - первая текстура материала
* `{second}` - вторая текстура
* `{detail}` - третья текстура, иначе detail-текстура базовой, иначе `null`

`{base}_nm` — bump-текстура базовой.

### Операции

| Узел | Атрибуты |
| --- | --- |
| `pass` | `vs`, `ps`, `gs` (необязательно). Начинает новый проход |
| `pass_full` | `vs`, `ps`, `gs` (необязательно), `fog`, `zread`, `zwrite`. Проход с туманом и настройками глубины |
| `uber_deffer` | Deferred-проход модели (`uber_deffer` версии 1) |
| `sorting` | `priority` (число), `strict` (0/1) |
| `emissive` | `value` (0/1) |
| `scope` | `value` (0/1) |
| `scope_postprocess` | `value` (0/1) |
| `distort` | `value` (0/1) |
| `wmark` | `value` (0/1) |
| `fog` | `value` (0/1) |
| `zb` | `test` (0/1), `write` (0/1) |
| `blend` | `enable` (0/1), `src`, `dest` (blend) |
| `iblend` | `index` (render target), `enable` (0/1), `src`, `dest` (blend) |
| `aref` | `enable` (0/1), `value` (число) |
| `color_write` | `r`, `g`, `b`, `a` (0/1) |
| `texture` | `name`, `value` |
| `sampler` | `name` |
| `stencil` | `enable` (0/1), `func` (compare), `mask`, `write_mask` (число), `fail`, `pass`, `zfail` (stencil) |
| `stencil_ref` | `value` (число) |
| `zfunc` | `func` (compare) |
| `cull_mode` | `mode` (`none`, `cw`, `ccw`) |
| `alpha_to_coverage` | `value` (0/1) |
| `shader_option` | `name`, `value`. Добавляет define для следующих шейдеров |
| `clear_shader_options` | Убирает define, добавленные `shader_option` |

* blend: `zero`, `one`, `srccolor`, `invsrccolor`, `srcalpha`, `invsrcalpha`, `destalpha`, `invdestalpha`, `destcolor`, `invdestcolor`, `srcalphasat`
* compare: `never`, `less`, `equal`, `lessequal`, `notequal`, `greater`, `greaterequal`, `always`
* stencil: `keep`, `zero`, `replace`, `incrsat`, `decrsat`, `invert`, `incr`, `decr`

### Lua → XML

| Lua | XML |
| --- | --- |
| `function normal(shader, t_base, t_second, t_detail)` | `<normal>` (так же для остальных функций) |
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
| `if GetShaderOption("NAME") then ... else ... end` внутри функции | Два элемента: с `option="NAME"`, затем без него |
| `if GetShaderOption("USE_R1_STATIC_LIGHTING")` на уровне файла | Набор `<static>` |
| `r1_static = true` | `r1_static="1"` |

Имена текстур пишутся с одним `\`: `fx\fx_noise`.

## Версия 1

Старый формат без `version` по-прежнему поддерживается.

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
