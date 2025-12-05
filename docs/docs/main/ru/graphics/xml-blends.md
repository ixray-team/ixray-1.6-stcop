# XML Blends
> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.2

## Общее

**XML Blends** - альтернативная система компиляции шейдеров, представляющая дальнейшую замену Lua компиляции шейдеров. 
Ниже предоставлен пример эквивалентный между XML и Lua.

* Lua:

```lua
function normal(shader, t_base, t_second, t_detail)
    shader:begin("accum_volumetric", "accum_volumetric")
	
        :fog(false)
        :zb(true, false)
        :blend(true, blend.one, blend.one)
        :sorting(2, false)

    shader:dx10texture("s_lmap", t_base)
    shader:dx10texture("s_smap", "null")
    shader:dx10texture("s_noise", "fx\\fx_noise")

    shader:dx10sampler("smp_rtlinear")
    shader:dx10sampler("smp_linear")
    shader:dx10sampler("smp_smap")
end
```

* XML:

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

## Синтаксис

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
