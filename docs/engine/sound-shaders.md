# Sound shaders

DSP classes that process one `ref_sound` voice or a whole mixer bus. Code: [SoundShader.h](../../src/xrSound/New/SoundShader.h), [SoundShaders.h](../../src/xrSound/New/SoundShaders.h), generator [SoundGenerator.cpp](../../src/utils/xrReflectTool/SoundGenerator.cpp).

## Registration

Mark the class, nothing else is edited:

```cpp
XREFLECT(SoundShader, Name="my_shader")
class SoundMyShader : public ISoundShader
{
	XSOUND_GENERATED_BODY()

public:
	XPROPERTY(SoundParam, Name="gain", Min=0, Max=4)
	float Gain = 1.0f;

	void Process(float** Data, u32 Frames, const SoundShaderContext& Context) override;
};
```

- The class lives in an xrSound header. `ixray_reflect_sound` (in `src/xrSound/CMakeLists.txt`) runs `xrReflectTool --targets sound` before xrSound is compiled.
- The tool writes `SoundShaderType.gen.h` (public include of xrSound): `SoundShaderType::None`, one value per shader (PascalCase of `Name`, `low_pass` → `LowPass`), `Count`, and `XRay::Sound::Shader::Names`. Values are sorted by name and are runtime-only, do not save them.
- `xrSound.sound.gen.cpp` defines `GetClass()` of every shader and its `ClassInfo`: factory and parameters. A parameter is a `float` field; the initializer is the default, `Min` / `Max` clamp it.
- The generator rejects a shader under `#if`, a `Name` that maps to an existing enum value, a duplicate parameter, a non-float parameter, and `XREFLECT(SoundShader)` outside xrSound. A shader whose name is taken by a built-in bus effect (`compressor`, ...) still works on voices and buses but is not a bus effect; the log says so.
- `Reset()` drops DSP history (voice start), `OnParamsChanged()` runs after any parameter write, `GetTailFrames()` keeps a bus rendering after its input stops.

Every shader is also registered as a bus effect under its name, so bus configs and the audio editor use it like `compressor`:

```ini
[bus:radio_chatter]
effects = radio
radio.drive = 8
```

A sound config section takes the same keys; the sound plays through these shaders every time, before the runtime shaders of its ref_sound:

```ini
[device\pda\pda_news]
shaders = radio
radio.noise = 0.05
```

Only non-default parameters are written. Audio Debug → Inspector → sound → Shaders edits them, Save writes the section.

## Runtime

Changes are queued and applied in `Mixer::Update` under the render lock.

| Scope | C++ | Lua |
| --- | --- | --- |
| Voice | `ref_sound::add_shader(SoundShaderType::Radio)`, `remove_shader`, `clear_shaders`, `set_shader_param(type, "drive", 6)`; `Mixer::AddShader(slot, type)` | `snd:add_shader("radio")`, `remove_shader`, `clear_shaders`, `set_shader_param("radio", "drive", 6)` on `sound_object` |
| Bus | `Mixer::AddBusShader(bus, type)`, `RemoveBusShader`, `ClearBusShaders`, `SetBusShaderParam` | `sound_mixer.add_bus_shader("voice", "radio")`, `remove_bus_shader`, `clear_bus_shaders`, `set_bus_shader_param` |

Audio debug window (`UIEditorAudio.cpp`): Buses tab → bus details → Shaders lists the runtime shaders of the bus with parameter sliders (right click resets to default), Add shader and Clear. Inspector → sound → Instances shows the same per voice slot.

Console: `snd_bus_shader <bus> <shader>`, `snd_bus_shader <bus> <shader> off`, `snd_bus_shader <bus> <shader> <param> <value>`, `snd_bus_shader <bus> clear`.

Add and Remove are counted per owner: a shader added twice (a script and the car, say) stays until both remove it; Clear drops it regardless. A bus shader added with `spatial_only` (C++ `AddBusShader(bus, type, true)`, Lua `sound_mixer.add_bus_shader(bus, shader, true)`, removed with the same flag) is meant for 3D voices: 2D voices (UI, HUD, PDA) skip all runtime shaders of a bus that has one. Audio Debug marks such shaders "3D only" and shared ones "(shared)"; its X drops one owner.

Voice shaders run right after decoding and resampling, before spatialization; they stay on the slot across plays and `play_no_feedback` copies them. Bus shaders run before the bus effects (compressor, convolution, ...) and the bus fader. A chain holds up to `SND_SHADER_CHAIN_COUNT` (4) shaders, one per type.

## Shipped shaders

| Name | Parameters |
| --- | --- |
| `low_pass` | `cutoff` Hz (12 dB/oct) |
| `radio` | `low_cut`, `high_cut` Hz (24 dB/oct band-pass), `drive` (tanh saturation), `noise` (hiss), `crackle` (static bursts), `mono`, `gain`, `mix` |

## Skipping bus shaders

`ref_sound::set_ignore_bus_shaders(true)` (Lua: `snd.ignore_bus_shaders = true`) mixes the voice into its buses after their runtime shaders; bus effects from the config still apply. It takes effect on the next play, `clone` keeps it. A bus without runtime shaders mixes such voices as usual, so the flag costs nothing there.

The vehicle cabin uses this: in first person `CCar` puts a spatial-only `low_pass` on `effects`, `shooting`, `shooting_far`, `shooting_indoor` and the zone reverb buses, with the cutoff following the interior volume, and removes its owner on exit. Gearbox and door sounds ignore bus shaders, 2D sounds skip the spatial-only shader, music has its own bus.
