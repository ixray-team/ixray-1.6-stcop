# Sound shaders

DSP classes that process one `ref_sound` voice or a whole mixer bus. Code: [SoundShader.h](../../src/xrSound/New/SoundShader.h) (base class, chain, automation), [src/xrSound/Shaders](../../src/xrSound/Shaders) (one `.h` / `.cpp` pair per shader, shared `SoundBiquad` filters and `SoundShaderUtils` helpers), [SoundShaders.h](../../src/xrSound/New/SoundShaders.h) (includes them all, config presets), generator [SoundGenerator.cpp](../../src/utils/xrReflectTool/SoundGenerator.cpp).

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

- The class lives in an xrSound header: add `src/xrSound/Shaders/Sound<Name>Shader.h` / `.cpp` (picked up by the source glob) and include the header from `SoundShaders.h`. `ixray_reflect_sound` (in `src/xrSound/CMakeLists.txt`) runs `xrReflectTool --targets sound` before xrSound is compiled.
- The tool writes `SoundShaderType.gen.h` (public include of xrSound): `SoundShaderType::None`, one value per shader (PascalCase of `Name`, `low_pass` → `LowPass`), `Count`, and `XRay::Sound::Shader::Names`. Values are sorted by name and are runtime-only, do not save them.
- `xrSound.sound.gen.cpp` defines `GetClass()` of every shader and its `ClassInfo`: factory and parameters. A parameter is a `float` field; the initializer is the default, `Min` / `Max` clamp it.
- The generator rejects a shader under `#if`, a `Name` that maps to an existing enum value, a duplicate parameter, a non-float parameter, and `XREFLECT(SoundShader)` outside xrSound. A shader whose name is taken by a built-in bus effect (`spatial`, `convolution`, ...) still works on voices and buses but is not a bus effect; the log says so.
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

Voice shaders run right after decoding and resampling, before spatialization. They may read the whole decoded source of the voice (`SoundShaderContext::Source`: random access read, prefetch hint, play position and rate), see the listener and voice positions and velocities (`Space`, 3D voices only) and scale the playback rate of the next block (`Pitch`, multiplied once per block); all three are nullptr on buses; they stay on the slot across plays and `play_no_feedback` copies them. Bus shaders run before the bus effects (compressor, convolution, ...) and the bus fader. A chain holds up to `SND_SHADER_CHAIN_COUNT` (4) shaders, one per type.

## Shipped shaders

| Name | Parameters |
| --- | --- |
| `low_pass` | `cutoff` Hz (20000 bypasses), `q` (0.5 soft, default; 0.7071 flat; higher resonates), `slope` dB/oct (12, 24, 36, 48) |
| `high_pass` | `cutoff` Hz (200; 20 bypasses), `q` (0.7071), `slope` dB/oct (12) |
| `compressor` | `attack` s (0.0001), `release` s (0.1), `threshold` dB (-20), `ratio` (2), `mix` (amount of the gain reduction, 1), `makeup` dB (0) |
| `doppler` | `amount` (velocity scale, times `snd_doppler`), `speed` m/s (343), `smooth` s (0.03), `min_pitch` (0.5), `max_pitch` (2) |
| `eq` | Three bell bands `freq1` / `gain1` / `q1` (250 Hz), `freq2` / `gain2` / `q2` (1 kHz), `freq3` / `gain3` / `q3` (4 kHz); gains in dB (-24..24), a band at 0 dB costs nothing. `output` dB |
| `distortion` | `drive` (1 clean .. 50), `mode` (0 soft tanh, 1 hard clip, 2 tube / asymmetric, 3 fold), `tone` Hz (low-pass after the shaper, 20000 is off), `gain`, `mix` |
| `delay` | `time` ms (1..2000), `feedback` (0..0.95), `damping` Hz (low-pass in the feedback loop), `ping_pong` (0..1), `mix` |
| `reverse` | `forward` (level of the normal playback), `backward` (level of the reversed copy, 0.8), `offset` ms (cut from the end before the reversed copy starts), `spread` (0 centered, 1 normal left / reversed right) |
| `radio` | `low_cut`, `high_cut` Hz (24 dB/oct band-pass), `drive` (tanh saturation), `noise` (hiss), `crackle` (static bursts), `mono`, `gain`, `mix` |

- The filters glide cutoff and Q over a block in log space, the EQ glides its bands, `distortion` its drive, so plain `set_*_shader_param` calls do not click.
- A filter cascade uses Butterworth stage Q values; `q` scales the last stage, so 24 dB/oct at 0.7071 is flat and higher values add one resonant peak.
- `delay` glides its time (about 60 ms), so automating `time` bends the pitch like tape. On a voice the echoes end with the sound; put the delay on a bus (`acquire_shader_bus("delay")`) to keep the tail, the bus renders until the echoes fall below -80 dB (30 s at most). One delay holds about 1 MB.
- `distortion` has no oversampling: high drive aliases, `tone` tames it.
- `compressor` is also the `compressor` bus effect: master (`snd_compression` drives its `mix`), the zone reverb buses and bus configs (`effects = compressor`, `compressor.mix = 0.5`) use this shader. The first five parameters kept their names, order and ranges, so old configs load unchanged; `makeup` is new. As a shader it goes on voices and buses and can be automated like the rest (ducking: ramp `threshold` or `mix`).
- `doppler` is on for every 3D voice by default, with the default parameters: the mixer gives such a voice its own instance unless the sound config or the voice chain has a `doppler` shader, which then replaces it (`shaders = doppler` with `doppler.amount = 0` turns it off for one sound, `snd_doppler 0` for all). The `spatial` and `spatialize` voice effects no longer shift the pitch and lost their `doppler` parameter (an old `spatial.doppler` key now logs "Unknown parameter"). The shift comes from the listener and voice velocities. The mixer derives them from the positions every game frame (exact frame time) and smooths them over about 80 ms, so objects that move every few frames (physics steps, scripted `set_position`) do not warble; a jump faster than 100 m/s counts as a teleport and resets the velocity to 0. `smooth` adds a little more on top. `snd_doppler` (1, physically correct) scales every doppler; it is saved in `user.ltx`, so an old saved 0.1 stays until it is changed. It only changes the playback rate of the voice, so the sound plays shorter or longer, like any pitch change. 2D voices get none; a `doppler` shader on a 2D voice glides back to 1, on a bus it does nothing.
- Voice playback rate (pitch, time factor, doppler) now glides over each block instead of jumping between blocks.
- `reverse` plays the sound from its end backwards on top of the normal playback, the two meet in the middle and end together (a ghostly "speaking both ways" voice). It follows pitch, doppler and the time factor and wraps on looped sounds. It reads the whole source, so it works on voices only (a sound config `shaders = reverse`, or `snd:add_shader("reverse")`); on a bus or in bus `effects` it passes the signal as is. Use `offset` when the file ends in silence, otherwise the reversed copy starts late. The first block of a voice may decode the end of the file on the audio thread; later blocks are prefetched.

## Automation

Any shader parameter of a voice or bus chain (runtime shaders, not config `effects`) can be ramped or modulated by an LFO. The mixer moves automated parameters every 32 frames (0.7 ms), so sweeps are smooth.

| Action | C++ | Lua voice (`sound_object`) | Lua bus (`sound_mixer`) |
| --- | --- | --- | --- |
| Ramp to a value | `ref_sound::ramp_shader_param(type, "cutoff", 400, 1.5f, Shader::Curve::Exponential)`, `Mixer::RampShaderParam` / `RampBusShaderParam` | `snd:ramp_shader_param("low_pass", "cutoff", 400, 1.5 [, "exp"])` | `ramp_bus_shader_param(bus, "low_pass", "cutoff", 400, 1.5 [, "exp"])` |
| LFO | `ref_sound::set_shader_lfo(type, "cutoff", 0.5f, 800, Shader::LfoShape::Sine)`, `Mixer::SetShaderLfo` / `SetBusShaderLfo` | `snd:set_shader_lfo("low_pass", "cutoff", 0.5, 800 [, "sine"])` | `set_bus_shader_lfo(bus, "low_pass", "cutoff", 0.5, 800 [, "sine"])` |
| Stop | `ref_sound::stop_shader_automation(type [, "cutoff"])`, `Mixer::StopShaderAutomation` / `StopBusShaderAutomation` | `snd:stop_shader_automation("low_pass" [, "cutoff"])` | `stop_bus_shader_automation(bus, "low_pass" [, "cutoff"])` |

- Ramp: from the current value (mid-ramp included) to the target over `seconds`; 0 seconds sets it at once. Curves: `linear` (default), `exp` (geometric, use it for Hz and ms; linear if a value is 0 or below), `smooth` (smoothstep).
- LFO: `rate` Hz, `depth` in parameter units around the value (`cutoff` 1000 with depth 800 swings 200..1800). Shapes: `sine` (default), `triangle`, `square`, `saw_up`, `saw_down`, `random` (new value every cycle). Rate or depth 0 stops it and restores the value under it. A ramp and an LFO on one parameter stack: the LFO swings around the ramp.
- `set_*_shader_param` drops the ramp of that parameter and keeps its LFO, which then swings around the new value. Stop keeps the current ramp value and drops the LFO. The result is clamped to the parameter range.
- Commands are queued like the other shader calls: the shader must be in the chain (add it first, in the same frame is fine) and a voice must be playing (`slot` is 0 otherwise and the call is dropped).
- Automation time runs with the rendered audio: a paused voice or a bus with no input and no tail holds its automation. Removing the shader drops it; `play_no_feedback` copies it with the chain.

Console: `snd_bus_shader <bus> <shader> <param> <value> <seconds> [curve]`, `snd_bus_shader <bus> <shader> <param> lfo <rate> <depth> [shape]`, `snd_bus_shader <bus> <shader> stop [param]`.

```lua
-- Wah on a radio voice, then close the filter over two seconds
snd:add_shader("low_pass")
snd:set_shader_param("low_pass", "q", 4)
snd:set_shader_lfo("low_pass", "cutoff", 2, 900, "triangle")
-- later
snd:stop_shader_automation("low_pass", "cutoff")
snd:ramp_shader_param("low_pass", "cutoff", 300, 2, "exp")

-- Voice played forwards and backwards at once
snd:add_shader("reverse")
snd:set_shader_param("reverse", "offset", 120)
snd:set_shader_param("reverse", "spread", 0.4)

-- Echo bus for a cave: the delay grows while the player walks in
local bus = sound_mixer.acquire_shader_bus("delay", "effects")
sound_mixer.set_bus_shader_param(bus, "delay", "mix", 0.25)
sound_mixer.ramp_bus_shader_param(bus, "delay", "time", 600, 3, "exp")
```

Audio Debug sliders show automated parameters moving; dragging one acts as `set_*_shader_param`.

## Skipping bus shaders

`ref_sound::set_ignore_bus_shaders(true)` (Lua: `snd.ignore_bus_shaders = true`) mixes the voice into its buses after their runtime shaders; bus effects from the config still apply. It takes effect on the next play, `clone` keeps it. A bus without runtime shaders mixes such voices as usual, so the flag costs nothing there.

The vehicle cabin uses this: in first person `CCar` puts a spatial-only `low_pass` on `effects`, `shooting`, `shooting_far`, `shooting_indoor` and the zone reverb buses, with the cutoff following the interior volume, and removes its owner on exit. Gearbox and door sounds ignore bus shaders, 2D sounds skip the spatial-only shader, music has its own bus.

## Dynamic buses

A dynamic bus is a runtime bus that is not in the config and is never saved. It renders like any bus: runtime shaders, then the fader, then its output. A voice or a whole bus routed into it goes through its shaders once, so ten radios on one `fx:radio` bus share one `radio` instance instead of ten.

| Action | C++ (`XRay::Sound::Mixer`) | Lua (`sound_mixer`) | Console |
| --- | --- | --- | --- |
| Create / add an owner | `AcquireDynamicBus(name, output = 0)` | `acquire_bus(name [, output])` | `snd_bus_dynamic <name> [output]` |
| Bus with one shader | `AcquireShaderBus(SoundShaderType::Radio, output = 0)` → `fx:radio` | `acquire_shader_bus("radio" [, output])` | |
| Drop an owner | `ReleaseDynamicBus(bus)` | `release_bus(name)` | `snd_bus_dynamic <name> off` |
| Route a whole bus | `RouteBus(bus, target)`, target 0 restores | `route_bus(bus, target)`, `unroute_bus(bus)` | `snd_bus_route <bus> <target \| off>` |
| Route a voice | `ref_sound::set_route_bus("fx:radio")`, `Mixer::RouteVoice(slot, bus)` | `snd.route_bus = "fx:radio"` (`""` restores) | |

- Acquire returns the bus index (Lua: its name, `""` on failure) and counts owners; the last Release frees the bus and drops every voice and bus route into it. A config bus of the same name is returned as is and is never freed. Output 0 is master; pass `effects` to keep the player's effects fader on it.
- `AcquireShaderBus` adds the shader once, on creation; tune it with `SetBusShaderParam(bus, ...)` / `set_bus_shader_param("fx:radio", ...)`. More shaders go in with `AddBusShader` as on any bus.
- `RouteBus` overrides the config `output` at runtime and is not written to the config. A route that makes a loop is dropped with a log line. Master cannot be routed. One route per bus, the last call wins.
- A routed voice keeps its own bus for voice effects (spatialization), zone and far / indoor sends; only its main mix goes to the route bus. The route of a `ref_sound` is kept by name across plays, `clone` and `play_no_feedback` copies; `Mixer::RouteVoice` on a slot lasts until its next play.
- Release and both kinds of routes are queued and applied in `Mixer::Update`; Acquire is immediate.

Audio Debug → Buses marks such buses "(dynamic)" and "(routed)" in the tree, which follows the effective output. Bus details → Route sets or clears the runtime route of any bus; the config fields of a dynamic bus are read-only.

Debug → Audio Bus Graph (or Audio Editor → Buses → Open graph) is a separate window that lists the buses in processing order: every bus comes right after its inputs and before its output, master is last, the indent shows the depth. A row shows the bus tags, playing voices (own and routed in), where the signal goes next (orange with the config output for a runtime route), far / indoor sends, shaders or effects, a compact analyzer and L / R peak meters in dB; the selected bus gets a large analyzer above its details, with a cursor readout (frequency, note, level per channel).

The analyzer draws gradient filled curves on a log frequency grid (20 Hz – 20 kHz) with a peak hold line. Its settings: channels (L / R overlaid, Mid, Left, Right), resolution (FFT of 1024, 2048 or 4096 frames), release speed, range (60, 90, 120 dB), tilt around 1 kHz (0 – 6 dB/oct, 4.5 by default, so music with a pink spectrum reads flat) and peak hold. Spectra come from `Mixer::GetBusSpectrum(bus, channel, bands_db, count, min_hz, max_hz, fft_size)`: pffft, Hann window, dB of a full scale sine, after the bus effects and fader; channel `SND_SCOPE_MID` is (L + R) / 2. The mixer keeps the last `SND_SCOPE_FRAMES` (4096) frames per bus in a ring. The mixer captures a bus only while someone reads it, and stops about a second after the last read.

Every dynamic bus is recorded when it is created (`Mixer::GetDynamicBuses`), so the graph shows it even if it lived for less than a frame. A released one stays as a faded row before its last output, with its last shaders and how many times it was created, and becomes live again when acquired. Forget released (`Mixer::ForgetDynamicBuses`) drops them; "Released buses" hides them.

```lua
local bus = sound_mixer.acquire_shader_bus("radio", "effects")
sound_mixer.set_bus_shader_param(bus, "radio", "drive", 6)
snd.route_bus = bus                      -- one sound
sound_mixer.route_bus("music", bus)      -- a whole bus
-- later
sound_mixer.unroute_bus("music")
sound_mixer.release_bus(bus)
```
