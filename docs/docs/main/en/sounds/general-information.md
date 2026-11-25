# New audio system (OpenAL replacement)
> [!IMPORTANT]
> **Status**: WIP <br>
> **Minimal version**: 2.0

## Overview

The system replaces the legacy **OpenAL / OpenAL Soft** audio stack with a modern pipeline: better spatialization, improved mixing, and richer processing without third-party sound libs. You can author custom filters and precise audio manipulations.

HRTF and reverb are built into the engine; no external audio library is needed on any platform. The HRTF uses the head-related impulse responses of the [SADIE](https://www.york.ac.uk/sadie-project/) database (KU100 dummy head), the same data Resonance Audio shipped.

![image](https://github.com/user-attachments/assets/8350c915-e1ec-4ea8-9485-a4dfc71c0543)

Implemented:
* HRTF
* Algorithmic reverb (no geometry)

## Key changes

---

## 1. Dynamic Range Compression

**Before:**  
- No built-in dynamic range control  
- Rendered in u8 (0–256) [1 byte]

**Now:**  
- Optional **dynamic compression** on the final mix  
- Processing and rendering in **float** (0.0–10.0) [4 bytes]  
- User-adjustable compression strength

**Benefits:**
- Better audibility of quiet sounds  
- Protects from clipping on loud peaks  
- More even loudness balance

---

## 2. Audio quality and content requirements

**Before:**  
- OpenAL Soft smoothed many artifacts automatically

**Now:**  
- **Manual preparation** of audio is required  
- IX-Ray SDK tools do not fix defects

**`.wav` requirements:**
- Sample rate: **44100 Hz**  
- Clean start/end (no noise)  
- Fade-in / fade-out recommended  
- Prefer pre-processing (Reaper, Audition, RX)

---

## 3. Sample rate

- Audio engine runs at a **fixed 44100 Hz**  
- Other sample rates are unsupported

---

## 4. Concurrent sound limits

**Before:**  
- 256 slot limit; overflow dropped sounds

**Now:**  
- Up to **4096 sounds** simultaneously  
- Enables denser, richer soundscapes

---

## 5. Reverb system

### EFX (legacy)
- **Type:** Preset-Based Auxiliary Reverb  
- **Model:** Standard OpenAL presets (room, tunnel, hall)  
- **Limitations:**  
  - Limited physical accuracy  
  - Minimal level adaptation  
  - Reverb applied even outside zone bounds

---

### Built-in room reverb (new)

- **Type:**  
  **Parametric Algorithmic Reverb**

- **Approach:**
  - Every reverb zone gets its own bus with the `reverb` effect; its parameters come from the zone preset (decay time, HF decay ratio, air absorption, size, diffusion, reflections, room HF)
  - The tail is a 16-line feedback delay network with separate decay times below and above about 1.4 kHz (`decay` and `decay * decay_hf * air_hf`), limited to about 11 kHz; the decay time holds in every octave up to 11 kHz
  - A 16-channel diffuser spreads every sound over the lines, and the line lengths drift slowly (about 1.5 cents of pitch), so the tail stays dense and smooth instead of ringing like metal on shots and other short sounds
  - Early reflections: one low-passed echo whose delay grows with the zone size (listener in the middle of a cube room)
  - Decay changes glide over about one second, so walking between zones does not click
  - The tail level over decay time matches the previous Resonance Audio reverb, so existing zone presets keep their loudness; a decay below about 0.16 s gives no tail, as before

- **Benefits:**
  - Context-aware sound  
  - Easy integration, minimal tuning  
  - Sounds outside the reverb zone play dry (no sampling)
  - Same sound on Windows, Linux and macOS
---

### Reverb comparison

| Parameter            | EFX (OpenAL)                      | Built-in room reverb                                  |
|----------------------|-----------------------------------|-------------------------------------------------------|
| Type                 | Preset algorithmic                | Parametric algorithmic                                |
| Early reflections    | Presets                           | Room-size echo                                        |
| Late reverb          | Algorithmic                       | Modulated 16-line feedback delay network, two-band decay |
| Adaptiveness         | Low                               | Per-zone parameters, smooth zone transitions          |
| Tuning               | Manual                            | Zone presets                                          |

---

## 6. HRTF

- The `spatialize` voice effect (default for the `effects` and `music` buses) renders 3D sounds binaurally for headphones
- `snd_hrtf 1` turns it on, `snd_hrtf 0` falls back to plain stereo panning (`spatial`)
- Direction changes are crossfaded every audio block, so moving sources do not click
- Up to 512 voices use HRTF at once; further voices are panned
- Parameters: `rolloff` (distance rolloff with HRTF), `fallback_rolloff` and `back_attenuation` (used by the panned fallback)
- Sounds in front of the listener are rendered in front: the previous Resonance Audio build mirrored front and back

---
## 7. Sound Debug Info
![image](https://github.com/user-attachments/assets/462bf94d-03f5-45e3-a1d0-1830953afff5)

Tools to visualize scene audio:
* Sound slots
* Active sound files
* Reverb zones on the level

## Tips for modders

### 1. Audio prep
- Use clean `.wav` 44100 Hz
- Apply fade-in/fade-out to all files
- Remove clicks, chopped tails, noise
- Verify in-game or via `Play In Editor` in **IX-Ray SDK**

### 2. Dynamic compression awareness
- Check loudness at different DRC levels
- Avoid cranking everything to max
- Balance sources manually

### 3. Working with reverb
- Assign correct surface materials (good practice in X-Ray SDK overall)
- Configure base presets in `Shader Editor`
- Pick presets matching expected zone intensity (test with `Play In Editor`)

---

## Technical notes

- **Vorbis** is used for storage.  
- Conversion is performed via IX-Ray SDK tools.  
