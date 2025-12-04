# New audio system (OpenAL replacement)
> [!IMPORTANT]
> **Status**: WIP <br>
> **Minimal version**: 2.0

## Overview

The system replaces the legacy **OpenAL / OpenAL Soft** audio stack with a modern pipeline: better spatialization, improved mixing, and richer processing without third-party sound libs. You can author custom filters and precise audio manipulations.

Reverb is powered by [Resonance Audio](https://github.com/resonance-audio/resonance-audio).

### Dev: SteamAudio (Phonon)
An initial SteamAudio (Phonon) implementation is present. To try it instead of Resonance Audio, remove `#define DISABLE_STEAM_AUDIO`.

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

### Resonance Audio (new)

- **Type:**  
  **Parametric Algorithmic Reverb**

- **Approach:**
  - Computes reverb params (RT60, decay, frequency response) from:
    - area size  
    - materials  
    - source/listener positions  
  - Tails are generated **algorithmically** (no convolution, no FDN)  
  - Early reflections are approximated with a simplified room model

- **Benefits:**
  - Context-aware sound  
  - Auto-adapts to environment  
  - Easy integration, minimal tuning  
  - Sounds outside the reverb zone play dry (no sampling)
---

### Reverb comparison

| Parameter            | EFX (OpenAL)                      | Resonance Audio                                       |
|----------------------|-----------------------------------|-------------------------------------------------------|
| Type                 | Preset algorithmic                | Parametric algorithmic                                |
| Early reflections    | Presets                           | Geometry-based modeling                               |
| Late reverb          | Algorithmic                       | Algorithmic, auto-tuned                               |
| Realism              | Medium                            | High (environment-adaptive)                           |
| Adaptiveness         | Low                               | High                                                  |
| Tuning               | Manual                            | Nearly automatic                                      |

---
## 6. Sound Debug Info
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

### 3. Working with reverb (Resonance Audio)
- Assign correct surface materials (good practice in X-Ray SDK overall)
- Configure base presets in `Shader Editor`
- Pick presets matching expected zone intensity (test with `Play In Editor`)

---

## Technical notes

- **Vorbis** is used for storage.  
- Conversion is performed via IX-Ray SDK tools.  
