#pragma once
#include "SoundShader.h"
#include "SoundMixer.h"

// Sound shader implementations, one pair of files per shader in src/xrSound/Shaders
#include "../Shaders/SoundBiquad.h"
#include "../Shaders/SoundCompressorShader.h"
#include "../Shaders/SoundDelayShader.h"
#include "../Shaders/SoundDistortionShader.h"
#include "../Shaders/SoundDopplerShader.h"
#include "../Shaders/SoundEqShader.h"
#include "../Shaders/SoundHighPassShader.h"
#include "../Shaders/SoundLowPassShader.h"
#include "../Shaders/SoundRadioShader.h"
#include "../Shaders/SoundReverseShader.h"

struct SoundConfigSection;

// Sound config sections: "shaders = radio, low_pass" and "<shader>.<param> = value" for non-default values
void Snd_ReadShaderPreset(const SoundConfigSection* Section, SoundShaderPreset& Preset);
void Snd_WriteShaderPreset(SoundConfigSection* Section, const SoundShaderPreset& Preset);

// Makes the chain match the preset, keeps the instances (and their DSP state) when the types did not change
void Snd_SyncShaderChain(SoundShaderChain& Chain, const SoundShaderPreset& Preset);
