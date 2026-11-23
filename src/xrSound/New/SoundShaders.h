#pragma once
#include "SoundShader.h"
#include "SoundMixer.h"

struct SoundConfigSection;

// Sound config sections: "shaders = radio, low_pass" and "<shader>.<param> = value" for non-default values
void Snd_ReadShaderPreset(const SoundConfigSection* Section, SoundShaderPreset& Preset);
void Snd_WriteShaderPreset(SoundConfigSection* Section, const SoundShaderPreset& Preset);

// Makes the chain match the preset, keeps the instances (and their DSP state) when the types did not change
void Snd_SyncShaderChain(SoundShaderChain& Chain, const SoundShaderPreset& Preset);

namespace XRay::Sound::Biquad
{
	struct Coefficients
	{
		float B0 = 1.0f;
		float B1 = 0.0f;
		float B2 = 0.0f;
		float A1 = 0.0f;
		float A2 = 0.0f;
	};

	struct State
	{
		float Z1 = 0.0f;
		float Z2 = 0.0f;
	};

	// RBJ cookbook filters at the mixer sample rate
	Coefficients LowPass(float CutoffHz, float Q);
	Coefficients HighPass(float CutoffHz, float Q);

	// Transposed direct form II
	inline float Process(const Coefficients& Coeffs, State& Filter, float Input)
	{
		const float Output = Coeffs.B0 * Input + Filter.Z1;
		Filter.Z1 = Coeffs.B1 * Input - Coeffs.A1 * Output + Filter.Z2;
		Filter.Z2 = Coeffs.B2 * Input - Coeffs.A2 * Output;
		return Output;
	}
}

// 12 dB/oct low-pass (two cascaded one-pole stages). Cutoff changes are ramped over a block;
// at XRay::Sound::LowPass::MaxCutoffHz it is bypassed
XREFLECT(SoundShader, Name="low_pass")
class SoundLowPassShader : public ISoundShader
{
	XSOUND_GENERATED_BODY()

public:
	// Min / Max match XRay::Sound::LowPass::MinCutoffHz / MaxCutoffHz
	XPROPERTY(SoundParam, Name="cutoff", Min=20, Max=20000)
	float CutoffHz = 1000.0f;

	void Reset() override;
	void Process(float** Data, u32 Frames, const SoundShaderContext& Context) override;

private:
	float State[SND_CHANNEL_COUNT][2] = {};
	// One-pole coefficient of the last block, 1 is fully open
	float Alpha = 1.0f;
};

// Walkie-talkie: narrow band-pass, saturation, hiss and crackle
XREFLECT(SoundShader, Name="radio")
class SoundRadioShader : public ISoundShader
{
	XSOUND_GENERATED_BODY()

public:
	// Band-pass edges
	XPROPERTY(SoundParam, Name="low_cut", Min=20, Max=4000)
	float LowCutHz = 400.0f;

	XPROPERTY(SoundParam, Name="high_cut", Min=500, Max=16000)
	float HighCutHz = 3200.0f;

	// Saturation amount, 1 is clean
	XPROPERTY(SoundParam, Name="drive", Min=1, Max=30)
	float Drive = 4.0f;

	// Constant hiss level
	XPROPERTY(SoundParam, Name="noise", Min=0, Max=0.5)
	float Noise = 0.015f;

	// Static bursts: density and loudness
	XPROPERTY(SoundParam, Name="crackle", Min=0, Max=1)
	float Crackle = 0.2f;

	// Stereo collapse, 1 is mono
	XPROPERTY(SoundParam, Name="mono", Min=0, Max=1)
	float Mono = 1.0f;

	XPROPERTY(SoundParam, Name="gain", Min=0, Max=4)
	float Gain = 0.8f;

	// Dry/wet
	XPROPERTY(SoundParam, Name="mix", Min=0, Max=1)
	float Mix = 1.0f;

	SoundRadioShader();

	void Reset() override;
	void OnParamsChanged() override;
	void Process(float** Data, u32 Frames, const SoundShaderContext& Context) override;

private:
	static constexpr u32 StageCount = 2;

	float Random();

	XRay::Sound::Biquad::Coefficients HighPassCoeffs;
	XRay::Sound::Biquad::Coefficients LowPassCoeffs;
	XRay::Sound::Biquad::State HighPassState[SND_CHANNEL_COUNT][StageCount];
	XRay::Sound::Biquad::State LowPassState[SND_CHANNEL_COUNT][StageCount];
	float DriveNorm = 1.0f;
	u32 Seed = 0x9E3779B9u;
	u32 CrackleFrames = 0;
	float CrackleLevel = 0.0f;
};
