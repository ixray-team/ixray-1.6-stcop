#pragma once
#include "../New/SoundShader.h"

// Helpers shared by the sound shader implementations
namespace XRay::Sound::ShaderUtils
{
	// Calls Body(Offset, Count, Factor) for every AutomationStep piece of Frames; Factor is the end of the piece in 0..1
	template <typename T>
	void ForEachPiece(u32 Frames, T&& Body)
	{
		for (u32 Offset = 0; Offset < Frames; Offset += XRay::Sound::Shader::AutomationStep)
		{
			const u32 Count = std::min(XRay::Sound::Shader::AutomationStep, Frames - Offset);
			Body(Offset, Count, (float)(Offset + Count) / (float)Frames);
		}
	}

	inline float LogLerp(float From, float To, float Factor)
	{
		return std::exp(lerp(std::log(From), std::log(To), Factor));
	}

	inline float DbToGain(float Db)
	{
		return std::pow(10.0f, Db / 20.0f);
	}

	// Filter slope in dB/oct to the number of 12 dB/oct biquad stages
	inline u32 SlopeStages(float Slope, u32 MaxStages)
	{
		return (u32)std::clamp((s32)std::lround(Slope / 12.0f), 1, (s32)MaxStages);
	}
}
