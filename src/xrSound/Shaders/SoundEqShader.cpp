#include "SoundEqShader.h"
#include "SoundShaderUtils.h"

using namespace XRay::Sound::ShaderUtils;

namespace
{
	// Bands quieter than this are skipped
	constexpr float EqMinGainDb = 0.01f;
}

void SoundEqShader::Reset()
{
	for (Band& Filter : Bands)
	{
		Filter = {};
	}
	OutputGain = 0.0f;
}

void SoundEqShader::ProcessBand(Band& Filter, float** Data, u32 Frames, float Freq, float Gain, float Q)
{
	const bool IsEngaged = Filter.Freq != 0.0f;
	if (!IsEngaged && std::abs(Gain) < EqMinGainDb)
	{
		return;
	}

	// A new band starts flat at its frequency and fades its gain in
	const float BeginFreq = IsEngaged ? Filter.Freq : Freq;
	const float BeginGain = IsEngaged ? Filter.Gain : 0.0f;
	const float BeginQ = IsEngaged ? Filter.Q : Q;

	ForEachPiece(Frames, [&](u32 Offset, u32 Count, float Factor)
	{
		Filter.Coeffs = XRay::Sound::Biquad::Peak(LogLerp(BeginFreq, Freq, Factor), lerp(BeginQ, Q, Factor), lerp(BeginGain, Gain, Factor));
		for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			float* Samples = Data[Channel] + Offset;
			XRay::Sound::Biquad::State& State = Filter.States[Channel];
			for (u32 Frame = 0; Frame < Count; Frame++)
			{
				Samples[Frame] = XRay::Sound::Biquad::Process(Filter.Coeffs, State, Samples[Frame]);
			}
		}
	});

	Filter.Freq = Freq;
	Filter.Gain = Gain;
	Filter.Q = Q;

	if (std::abs(Gain) < EqMinGainDb)
	{
		// Flat again: a 0 dB bell passes the signal as is, the history can go
		Filter = {};
	}
}

void SoundEqShader::Process(float** Data, u32 Frames, const SoundShaderContext& Context)
{
	if (Frames == 0)
	{
		return;
	}

	ProcessBand(Bands[0], Data, Frames, Freq1, Gain1, Q1);
	ProcessBand(Bands[1], Data, Frames, Freq2, Gain2, Q2);
	ProcessBand(Bands[2], Data, Frames, Freq3, Gain3, Q3);

	const float TargetGain = DbToGain(Output);
	const float BeginGain = OutputGain != 0.0f ? OutputGain : TargetGain;
	OutputGain = TargetGain;
	if (BeginGain == 1.0f && TargetGain == 1.0f)
	{
		return;
	}

	const float Step = (TargetGain - BeginGain) / (float)Frames;
	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		float CurrentGain = BeginGain;
		for (u32 Frame = 0; Frame < Frames; Frame++)
		{
			CurrentGain += Step;
			Data[Channel][Frame] *= CurrentGain;
		}
	}
}
