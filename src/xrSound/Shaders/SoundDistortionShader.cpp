#include "SoundDistortionShader.h"
#include "SoundShaderUtils.h"

using namespace XRay::Sound::ShaderUtils;

namespace
{
	enum class DistortionMode : u8
	{
		Soft,
		Hard,
		Tube,
		Fold
	};

	// Bias of the tube curve: even harmonics
	constexpr float TubeBias = 0.3f;
	// One-pole DC blocker after the shaper, about 10 Hz
	constexpr float DcBlockerPole = 0.9986f;

	float Shape(DistortionMode Mode, float Value)
	{
		switch (Mode)
		{
		case DistortionMode::Hard:
			return std::clamp(Value, -1.0f, 1.0f);
		case DistortionMode::Tube:
			return std::tanh(Value + TubeBias) - std::tanh(TubeBias);
		case DistortionMode::Fold:
			return std::sin(Value);
		default:
			return std::tanh(Value);
		}
	}

	// Keeps a full-scale input near full scale after the shaper
	float ShapeNorm(DistortionMode Mode, float Drive)
	{
		switch (Mode)
		{
		case DistortionMode::Hard:
			return 1.0f;
		case DistortionMode::Tube:
			return 1.0f / std::max(std::tanh(Drive + TubeBias) - std::tanh(TubeBias), 0.1f);
		case DistortionMode::Fold:
			return 1.0f;
		default:
			return 1.0f / std::tanh(Drive);
		}
	}
}

void SoundDistortionShader::Reset()
{
	CurrentDrive = 0.0f;
	memset(DcIn, 0, sizeof(DcIn));
	memset(DcOut, 0, sizeof(DcOut));
	Tone.Reset();
}

void SoundDistortionShader::Process(float** Data, u32 Frames, const SoundShaderContext& Context)
{
	if (Frames == 0)
	{
		return;
	}

	const DistortionMode ShapeMode = (DistortionMode)std::clamp((s32)std::lround(Mode), 0, 3);
	const float BeginDrive = CurrentDrive != 0.0f ? CurrentDrive : Drive;
	CurrentDrive = Drive;

	if (Mix <= 0.0f)
	{
		Tone.Reset();
		return;
	}

	ForEachPiece(Frames, [&](u32 Offset, u32 Count, float Factor)
	{
		const float PieceDrive = lerp(BeginDrive, Drive, Factor);
		const float Norm = ShapeNorm(ShapeMode, PieceDrive) * Gain;
		float* WetData[SND_CHANNEL_COUNT];

		for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			const float* Samples = Data[Channel] + Offset;
			float* Out = Wet[Channel];
			float In = DcIn[Channel];
			float Prev = DcOut[Channel];
			for (u32 Frame = 0; Frame < Count; Frame++)
			{
				const float Shaped = Shape(ShapeMode, Samples[Frame] * PieceDrive);
				Prev = Shaped - In + DcBlockerPole * Prev;
				In = Shaped;
				Out[Frame] = Prev * Norm;
			}
			DcIn[Channel] = In;
			DcOut[Channel] = std::abs(Prev) < 1e-20f ? 0.0f : Prev;
			WetData[Channel] = Out;
		}

		Tone.Process(WetData, Count, ToneHz, 0.7071f, 1);

		for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			float* Samples = Data[Channel] + Offset;
			for (u32 Frame = 0; Frame < Count; Frame++)
			{
				Samples[Frame] = lerp(Samples[Frame], Wet[Channel][Frame], Mix);
			}
		}
	});
}
