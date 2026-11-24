#include "SoundDelayShader.h"
#include "SoundShaderUtils.h"
#include "../New/SoundMixer.h"

using namespace XRay::Sound::ShaderUtils;

namespace
{
	// Delay time glide, about 60 ms
	constexpr float DelayGlide = 1.0f / (0.06f * (float)SND_SAMPLERATE);
	// Echo level at which the tail ends
	constexpr float DelayTailLevel = 1e-4f;
	constexpr u32 DelayMaxTailFrames = SND_SAMPLERATE * 30;

	float OnePoleAlpha(float CutoffHz)
	{
		return 1.0f - std::exp(-2.0f * PI * CutoffHz / (float)SND_SAMPLERATE);
	}
}

void SoundDelayShader::Reset()
{
	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		if (Buffer[Channel].size() != BufferFrames)
		{
			Buffer[Channel].assign(BufferFrames, 0.0f);
		}
		else if (!IsClear)
		{
			std::fill(Buffer[Channel].begin(), Buffer[Channel].end(), 0.0f);
		}
		DampState[Channel] = 0.0f;
	}

	IsClear = true;
	WritePos = 0;
	CurrentDelay = 0.0f;
}

u32 SoundDelayShader::GetTailFrames() const
{
	const float DelayFrames = TimeMs * 0.001f * (float)SND_SAMPLERATE;
	float Repeats = 1.0f;
	if (Feedback > DelayTailLevel)
	{
		Repeats += std::ceil(std::log(DelayTailLevel) / std::log(Feedback));
	}
	return std::min((u32)(DelayFrames * Repeats) + SND_BLOCKSIZE, DelayMaxTailFrames);
}

void SoundDelayShader::Process(float** Data, u32 Frames, const SoundShaderContext& Context)
{
	if (Buffer[0].size() != BufferFrames)
	{
		Reset();
	}

	if (Frames == 0 || (Mix <= 0.0f && IsClear))
	{
		return;
	}

	static_assert(SND_CHANNEL_COUNT == 2, "SoundDelayShader is stereo");
	IsClear = false;

	const float TargetDelay = std::clamp(TimeMs * 0.001f * (float)SND_SAMPLERATE, 1.0f, (float)(BufferFrames - 2));
	if (CurrentDelay == 0.0f)
	{
		CurrentDelay = TargetDelay;
	}

	const float Damp = DampingHz >= XRay::Sound::LowPass::MaxCutoffHz ? 1.0f : OnePoleAlpha(DampingHz);
	const float Cross = Feedback * PingPong;
	const float Straight = Feedback * (1.0f - PingPong);

	float* Left = Data[0];
	float* Right = Data[1];
	float* BufferLeft = Buffer[0].data();
	float* BufferRight = Buffer[1].data();
	float DampLeft = DampState[0];
	float DampRight = DampState[1];
	float Delay = CurrentDelay;
	u32 Pos = WritePos;

	for (u32 Frame = 0; Frame < Frames; Frame++)
	{
		Delay += (TargetDelay - Delay) * DelayGlide;

		// Linear interpolation between the two nearest frames, Delay >= 1 so the current frame is never read
		const float Whole = std::floor(Delay);
		const float Fraction = Delay - Whole;
		const u32 Idx0 = (Pos + BufferFrames - (u32)Whole) & BufferMask;
		const u32 Idx1 = (Idx0 + BufferMask) & BufferMask;
		const float EchoLeft = lerp(BufferLeft[Idx0], BufferLeft[Idx1], Fraction);
		const float EchoRight = lerp(BufferRight[Idx0], BufferRight[Idx1], Fraction);

		DampLeft += Damp * (EchoLeft - DampLeft);
		DampRight += Damp * (EchoRight - DampRight);

		const float DryLeft = Left[Frame];
		const float DryRight = Right[Frame];

		// Ping-pong feeds the mono input into the left line only, the echoes cross over
		const float Mono = (DryLeft + DryRight) * 0.5f;
		BufferLeft[Pos] = lerp(DryLeft, Mono, PingPong) + DampLeft * Straight + DampRight * Cross;
		BufferRight[Pos] = DryRight * (1.0f - PingPong) + DampRight * Straight + DampLeft * Cross;

		Left[Frame] = lerp(DryLeft, EchoLeft, Mix);
		Right[Frame] = lerp(DryRight, EchoRight, Mix);

		Pos = (Pos + 1) & BufferMask;
	}

	DampState[0] = std::abs(DampLeft) < 1e-20f ? 0.0f : DampLeft;
	DampState[1] = std::abs(DampRight) < 1e-20f ? 0.0f : DampRight;
	CurrentDelay = Delay;
	WritePos = Pos;
}
