#pragma once
#include "../New/SoundShader.h"

// Stereo echo with feedback damping and ping-pong. Delay time changes glide (tape-like pitch bend).
// On a voice the echoes stop with the sound; a bus keeps rendering them (GetTailFrames)
XREFLECT(SoundShader, Name="delay")
class SoundDelayShader : public ISoundShader
{
	XSOUND_GENERATED_BODY()

public:
	// Milliseconds
	XPROPERTY(SoundParam, Name="time", Min=1, Max=2000)
	float TimeMs = 350.0f;

	XPROPERTY(SoundParam, Name="feedback", Min=0, Max=0.95)
	float Feedback = 0.4f;

	// Low-pass in the feedback loop, 20000 is off
	XPROPERTY(SoundParam, Name="damping", Min=500, Max=20000)
	float DampingHz = 6000.0f;

	// 0 plain stereo echo, 1 echoes bounce between left and right
	XPROPERTY(SoundParam, Name="ping_pong", Min=0, Max=1)
	float PingPong = 0.0f;

	// Dry/wet
	XPROPERTY(SoundParam, Name="mix", Min=0, Max=1)
	float Mix = 0.35f;

	void Reset() override;
	void Process(float** Data, u32 Frames, const SoundShaderContext& Context) override;
	u32 GetTailFrames() const override;

private:
	// Power of two above the longest delay
	static constexpr u32 BufferFrames = 1u << 17;
	static constexpr u32 BufferMask = BufferFrames - 1;

	xr_vector<float> Buffer[SND_CHANNEL_COUNT];
	u32 WritePos = 0;
	// Frames, 0 until the first block
	float CurrentDelay = 0.0f;
	float DampState[SND_CHANNEL_COUNT] = {};
	bool IsClear = true;
};
