#pragma once
#include "../New/SoundShader.h"

// Plays the sound backwards from its end on top of the normal playback, the two meet in the middle.
// Voices only: reads the whole source (SoundShaderContext::Source); on a bus it passes the signal as is
XREFLECT(SoundShader, Name="reverse")
class SoundReverseShader : public ISoundShader
{
	XSOUND_GENERATED_BODY()

public:
	// Level of the normal playback
	XPROPERTY(SoundParam, Name="forward", Min=0, Max=1)
	float Forward = 1.0f;

	// Level of the reversed copy
	XPROPERTY(SoundParam, Name="backward", Min=0, Max=1)
	float Backward = 0.8f;

	// Milliseconds cut from the end before the reversed copy starts (skips a silent tail)
	XPROPERTY(SoundParam, Name="offset", Min=0, Max=5000)
	float OffsetMs = 0.0f;

	// 0 both centered, 1 the normal playback on the left, the reversed copy on the right
	XPROPERTY(SoundParam, Name="spread", Min=0, Max=1)
	float Spread = 0.0f;

	void Reset() override;
	void Process(float** Data, u32 Frames, const SoundShaderContext& Context) override;

private:
	// SND_BLOCKSIZE at the highest mixer pitch (4) and the interpolation frames
	static constexpr u32 MaxReadFrames = SND_BLOCKSIZE * 4 + 4;

	// Reads Count frames from First (may be outside the source: silence, or wrapped for a loop) into Scratch
	void ReadRange(const SoundShaderSource& Source, s64 First, u32 Count);

	xr_vector<float> Scratch[SND_CHANNEL_COUNT];
	// -1 until the first block
	float LastForward = -1.0f;
	float LastBackward = 0.0f;
	float LastSpread = 0.0f;
};
