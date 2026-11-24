#include "SoundDopplerShader.h"
#include "SoundShaderUtils.h"
#include "../Sound.h"

using namespace XRay::Sound::ShaderUtils;

void SoundDopplerShader::Reset()
{
	Current = 0.0f;
}

void SoundDopplerShader::Process(float** Data, u32 Frames, const SoundShaderContext& Context)
{
	// Once per block, the rate is per voice
	if (Context.Pitch == nullptr || Context.Offset != 0)
	{
		return;
	}

	float Target = 1.0f;
	if (const SoundShaderSpace* Space = Context.Space)
	{
		Fvector ToListener;
		ToListener.sub(Space->ListenerPosition, Space->VoicePosition);
		const float Distance = ToListener.magnitude();
		if (Distance > EPS_S)
		{
			ToListener.mul(1.0f / Distance);

			// Listener moving away from the voice lowers the pitch, the voice moving to the listener raises it
			const float Scale = psSoundDoppler * Amount;
			const float ListenerAway = ToListener.dotproduct(Space->ListenerVelocity) * Scale;
			const float VoiceToward = ToListener.dotproduct(Space->VoiceVelocity) * Scale;
			const float Speed = SpeedOfSound;
			Target = (Speed - ListenerAway) / std::max(Speed - VoiceToward, Speed * 0.05f);
		}
	}

	Target = std::clamp(Target, MinPitch, std::max(MinPitch, MaxPitch));
	if (Current == 0.0f)
	{
		Current = Target;
	}
	else
	{
		const float BlockSeconds = (float)SND_BLOCKSIZE / (float)SND_SAMPLERATE;
		const float Alpha = Smooth > 0.0f ? 1.0f - std::exp(-BlockSeconds / Smooth) : 1.0f;
		Current += (Target - Current) * Alpha;
	}

	*Context.Pitch *= Current;
}
