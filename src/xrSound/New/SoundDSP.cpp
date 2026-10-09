/**************************************************************************************
* Copyright (C) 2025 Anton Kovalev (vertver)
* New Sound Engine
***************************************************************************************
* Source code is licensed under the following terms:
*
* 1. IX-Ray Team License
*    Non-exclusive, royalty-free, perpetual license is hereby granted to:
*      - ForserX   (https://github.com/ForserX)
*      - Drombeys  (https://github.com/Drombeys)
*      - v2v3v4    (https://github.com/v2v3v4)
*
*    Permitted rights:
*      - Copy, modify, merge, publish and distribute this Software
*        and its documentation.
*
* 2. Public Access License
*    Non-exclusive, "access-view-study" rights granted to everyone else.
*
*    Permitted rights:
*      - Private copying is allowed, provided that no distribution occurs.
*      - Public cloning (i.e. "forking") is allowed, but any source code
*        modification or binary redistribution is prohibited.
*
* Usage of this Software beyond the rights granted above is strictly prohibited.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "SoundDSP.h"
#include <Sound.h>

void DSP_ResampleBuffer(float** Input, float** Output, float History[SND_CHANNEL_COUNT][SND_RESAMPLING_QUALITY + 1], u32 InputFrames, u32 OutputFrames)
{
	float ratio = (float)InputFrames / (float)OutputFrames;

	for (size_t i = 0; i < SND_CHANNEL_COUNT; i++)
	{
		History[i][0] = fmodf(History[i][0], 1.0f);
	}

	for (size_t k = 0; k < SND_CHANNEL_COUNT; k++)
	{
		for (u32 i = 0; i < OutputFrames; ++i)
		{
			float& phase = History[k][0];

			// InputFrames is the last valid index: the caller decodes InputFrames+1 frames
			u32 idx0 = std::min((u32)phase, InputFrames);
			u32 idx1 = std::min(idx0 + 1, InputFrames);

			float delta = phase - (float)idx0;
			float sample0 = Input[k][idx0];
			float sample1 = Input[k][idx1];

			float sample = lerp(sample0, sample1, delta);
			Output[k][i] += sample;
			phase += ratio;
		}
	}
}

void DSP_MixBuffer(float** MixBuffer, float** Data, float BeginFactor, float EndFactor, u32 Frames)
{
	for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		for (size_t Key = 0; Key < Frames; Key++)
		{
			float Factor = lerp(BeginFactor, EndFactor, (float)(Key) / (float)(Frames - 1));
			float Sample = Data[Channel][Key];
			MixBuffer[Channel][Key] += Sample * Factor;
		}
	}
}

void DSP_MixBufferPanning(float** MixBuffer, float** Data, float BeginFactor, float EndFactor, float Left, float Right, u32 Frames)
{
	float Factors[SND_CHANNEL_COUNT] = {Left, Right};
	for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		for (size_t Key = 0; Key < Frames; Key++)
		{
			float Factor = lerp(BeginFactor, EndFactor, (float)(Key) / (float)(Frames - 1)) * Factors[Channel];
			float Sample = Data[Channel][Key];
			MixBuffer[Channel][Key] += Sample * Factor;
		}
	}
}

void DSP_Compressor(float AttackMs, float ReleaseMs, float ThresholdDb, float Ratio, float** Data, float Drywet, u32 Frames, float Envelope[SND_CHANNEL_COUNT])
{
	float LinAttack = AttackMs == 0.0f ? 0.0 : (f32)exp(-1.0 / ((float)SND_SAMPLERATE * AttackMs));
	float LinRelease = ReleaseMs == 0.0f ? 0.0 : (f32)exp(-1.0 / ((float)SND_SAMPLERATE * ReleaseMs));
	Ratio = (1.0f - 1.0f / (Ratio));

	for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++)
	{
		for (size_t k = 0; k < Frames; k++)
		{
			float& Sample = Data[ch][k];

			float temp = lin2dB(std::abs(Sample) + FLT_EPSILON);
			float OverDB = temp - ThresholdDb;
			if (OverDB < 0.0f)
			{
				OverDB = 0.0f;
			}
			OverDB += FLT_EPSILON;

			float theta = (OverDB > Envelope[ch]) ? LinAttack : LinRelease;
			Envelope[ch] = OverDB + theta * (Envelope[ch] - OverDB);

			float PVart = Envelope[ch] - FLT_EPSILON;
			if (PVart > 0.0f)
			{
				PVart -= Envelope[ch] * Envelope[ch] * 0.001f; // opto pseudo curve
			}
			float Gain = 0.0f - PVart * Ratio;

			float Comp = lerp(1.0f, dB2lin(Gain), Drywet);
			Sample *= Comp;
		}
	}
}