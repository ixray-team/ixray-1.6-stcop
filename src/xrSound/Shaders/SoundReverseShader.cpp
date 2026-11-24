#include "SoundReverseShader.h"
#include "SoundShaderUtils.h"

using namespace XRay::Sound::ShaderUtils;

void SoundReverseShader::Reset()
{
	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		if (Scratch[Channel].size() != MaxReadFrames)
		{
			Scratch[Channel].assign(MaxReadFrames, 0.0f);
		}
	}
	LastForward = -1.0f;
}

void SoundReverseShader::ReadRange(const SoundShaderSource& Source, s64 First, u32 Count)
{
	const s64 Total = (s64)Source.TotalFrames;
	u32 Done = 0;
	while (Done < Count)
	{
		s64 Position = First + (s64)Done;
		if (Source.IsLooped)
		{
			Position = ((Position % Total) + Total) % Total;
		}

		u32 Chunk = Count - Done;
		u32 Read = 0;
		if (Position < 0)
		{
			Chunk = (u32)std::min<s64>(Chunk, -Position);
		}
		else if (Position < Total)
		{
			Chunk = (u32)std::min<s64>(Chunk, Total - Position);
			float* Dest[SND_CHANNEL_COUNT];
			for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
			{
				Dest[Channel] = Scratch[Channel].data() + Done;
			}
			Read = Source.Read(Source.User, (u32)Position, Dest, Chunk);
		}

		// Outside the source, or not decodable: silence
		for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			std::fill_n(Scratch[Channel].data() + Done + Read, Chunk - Read, 0.0f);
		}
		Done += Chunk;
	}
}

void SoundReverseShader::Process(float** Data, u32 Frames, const SoundShaderContext& Context)
{
	const SoundShaderSource* Source = Context.Source;
	if (Source == nullptr || Source->Read == nullptr || Source->TotalFrames < 2 || Frames == 0)
	{
		return;
	}

	if (Scratch[0].size() != MaxReadFrames)
	{
		Reset();
	}

	const double Rate = std::clamp((double)Source->Rate, 0.0, 4.0);
	const double OffsetFrames = (double)OffsetMs * 0.001 * (double)SND_SAMPLERATE;
	// Source frame the normal playback is at, mirrored around the end
	const double ForwardPos = (double)Source->Position + (double)Context.Offset * Rate;
	const double MirrorPos = (double)Source->TotalFrames - 1.0 - ForwardPos - OffsetFrames;

	// Output frame I reads MirrorPos - I * Rate, so the block covers [Last, MirrorPos]
	const double LastPos = MirrorPos - (double)(Frames - 1) * Rate;
	const s64 First = (s64)std::floor(LastPos);
	const u32 Count = (u32)std::min<s64>((s64)std::floor(MirrorPos) - First + 2, MaxReadFrames);
	ReadRange(*Source, First, Count);

	// Keep the next blocks (further down the source) cached
	if (Source->Prefetch != nullptr)
	{
		const s64 Ahead = First - (s64)(SND_BLOCKSIZE * 6);
		Source->Prefetch(Source->User, (u32)std::max<s64>(Ahead, 0));
	}

	const float BeginForward = LastForward >= 0.0f ? LastForward : Forward;
	const float BeginBackward = LastForward >= 0.0f ? LastBackward : Backward;
	const float BeginSpread = LastForward >= 0.0f ? LastSpread : Spread;
	LastForward = Forward;
	LastBackward = Backward;
	LastSpread = Spread;

	const float InvFrames = 1.0f / (float)Frames;
	for (u32 Frame = 0; Frame < Frames; Frame++)
	{
		const float Factor = (float)(Frame + 1) * InvFrames;
		const float ForwardGain = lerp(BeginForward, Forward, Factor);
		const float BackwardGain = lerp(BeginBackward, Backward, Factor);
		const float Side = lerp(BeginSpread, Spread, Factor);

		const double Position = MirrorPos - (double)Frame * Rate - (double)First;
		const u32 Index = (u32)std::clamp(Position, 0.0, (double)(Count - 2));
		const float Fraction = std::clamp((float)(Position - (double)Index), 0.0f, 1.0f);

		for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			// Channel 0 is left: the normal playback leans left, the reversed copy right
			const float ForwardSide = Channel == 0 ? 1.0f : 1.0f - Side;
			const float BackwardSide = Channel == 0 ? 1.0f - Side : 1.0f;
			const float* Samples = Scratch[Channel].data();
			const float Reversed = lerp(Samples[Index], Samples[Index + 1], Fraction);
			Data[Channel][Frame] = Data[Channel][Frame] * ForwardGain * ForwardSide + Reversed * BackwardGain * BackwardSide;
		}
	}
}
