#include "SoundHrtf.h"
#include "SoundEffect.h"
#include "SoundHrtfData.h"
#include <pffft.h>

namespace
{
	namespace HrtfData = XRay::Sound::HrtfData;

	// Overlap-save frame: one mixer block plus the HRIR history (1024 + 256 = 2^8 * 5, a valid pffft size)
	constexpr u32 FftSize = SND_BLOCKSIZE + 256;
	constexpr u32 HistoryFrames = FftSize - SND_BLOCKSIZE;
	constexpr u32 RendererLimit = 512;
	// Coefficient change below which the voice keeps its HRIR pair instead of crossfading to a new one
	constexpr float CoeffEpsilon = 1e-4f;

	static_assert(SND_CHANNEL_COUNT == 2, "Binaural output is stereo");
	static_assert(HrtfData::Channels == 16, "EncodeDirection writes 3rd order coefficients");
	static_assert(HrtfData::SampleRate == SND_SAMPLERATE, "Regenerate SoundHrtfData.h for the mixer sample rate");
	static_assert(HrtfData::Taps <= HistoryFrames + 1, "The HRIR does not fit into the overlap-save history");

	enum HrtfParam : u32
	{
		HrtfRolloff,
		HrtfFallbackRolloff,
		HrtfBackAttenuation
	};

	const SoundEffectParam HrtfParams[] =
	{
		{"rolloff", 2.0f, 0.1f, 4.0f},
		{"fallback_rolloff", 1.3f, 0.1f, 4.0f},
		{"back_attenuation", 0.3f, 0.0f, 1.0f},
	};

	struct HrtfVoiceState
	{
		SoundSpatialState Pan;
		// 1-based renderer index in the pool, 0 while the voice is panned
		u32 Renderer;
	};

	static_assert(sizeof(HrtfVoiceState) <= SND_VOICE_EFFECT_STATE_SIZE, "Voice effect state is too large");

	class CFftBuffer
	{
	public:
		CFftBuffer() = default;
		~CFftBuffer()
		{
			Free();
		}

		CFftBuffer(const CFftBuffer&) = delete;
		CFftBuffer& operator=(const CFftBuffer&) = delete;

		bool Allocate(size_t Count)
		{
			Free();
			Data = (float*)pffft_aligned_malloc(Count * sizeof(float));
			if (Data == nullptr)
			{
				return false;
			}

			memset(Data, 0, Count * sizeof(float));
			return true;
		}

		void Free()
		{
			pffft_aligned_free(Data);
			Data = nullptr;
		}

		float* Get() const
		{
			return Data;
		}

	private:
		float* Data = nullptr;
	};

	// Channels with a negative degree are antisymmetric about the median plane: the right ear gets them negated
	constexpr bool IsAntisymmetric(u32 Channel)
	{
		u32 Order = 0;
		while ((Order + 1) * (Order + 1) <= Channel)
		{
			Order++;
		}

		return Channel < Order * (Order + 1);
	}

	// Real SN3D spherical harmonics up to 3rd order in ACN order, without the Condon-Shortley phase
	void EncodeDirection(const Fvector& Local, float* Coeffs)
	{
		// Listener space is +X right, +Y up, +Z forward; ambisonics uses +X front, +Y left, +Z up
		const float X = Local.z;
		const float Y = -Local.x;
		const float Z = Local.y;

		constexpr float Sqrt3 = 1.73205081f;
		constexpr float Sqrt3Div8 = 0.61237244f;
		constexpr float Sqrt5Div8 = 0.79056942f;
		constexpr float Sqrt15 = 3.87298335f;

		const float XX = X * X;
		const float YY = Y * Y;
		const float ZZ = Z * Z;

		Coeffs[0] = 1.0f;
		Coeffs[1] = Y;
		Coeffs[2] = Z;
		Coeffs[3] = X;
		Coeffs[4] = Sqrt3 * X * Y;
		Coeffs[5] = Sqrt3 * Y * Z;
		Coeffs[6] = 0.5f * (3.0f * ZZ - 1.0f);
		Coeffs[7] = Sqrt3 * X * Z;
		Coeffs[8] = 0.5f * Sqrt3 * (XX - YY);
		Coeffs[9] = Sqrt5Div8 * Y * (3.0f * XX - YY);
		Coeffs[10] = Sqrt15 * X * Y * Z;
		Coeffs[11] = Sqrt3Div8 * Y * (5.0f * ZZ - 1.0f);
		Coeffs[12] = 0.5f * Z * (5.0f * ZZ - 3.0f);
		Coeffs[13] = Sqrt3Div8 * X * (5.0f * ZZ - 1.0f);
		Coeffs[14] = 0.5f * Sqrt15 * Z * (XX - YY);
		Coeffs[15] = Sqrt5Div8 * X * (XX - 3.0f * YY);
	}

	class CHrtfLibrary;

	// Per-voice binaural renderer: the SH HRIR spectra weighted by the voice direction give one HRIR pair,
	// applied to the mono input with an overlap-save FFT convolution
	class CHrtfRenderer
	{
	public:
		bool Initialize()
		{
			return Filters[0].Allocate(FftSize) && Filters[1].Allocate(FftSize);
		}

		void Reset()
		{
			memset(History, 0, sizeof(History));
			HasFilter = false;
		}

		// Data[0] is the mono input; both channels receive the binaural output scaled by TargetAttenuation
		void Process(const CHrtfLibrary& Library, float** Data, const float* Target, float TargetAttenuation);

	private:
		void BuildFilters(const CHrtfLibrary& Library, const float* NewCoeffs);

		float History[HistoryFrames] = {};
		float Coeffs[HrtfData::Channels] = {};
		float Attenuation = 0.0f;
		bool HasFilter = false;
		CFftBuffer Filters[SND_CHANNEL_COUNT];
	};

	// Shared SH HRIR spectra and the renderer pool
	class CHrtfLibrary
	{
	public:
		~CHrtfLibrary()
		{
			Shutdown();
		}

		void Shutdown()
		{
			xr_vector<xr_unique_ptr<CHrtfRenderer>>().swap(Renderers);
			xr_vector<u32>().swap(FreeRenderers);
			Spectra.Free();

			if (Setup != nullptr)
			{
				pffft_destroy_setup(Setup);
				Setup = nullptr;
			}

			Failed = false;
		}

		u32 Acquire()
		{
			if (!Initialize())
			{
				return 0;
			}

			u32 Index = 0;
			if (!FreeRenderers.empty())
			{
				Index = FreeRenderers.back();
				FreeRenderers.pop_back();
			}
			else
			{
				if (Renderers.size() >= RendererLimit)
				{
					return 0;
				}

				xr_unique_ptr<CHrtfRenderer> Renderer = xr_make_unique<CHrtfRenderer>();
				if (!Renderer->Initialize())
				{
					return 0;
				}

				Renderers.push_back(std::move(Renderer));
				Index = (u32)Renderers.size();
			}

			Renderers[Index - 1]->Reset();
			return Index;
		}

		void Release(u32* Renderer)
		{
			// Indices of a pool that was shut down in between are dropped
			if (*Renderer != 0 && *Renderer <= Renderers.size())
			{
				FreeRenderers.push_back(*Renderer);
			}

			*Renderer = 0;
		}

		CHrtfRenderer* Get(u32 Renderer) const
		{
			return Renderer != 0 && Renderer <= Renderers.size() ? Renderers[Renderer - 1].get() : nullptr;
		}

		PFFFT_Setup* GetSetup() const
		{
			return Setup;
		}

		const float* GetSpectrum(u32 Channel) const
		{
			return Spectra.Get() + (size_t)Channel * FftSize;
		}

	private:
		bool Initialize()
		{
			if (Setup != nullptr)
			{
				return true;
			}

			if (Failed)
			{
				return false;
			}

			Setup = pffft_new_setup((int)FftSize, PFFFT_REAL);
			if (Setup == nullptr || !Spectra.Allocate((size_t)HrtfData::Channels * FftSize))
			{
				Msg("! [Sound] HRTF: can't create the %u point FFT, voices fall back to stereo panning", FftSize);
				Shutdown();
				Failed = true;
				return false;
			}

			alignas(64) float Time[FftSize];
			alignas(64) float Work[FftSize];
			for (u32 Channel = 0; Channel < HrtfData::Channels; Channel++)
			{
				memset(Time, 0, sizeof(Time));
				memcpy(Time, HrtfData::ShHrir[Channel], sizeof(HrtfData::ShHrir[Channel]));
				pffft_transform(Setup, Time, Spectra.Get() + (size_t)Channel * FftSize, Work, PFFFT_FORWARD);
			}

			Renderers.reserve(RendererLimit);
			FreeRenderers.reserve(RendererLimit);
			return true;
		}

		PFFFT_Setup* Setup = nullptr;
		CFftBuffer Spectra;
		bool Failed = false;
		xr_vector<xr_unique_ptr<CHrtfRenderer>> Renderers;
		xr_vector<u32> FreeRenderers;
	};

	CHrtfLibrary GHrtf;

	void RenderBlock(PFFFT_Setup* Setup, const float* Input, const float* Filter, float* Output, float* Spectrum, float* Time, float* Work)
	{
		memset(Spectrum, 0, FftSize * sizeof(float));
		pffft_zconvolve_accumulate(Setup, Input, Filter, Spectrum, 1.0f / (float)FftSize);
		pffft_transform(Setup, Spectrum, Time, Work, PFFFT_BACKWARD);

		// Overlap-save: the first HistoryFrames samples are circular-convolution garbage
		memcpy(Output, Time + HistoryFrames, SND_BLOCKSIZE * sizeof(float));
	}

	void CHrtfRenderer::BuildFilters(const CHrtfLibrary& Library, const float* NewCoeffs)
	{
		float* Left = Filters[0].Get();
		float* Right = Filters[1].Get();
		memset(Left, 0, FftSize * sizeof(float));
		memset(Right, 0, FftSize * sizeof(float));

		// The spectra are linear in the HRIRs, so the SH weighting is done once per direction in the frequency domain
		for (u32 Channel = 0; Channel < HrtfData::Channels; Channel++)
		{
			const float Gain = NewCoeffs[Channel];
			if (std::abs(Gain) < CoeffEpsilon)
			{
				continue;
			}

			const float RightGain = IsAntisymmetric(Channel) ? -Gain : Gain;
			const float* Spectrum = Library.GetSpectrum(Channel);
			for (u32 Bin = 0; Bin < FftSize; Bin++)
			{
				Left[Bin] += Gain * Spectrum[Bin];
				Right[Bin] += RightGain * Spectrum[Bin];
			}
		}

		memcpy(Coeffs, NewCoeffs, sizeof(Coeffs));
		HasFilter = true;
	}

	void CHrtfRenderer::Process(const CHrtfLibrary& Library, float** Data, const float* Target, float TargetAttenuation)
	{
		PFFFT_Setup* Setup = Library.GetSetup();

		alignas(64) float Window[FftSize];
		alignas(64) float Input[FftSize];
		alignas(64) float Spectrum[FftSize];
		alignas(64) float Time[FftSize];
		alignas(64) float Work[FftSize];
		alignas(64) float Previous[SND_CHANNEL_COUNT][SND_BLOCKSIZE];

		memcpy(Window, History, sizeof(History));
		memcpy(Window + HistoryFrames, Data[0], SND_BLOCKSIZE * sizeof(float));
		memcpy(History, Window + SND_BLOCKSIZE, sizeof(History));
		pffft_transform(Setup, Window, Input, Work, PFFFT_FORWARD);

		float Delta = 0.0f;
		for (u32 Channel = 0; Channel < HrtfData::Channels; Channel++)
		{
			Delta = std::max(Delta, std::abs(Target[Channel] - Coeffs[Channel]));
		}

		// A direction change renders the block with both HRIR pairs and crossfades between them
		const bool IsCrossfade = HasFilter && Delta > CoeffEpsilon;
		const float StartAttenuation = HasFilter ? Attenuation : TargetAttenuation;
		if (IsCrossfade)
		{
			for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
			{
				RenderBlock(Setup, Input, Filters[Channel].Get(), Previous[Channel], Spectrum, Time, Work);
			}
		}

		if (!HasFilter || IsCrossfade)
		{
			BuildFilters(Library, Target);
		}

		for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			RenderBlock(Setup, Input, Filters[Channel].Get(), Data[Channel], Spectrum, Time, Work);
		}

		const float Step = 1.0f / (float)SND_BLOCKSIZE;
		for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			float* Output = Data[Channel];
			for (u32 Frame = 0; Frame < SND_BLOCKSIZE; Frame++)
			{
				const float Factor = (float)(Frame + 1) * Step;
				const float Sample = IsCrossfade ? lerp(Previous[Channel][Frame], Output[Frame], Factor) : Output[Frame];
				Output[Frame] = Sample * lerp(StartAttenuation, TargetAttenuation, Factor);
			}
		}

		Attenuation = TargetAttenuation;
	}

	void Spatialize(HrtfVoiceState* State, SoundEffectProcess* Process)
	{
		const float* Params = Process->Params;
		SoundSpatialPosition Position;
		Snd_SpatialLocate(Process, &Position);

		if (!psSoundFlags.is(ss_HRTF))
		{
			GHrtf.Release(&State->Renderer);
		}
		else if (State->Renderer == 0)
		{
			State->Renderer = GHrtf.Acquire();
		}

		CHrtfRenderer* Renderer = GHrtf.Get(State->Renderer);
		if (Renderer == nullptr)
		{
			Snd_SpatialPan(&State->Pan, Process, &Position, Params[HrtfFallbackRolloff], Params[HrtfBackAttenuation]);
			return;
		}

		State->Pan.IsPanned = false;

		Fvector Direction = Position.Local;
		if (Direction.square_magnitude() < EPS)
		{
			Direction.set(0.0f, 0.0f, 1.0f);
		}

		float Coeffs[HrtfData::Channels];
		EncodeDirection(Direction, Coeffs);
		Renderer->Process(GHrtf, Process->Data, Coeffs, Snd_DistanceAttenuation(&Position, Params[HrtfRolloff]));
	}
}

bool Snd_HrtfProc(void* StatePtr, SoundEffectOp Op, void* Arg)
{
	HrtfVoiceState* State = (HrtfVoiceState*)StatePtr;
	switch (Op)
	{
	case SoundEffectOp::Describe:
		*(SoundEffectDesc*)Arg = {SoundEffectScope::Voice, sizeof(HrtfVoiceState), (u32)std::size(HrtfParams), HrtfParams};
		return true;
	case SoundEffectOp::Create:
		return true;
	case SoundEffectOp::Reset:
	case SoundEffectOp::Destroy:
		GHrtf.Release(&State->Renderer);
		State->Pan.IsPanned = false;
		return true;
	case SoundEffectOp::Process:
		Spatialize(State, (SoundEffectProcess*)Arg);
		return true;
	default:
		return false;
	}
}

void Snd_ShutdownHrtf()
{
	GHrtf.Shutdown();
}
