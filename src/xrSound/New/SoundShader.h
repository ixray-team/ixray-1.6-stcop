#pragma once
#include "../../xrCore/xrReflectMarkers.h"

#include "SoundMeta.h"
#include "SoundShaderType.gen.h"

// Random access to the decoded source of a voice, for shaders that read more than the current block
struct SoundShaderSource
{
	void* User = nullptr;
	// Reads Frames from Position forward into Data (SND_CHANNEL_COUNT channels), returns the frames read.
	// Decodes on the calling thread when the range is not cached
	u32 (*Read)(void* User, u32 Position, float** Data, u32 Frames) = nullptr;
	// Asks the mixer to keep the range from Position on cached for the next blocks
	void (*Prefetch)(void* User, u32 Position) = nullptr;
	u32 TotalFrames = 0;
	// Source frame of the first output frame of the block
	u32 Position = 0;
	// Source frames per output frame (pitch, doppler, time factor)
	float Rate = 1.0f;
	bool IsLooped = false;
};

// Where a 3D voice and the listener are, in world space (meters, meters per second)
struct SoundShaderSpace
{
	Fvector ListenerPosition;
	Fvector ListenerVelocity;
	Fvector VoicePosition;
	Fvector VoiceVelocity;
};

struct SoundShaderContext
{
	bool IsBus;
	bool HasInput;
	// Voice shaders only, nullptr on buses
	const SoundShaderSource* Source = nullptr;
	// Voice shaders only, nullptr on buses and 2D voices
	const SoundShaderSpace* Space = nullptr;
	// Voice shaders only, nullptr on buses: playback rate factor of the voice, starts at 1 every block.
	// A shader multiplies it once per block (at Offset 0); the resampler glides to it over the next block
	float* Pitch = nullptr;
	// Output frames from the start of the block to Data (automated shaders are processed in pieces)
	u32 Offset = 0;
};

class ISoundShader;

namespace XRay::Sound::Shader
{
	constexpr const char* GetName(SoundShaderType Type)
	{
		return (u32)Type < (u32)SoundShaderType::Count ? Names[(u32)Type] : "";
	}

	struct ClassInfo;

	// Grants the generated code access to the parameters, see XSOUND_GENERATED_BODY
	struct Generated;
}

class ISoundShader
{
public:
	virtual ~ISoundShader() = default;

	virtual const XRay::Sound::Shader::ClassInfo& GetClass() const = 0;

	virtual void Reset()
	{
	}

	virtual void OnParamsChanged()
	{
	}

	virtual void Process(float** Data, u32 Frames, const SoundShaderContext& Context) = 0;

	virtual u32 GetTailFrames() const
	{
		return 0;
	}

	SoundShaderType GetType() const;
	u32 GetParamCount() const;
	const SoundEffectParam* GetParamDesc(u32 Index) const;
	s32 FindParam(const char* Name) const;
	float GetParam(u32 Index) const;
	bool SetParam(u32 Index, float Value);
	bool SetParam(const char* Name, float Value);
	void SetParams(const float* Values, u32 Count);
	void CopyParams(const ISoundShader& Other);
};

namespace XRay::Sound::Shader
{
	struct ClassInfo
	{
		SoundShaderType Type = SoundShaderType::None;
		const char* Name = "";
		u32 ParamCount = 0;
		SoundEffectParam Params[SND_EFFECT_PARAM_COUNT] = {};
		float ISoundShader::*Fields[SND_EFFECT_PARAM_COUNT] = {};
		ISoundShader* (*Create)() = nullptr;

		void AddParam(const char* ParamName, float Default, float Min, float Max, float ISoundShader::*Field)
		{
			if (ParamCount >= SND_EFFECT_PARAM_COUNT)
			{
				Msg("! [Sound] Too many parameters in shader '%s'", Name);
				return;
			}

			Params[ParamCount] = {ParamName, Default, Min, Max};
			Fields[ParamCount] = Field;
			ParamCount++;
		}
	};

	struct Generated
	{
		template <typename T>
		static const ClassInfo& Info();
		static const ClassInfo* GetClass(SoundShaderType Type);
	};
}

// Declares the generated members of an XREFLECT(SoundShader) class
#define XSOUND_GENERATED_BODY() \
	friend struct ::XRay::Sound::Shader::Generated; \
\
public: \
	const ::XRay::Sound::Shader::ClassInfo& GetClass() const override; \
\
private:

inline SoundShaderType ISoundShader::GetType() const
{
	return GetClass().Type;
}

inline u32 ISoundShader::GetParamCount() const
{
	return GetClass().ParamCount;
}

inline const SoundEffectParam* ISoundShader::GetParamDesc(u32 Index) const
{
	return Index < GetParamCount() ? &GetClass().Params[Index] : nullptr;
}

inline s32 ISoundShader::FindParam(const char* Name) const
{
	const XRay::Sound::Shader::ClassInfo& Info = GetClass();
	for (u32 Index = 0; Index < Info.ParamCount; Index++)
	{
		if (xr_strcmp(Info.Params[Index].Name, Name) == 0)
		{
			return (s32)Index;
		}
	}
	return -1;
}

inline float ISoundShader::GetParam(u32 Index) const
{
	return Index < GetParamCount() ? this->*GetClass().Fields[Index] : 0.0f;
}

inline bool ISoundShader::SetParam(u32 Index, float Value)
{
	const SoundEffectParam* Desc = GetParamDesc(Index);
	if (Desc == nullptr)
	{
		return false;
	}

	this->*GetClass().Fields[Index] = std::clamp(Value, Desc->Min, Desc->Max);
	OnParamsChanged();
	return true;
}

inline bool ISoundShader::SetParam(const char* Name, float Value)
{
	const s32 Index = FindParam(Name);
	return Index >= 0 && SetParam((u32)Index, Value);
}

inline void ISoundShader::SetParams(const float* Values, u32 Count)
{
	const XRay::Sound::Shader::ClassInfo& Info = GetClass();
	for (u32 Index = 0; Index < std::min(Count, Info.ParamCount); Index++)
	{
		this->*Info.Fields[Index] = std::clamp(Values[Index], Info.Params[Index].Min, Info.Params[Index].Max);
	}
	OnParamsChanged();
}

inline void ISoundShader::CopyParams(const ISoundShader& Other)
{
	if (Other.GetType() != GetType())
	{
		return;
	}

	const XRay::Sound::Shader::ClassInfo& Info = GetClass();
	for (u32 Index = 0; Index < Info.ParamCount; Index++)
	{
		this->*Info.Fields[Index] = Other.*Info.Fields[Index];
	}
	OnParamsChanged();
}

namespace XRay::Sound::Shader
{
	// Shape of a parameter ramp
	enum class Curve : u8
	{
		Linear,
		// Geometric between two positive values (frequencies, times); linear otherwise
		Exponential,
		// Smoothstep: slow start and end
		Smooth
	};

	// LFO waveform, -1..1 around the parameter value
	enum class LfoShape : u8
	{
		Sine,
		Triangle,
		Square,
		SawUp,
		SawDown,
		// New random value every cycle (sample and hold)
		Random
	};

	// Frames between automation updates: an automated shader is processed in pieces of this size
	constexpr u32 AutomationStep = 32;

	// "linear", "exp", "smooth"; nullptr or "" is linear, an unknown name is logged and linear
	XRSOUND_API Curve FindCurve(const char* Name);
	// "sine", "triangle", "square", "saw_up", "saw_down", "random"; nullptr or "" is sine
	XRSOUND_API LfoShape FindLfoShape(const char* Name);

	struct ParamLane
	{
		// Value without the LFO, valid while the lane is active
		float Base = 0.0f;
		float RampFrom = 0.0f;
		float RampTo = 0.0f;
		u32 RampFrame = 0;
		u32 RampFrames = 0;
		float LfoRate = 0.0f;
		float LfoDepth = 0.0f;
		float LfoPhase = 0.0f;
		float LfoHold = 0.0f;
		Curve RampCurve = Curve::Linear;
		LfoShape Shape = LfoShape::Sine;
		bool HasRamp = false;
		bool HasLfo = false;
	};

	// Ramps and LFOs of the parameters of one shader instance. Time runs with the processed frames:
	// a stopped voice or a silent bus holds its automation
	class XRSOUND_API Automation
	{
	public:
		void Ramp(ISoundShader& Shader, u32 Param, float Target, float Seconds, Curve Shape);
		// Rate or depth 0 stops the LFO and restores the value under it
		void SetLfo(ISoundShader& Shader, u32 Param, float RateHz, float Depth, LfoShape Shape);
		// Keeps the current ramp value, drops the LFO
		void Stop(ISoundShader& Shader, u32 Param);
		void StopAll(ISoundShader& Shader);
		// A direct parameter write: drops the ramp, the LFO keeps running around the new value
		void OnSet(u32 Param, float Value);
		void CopyFrom(const Automation& Other);

		// Writes the automated values to the shader
		void Apply(ISoundShader& Shader);
		// Moves the automation by Frames; finished ramps write their target
		void Step(ISoundShader& Shader, u32 Frames);

		bool IsActive() const
		{
			return ActiveMask != 0;
		}

		bool IsActive(u32 Param) const
		{
			return Param < SND_EFFECT_PARAM_COUNT && (ActiveMask & (1u << Param)) != 0;
		}

		const ParamLane& GetLane(u32 Param) const
		{
			return Lanes[Param];
		}

	private:
		float GetBase(const ISoundShader& Shader, u32 Param) const;
		float RampValue(const ParamLane& Lane) const;
		float LfoValue(const ParamLane& Lane) const;
		float Random();

		ParamLane Lanes[SND_EFFECT_PARAM_COUNT];
		u32 ActiveMask = 0;
		u32 Seed = 0x2545F491u;
	};
}

// Ordered shader instances of one voice or bus. One instance per shader type
class XRSOUND_API SoundShaderChain
{
public:
	SoundShaderChain();
	~SoundShaderChain();
	SoundShaderChain(SoundShaderChain&& Other) noexcept;
	SoundShaderChain& operator=(SoundShaderChain&& Other) noexcept;
	SoundShaderChain(const SoundShaderChain&) = delete;
	SoundShaderChain& operator=(const SoundShaderChain&) = delete;

	// Adds an owner; an existing instance of the type is returned
	ISoundShader* Add(SoundShaderType Type, bool SpatialOnly = false);
	// Drops an owner; the instance goes with the last one
	bool Remove(SoundShaderType Type, bool SpatialOnly = false);
	// Ignores owners
	void Clear();
	ISoundShader* Find(SoundShaderType Type) const;
	bool SetParam(SoundShaderType Type, const char* Name, float Value);
	// Automation (XRay::Sound::Shader::Automation). Seconds <= 0 sets the value at once
	bool RampParam(SoundShaderType Type, const char* Name, float Target, float Seconds, XRay::Sound::Shader::Curve Shape);
	bool SetParamLfo(SoundShaderType Type, const char* Name, float RateHz, float Depth, XRay::Sound::Shader::LfoShape Shape);
	// nullptr or "" stops every parameter of the shader
	bool StopAutomation(SoundShaderType Type, const char* Name);

	// Replaces this chain with copies of the other one (types and parameters, not DSP state)
	void CopyFrom(const SoundShaderChain& Other);

	void Reset();
	void Process(float** Data, u32 Frames, const SoundShaderContext& Context);
	u32 GetTailFrames() const;

	bool IsEmpty() const
	{
		return Count == 0;
	}

	u32 GetCount() const
	{
		return Count;
	}

	ISoundShader* Get(u32 Index) const
	{
		return Index < Count ? Items[Index].get() : nullptr;
	}

	u32 GetOwnerCount(u32 Index) const
	{
		return Index < Count ? Owners[Index] : 0;
	}

	// nullptr when no parameter of the shader was ever automated
	const XRay::Sound::Shader::Automation* GetAutomation(u32 Index) const
	{
		return Index < Count ? Automations[Index].get() : nullptr;
	}

	bool IsSpatialOnly(u32 Index) const
	{
		return Index < Count && AllVoiceOwners[Index] == 0;
	}

	bool HasSpatialOnly() const
	{
		for (u32 Index = 0; Index < Count; Index++)
		{
			if (AllVoiceOwners[Index] == 0)
			{
				return true;
			}
		}
		return false;
	}

private:
	void RemoveAt(u32 Index);
	s32 IndexOf(SoundShaderType Type) const;
	// Shader and the index of its parameter, logs an unknown one
	ISoundShader* FindParam(SoundShaderType Type, const char* Name, u32& Index, u32& Param) const;
	XRay::Sound::Shader::Automation& GetOrCreateAutomation(u32 Index);

	xr_unique_ptr<ISoundShader> Items[SND_SHADER_CHAIN_COUNT];
	xr_unique_ptr<XRay::Sound::Shader::Automation> Automations[SND_SHADER_CHAIN_COUNT];
	u16 Owners[SND_SHADER_CHAIN_COUNT] = {};
	u16 AllVoiceOwners[SND_SHADER_CHAIN_COUNT] = {};
	u32 Count = 0;
};

namespace XRay::Sound::Shader
{
	XRSOUND_API SoundShaderType Find(const char* Name);
	XRSOUND_API const ClassInfo* GetClass(SoundShaderType Type);
	XRSOUND_API ISoundShader* Create(SoundShaderType Type);

	// Appends the shader with default parameters; false when it is already there or the preset is full
	XRSOUND_API bool PresetAdd(SoundShaderPreset& Preset, SoundShaderType Type);
	XRSOUND_API void PresetRemove(SoundShaderPreset& Preset, u32 Index);
}
