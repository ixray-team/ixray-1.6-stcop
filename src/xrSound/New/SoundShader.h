#pragma once
#include "../../xrCore/xrReflectMarkers.h"

#include "SoundMeta.h"
#include "SoundShaderType.gen.h"

struct SoundShaderContext
{
	bool IsBus;
	bool HasInput;
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

	xr_unique_ptr<ISoundShader> Items[SND_SHADER_CHAIN_COUNT];
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
