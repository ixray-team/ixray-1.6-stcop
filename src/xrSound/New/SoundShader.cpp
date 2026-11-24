#include "SoundShaders.h"
#include "SoundEffect.h"
#include "SoundConfig.h"

const XRay::Sound::Shader::ClassInfo* GetClassTable(SoundShaderType Type)
{
	return XRay::Sound::Shader::Generated::GetClass(Type);
}

// Lets a shader sit in a bus "effects" list: the effect state holds the shader instance
template <SoundShaderType Type>
bool ShaderEffectProc(void* StatePtr, SoundEffectOp Op, void* Arg)
{
	const XRay::Sound::Shader::ClassInfo* Info = GetClassTable(Type);
	ISoundShader** Shader = (ISoundShader**)StatePtr;
	if (Info == nullptr)
	{
		return false;
	}

	switch (Op)
	{
		case SoundEffectOp::Describe:
		{
			*(SoundEffectDesc*)Arg = {SoundEffectScope::Bus, sizeof(ISoundShader*), Info->ParamCount, Info->Params};
			return true;
		}
		case SoundEffectOp::Create:
		{
			*Shader = XRay::Sound::Shader::Create(Type);
			const SoundEffectCreate* Create = (const SoundEffectCreate*)Arg;
			if (*Shader != nullptr && Create != nullptr && Create->Params != nullptr)
			{
				(*Shader)->SetParams(Create->Params, Info->ParamCount);
			}
			return *Shader != nullptr;
		}
		case SoundEffectOp::Destroy:
		{
			xr_delete(*Shader);
			return true;
		}
		case SoundEffectOp::Reset:
		{
			if (*Shader != nullptr)
			{
				(*Shader)->Reset();
			}
			return true;
		}
		case SoundEffectOp::Update:
		{
			if (*Shader != nullptr && Arg != nullptr)
			{
				(*Shader)->SetParams((const float*)Arg, Info->ParamCount);
			}
			return true;
		}
		case SoundEffectOp::Process:
		{
			const SoundEffectProcess* Process = (const SoundEffectProcess*)Arg;
			if (*Shader != nullptr)
			{
				SoundShaderContext Context = {true, Process->HasInput};
				(*Shader)->Process(Process->Data, SND_BLOCKSIZE, Context);
			}
			return true;
		}
		case SoundEffectOp::Tail:
		{
			*(u32*)Arg = *Shader != nullptr ? (*Shader)->GetTailFrames() : 0;
			return true;
		}
		default:
		{
			return false;
		}
	}
}

namespace XRay::Sound::Shader
{
SoundShaderType Find(const char* Name)
{
	if (Name == nullptr || Name[0] == 0)
	{
		return SoundShaderType::None;
	}

	for (u32 Type = 1; Type < (u32)SoundShaderType::Count; Type++)
	{
		if (xr_strcmp(Names[Type], Name) == 0)
		{
			return (SoundShaderType)Type;
		}
	}

	return SoundShaderType::None;
}

const ClassInfo* GetClass(SoundShaderType Type)
{
	return GetClassTable(Type);
}

ISoundShader* Create(SoundShaderType Type)
{
	const ClassInfo* Info = GetClassTable(Type);
	if (Info == nullptr || Info->Create == nullptr)
	{
		return nullptr;
	}

	ISoundShader* Shader = Info->Create();
	Shader->Reset();
	return Shader;
}

bool PresetAdd(SoundShaderPreset& Preset, SoundShaderType Type)
{
	const ClassInfo* Info = GetClassTable(Type);
	if (Info == nullptr || Preset.Count >= SND_SHADER_CHAIN_COUNT)
	{
		return false;
	}

	for (u32 Index = 0; Index < Preset.Count; Index++)
	{
		if (Preset.Types[Index] == (u8)Type)
		{
			return false;
		}
	}

	Preset.Types[Preset.Count] = (u8)Type;
	for (u32 ParamIdx = 0; ParamIdx < SND_EFFECT_PARAM_COUNT; ParamIdx++)
	{
		Preset.Params[Preset.Count][ParamIdx] = ParamIdx < Info->ParamCount ? Info->Params[ParamIdx].Default : 0.0f;
	}
	Preset.Count++;
	return true;
}

void PresetRemove(SoundShaderPreset& Preset, u32 Index)
{
	if (Index >= Preset.Count)
	{
		return;
	}

	for (u32 Next = Index + 1; Next < Preset.Count; Next++)
	{
		Preset.Types[Next - 1] = Preset.Types[Next];
		memcpy(Preset.Params[Next - 1], Preset.Params[Next], sizeof(Preset.Params[Next]));
	}

	Preset.Count--;
	Preset.Types[Preset.Count] = 0;
	memset(Preset.Params[Preset.Count], 0, sizeof(Preset.Params[Preset.Count]));
}
}

SoundShaderType SplitShaderKey(const char* Key, const char*& Param)
{
	const char* Dot = strchr(Key, '.');
	if (Dot == nullptr || Dot == Key || (size_t)(Dot - Key) >= sizeof(string128))
	{
		return SoundShaderType::None;
	}

	string128 Name;
	memcpy(Name, Key, Dot - Key);
	Name[Dot - Key] = 0;
	Param = Dot + 1;
	return XRay::Sound::Shader::Find(Name);
}

void Snd_ReadShaderPreset(const SoundConfigSection* Section, SoundShaderPreset& Preset)
{
	Preset = {};
	const char* List = Section != nullptr ? Snd_ConfigValue(Section, "shaders") : nullptr;
	if (List == nullptr || List[0] == 0)
	{
		return;
	}

	const u32 ItemCount = (u32)_GetItemCount(List);
	for (u32 ItemIdx = 0; ItemIdx < ItemCount; ItemIdx++)
	{
		string128 Item;
		_GetItem(List, ItemIdx, Item);
		const SoundShaderType Type = XRay::Sound::Shader::Find(Item);
		if (Type == SoundShaderType::None)
		{
			Msg("! [Sound] Unknown sound shader '%s'", Item);
			continue;
		}

		XRay::Sound::Shader::PresetAdd(Preset, Type);
	}

	for (const CInifile::Item& Item : Section->Items)
	{
		const char* Param = nullptr;
		const SoundShaderType Type = SplitShaderKey(Item.first.c_str(), Param);
		const XRay::Sound::Shader::ClassInfo* Info = XRay::Sound::Shader::GetClass(Type);
		if (Info == nullptr || Item.second.size() == 0)
		{
			continue;
		}

		for (u32 Index = 0; Index < Preset.Count; Index++)
		{
			if (Preset.Types[Index] != (u8)Type)
			{
				continue;
			}

			for (u32 ParamIdx = 0; ParamIdx < Info->ParamCount; ParamIdx++)
			{
				const SoundEffectParam& Desc = Info->Params[ParamIdx];
				if (xr_strcmp(Desc.Name, Param) == 0)
				{
					Preset.Params[Index][ParamIdx] = std::clamp((float)atof(Item.second.c_str()), Desc.Min, Desc.Max);
				}
			}
		}
	}
}

void Snd_WriteShaderPreset(SoundConfigSection* Section, const SoundShaderPreset& Preset)
{
	std::erase_if
	(
		Section->Items, [](const CInifile::Item& Item)
		{
			const char* Param = nullptr;
			return xr_strcmp(Item.first.c_str(), "shaders") == 0 || SplitShaderKey(Item.first.c_str(), Param) != SoundShaderType::None; 
		}
	);

	if (Preset.Count == 0)
	{
		return;
	}

	xr_string List;
	for (u32 Index = 0; Index < Preset.Count; Index++)
	{
		List += Index != 0 ? ", " : "";
		List += XRay::Sound::Shader::GetName((SoundShaderType)Preset.Types[Index]);
	}
	Snd_SetConfigValue(Section, "shaders", List.c_str());

	for (u32 Index = 0; Index < Preset.Count; Index++)
	{
		const XRay::Sound::Shader::ClassInfo* Info = XRay::Sound::Shader::GetClass((SoundShaderType)Preset.Types[Index]);
		for (u32 ParamIdx = 0; Info != nullptr && ParamIdx < Info->ParamCount; ParamIdx++)
		{
			const float Value = Preset.Params[Index][ParamIdx];
			if (Value == Info->Params[ParamIdx].Default)
			{
				continue;
			}

			string128 Key;
			string32 Text;
			xr_strconcat(Key, Info->Name, ".", Info->Params[ParamIdx].Name);
			xr_sprintf(Text, "%g", Value);
			Snd_SetConfigValue(Section, Key, Text);
		}
	}
}

void Snd_SyncShaderChain(SoundShaderChain& Chain, const SoundShaderPreset& Preset)
{
	bool IsSame = Chain.GetCount() == Preset.Count;
	for (u32 Index = 0; IsSame && Index < Preset.Count; Index++)
	{
		IsSame = Chain.Get(Index)->GetType() == (SoundShaderType)Preset.Types[Index];
	}

	if (!IsSame)
	{
		Chain.Clear();
		for (u32 Index = 0; Index < Preset.Count; Index++)
		{
			Chain.Add((SoundShaderType)Preset.Types[Index]);
		}
	}

	for (u32 Index = 0; Index < Preset.Count; Index++)
	{
		if (ISoundShader* Shader = Chain.Find((SoundShaderType)Preset.Types[Index]))
		{
			Shader->SetParams(Preset.Params[Index], SND_EFFECT_PARAM_COUNT);
		}
	}
}

void RegisterShaderEffect(const char* Name, SoundEffectProc Proc)
{
	// Snd_RegisterEffect returns the existing effect when the name is taken
	const SoundEffectEntry* Entry = Snd_GetEffect(Snd_RegisterEffect(Name, Proc));
	if (Entry == nullptr || Entry->Proc != Proc)
	{
		Msg("! [Sound] Shader '%s' is not a bus effect: the name is taken by another effect", Name);
	}
}

template <u8... Types>
void RegisterShaderEffects(std::integer_sequence<u8, Types...>)
{
	// Type 0 -> SoundShaderType::None
	((Types != 0 ? RegisterShaderEffect(XRay::Sound::Shader::Names[Types], ShaderEffectProc<(SoundShaderType)Types>) : (void)0), ...);
}

void Snd_RegisterShaderEffects()
{
	RegisterShaderEffects(std::make_integer_sequence<u8, (u8)SoundShaderType::Count>{});
}

namespace XRay::Sound::Shader
{
	namespace
	{
		struct NamedValue
		{
			const char* Name;
			u8 Value;
		};

		constexpr NamedValue CurveNames[] = {
			{"linear", (u8)Curve::Linear}, {"exp", (u8)Curve::Exponential}, {"exponential", (u8)Curve::Exponential}, {"smooth", (u8)Curve::Smooth}};

		constexpr NamedValue LfoShapeNames[] = {
			{"sine", (u8)LfoShape::Sine}, {"triangle", (u8)LfoShape::Triangle}, {"square", (u8)LfoShape::Square},
			{"saw_up", (u8)LfoShape::SawUp}, {"saw_down", (u8)LfoShape::SawDown}, {"random", (u8)LfoShape::Random}};

		template <size_t N>
		u8 FindNamed(const NamedValue (&Table)[N], const char* Name, const char* What)
		{
			if (Name == nullptr || Name[0] == 0)
			{
				return Table[0].Value;
			}

			for (const NamedValue& Item : Table)
			{
				if (xr_strcmp(Item.Name, Name) == 0)
				{
					return Item.Value;
				}
			}

			Msg("! [Sound] Unknown %s '%s', using '%s'", What, Name, Table[0].Name);
			return Table[0].Value;
		}

		float ClampParam(const ISoundShader& Shader, u32 Param, float Value)
		{
			const SoundEffectParam& Desc = Shader.GetClass().Params[Param];
			return std::clamp(Value, Desc.Min, Desc.Max);
		}

		float& ParamField(ISoundShader& Shader, u32 Param)
		{
			return Shader.*Shader.GetClass().Fields[Param];
		}
	}

	Curve FindCurve(const char* Name)
	{
		return (Curve)FindNamed(CurveNames, Name, "automation curve");
	}

	LfoShape FindLfoShape(const char* Name)
	{
		return (LfoShape)FindNamed(LfoShapeNames, Name, "LFO shape");
	}

	float Automation::Random()
	{
		// xorshift32, -1..1
		Seed ^= Seed << 13;
		Seed ^= Seed >> 17;
		Seed ^= Seed << 5;
		return (float)(Seed >> 8) * (2.0f / 16777216.0f) - 1.0f;
	}

	float Automation::GetBase(const ISoundShader& Shader, u32 Param) const
	{
		if (!IsActive(Param))
		{
			return Shader.GetParam(Param);
		}

		const ParamLane& Lane = Lanes[Param];
		return Lane.HasRamp ? RampValue(Lane) : Lane.Base;
	}

	float Automation::RampValue(const ParamLane& Lane) const
	{
		if (Lane.RampFrames == 0 || Lane.RampFrame >= Lane.RampFrames)
		{
			return Lane.RampTo;
		}

		const float Factor = (float)Lane.RampFrame / (float)Lane.RampFrames;
		switch (Lane.RampCurve)
		{
		case Curve::Exponential:
			if (Lane.RampFrom > 0.0f && Lane.RampTo > 0.0f)
			{
				return Lane.RampFrom * std::pow(Lane.RampTo / Lane.RampFrom, Factor);
			}
			break;
		case Curve::Smooth:
			return lerp(Lane.RampFrom, Lane.RampTo, Factor * Factor * (3.0f - 2.0f * Factor));
		default:
			break;
		}

		return lerp(Lane.RampFrom, Lane.RampTo, Factor);
	}

	float Automation::LfoValue(const ParamLane& Lane) const
	{
		const float Phase = Lane.LfoPhase;
		float Wave = 0.0f;
		switch (Lane.Shape)
		{
		case LfoShape::Triangle:
			Wave = 1.0f - 4.0f * std::abs(Phase - 0.5f);
			break;
		case LfoShape::Square:
			Wave = Phase < 0.5f ? 1.0f : -1.0f;
			break;
		case LfoShape::SawUp:
			Wave = 2.0f * Phase - 1.0f;
			break;
		case LfoShape::SawDown:
			Wave = 1.0f - 2.0f * Phase;
			break;
		case LfoShape::Random:
			Wave = Lane.LfoHold;
			break;
		default:
			Wave = std::sin(2.0f * PI * Phase);
			break;
		}
		return Wave * Lane.LfoDepth;
	}

	void Automation::Ramp(ISoundShader& Shader, u32 Param, float Target, float Seconds, Curve Shape)
	{
		if (Param >= Shader.GetParamCount())
		{
			return;
		}

		ParamLane& Lane = Lanes[Param];
		const float From = GetBase(Shader, Param);
		Target = ClampParam(Shader, Param, Target);
		const u32 Frames = Seconds > 0.0f ? (u32)(Seconds * (float)SND_SAMPLERATE) : 0;

		if (Frames == 0)
		{
			Shader.SetParam(Param, Target);
			OnSet(Param, Target);
			return;
		}

		Lane.Base = From;
		Lane.RampFrom = From;
		Lane.RampTo = Target;
		Lane.RampFrame = 0;
		Lane.RampFrames = Frames;
		Lane.RampCurve = Shape;
		Lane.HasRamp = true;
		ActiveMask |= 1u << Param;
	}

	void Automation::SetLfo(ISoundShader& Shader, u32 Param, float RateHz, float Depth, LfoShape Shape)
	{
		if (Param >= Shader.GetParamCount())
		{
			return;
		}

		ParamLane& Lane = Lanes[Param];
		if (RateHz <= 0.0f || Depth == 0.0f)
		{
			if (Lane.HasLfo)
			{
				Lane.HasLfo = false;
				if (!Lane.HasRamp)
				{
					Shader.SetParam(Param, Lane.Base);
					ActiveMask &= ~(1u << Param);
				}
			}
			return;
		}

		if (!IsActive(Param))
		{
			Lane.Base = Shader.GetParam(Param);
		}

		if (!Lane.HasLfo)
		{
			Lane.LfoPhase = 0.0f;
			Lane.LfoHold = Random();
		}

		Lane.LfoRate = RateHz;
		Lane.LfoDepth = Depth;
		Lane.Shape = Shape;
		Lane.HasLfo = true;
		ActiveMask |= 1u << Param;
	}

	void Automation::Stop(ISoundShader& Shader, u32 Param)
	{
		if (!IsActive(Param))
		{
			return;
		}

		const float Value = GetBase(Shader, Param);
		Lanes[Param].HasRamp = false;
		Lanes[Param].HasLfo = false;
		ActiveMask &= ~(1u << Param);
		Shader.SetParam(Param, Value);
	}

	void Automation::StopAll(ISoundShader& Shader)
	{
		for (u32 Param = 0; Param < SND_EFFECT_PARAM_COUNT; Param++)
		{
			Stop(Shader, Param);
		}
	}

	void Automation::OnSet(u32 Param, float Value)
	{
		if (!IsActive(Param))
		{
			return;
		}

		ParamLane& Lane = Lanes[Param];
		Lane.HasRamp = false;
		Lane.Base = Value;
		if (!Lane.HasLfo)
		{
			ActiveMask &= ~(1u << Param);
		}
	}

	void Automation::CopyFrom(const Automation& Other)
	{
		*this = Other;
	}

	void Automation::Apply(ISoundShader& Shader)
	{
		if (ActiveMask == 0)
		{
			return;
		}

		for (u32 Param = 0; Param < Shader.GetParamCount(); Param++)
		{
			if (!IsActive(Param))
			{
				continue;
			}

			const ParamLane& Lane = Lanes[Param];
			float Value = Lane.HasRamp ? RampValue(Lane) : Lane.Base;
			if (Lane.HasLfo)
			{
				Value += LfoValue(Lane);
			}
			ParamField(Shader, Param) = ClampParam(Shader, Param, Value);
		}

		Shader.OnParamsChanged();
	}

	void Automation::Step(ISoundShader& Shader, u32 Frames)
	{
		bool IsChanged = false;
		for (u32 Param = 0; Param < SND_EFFECT_PARAM_COUNT; Param++)
		{
			if (!IsActive(Param))
			{
				continue;
			}

			ParamLane& Lane = Lanes[Param];
			if (Lane.HasLfo)
			{
				Lane.LfoPhase += Lane.LfoRate * (float)Frames / (float)SND_SAMPLERATE;
				if (Lane.LfoPhase >= 1.0f)
				{
					Lane.LfoPhase -= std::floor(Lane.LfoPhase);
					Lane.LfoHold = Random();
				}
			}

			if (!Lane.HasRamp)
			{
				continue;
			}

			Lane.RampFrame = std::min(Lane.RampFrame + Frames, Lane.RampFrames);
			if (Lane.RampFrame < Lane.RampFrames)
			{
				continue;
			}

			Lane.HasRamp = false;
			Lane.Base = Lane.RampTo;
			if (!Lane.HasLfo && Param < Shader.GetParamCount())
			{
				ActiveMask &= ~(1u << Param);
				ParamField(Shader, Param) = ClampParam(Shader, Param, Lane.RampTo);
				IsChanged = true;
			}
		}

		if (IsChanged)
		{
			Shader.OnParamsChanged();
		}
	}
}

SoundShaderChain::SoundShaderChain() = default;
SoundShaderChain::~SoundShaderChain() = default;

SoundShaderChain::SoundShaderChain(SoundShaderChain&& Other) noexcept
{
	*this = std::move(Other);
}

SoundShaderChain& SoundShaderChain::operator=(SoundShaderChain&& Other) noexcept
{
	if (this != &Other)
	{
		for (u32 Index = 0; Index < SND_SHADER_CHAIN_COUNT; Index++)
		{
			Items[Index] = std::move(Other.Items[Index]);
			Automations[Index] = std::move(Other.Automations[Index]);
			Owners[Index] = Other.Owners[Index];
			AllVoiceOwners[Index] = Other.AllVoiceOwners[Index];
		}
		Count = Other.Count;
		Other.Count = 0;
	}
	return *this;
}

ISoundShader* SoundShaderChain::Add(SoundShaderType Type, bool SpatialOnly)
{
	for (u32 Index = 0; Index < Count; Index++)
	{
		if (Items[Index]->GetType() == Type)
		{
			Owners[Index]++;
			AllVoiceOwners[Index] += SpatialOnly ? 0 : 1;
			return Items[Index].get();
		}
	}

	if (Count >= SND_SHADER_CHAIN_COUNT)
	{
		Msg("! [Sound] Shader chain is full, can't add '%s'", XRay::Sound::Shader::GetName(Type));
		return nullptr;
	}

	ISoundShader* Shader = XRay::Sound::Shader::Create(Type);
	if (Shader == nullptr)
	{
		return nullptr;
	}

	Items[Count].reset(Shader);
	Owners[Count] = 1;
	AllVoiceOwners[Count] = SpatialOnly ? 0 : 1;
	Count++;
	return Shader;
}

void SoundShaderChain::RemoveAt(u32 Index)
{
	Items[Index].reset();
	Automations[Index].reset();
	for (u32 Next = Index + 1; Next < Count; Next++)
	{
		Items[Next - 1] = std::move(Items[Next]);
		Automations[Next - 1] = std::move(Automations[Next]);
		Owners[Next - 1] = Owners[Next];
		AllVoiceOwners[Next - 1] = AllVoiceOwners[Next];
	}
	Count--;
	Owners[Count] = 0;
	AllVoiceOwners[Count] = 0;
}

bool SoundShaderChain::Remove(SoundShaderType Type, bool SpatialOnly)
{
	for (u32 Index = 0; Index < Count; Index++)
	{
		if (Items[Index]->GetType() != Type)
		{
			continue;
		}

		if (!SpatialOnly && AllVoiceOwners[Index] > 0)
		{
			AllVoiceOwners[Index]--;
		}

		if (--Owners[Index] == 0)
		{
			RemoveAt(Index);
		}
		return true;
	}

	return false;
}

void SoundShaderChain::Clear()
{
	for (u32 Index = 0; Index < Count; Index++)
	{
		Items[Index].reset();
		Automations[Index].reset();
		Owners[Index] = 0;
		AllVoiceOwners[Index] = 0;
	}
	Count = 0;
}

ISoundShader* SoundShaderChain::Find(SoundShaderType Type) const
{
	for (u32 Index = 0; Index < Count; Index++)
	{
		if (Items[Index]->GetType() == Type)
		{
			return Items[Index].get();
		}
	}

	return nullptr;
}

s32 SoundShaderChain::IndexOf(SoundShaderType Type) const
{
	for (u32 Index = 0; Index < Count; Index++)
	{
		if (Items[Index]->GetType() == Type)
		{
			return (s32)Index;
		}
	}

	return -1;
}

ISoundShader* SoundShaderChain::FindParam(SoundShaderType Type, const char* Name, u32& Index, u32& Param) const
{
	const s32 ShaderIdx = IndexOf(Type);
	const s32 ParamIdx = ShaderIdx >= 0 && Name != nullptr ? Items[ShaderIdx]->FindParam(Name) : -1;
	if (ParamIdx < 0)
	{
		Msg("! [Sound] Unknown shader parameter '%s.%s'", XRay::Sound::Shader::GetName(Type), Name ? Name : "");
		return nullptr;
	}

	Index = (u32)ShaderIdx;
	Param = (u32)ParamIdx;
	return Items[ShaderIdx].get();
}

XRay::Sound::Shader::Automation& SoundShaderChain::GetOrCreateAutomation(u32 Index)
{
	if (!Automations[Index])
	{
		Automations[Index].reset(new XRay::Sound::Shader::Automation());
	}
	return *Automations[Index];
}

bool SoundShaderChain::SetParam(SoundShaderType Type, const char* Name, float Value)
{
	u32 Index = 0, Param = 0;
	ISoundShader* Shader = FindParam(Type, Name, Index, Param);
	if (Shader == nullptr)
	{
		return false;
	}

	Shader->SetParam(Param, Value);
	if (Automations[Index])
	{
		Automations[Index]->OnSet(Param, Shader->GetParam(Param));
	}
	return true;
}

bool SoundShaderChain::RampParam(SoundShaderType Type, const char* Name, float Target, float Seconds, XRay::Sound::Shader::Curve Shape)
{
	u32 Index = 0, Param = 0;
	ISoundShader* Shader = FindParam(Type, Name, Index, Param);
	if (Shader == nullptr)
	{
		return false;
	}

	GetOrCreateAutomation(Index).Ramp(*Shader, Param, Target, Seconds, Shape);
	return true;
}

bool SoundShaderChain::SetParamLfo(SoundShaderType Type, const char* Name, float RateHz, float Depth, XRay::Sound::Shader::LfoShape Shape)
{
	u32 Index = 0, Param = 0;
	ISoundShader* Shader = FindParam(Type, Name, Index, Param);
	if (Shader == nullptr)
	{
		return false;
	}

	GetOrCreateAutomation(Index).SetLfo(*Shader, Param, RateHz, Depth, Shape);
	return true;
}

bool SoundShaderChain::StopAutomation(SoundShaderType Type, const char* Name)
{
	if (Name == nullptr || Name[0] == 0)
	{
		const s32 Index = IndexOf(Type);
		if (Index < 0)
		{
			return false;
		}

		if (Automations[Index])
		{
			Automations[Index]->StopAll(*Items[Index]);
		}
		return true;
	}

	u32 Index = 0, Param = 0;
	ISoundShader* Shader = FindParam(Type, Name, Index, Param);
	if (Shader == nullptr)
	{
		return false;
	}

	if (Automations[Index])
	{
		Automations[Index]->Stop(*Shader, Param);
	}
	return true;
}

void SoundShaderChain::CopyFrom(const SoundShaderChain& Other)
{
	if (this == &Other)
	{
		return;
	}

	Clear();
	for (u32 Index = 0; Index < Other.Count; Index++)
	{
		const ISoundShader* Source = Other.Items[Index].get();
		if (ISoundShader* Shader = Add(Source->GetType()))
		{
			Shader->CopyParams(*Source);
			if (Other.Automations[Index] && Other.Automations[Index]->IsActive())
			{
				GetOrCreateAutomation(Count - 1).CopyFrom(*Other.Automations[Index]);
			}
		}
	}
}

void SoundShaderChain::Reset()
{
	for (u32 Index = 0; Index < Count; Index++)
	{
		Items[Index]->Reset();
	}
}

void SoundShaderChain::Process(float** Data, u32 Frames, const SoundShaderContext& Context)
{
	for (u32 Index = 0; Index < Count; Index++)
	{
		ISoundShader& Shader = *Items[Index];
		XRay::Sound::Shader::Automation* Auto = Automations[Index].get();
		if (Auto == nullptr || !Auto->IsActive())
		{
			Shader.Process(Data, Frames, Context);
			continue;
		}

		// Automated parameters move every AutomationStep frames
		for (u32 Offset = 0; Offset < Frames;)
		{
			float* Piece[SND_CHANNEL_COUNT];
			for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
			{
				Piece[Channel] = Data[Channel] + Offset;
			}

			if (!Auto->IsActive())
			{
				SoundShaderContext PieceContext = Context;
				PieceContext.Offset = Context.Offset + Offset;
				Shader.Process(Piece, Frames - Offset, PieceContext);
				break;
			}

			const u32 PieceFrames = std::min(XRay::Sound::Shader::AutomationStep, Frames - Offset);
			SoundShaderContext PieceContext = Context;
			PieceContext.Offset = Context.Offset + Offset;
			Auto->Apply(Shader);
			Shader.Process(Piece, PieceFrames, PieceContext);
			Auto->Step(Shader, PieceFrames);
			Offset += PieceFrames;
		}
	}
}

u32 SoundShaderChain::GetTailFrames() const
{
	u32 Tail = 0;
	for (u32 Index = 0; Index < Count; Index++)
	{
		Tail = std::max(Tail, Items[Index]->GetTailFrames());
	}
	return Tail;
}
