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
	for (u32 Next = Index + 1; Next < Count; Next++)
	{
		Items[Next - 1] = std::move(Items[Next]);
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

bool SoundShaderChain::SetParam(SoundShaderType Type, const char* Name, float Value)
{
	ISoundShader* Shader = Find(Type);
	if (Shader == nullptr || !Shader->SetParam(Name, Value))
	{
		Msg("! [Sound] Unknown shader parameter '%s.%s'", XRay::Sound::Shader::GetName(Type), Name);
		return false;
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
		Items[Index]->Process(Data, Frames, Context);
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
