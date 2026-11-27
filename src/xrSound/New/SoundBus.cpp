/**************************************************************************************
 * Copyright (C) 2026 Anton Kovalev (vertver)
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
#include "SoundBus.h"
#include "SoundConfig.h"
#include "SoundEffect.h"
#include "SoundDSP.h"

#define SND_ZONE_RAY_RANGE (1000.0f)
#define SND_ZONE_REVERB_GAIN (0.005f)
// Blocks a bus keeps being captured after the last Snd_ReadBusScope (about one second)
#define SND_SCOPE_HOLD_BLOCKS (48)

struct SoundBusState
{
	SoundBus Buses[SND_BUS_COUNT];
	// Runtime shaders, applied before the bus effects
	SoundShaderChain Shaders[SND_BUS_COUNT];
	// Voices that skip the runtime shaders, added after them
	float DryData[SND_BUS_COUNT][SND_CHANNEL_COUNT][SND_BLOCKSIZE] = {};
	bool HasDryInput[SND_BUS_COUNT] = {};
	u32 Order[SND_BUS_COUNT] = {};
	u32 OrderCount = 0;
	u32 Master = 0;
	xr_vector<sound_zone_params> Zones;
	xr_hash_map<shared_str, shared_str> ZoneBuses;
	xr_vector<SoundDynamicBusInfo> DynamicHistory;
	bool IsEditorZone = false;
};

static SoundBusState GBuses = {};

// Last SND_SCOPE_FRAMES of the buses someone looks at (Mixer::GetBusSpectrum): per channel, after the effects and
// the fader, a ring written one block at a time
struct SoundBusScope
{
	xrCriticalSection Lock;
	xr_atomic_u32 Requests[SND_BUS_COUNT] = {};
	u32 Head[SND_BUS_COUNT] = {};
	float Data[SND_BUS_COUNT][SND_CHANNEL_COUNT][SND_SCOPE_FRAMES] = {};
};

static SoundBusScope GScope;

static void Snd_CaptureScopes(const bool* IsRendered)
{
	bool HasRequests = false;
	for (u32 Index = 0; Index < SND_BUS_COUNT && !HasRequests; Index++)
	{
		HasRequests = GScope.Requests[Index].load(std::memory_order_relaxed) != 0;
	}

	if (!HasRequests)
	{
		return;
	}

	xrCriticalSectionGuard Guard(GScope.Lock);
	for (u32 Index = 0; Index < SND_BUS_COUNT; Index++)
	{
		if (GScope.Requests[Index].load(std::memory_order_relaxed) == 0)
		{
			continue;
		}

		GScope.Requests[Index].fetch_sub(1, std::memory_order_relaxed);
		const SoundBus& Bus = GBuses.Buses[Index];
		const bool IsSilent = !IsRendered[Index] || !Bus.IsUsed;
		const float Gain = std::max(Bus.Gain, 0.0f);
		const u32 Head = GScope.Head[Index];
		for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			float* Scope = GScope.Data[Index][Channel] + Head;
			for (u32 Frame = 0; Frame < SND_BLOCKSIZE; Frame++)
			{
				Scope[Frame] = IsSilent ? 0.0f : Bus.Data[Channel][Frame] * Gain;
			}
		}

		GScope.Head[Index] = (Head + SND_BLOCKSIZE) % SND_SCOPE_FRAMES;
	}
}

bool Snd_ReadBusScope(u32 BusIdx, u32 Channel, float* Out, u32 Frames)
{
	if (Snd_GetBus(BusIdx) == nullptr || Channel > SND_SCOPE_MID || Frames == 0 || Frames > SND_SCOPE_FRAMES)
	{
		return false;
	}

	// The first read after a pause sees old frames until the ring is rewritten
	const u32 Index = BusIdx - 1;
	GScope.Requests[Index].store(SND_SCOPE_HOLD_BLOCKS, std::memory_order_relaxed);
	xrCriticalSectionGuard Guard(GScope.Lock);
	const u32 Start = (GScope.Head[Index] + SND_SCOPE_FRAMES - Frames) % SND_SCOPE_FRAMES;
	for (u32 Frame = 0; Frame < Frames; Frame++)
	{
		const u32 Ring = (Start + Frame) % SND_SCOPE_FRAMES;
		Out[Frame] = Channel == SND_SCOPE_MID ? (GScope.Data[Index][0][Ring] + GScope.Data[Index][1][Ring]) * 0.5f : GScope.Data[Index][Channel][Ring];
	}

	return true;
}

u32 Snd_ReserveBus(const char* Name)
{
	shared_str BusName = Name;
	u32 FreeIdx = 0;
	for (u32 BusIdx = 0; BusIdx < SND_BUS_COUNT; BusIdx++)
	{
		if (GBuses.Buses[BusIdx].Name == BusName)
		{
			return BusIdx + 1;
		}

		if (FreeIdx == 0 && GBuses.Buses[BusIdx].Name.size() == 0)
		{
			FreeIdx = BusIdx + 1;
		}
	}

	if (FreeIdx == 0)
	{
		Msg("! [Sound] Too many buses, '%s' skipped", Name);
		return 0;
	}

	GBuses.Buses[FreeIdx - 1].Name = BusName;
	return FreeIdx;
}

u32 Snd_FindBus(const char* Name)
{
	shared_str BusName = Name;
	for (u32 BusIdx = 0; BusIdx < SND_BUS_COUNT; BusIdx++)
	{
		if (GBuses.Buses[BusIdx].IsUsed && GBuses.Buses[BusIdx].Name == BusName)
		{
			return BusIdx + 1;
		}
	}

	return 0;
}

u32 Snd_GetMasterBus()
{
	if (GBuses.Master == 0)
	{
		GBuses.Master = Snd_ReserveBus("master");
	}

	return GBuses.Master;
}

SoundBus* Snd_GetBus(u32 BusIdx)
{
	return (BusIdx == 0 || BusIdx > SND_BUS_COUNT || !GBuses.Buses[BusIdx - 1].IsUsed) ? nullptr : &GBuses.Buses[BusIdx - 1];
}

SoundBus* Snd_GetBuses()
{
	return GBuses.Buses;
}

SoundShaderChain* Snd_GetBusShaders(u32 BusIdx)
{
	return Snd_GetBus(BusIdx) != nullptr ? &GBuses.Shaders[BusIdx - 1] : nullptr;
}

// Keeps the name, the user fader and the runtime routing (config reloads go through here)
static void Snd_ClearBus(SoundBus* Bus)
{
	for (u32 EffectIdx = 0; EffectIdx < Bus->EffectCount; EffectIdx++)
	{
		Snd_DestroyEffect(&Bus->Effects[EffectIdx]);
	}

	shared_str Name = Bus->Name;
	float UserVolume = Bus->UserVolume;
	u32 Route = Bus->Route;
	u32 DynamicOwners = Bus->DynamicOwners;
	bool IsDynamic = Bus->IsDynamic;
	*Bus = SoundBus();
	Bus->Name = Name;
	Bus->UserVolume = UserVolume;
	Bus->Route = Route;
	Bus->DynamicOwners = DynamicOwners;
	Bus->IsDynamic = IsDynamic;
}

// Drops everything of a runtime bus (zone, dynamic) and the routes into it; the slot can be reserved again
static void Snd_FreeBus(u32 BusIdx)
{
	SoundBus* Bus = &GBuses.Buses[BusIdx - 1];
	Snd_ClearBus(Bus);
	*Bus = SoundBus();
	GBuses.Shaders[BusIdx - 1].Clear();
	GBuses.HasDryInput[BusIdx - 1] = false;

	for (u32 OtherIdx = 0; OtherIdx < SND_BUS_COUNT; OtherIdx++)
	{
		if (GBuses.Buses[OtherIdx].Route == BusIdx)
		{
			GBuses.Buses[OtherIdx].Route = 0;
		}
	}
}

static u32 Snd_ParseEffects(SoundBusEffect* Effects, u32 Capacity, SoundEffectScope Scope, const char* List)
{
	u32 Count = 0;
	u32 ItemCount = (u32)_GetItemCount(List);
	for (u32 ItemIdx = 0; ItemIdx < ItemCount && Count < Capacity; ItemIdx++)
	{
		string128 Item;
		_GetItem(List, ItemIdx, Item);

		u8 Id = 0;
		u32 AlternativeCount = (u32)_GetItemCount(Item, '|');
		for (u32 AlternativeIdx = 0; AlternativeIdx < AlternativeCount && Id == 0; AlternativeIdx++)
		{
			string128 Alternative;
			Id = Snd_FindEffect(_GetItem(Item, AlternativeIdx, Alternative, '|'));
			Id = (Id != 0 && Snd_GetEffect(Id)->Desc.Scope == Scope) ? Id : 0;
		}

		if (Id == 0)
		{
			Msg("! [Sound] Unknown effect '%s'", Item);
			continue;
		}

		Snd_ResetEffect(&Effects[Count++], Id);
	}

	return Count;
}

static void Snd_SetEffectsParam(SoundBusEffect* Effects, u32 Count, const char* Effect, const char* Param, const char* Value)
{
	for (u32 EffectIdx = 0; EffectIdx < Count; EffectIdx++)
	{
		SoundBusEffect* Target = &Effects[EffectIdx];
		const SoundEffectEntry* Entry = Snd_GetEffect(Target->Effect);
		if (Entry == nullptr || xr_strcmp(Entry->Name.c_str(), Effect) != 0)
		{
			continue;
		}

		if (xr_strcmp(Param, "resource") == 0)
		{
			Target->Resource = Value;
		}
		else if (!Snd_SetEffectParam(Target, Param, (float)atof(Value)))
		{
			Msg("! [Sound] Unknown parameter '%s.%s'", Effect, Param);
		}
	}
}

static void Snd_CreateBusEffects(SoundBus* Bus)
{
	for (u32 EffectIdx = 0; EffectIdx < Bus->EffectCount; EffectIdx++)
	{
		Snd_CreateEffect(&Bus->Effects[EffectIdx]);
	}
}

static void Snd_ApplyBusConfig(u32 BusIdx, const SoundConfigSection* Section)
{
	SoundBus* Bus = &GBuses.Buses[BusIdx - 1];
	Snd_ClearBus(Bus);
	Bus->IsUsed = true;
	Bus->Output = BusIdx == GBuses.Master ? 0 : GBuses.Master;

	for (const CInifile::Item& Item : Section->Items)
	{
		const char* Key = Item.first.c_str();
		const char* Value = Item.second.size() ? Item.second.c_str() : "";
		if (xr_strcmp(Key, "output") == 0)
		{
			Bus->Output = Value[0] != 0 ? Snd_ReserveBus(Value) : Bus->Output;
		} else if (xr_strcmp(Key, "volume") == 0)
		{
			Bus->Volume = (float)atof(Value);
		} else if (xr_strcmp(Key, "zone_send") == 0)
		{
			Bus->ZoneSend = (float)atof(Value);
		} else if (xr_strcmp(Key, "direct_ratio") == 0)
		{
			Bus->DirectRatio = (float)atof(Value);
		} else if (xr_strcmp(Key, "far_send") == 0)
		{
			Bus->FarSend = Value[0] != 0 ? Snd_ReserveBus(Value) : 0;
		} else if (xr_strcmp(Key, "indoor_send") == 0)
		{
			Bus->IndoorSend = Value[0] != 0 ? Snd_ReserveBus(Value) : 0;
		} else if (xr_strcmp(Key, "effects") == 0)
		{
			Bus->EffectCount = Snd_ParseEffects(Bus->Effects, SND_BUS_EFFECT_COUNT, SoundEffectScope::Bus, Value);
		} else if (xr_strcmp(Key, "voice_effects") == 0)
		{
			Bus->VoiceEffectCount = Snd_ParseEffects(Bus->VoiceEffects, SND_VOICE_EFFECT_COUNT, SoundEffectScope::Voice, Value);
		}
	}

	for (const CInifile::Item& Item : Section->Items)
	{
		const char* Dot = strchr(Item.first.c_str(), '.');
		if (Dot == nullptr || Item.second.size() == 0)
		{
			continue;
		}

		string128 Effect;
		xr_strcpy(Effect, Item.first.c_str());
		Effect[Dot - Item.first.c_str()] = 0;
		Snd_SetEffectsParam(Bus->Effects, Bus->EffectCount, Effect, Dot + 1, Item.second.c_str());
		Snd_SetEffectsParam(Bus->VoiceEffects, Bus->VoiceEffectCount, Effect, Dot + 1, Item.second.c_str());
	}

	Snd_CreateBusEffects(Bus);
}

// True when the output chain that starts at From reaches BusIdx
static bool Snd_IsRoutedInto(u32 From, u32 BusIdx)
{
	u32 Cursor = From;
	for (u32 Step = 0; Step <= SND_BUS_COUNT && Cursor != 0; Step++)
	{
		if (Cursor == BusIdx)
		{
			return true;
		}

		const SoundBus* Bus = Snd_GetBus(Cursor);
		Cursor = Bus != nullptr ? Bus->GetOutput() : 0;
	}

	// A loop that misses BusIdx is a config loop, Snd_RebuildOrder breaks it
	return false;
}

static void Snd_RebuildOrder()
{
	u32 Master = GBuses.Master;
	for (u32 BusIdx = 1; BusIdx <= SND_BUS_COUNT; BusIdx++)
	{
		SoundBus* Bus = Snd_GetBus(BusIdx);
		if (Bus == nullptr)
		{
			GBuses.Buses[BusIdx - 1].Route = 0;
			continue;
		}

		if (BusIdx == Master)
		{
			Bus->Route = 0;
			continue;
		}

		if (Snd_GetBus(Bus->Output) == nullptr)
		{
			Bus->Output = Master;
		}

		if (Bus->Route != 0 && Snd_GetBus(Bus->Route) == nullptr)
		{
			Bus->Route = 0;
		}
	}

	// A runtime route that closes a loop is dropped, the config output stays
	for (u32 BusIdx = 1; BusIdx <= SND_BUS_COUNT; BusIdx++)
	{
		SoundBus* Bus = Snd_GetBus(BusIdx);
		if (Bus != nullptr && Bus->Route != 0 && Snd_IsRoutedInto(Bus->Route, BusIdx))
		{
			Msg("! [Sound] Route '%s' -> '%s' makes a loop, dropped", Bus->Name.c_str(), GBuses.Buses[Bus->Route - 1].Name.c_str());
			Bus->Route = 0;
		}
	}

	GBuses.OrderCount = 0;
	for (u32 BusIdx = 1; BusIdx <= SND_BUS_COUNT; BusIdx++)
	{
		SoundBus* Bus = Snd_GetBus(BusIdx);
		if (Bus == nullptr)
		{
			continue;
		}

		Bus->Depth = 0;
		for (u32 Cursor = BusIdx; Cursor != Master; Cursor = Snd_GetBus(Cursor)->GetOutput())
		{
			if (++Bus->Depth > SND_BUS_COUNT)
			{
				Msg("! [Sound] Bus '%s' output loop, routed to master", Bus->Name.c_str());
				Bus->Output = Master;
				Bus->Route = 0;
				Bus->Depth = 1;
				break;
			}
		}

		u32 InsertIdx = GBuses.OrderCount++;
		while (InsertIdx > 0 && GBuses.Buses[GBuses.Order[InsertIdx - 1] - 1].Depth < Bus->Depth)
		{
			GBuses.Order[InsertIdx] = GBuses.Order[InsertIdx - 1];
			InsertIdx--;
		}

		GBuses.Order[InsertIdx] = BusIdx;
	}
}

void Snd_AssignZoneBus(sound_zone_params* Zone)
{
	string256 Name;
	xr_strcpy(Name, Zone->name.c_str());
	xr_strlwr(Name);

	auto Found = GBuses.ZoneBuses.find(shared_str(Name));
	if (Found != GBuses.ZoneBuses.end())
	{
		Zone->bus = Snd_FindBus(Found->second.c_str());
		return;
	}

	u8 Reverb = Snd_FindEffect("reverb");
	u8 Compressor = Snd_FindEffect("compressor");
	xr_strconcat(Name, "env:", Zone->name.c_str());
	Zone->bus = Snd_FindBus(Name);
	if (Zone->bus != 0 || Reverb == 0)
	{
		return;
	}

	u32 BusIdx = Snd_ReserveBus(Name);
	if (BusIdx == 0)
	{
		return;
	}

	SoundBus* Bus = &GBuses.Buses[BusIdx - 1];
	Snd_ClearBus(Bus);
	Bus->IsUsed = true;
	Bus->IsGenerated = true;
	Bus->Output = GBuses.Master;
	Bus->EffectCount = 3;
	Snd_ResetEffect(&Bus->Effects[0], Compressor);
	Snd_ResetEffect(&Bus->Effects[1], Reverb);
	Snd_ResetEffect(&Bus->Effects[2], Compressor);

	const sound_reverb_settings& Settings = Zone->settings;
	SoundBusEffect* Effect = &Bus->Effects[1];
	Snd_SetEffectParam(Effect, "gain", std::clamp(Settings.reverb, 0.0f, 1.0f) * SND_ZONE_REVERB_GAIN);
	Snd_SetEffectParam(Effect, "size", Settings.environment_size);
	Snd_SetEffectParam(Effect, "cutoff", 5000.0f * Settings.room_hf);
	Snd_SetEffectParam(Effect, "diffusion", Settings.environment_diffusion);
	Snd_SetEffectParam(Effect, "reflections", powf(10.0f, Settings.reflections / 2000.0f));
	Snd_SetEffectParam(Effect, "decay", Settings.decay_time);
	Snd_SetEffectParam(Effect, "decay_hf", Settings.decay_hf_ratio);
	Snd_SetEffectParam(Effect, "air_hf", Settings.air_absorption_hf);

	Snd_CreateBusEffects(Bus);
	Zone->bus = BusIdx;
}

void Snd_InitBuses()
{
	u32 Master = Snd_GetMasterBus();

	GBuses.ZoneBuses.clear();
	for (auto& [Name, Section] : Snd_GetConfig())
	{
		const char* SectionName = Name.c_str();
		if (strstr(SectionName, "bus:") == SectionName)
		{
			u32 BusIdx = Snd_ReserveBus(SectionName + 4);
			if (BusIdx != 0)
			{
				Snd_ApplyBusConfig(BusIdx, &Section);
			}
		}
		else if (strstr(SectionName, "zone:") == SectionName && Snd_ConfigValue(&Section, "bus") != nullptr)
		{
			GBuses.ZoneBuses[shared_str(SectionName + 5)] = Snd_ConfigValue(&Section, "bus");
		}
	}

	if (Snd_GetBus(Master) == nullptr)
	{
		SoundConfigSection Empty;
		Snd_ApplyBusConfig(Master, &Empty);
	}

	for (sound_zone_params& Zone : GBuses.Zones)
	{
		Snd_AssignZoneBus(&Zone);
	}

	Snd_RebuildOrder();
}

void Snd_ShutdownBuses()
{
	for (u32 BusIdx = 0; BusIdx < SND_BUS_COUNT; BusIdx++)
	{
		SoundBus* Bus = &GBuses.Buses[BusIdx];
		if (Bus->IsDynamic)
		{
			Snd_FreeBus(BusIdx + 1);
			continue;
		}

		Snd_ClearBus(Bus);
		Bus->Route = 0;
		GBuses.Shaders[BusIdx].Clear();
	}

	GBuses.OrderCount = 0;
	GBuses.ZoneBuses.clear();
	GBuses.DynamicHistory.clear();
}

void Snd_SetBusParam(u32 BusIdx, const char* Effect, const char* Param, float Value)
{
	SoundBus* Bus = Snd_GetBus(BusIdx);
	for (u32 EffectIdx = 0; Bus != nullptr && EffectIdx < Bus->EffectCount; EffectIdx++)
	{
		const SoundEffectEntry* Entry = Snd_GetEffect(Bus->Effects[EffectIdx].Effect);
		if (Entry != nullptr && xr_strcmp(Entry->Name.c_str(), Effect) == 0)
		{
			Snd_SetEffectParam(&Bus->Effects[EffectIdx], Param, Value);
		}
	}
}

static SoundConfigSection* Snd_EditBusConfig(const SoundBus* Bus)
{
	string128 Name;
	xr_strconcat(Name, "bus:", Bus->Name.c_str());
	return Snd_EditConfig(Name);
}

u32 Snd_CreateBus(const char* Name)
{
	u32 BusIdx = Snd_ReserveBus(Name);
	if (BusIdx == 0 || GBuses.Buses[BusIdx - 1].IsUsed)
	{
		return BusIdx;
	}

	SoundConfigSection* Section = Snd_EditBusConfig(&GBuses.Buses[BusIdx - 1]);
	Section->IsDeleted = false;
	Snd_ApplyBusConfig(BusIdx, Section);
	Snd_RebuildOrder();
	return BusIdx;
}

void Snd_DeleteBus(u32 BusIdx)
{
	SoundBus* Bus = Snd_GetBus(BusIdx);
	if (Bus == nullptr || BusIdx == GBuses.Master || Bus->IsRuntime())
	{
		return;
	}

	SoundConfigSection* Section = Snd_EditBusConfig(Bus);
	Section->Items.clear();
	Section->IsDeleted = true;
	Snd_ClearBus(Bus);
	GBuses.Shaders[BusIdx - 1].Clear();
	Snd_RebuildOrder();
}

void Snd_SetBusValue(u32 BusIdx, const char* Key, const char* Value)
{
	SoundBus* Bus = Snd_GetBus(BusIdx);
	if (Bus == nullptr || Bus->IsRuntime())
	{
		return;
	}

	SoundConfigSection* Section = Snd_EditBusConfig(Bus);
	Snd_SetConfigValue(Section, Key, Value);

	float* Scalars[] = {&Bus->Volume, &Bus->ZoneSend, &Bus->DirectRatio};
	const char* ScalarKeys[] = {"volume", "zone_send", "direct_ratio"};
	for (u32 ScalarIdx = 0; ScalarIdx < std::size(Scalars); ScalarIdx++)
	{
		if (xr_strcmp(Key, ScalarKeys[ScalarIdx]) == 0)
		{
			*Scalars[ScalarIdx] = (float)atof(Value);
			return;
		}
	}

	Snd_ApplyBusConfig(BusIdx, Section);
	Snd_RebuildOrder();
}

void Snd_SetBusEffectParam(u32 BusIdx, bool IsVoice, u32 EffectIdx, u32 ParamIdx, float Value)
{
	SoundBus* Bus = Snd_GetBus(BusIdx);
	if (Bus == nullptr || EffectIdx >= (IsVoice ? Bus->VoiceEffectCount : Bus->EffectCount))
	{
		return;
	}

	SoundBusEffect* Effect = IsVoice ? &Bus->VoiceEffects[EffectIdx] : &Bus->Effects[EffectIdx];
	const SoundEffectEntry* Entry = Snd_GetEffect(Effect->Effect);
	if (Entry == nullptr || ParamIdx >= Entry->Desc.ParamCount)
	{
		return;
	}

	const char* ParamName = Entry->Desc.Params[ParamIdx].Name;
	Snd_SetEffectParam(Effect, ParamName, Value);
	if (Bus->IsRuntime())
	{
		return;
	}

	string128 Key;
	string32 Text;
	xr_strconcat(Key, Entry->Name.c_str(), ".", ParamName);
	xr_sprintf(Text, "%g", Effect->Params[ParamIdx]);
	Snd_SetConfigValue(Snd_EditBusConfig(Bus), Key, Text);
}

const char* Snd_GetBusValue(u32 BusIdx, const char* Key)
{
	SoundBus* Bus = Snd_GetBus(BusIdx);
	if (Bus == nullptr)
	{
		return nullptr;
	}

	string128 Name;
	xr_strconcat(Name, "bus:", Bus->Name.c_str());
	const SoundConfigSection* Section = Snd_FindConfig(Name);
	return Section != nullptr ? Snd_ConfigValue(Section, Key) : nullptr;
}

bool Snd_SaveBus(u32 BusIdx)
{
	if (BusIdx == 0 || BusIdx > SND_BUS_COUNT || GBuses.Buses[BusIdx - 1].IsRuntime())
	{
		return false;
	}

	string128 Name;
	xr_strconcat(Name, "bus:", GBuses.Buses[BusIdx - 1].Name.c_str());
	return Snd_SaveConfig(Name);
}

static SoundDynamicBusInfo* Snd_FindDynamicRecord(const shared_str& Name)
{
	for (SoundDynamicBusInfo& Info : GBuses.DynamicHistory)
	{
		if (Info.Name == Name)
		{
			return &Info;
		}
	}

	return nullptr;
}

static void Snd_RecordDynamicBus(u32 BusIdx, bool IsReleased)
{
	const SoundBus& Bus = GBuses.Buses[BusIdx - 1];
	SoundDynamicBusInfo* Info = Snd_FindDynamicRecord(Bus.Name);
	if (Info == nullptr)
	{
		Info = &GBuses.DynamicHistory.emplace_back();
		Info->Name = Bus.Name;
	}

	const SoundBus* Output = Snd_GetBus(Bus.GetOutput());
	Info->Output = Output != nullptr ? Output->Name : shared_str();
	Info->Bus = IsReleased ? 0 : BusIdx;
	Info->CreateCount += IsReleased ? 0 : 1;
	if (!IsReleased)
	{
		return;
	}

	string256 Shaders = {};
	const SoundShaderChain& Chain = GBuses.Shaders[BusIdx - 1];
	for (u32 ShaderIdx = 0; ShaderIdx < Chain.GetCount(); ShaderIdx++)
	{
		xr_strcat(Shaders, ShaderIdx == 0 ? "" : ", ");
		xr_strcat(Shaders, Chain.Get(ShaderIdx)->GetClass().Name);
	}
	Info->Shaders = Shaders;
}

void Snd_GetDynamicBuses(xr_vector<SoundDynamicBusInfo>& Out)
{
	Out = GBuses.DynamicHistory;
}

void Snd_ForgetDynamicBuses()
{
	xr_vector<SoundDynamicBusInfo>& History = GBuses.DynamicHistory;
	History.erase(std::remove_if(History.begin(), History.end(), [](const SoundDynamicBusInfo& Info) { return Info.Bus == 0; }), History.end());
}

u32 Snd_AcquireDynamicBus(const char* Name, u32 Output)
{
	if (Name == nullptr || Name[0] == 0)
	{
		return 0;
	}

	u32 BusIdx = Snd_FindBus(Name);
	if (BusIdx != 0)
	{
		// A config bus of that name is used as is, it has no owners
		SoundBus* Bus = &GBuses.Buses[BusIdx - 1];
		Bus->DynamicOwners += Bus->IsDynamic ? 1 : 0;
		return BusIdx;
	}

	BusIdx = Snd_ReserveBus(Name);
	if (BusIdx == 0)
	{
		return 0;
	}

	SoundBus* Bus = &GBuses.Buses[BusIdx - 1];
	Snd_ClearBus(Bus);
	Bus->IsUsed = true;
	Bus->IsDynamic = true;
	Bus->DynamicOwners = 1;
	Bus->Route = 0;
	Bus->Output = (Output != BusIdx && Snd_GetBus(Output) != nullptr) ? Output : GBuses.Master;
	GBuses.Shaders[BusIdx - 1].Clear();
	GBuses.HasDryInput[BusIdx - 1] = false;
	Snd_RebuildOrder();
	Snd_RecordDynamicBus(BusIdx, false);
	return BusIdx;
}

bool Snd_ReleaseDynamicBus(u32 BusIdx)
{
	SoundBus* Bus = Snd_GetBus(BusIdx);
	if (Bus == nullptr || !Bus->IsDynamic || Bus->DynamicOwners == 0)
	{
		return false;
	}

	if (--Bus->DynamicOwners != 0)
	{
		return false;
	}

	Snd_RecordDynamicBus(BusIdx, true);
	Snd_FreeBus(BusIdx);
	Snd_RebuildOrder();
	return true;
}

void Snd_RouteBus(u32 BusIdx, u32 Target)
{
	SoundBus* Bus = Snd_GetBus(BusIdx);
	if (Bus == nullptr || BusIdx == GBuses.Master || (Target != 0 && Snd_GetBus(Target) == nullptr))
	{
		return;
	}

	Bus->Route = Target == BusIdx ? 0 : Target;
	Snd_RebuildOrder();
}

void Snd_BeginBuses()
{
	for (u32 OrderIdx = 0; OrderIdx < GBuses.OrderCount; OrderIdx++)
	{
		SoundBus* Bus = &GBuses.Buses[GBuses.Order[OrderIdx] - 1];
		if (Bus->TailFrames != 0)
		{
			memset(Bus->Data, 0, sizeof(Bus->Data));
		}

		Bus->HasInput = false;
	}
}

static float** Snd_OpenBus(SoundBus* Bus, float** Data)
{
	if (!Bus->HasInput && Bus->TailFrames == 0)
	{
		memset(Bus->Data, 0, sizeof(Bus->Data));
	}

	Bus->HasInput = true;
	Data[0] = Bus->Data[0];
	Data[1] = Bus->Data[1];
	return Data;
}

bool Snd_SendToBus(u32 BusIdx, float** Data, float BeginFactor, float EndFactor, float Left, float Right, u8 VoiceFlags)
{
	SoundBus* Bus = Snd_GetBus(BusIdx);
	if (Bus == nullptr)
	{
		return false;
	}

	if (BeginFactor <= 0.0f && EndFactor <= 0.0f)
	{
		return true;
	}

	float* BusData[SND_CHANNEL_COUNT];
	Snd_OpenBus(Bus, BusData);

	// Only a bus with runtime shaders needs the separate dry mix
	const u32 Index = BusIdx - 1;
	const SoundShaderChain& Shaders = GBuses.Shaders[Index];
	const bool IsSpatial = (VoiceFlags & (u8)XRay::Sound::Mixer::Flags::Spatial) != 0;
	const bool IsDry = (VoiceFlags & (u8)XRay::Sound::Mixer::Flags::NoBusShaders) != 0 || (!IsSpatial && Shaders.HasSpatialOnly());
	if (IsDry && !Shaders.IsEmpty())
	{
		if (!GBuses.HasDryInput[Index])
		{
			memset(GBuses.DryData[Index], 0, sizeof(GBuses.DryData[Index]));
			GBuses.HasDryInput[Index] = true;
		}

		float* DryData[SND_CHANNEL_COUNT] = {GBuses.DryData[Index][0], GBuses.DryData[Index][1]};
		DSP_MixBufferPanning(DryData, Data, BeginFactor, EndFactor, Left, Right, SND_BLOCKSIZE);
		return true;
	}

	DSP_MixBufferPanning(BusData, Data, BeginFactor, EndFactor, Left, Right, SND_BLOCKSIZE);
	return true;
}

void Snd_RenderBuses(float* Output, float MuteVolume)
{
	memset(Output, 0, SND_BLOCKSIZE * SND_CHANNEL_COUNT * sizeof(float));

	bool IsRendered[SND_BUS_COUNT] = {};
	for (u32 OrderIdx = 0; OrderIdx < GBuses.OrderCount; OrderIdx++)
	{
		u32 BusIdx = GBuses.Order[OrderIdx];
		SoundBus* Bus = &GBuses.Buses[BusIdx - 1];
		if (!Bus->HasInput && Bus->TailFrames == 0)
		{
			Bus->Peak[0] = Bus->Peak[1] = 0.0f;
			continue;
		}

		IsRendered[BusIdx - 1] = true;

		float* Data[SND_CHANNEL_COUNT] = {Bus->Data[0], Bus->Data[1]};
		u32 TailFrames = 0;

		// Runtime shaders first, so the voices that skip them (NoBusShaders) still get the bus effects
		SoundShaderChain& Shaders = GBuses.Shaders[BusIdx - 1];
		if (!Shaders.IsEmpty())
		{
			SoundShaderContext Context = {true, Bus->HasInput};
			Shaders.Process(Data, SND_BLOCKSIZE, Context);
			TailFrames = std::max(TailFrames, Shaders.GetTailFrames());
		}

		if (GBuses.HasDryInput[BusIdx - 1])
		{
			GBuses.HasDryInput[BusIdx - 1] = false;
			for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
			{
				const float* Dry = GBuses.DryData[BusIdx - 1][Channel];
				for (u32 Frame = 0; Frame < SND_BLOCKSIZE; Frame++)
				{
					Data[Channel][Frame] += Dry[Frame];
				}
			}
		}

		SoundEffectProcess Process = {Data, nullptr, nullptr, nullptr, Bus->HasInput};
		for (u32 EffectIdx = 0; EffectIdx < Bus->EffectCount; EffectIdx++)
		{
			SoundBusEffect* Effect = &Bus->Effects[EffectIdx];
			Process.Params = Effect->Params;
			Snd_CallEffect(Effect->Effect, Effect->State, SoundEffectOp::Process, &Process);

			u32 EffectTail = 0;
			Snd_CallEffect(Effect->Effect, Effect->State, SoundEffectOp::Tail, &EffectTail);
			TailFrames = std::max(TailFrames, EffectTail);
		}

		Bus->TailFrames = Bus->HasInput ? TailFrames : Bus->TailFrames - std::min(Bus->TailFrames, (u32)SND_BLOCKSIZE);

		float Gain = Bus->Volume * Bus->UserVolume * (BusIdx == GBuses.Master ? MuteVolume : 1.0f);
		float BeginGain = Bus->Gain < 0.0f ? Gain : Bus->Gain;
		Bus->Gain = Gain;

		for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			float Peak = 0.0f;
			for (u32 Frame = 0; Frame < SND_BLOCKSIZE; Frame++)
			{
				Peak = std::max(Peak, std::abs(Data[Channel][Frame]));
			}

			Bus->Peak[Channel] = Peak * Gain;
		}

		SoundBus* OutputBus = Snd_GetBus(Bus->GetOutput());
		if (OutputBus != nullptr)
		{
			float* OutputData[SND_CHANNEL_COUNT];
			DSP_MixBuffer(Snd_OpenBus(OutputBus, OutputData), Data, BeginGain, Gain, SND_BLOCKSIZE);
			continue;
		}

		for (u32 Frame = 0; Frame < SND_BLOCKSIZE; Frame++)
		{
			float FrameGain = lerp(BeginGain, Gain, (float)Frame / (float)(SND_BLOCKSIZE - 1));
			for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
			{
				float& Sample = Output[Frame * SND_CHANNEL_COUNT + Channel];
				Sample = std::clamp(Sample + Data[Channel][Frame] * FrameGain, -1.0f, 1.0f);
			}
		}
	}

	Snd_CaptureScopes(IsRendered);
}

void Snd_AddZone(sound_zone_params* Params, bool IsEditor)
{
	if (IsEditor)
	{
		Snd_ResetZones();
	}

	Snd_AssignZoneBus(Params);
	GBuses.Zones.emplace_back(std::move(*Params));
	GBuses.IsEditorZone = IsEditor;
	Snd_RebuildOrder();
}

void Snd_ResetZones()
{
	for (u32 BusIdx = 0; BusIdx < SND_BUS_COUNT; BusIdx++)
	{
		if (GBuses.Buses[BusIdx].IsGenerated)
		{
			Snd_FreeBus(BusIdx + 1);
		}
	}

	GBuses.Zones.clear();
	GBuses.IsEditorZone = false;
	Snd_RebuildOrder();
}

void Snd_SwapZones(SoundZoneVector& Zones)
{
	GBuses.Zones.swap(Zones);
}

u32 Snd_FindZone(const Fvector& Position)
{
	xrSRWLockGuard GeometryGuard(::Sound->GetGeometryLock(), true);
	CDB::MODEL* EnvModel = ::Sound->get_geometry_env();
	CDB::COLLIDER* Collider = ::Sound->get_geometry_db();
	if (EnvModel == nullptr || Collider == nullptr)
	{
		return 0;
	}

	Fvector Dir = {0.0f, -1.0f, 0.0f};
	Collider->ray_options(CDB::OPT_ONLYNEAREST);
	Collider->ray_query(*EnvModel, Position, Dir, SND_ZONE_RAY_RANGE);
	if (Collider->r_count() == 0)
	{
		return 0;
	}

	auto& Res = Collider->r_any();
	u32 ZoneIdx = Res.model->get_tris()[Res.tris_id].dummy;
	return ZoneIdx < GBuses.Zones.size() ? ZoneIdx + 1 : 0;
}

u32 Snd_GetZoneBus(u32 ZoneIdx)
{
	ZoneIdx = GBuses.IsEditorZone ? 1 : ZoneIdx;
	return (ZoneIdx == 0 || ZoneIdx > GBuses.Zones.size()) ? 0 : GBuses.Zones[ZoneIdx - 1].bus;
}

const xr_vector<sound_zone_params>& Snd_GetZones()
{
	return GBuses.Zones;
}
