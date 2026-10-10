#include "StdAfx.h"
#include "WatchDevice.h"
#include "WatchInternal.h"

namespace
{
enum class EWatchFieldType : u8
{
	Float,
	Bool,
	String,
	Vector2,
	Vector3,
	Vector4
};

struct SWatchFieldRef
{
	EWatchFieldType Type = EWatchFieldType::Float;
	void* Ptr = nullptr;
	const char* Key = nullptr;
	float Min = -flt_max;
	float Max = flt_max;
};

class CWatchFieldCollector
{
public:
	explicit CWatchFieldCollector(xr_vector<SWatchFieldRef>& Out)
		: Fields(Out) {}

	void Field(float& Value, const char* Key, float Min = -flt_max, float Max = flt_max)
	{
		Fields.push_back({EWatchFieldType::Float, &Value, Key, Min, Max});
	}

	void Field(bool& Value, const char* Key)
	{
		Fields.push_back({EWatchFieldType::Bool, &Value, Key});
	}

	void Field(shared_str& Value, const char* Key)
	{
		Fields.push_back({EWatchFieldType::String, &Value, Key});
	}

	void Field(Fvector2& Value, const char* Key)
	{
		Fields.push_back({EWatchFieldType::Vector2, &Value, Key});
	}

	void Field(Fvector& Value, const char* Key)
	{
		Fields.push_back({EWatchFieldType::Vector3, &Value, Key});
	}

	void Field(Fvector4& Value, const char* Key)
	{
		Fields.push_back({EWatchFieldType::Vector4, &Value, Key});
	}

private:
	xr_vector<SWatchFieldRef>& Fields;
};

template <class T>
T& FieldAs(const SWatchFieldRef& Ref)
{
	return *static_cast<T*>(Ref.Ptr);
}

template <class Block>
void CollectFields(Block& Value, xr_vector<SWatchFieldRef>& Out)
{
	Out.clear();
	CWatchFieldCollector Collector(Out);
	Value.ForEach(Collector);
}

void LoadField(const CInifile& Ini, const char* SectionName, const SWatchFieldRef& Ref)
{
	switch (Ref.Type)
	{
		case EWatchFieldType::Float:
		{
			float& Value = FieldAs<float>(Ref);
			Value = clampr(Ini.read_if_exists<float>(SectionName, Ref.Key, Value), Ref.Min, Ref.Max);
			break;
		}
		case EWatchFieldType::Bool:
		{
			bool& Value = FieldAs<bool>(Ref);
			Value = Ini.read_if_exists<bool>(SectionName, Ref.Key, Value);
			break;
		}
		case EWatchFieldType::String:
		{
			shared_str& Value = FieldAs<shared_str>(Ref);
			if (Ini.line_exist(SectionName, Ref.Key))
			{
				Value = Ini.r_string(SectionName, Ref.Key);
			}
			break;
		}
		case EWatchFieldType::Vector2:
		{
			Fvector2& Value = FieldAs<Fvector2>(Ref);
			Value = Ini.read_if_exists<Fvector2>(SectionName, Ref.Key, Value);
			break;
		}
		case EWatchFieldType::Vector3:
		{
			Fvector& Value = FieldAs<Fvector>(Ref);
			Value = Ini.read_if_exists<Fvector>(SectionName, Ref.Key, Value);
			break;
		}
		case EWatchFieldType::Vector4:
		{
			Fvector4& Value = FieldAs<Fvector4>(Ref);
			Value = Ini.read_if_exists<Fvector4>(SectionName, Ref.Key, Value);
			break;
		}
	}
}

bool FieldsDiffer(const SWatchFieldRef& A, const SWatchFieldRef& B)
{
	switch (A.Type)
	{
		case EWatchFieldType::Float:
			return !fsimilar(FieldAs<float>(A), FieldAs<float>(B));
		case EWatchFieldType::Bool:
			return FieldAs<bool>(A) != FieldAs<bool>(B);
		case EWatchFieldType::String:
			return !FieldAs<shared_str>(A).equal(FieldAs<shared_str>(B));
		case EWatchFieldType::Vector2:
			return !FieldAs<Fvector2>(A).similar(FieldAs<Fvector2>(B), EPS);
		case EWatchFieldType::Vector3:
			return !FieldAs<Fvector>(A).similar(FieldAs<Fvector>(B), EPS);
		case EWatchFieldType::Vector4:
		{
			const Fvector4& ValueA = FieldAs<Fvector4>(A);
			const Fvector4& ValueB = FieldAs<Fvector4>(B);
			return !(fsimilar(ValueA.x, ValueB.x) && fsimilar(ValueA.y, ValueB.y) && fsimilar(ValueA.z, ValueB.z) && fsimilar(ValueA.w, ValueB.w));
		}
	}
	return false;
}

void WriteField(CInifile& File, const char* SectionName, const SWatchFieldRef& Ref)
{
	switch (Ref.Type)
	{
		case EWatchFieldType::Float:
			File.w_float(SectionName, Ref.Key, FieldAs<float>(Ref));
			break;
		case EWatchFieldType::Bool:
			File.w_bool(SectionName, Ref.Key, FieldAs<bool>(Ref));
			break;
		case EWatchFieldType::String:
		{
			const shared_str& Value = FieldAs<shared_str>(Ref);
			File.w_string(SectionName, Ref.Key, Value.size() ? Value.c_str() : "");
			break;
		}
		case EWatchFieldType::Vector2:
			File.w_fvector2(SectionName, Ref.Key, FieldAs<Fvector2>(Ref));
			break;
		case EWatchFieldType::Vector3:
			File.w_fvector3(SectionName, Ref.Key, FieldAs<Fvector>(Ref));
			break;
		case EWatchFieldType::Vector4:
			File.w_fvector4(SectionName, Ref.Key, FieldAs<Fvector4>(Ref));
			break;
	}
}

template <class Block>
void LoadBlock(const CInifile& Ini, const shared_str& SectionName, Block& Value, xr_vector<SWatchFieldRef>& Scratch)
{
	if (!Ini.section_exist(SectionName.c_str()))
	{
		return;
	}

	CollectFields(Value, Scratch);
	for (const SWatchFieldRef& Ref : Scratch)
	{
		LoadField(Ini, SectionName.c_str(), Ref);
	}
}

template <class Block>
bool IsBlockDirty(Block& A, Block& B, xr_vector<SWatchFieldRef>& ScratchA, xr_vector<SWatchFieldRef>& ScratchB)
{
	CollectFields(A, ScratchA);
	CollectFields(B, ScratchB);
	for (size_t Index = 0; Index < ScratchA.size(); ++Index)
	{
		if (FieldsDiffer(ScratchA[Index], ScratchB[Index]))
		{
			return true;
		}
	}
	return false;
}

template <class Block>
void WriteBlock(CInifile& File, const shared_str& SectionName, Block& A, Block& B, bool ChangedOnly, xr_vector<SWatchFieldRef>& ScratchA, xr_vector<SWatchFieldRef>& ScratchB)
{
	CollectFields(A, ScratchA);
	CollectFields(B, ScratchB);
	for (size_t Index = 0; Index < ScratchA.size(); ++Index)
	{
		if (!ChangedOnly || FieldsDiffer(ScratchA[Index], ScratchB[Index]))
		{
			WriteField(File, SectionName.c_str(), ScratchA[Index]);
		}
	}
}
}

void CWatchDevice::LoadConfig(const CInifile& Ini, const shared_str& Root, bool PersistentOnly)
{
	xr_vector<SWatchFieldRef> Scratch;
	WatchDetail::VisitSections(Config, Config, [&](const char* Suffix, auto& Block, auto&, bool Persistent)
							   {
		if (PersistentOnly && !Persistent)
		{
			return;
		}
		LoadBlock(Ini, WatchDetail::MakeWatchSection(Root, Suffix), Block, Scratch); });
}

void CWatchDevice::ApplyIndicatorMasters()
{
	for (u32 Index = 0; Index < WatchLedChannelCount; ++Index)
	{
		Config.Present[Index].Enabled = Config.Masters.Enabled[Index] && Config.Present[Index].Enabled;
	}
}

void CWatchDevice::ApplyConditionMasters()
{
	for (u32 Index = 0; Index < WatchConditionCount; ++Index)
	{
		Config.Conditions[Index].Enabled =
			Config.ConditionMasters.Enabled[Index] && Config.Conditions[Index].Enabled;
	}
}

bool CWatchDevice::ResolveOverridePath(string_path& Out) const
{
	FS.update_path(Out, "$game_config$", "debug\\watch_override.ltx");
	if (FS.exist(Out))
	{
		return true;
	}

	FS.update_path(
		Out,
		"$arch_dir_addons$",
		"ixray-secret-weapon-pack\\configs\\debug\\watch_override.ltx"
	);
	return FS.exist(Out) != nullptr;
}

void CWatchDevice::ApplyOverrideFromDisk()
{
	if (!ResolveOverridePath(OverridePath))
	{
		return;
	}

	CInifile File(OverridePath, true, true, false);
	LoadConfig(File, Section, true);
}

void CWatchDevice::FormatDirtySections(string1024& Out)
{
	Out[0] = 0;
	xr_vector<SWatchFieldRef> ScratchA;
	xr_vector<SWatchFieldRef> ScratchB;
	const char* LastSuffix = nullptr;
	WatchDetail::VisitSections(Config, ConfigBaseline, [&](const char* Suffix, auto& A, auto& B, bool Persistent)
							   {
		if (!Persistent || !IsBlockDirty(A, B, ScratchA, ScratchB))
		{
			return;
		}
		if (LastSuffix && !xr_strcmp(LastSuffix, Suffix))
		{
			return;
		}
		LastSuffix = Suffix;
		if (Out[0])
		{
			xr_strcat(Out, " ");
		}
		xr_strcat(Out, Suffix[0] ? Suffix : "watch"); });
	if (!Out[0])
	{
		xr_strcpy(Out, "none");
	}
}

bool CWatchDevice::IsAnyDirty()
{
	xr_vector<SWatchFieldRef> ScratchA;
	xr_vector<SWatchFieldRef> ScratchB;
	bool Dirty = false;
	WatchDetail::VisitSections(Config, ConfigBaseline, [&](const char*, auto& A, auto& B, bool Persistent)
							   {
		if (Dirty || !Persistent)
		{
			return;
		}
		Dirty = IsBlockDirty(A, B, ScratchA, ScratchB); });
	return Dirty;
}

void CWatchDevice::HotSaveChanged()
{
	if (!Loaded)
	{
		xr_strcpy(LastHotSaveStatus, "HotSave failed: watch not loaded");
		return;
	}

	if (!IsAnyDirty())
	{
		xr_strcpy(LastHotSaveStatus, "HotSave Changed: nothing dirty");
		Msg("! [watch] %s", LastHotSaveStatus);
		return;
	}

	WriteOverride(true, "Saved changed");
}

void CWatchDevice::HotSaveAll()
{
	if (!Loaded)
	{
		xr_strcpy(LastHotSaveStatus, "HotSave failed: watch not loaded");
		return;
	}

	WriteOverride(false, "Saved all");
}

void CWatchDevice::WriteOverride(bool ChangedOnly, const char* Label)
{
	const bool LoadExisting = ResolveOverridePath(OverridePath);
	VerifyPath(OverridePath);

	{
		CInifile File(OverridePath, false, LoadExisting, true);
		File.set_override_names(true);

		xr_vector<SWatchFieldRef> ScratchA;
		xr_vector<SWatchFieldRef> ScratchB;
		WatchDetail::VisitSections(Config, ConfigBaseline, [&](const char* Suffix, auto& A, auto& B, bool Persistent)
								   {
			if (!Persistent)
			{
				return;
			}
			WriteBlock(File, WatchDetail::MakeWatchSection(Section, Suffix), A, B, ChangedOnly, ScratchA, ScratchB); });
	}
	ConfigBaseline = Config;

	xr_sprintf(LastHotSaveStatus, "%s -> %s", Label, OverridePath);
	Msg("[watch] %s", LastHotSaveStatus);
}
