#include "StdAfx.h"
#include "DofManager.h"
#include "Actor.h"
#include "Inventory.h"
#include "Weapon.h"
#include "HudAnimatorManager.h"
#include "HUDManager.h"
#include "Level.h"
#include "../xrEngine/IGame_Persistent.h"
#include "ui/UIGameCustom.h"
#include "ui/UIPdaWnd.h"
#include "../xrEngine/XR_IOConsole.h"

SDofOptions g_DofOptions;

xr_unique_ptr<CInifile> DofSettings::Applied;
u64 DofSettings::Revision = 0;

const CInifile& DofSettings::Get()
{
	return Applied ? *Applied : *pSettings;
}

u64 DofSettings::GetRevision()
{
	return Revision;
}

void DofSettings::Apply(xr_unique_ptr<CInifile> Config)
{
	Applied = std::move(Config);
	SDofCameraDesc::Registry::ReloadCached();
	SHudDofDefaults::Registry::ReloadCached();
	SHudDofDesc::Registry::ReloadCached();
	++Revision;
}

void DofSettings::Clear()
{
	Applied.reset();
	Revision = 0;
}

namespace DofConfig
{
static const char* CameraEvents[] = {
	"talk", "zone", "pickable", "inventory", "pda", "hud_animator", "reload", "weapon_action",
	"aim", "aim_alt", "aim_grenade", "aim_lens", "effector"
};
static_assert(sizeof(CameraEvents) / sizeof(CameraEvents[0]) == static_cast<u32>(EDofEvent::Count));

enum ECameraFields : u8
{
	CameraFNumber = 1 << 0,
	CameraFocalLength = 1 << 1,
	CameraFocusTime = 1 << 2,
	CameraFocusDistance = 1 << 3,
	CameraTransitionTime = 1 << 4,
	CameraMode = 1 << 5,
	CameraManualFocalLength = 1 << 6,
	CameraFocusPoint = 1 << 7
};

struct SCameraFloatField
{
	const char* Key;
	float SDofCameraSettings::* Member;
	float Min;
	float Max;
	u8 Field;
};

static const SCameraFloatField CameraFloats[] = {
	{"f_number", &SDofCameraSettings::FNumber, 0.7f, 64.0f, CameraFNumber},
	{"focal_length", &SDofCameraSettings::FocalLength, 1.0f, 1000.0f, CameraFocalLength},
	{"focus_time", &SDofCameraSettings::FocusTime, 0.01f, 10.0f, CameraFocusTime},
	{"focus_distance", &SDofCameraSettings::FocusDistance, 0.01f, 10000.0f, CameraFocusDistance},
	{"transition_time", &SDofCameraSettings::TransitionTime, 0.01f, 10.0f, CameraTransitionTime}
};

static bool ReadCameraBool(const shared_str& Section, const char* Key, bool& Value, const CInifile& Config)
{
	if (!Config.line_exist(Section.c_str(), Key))
	{
		return false;
	}
	const char* RawText = Config.r_string(Section, Key);
	const xr_string_view Text = RawText ? RawText : "";
	if (Text == "true" || Text == "on" || Text == "yes" || Text == "1")
	{
		Value = true;
		return true;
	}
	if (Text == "false" || Text == "off" || Text == "no" || Text == "0")
	{
		Value = false;
		return true;
	}
	Msg("! DOF: invalid [%s] %s, using the default", Section.c_str(), Key);
	return false;
}

static const SDofCameraDesc* ReadCameraProfile(const shared_str& Section, const char* Key)
{
	if (!Section.size() || !DofSettings::Get().section_exist(Section.c_str()) || !DofSettings::Get().line_exist(Section.c_str(), Key))
	{
		return nullptr;
	}
	const shared_str Profile = DofSettings::Get().r_string(Section, Key);
	if (!Profile.size() || !DofSettings::Get().section_exist(Profile.c_str()))
	{
		Msg("! DOF: missing profile [%s] referenced by [%s] %s", Profile.size() ? Profile.c_str() : "", Section.c_str(), Key);
		return nullptr;
	}
	return &SDofCameraDesc::Registry::Get(Profile);
}

static float ReadFloat(const CInifile& Config, const shared_str& Section, const char* Key, float Default)
{
	const float Value = Config.read_if_exists<float>(Section.c_str(), Key, Default);
	return _valid(Value) ? Value : Default;
}

static float ReadSpeed(const CInifile& Config, const shared_str& Section, const char* Key, float Default)
{
	return clampr(ReadFloat(Config, Section, Key, Default), 0.0f, 1000.0f);
}

static Fvector ReadVector(const CInifile& Config, const shared_str& Section, const char* Prefix, const Fvector& Default, Flags8* Fields = nullptr, Fvector* Overrides = nullptr)
{
	Fvector Result;
	const char* Suffixes[] = {"near", "focus", "far"};
	const u8 Bits[] = {DofNear, DofFocus, DofFar};
	for (u32 Index = 0; Index < 3; ++Index)
	{
		const shared_str Key = shared_str().printf("%s_%s", Prefix, Suffixes[Index]);
		Result[Index] = ReadFloat(Config, Section, Key.c_str(), Default[Index]);
		if (Fields)
		{
			Fields->set(Bits[Index], Config.line_exist(Section.c_str(), Key.c_str()));
		}
	}
	if (Overrides)
	{
		*Overrides = Result;
	}
	return Result.x <= Result.y && Result.y <= Result.z ? Result : Default;
}

static bool DefaultAction(xr_string_view Name)
{
	return Name.starts_with("anm_reload") || Name.starts_with("anm_open") ||
		   Name.starts_with("anm_close") || Name.starts_with("anm_add_cartridge");
}

static SHudDofActionDesc ReadAction(const CInifile& Config, const shared_str& Section, const shared_str& Motion, const SHudDofActionDesc& Default)
{
	SHudDofActionDesc Result = Default;
	Result.Fields.zero();
	const shared_str EnableKey = shared_str().printf("use_dof_%s", Motion.c_str());
	const bool HasEnable = Config.line_exist(Section.c_str(), EnableKey.c_str());
	Result.Fields.set(DofExplicitEnable, HasEnable);
	Result.Fields.set(DofEnabled, Config.read_if_exists<bool>(Section.c_str(), EnableKey.c_str(), Default.Fields.test(DofEnabled)));
	Result.Request.Values = ReadVector(Config, Section, shared_str().printf("dof_%s", Motion.c_str()).c_str(), Default.Request.Values, &Result.Fields, &Result.Overrides);
	const shared_str SpeedInKey = shared_str().printf("dof_speed_in_%s", Motion.c_str());
	const shared_str SpeedOutKey = shared_str().printf("dof_speed_out_%s", Motion.c_str());
	const shared_str TimeKey = shared_str().printf("dof_time_offset_%s", Motion.c_str());
	Result.Fields.set(DofSpeedIn, Config.line_exist(Section.c_str(), SpeedInKey.c_str()));
	Result.Fields.set(DofSpeedOut, Config.line_exist(Section.c_str(), SpeedOutKey.c_str()));
	Result.Fields.set(DofTimeOffset, Config.line_exist(Section.c_str(), TimeKey.c_str()));
	Result.Request.SpeedIn = ReadSpeed(Config, Section, SpeedInKey.c_str(), Default.Request.SpeedIn);
	Result.Request.SpeedOut = ReadSpeed(Config, Section, SpeedOutKey.c_str(), Default.Request.SpeedOut);
	Result.TimeOffset = ReadFloat(Config, Section, TimeKey.c_str(), Default.TimeOffset);
	return Result;
}

static void MergeAction(SHudDofActionDesc& Target, const SHudDofActionDesc& Source)
{
	if (Source.Fields.test(DofExplicitEnable))
	{
		Target.Fields.set(DofEnabled, Source.Fields.test(DofEnabled));
	}
	const u8 Bits[] = {DofNear, DofFocus, DofFar};
	for (u32 Index = 0; Index < 3; ++Index)
	{
		if (Source.Fields.test(Bits[Index]))
		{
			Target.Request.Values[Index] = Source.Overrides[Index];
		}
	}
	if (Source.Fields.test(DofSpeedIn))
	{
		Target.Request.SpeedIn = Source.Request.SpeedIn;
	}
	if (Source.Fields.test(DofSpeedOut))
	{
		Target.Request.SpeedOut = Source.Request.SpeedOut;
	}
	if (Source.Fields.test(DofTimeOffset))
	{
		Target.TimeOffset = Source.TimeOffset;
	}
}

static bool ReadLegacy(const CInifile& Config, const shared_str& Section, const char* Key, SHudDofActionDesc& Action)
{
	if (!Config.line_exist(Section.c_str(), Key))
	{
		return false;
	}
	const Fvector4 Value = Config.r_fvector4(Section.c_str(), Key);
	if (!_valid(Value.x) || !_valid(Value.y) || !_valid(Value.z) || !_valid(Value.w) ||
		Value.w < 0.0f || Value.x > Value.y || Value.y > Value.z)
	{
		return false;
	}
	Action.Request.Values.set(Value.x, Value.y, Value.z);
	Action.TimeOffset = Value.w;
	Action.Fields.set(DofEnabled, true);
	return true;
}

static shared_str MotionFromKey(xr_string_view Key)
{
	if (Key.starts_with("anm_"))
	{
		return shared_str(xr_string(Key.data(), static_cast<u32>(Key.size())).c_str());
	}
	const xr_string_view Prefixes[] = {"use_dof_", "dof_speed_in_", "dof_speed_out_", "dof_time_offset_"};
	for (const xr_string_view Prefix : Prefixes)
	{
		if (Key.starts_with(Prefix))
		{
			return shared_str(xr_string(Key.data() + Prefix.size(), static_cast<u32>(Key.size() - Prefix.size())).c_str());
		}
	}
	if (Key.starts_with("dof_"))
	{
		const xr_string_view Suffixes[] = {"_near", "_focus", "_far"};
		for (const xr_string_view Suffix : Suffixes)
		{
			if (Key.size() > 4 + Suffix.size() && Key.ends_with(Suffix))
			{
				return shared_str(xr_string(Key.data() + 4, static_cast<u32>(Key.size() - 4 - Suffix.size())).c_str());
			}
		}
	}
	return {};
}

static float ReleaseTime(const SHudDofActionDesc& Action, u32 Duration)
{
	const float Length = Duration / 1000.0f;
	float Release = flt_max;
	if (Action.TimeOffset > 0.0f)
	{
		Release = Action.TimeOffset;
	}
	else if (Duration != 0)
	{
		Release = std::max(0.0f, Length + Action.TimeOffset);
	}
	return Duration != 0 ? std::min(Release, Length) : Release;
}

static bool CameraActionActive(const SHudDofSource& Source)
{
	return Source.Playing && (Device.dwTimeGlobal - Source.StartTime) / 1000.0f < Source.CameraReleaseAfter;
}
} // namespace DofConfig

const char* CDofManager::EventName(EDofEvent Event)
{
	return Event < EDofEvent::Count ? DofConfig::CameraEvents[static_cast<u32>(Event)] : "base";
}

bool DofSettings::IsDofKey(xr_string_view Key)
{
	return Key.starts_with("ndof_profile_") || Key.starts_with("use_dof_") || Key.starts_with("dof_") ||
		   Key.starts_with("zoom_dof_") || Key.starts_with("alter_zoom_dof_") || Key.starts_with("gl_dof_") ||
		   Key.starts_with("lens_dof_") || Key.starts_with("inventory_dof_") || Key.starts_with("default_zoom_dof_") ||
		   Key.starts_with("default_action_dof_") || Key.starts_with("default_dof_") || Key == "dof" ||
		   Key == "reload_dof" || Key == "disable_zoom_dof" || Key == "zoom_in_dof_speed" ||
		   Key == "alter_zoom_in_dof_speed" || Key == "gl_in_dof_speed" || Key == "zoom_out_dof_speed";
}

shared_str DofSettings::AnimationAlias(xr_string_view Key)
{
	if (Key.starts_with("ndof_profile_anm_"))
	{
		return xr_string(Key.data() + 13, Key.size() - 13).c_str();
	}
	return DofConfig::MotionFromKey(Key);
}

bool DofSettings::Validate(const CInifile& Config, xr_string& Error)
{
	// Existing malformed mod values keep the loader's fallback behavior. Reject new errors.
	auto ChangedLambda = [&](const char* Section, const char* Key)
	{
		const char* Before = Get().r_string_nullable(Section, Key);
		const char* After = Config.r_string_nullable(Section, Key);
		return (Before == nullptr) != (After == nullptr) || xr_string_view(Before ? Before : "") != xr_string_view(After ? After : "");
	};
	auto NumberLambda = [](const char* Text, float& Value)
	{
		char Extra = 0;
		return Text && sscanf(Text, "%f %c", &Value, &Extra) == 1 && _valid(Value);
	};
	auto BoolLambda = [](xr_string_view Value)
	{
		return Value == "true" || Value == "false" || Value == "on" || Value == "off" || Value == "yes" || Value == "no" || Value == "0" || Value == "1";
	};
	SHudDofDefaults Defaults;
	Defaults.LoadFrom(Config, "gunslinger_base");
	for (const auto& Section : Config.sections())
	{
		const char* Name = Section.Name.c_str();
		const bool IsCamera = Config.line_exist(Name, "ndof_mode") || Config.line_exist(Name, "f_number") || Config.line_exist(Name, "focus_point");
		if (IsCamera)
		{
			for (const auto& Field : DofConfig::CameraFloats)
			{
				const char* Text = Config.r_string_nullable(Name, Field.Key);
				float Value = 0.0f;
				if (Text && ChangedLambda(Name, Field.Key) && (!NumberLambda(Text, Value) || Value < Field.Min || Value > Field.Max))
				{
					Error = xr_string("Invalid [") + Name + "] " + Field.Key;
					return false;
				}
			}
			if (const char* Text = Config.r_string_nullable(Name, "focus_point"); Text && ChangedLambda(Name, "focus_point"))
			{
				Fvector2 Value = {};
				char Extra = 0;
				if (sscanf(Text, "%f , %f %c", &Value.x, &Value.y, &Extra) != 2 || !_valid(Value.x) || !_valid(Value.y) || Value.x < 0 || Value.x > 1 || Value.y < 0 || Value.y > 1)
				{
					Error = xr_string("Invalid [") + Name + "] focus_point";
					return false;
				}
			}
			if (const char* Mode = Config.r_string_nullable(Name, "ndof_mode"); Mode && ChangedLambda(Name, "ndof_mode") && xr_strcmp(Mode, "manual") && xr_strcmp(Mode, "auto") && xr_strcmp(Mode, "autofocus"))
			{
				Error = xr_string("Invalid [") + Name + "] ndof_mode";
				return false;
			}
		}
		xr_hash_set<shared_str> Prefixes;
		for (const auto& Entry : Section.Data)
		{
			if (!Entry.first.size())
			{
				continue;
			}
			const char* KeyName = Entry.first.c_str();
			const xr_string_view Key = KeyName;
			const char* Text = Entry.second.size() ? Entry.second.c_str() : "";
			const bool Changed = ChangedLambda(Name, KeyName);
			if (Key.starts_with("ndof_profile_") || xr_string_view(Name) == "ndof_events")
			{
				if (Changed && (!Entry.second.size() || !Config.section_exist(Text)))
				{
					Error = xr_string("Missing profile referenced by [") + Name + "] " + KeyName;
					return false;
				}
				continue;
			}
			if (Changed && (Key.starts_with("use_dof_") || Key == "disable_zoom_dof" || (IsCamera && (Key == "enabled" || Key == "manual_focal_length"))) && !BoolLambda(Text))
			{
				Error = xr_string("Invalid boolean in [") + Name + "] " + KeyName;
				return false;
			}
			if (Changed && (Key == "dof" || Key == "reload_dof"))
			{
				Fvector4 Value = {};
				char Extra = 0;
				if (sscanf(Text, "%f , %f , %f , %f %c", &Value.x, &Value.y, &Value.z, &Value.w, &Extra) != 4 || !_valid(Value.x) || !_valid(Value.y) || !_valid(Value.z) || !_valid(Value.w) || Value.x > Value.y || Value.y > Value.z || Value.w < 0)
				{
					Error = xr_string("Expected finite near <= focus <= far and nonnegative legacy release in [") + Name + "] " + KeyName;
					return false;
				}
			}
			const bool IsDistance = IsDofKey(Key) && !Key.starts_with("use_dof_") && !Key.starts_with("dof_speed_") && !Key.starts_with("dof_time_offset_") && (Key.ends_with("_near") || Key.ends_with("_focus") || Key.ends_with("_far"));
			const bool IsSpeed = IsDofKey(Key) && Key.find("speed") != xr_string_view::npos;
			const bool IsTime = Key.starts_with("dof_time_offset_") || Key == "default_dof_time_offset";
			const bool IsZone = xr_string_view(Name) == "zone_pick_dof" && (Key == "near" || Key == "far");
			if (Changed && (IsDistance || IsSpeed || IsTime || IsZone))
			{
				float Value = 0;
				if (!NumberLambda(Text, Value) || (IsSpeed && (Value < 0 || Value > 1000)) || (IsZone && ((Key == "near" && Value > 0) || (Key == "far" && Value < 0))))
				{
					Error = xr_string("Invalid number in [") + Name + "] " + KeyName;
					return false;
				}
			}
			if (IsDistance)
			{
				Prefixes.insert(xr_string(Key.data(), Key.rfind('_')).c_str());
			}
		}
		for (const auto& Prefix : Prefixes)
		{
			const xr_string Text = Prefix.c_str();
			const bool IsZoom = Text == "zoom_dof" || Text == "alter_zoom_dof" || Text == "gl_dof" || Text == "lens_dof" || Text == "default_zoom_dof";
			Fvector Base = IsZoom ? Defaults.Zoom : Defaults.Action.Request.Values;
			if (Text == "default_zoom_dof")
			{
				Base.set(0.5f, 0.8f, 100.0f);
			}
			if (Text == "default_action_dof")
			{
				Base.set(0.0f, 0.5f, 5.0f);
			}
			if (Text == "alter_zoom_dof")
			{
				Base = DofConfig::ReadVector(Config, Name, "zoom_dof", Defaults.Zoom);
			}
			Fvector Values;
			bool Changed = false;
			const char* Suffixes[] = {"near", "focus", "far"};
			for (u32 Index = 0; Index < 3; ++Index)
			{
				const xr_string Key = Text + "_" + Suffixes[Index];
				Values[Index] = DofConfig::ReadFloat(Config, Name, Key.c_str(), Base[Index]);
				Changed = Changed || ChangedLambda(Name, Key.c_str());
			}
			if (Changed && (Values.x > Values.y || Values.y > Values.z))
			{
				Error = xr_string("Expected near <= focus <= far in [") + Name + "] " + Text;
				return false;
			}
		}
	}
	Error.clear();
	return true;
}

void CDofManager::SetPreview(const SDofCameraDesc& Profile)
{
	if (!IsPreviewing())
	{
		PreviewOriginalMode = g_NewDofMode;
	}
	PreviewCamera = Profile;
	IsCameraPreview = true;
	IsStaticPreview = false;
	Console->ExecuteCommand(Profile.Camera.Mode == EDofMode::Manual ? "r4_ndof_mode manual" : "r4_ndof_mode autofocus", false);
}

void CDofManager::SetPreview(const SHudDofRequest& Profile)
{
	if (!IsPreviewing())
	{
		PreviewOriginalMode = g_NewDofMode;
	}
	PreviewStatic = Profile;
	IsStaticPreview = true;
	IsCameraPreview = false;
	if (psDeviceFlags.test(rsR4))
	{
		Console->ExecuteCommand("r4_ndof_mode static", false);
	}
}

void CDofManager::StopPreview()
{
	if (!IsPreviewing())
	{
		return;
	}
	if (IsPreviewing() && psDeviceFlags.test(rsR4))
	{
		const char* Mode = PreviewOriginalMode == static_cast<u32>(EDofMode::Static) ? "static" : PreviewOriginalMode == static_cast<u32>(EDofMode::Manual) ? "manual"
																																							: "autofocus";
		Console->ExecuteCommand((xr_string("r4_ndof_mode ") + Mode).c_str(), false);
	}
	IsCameraPreview = IsStaticPreview = false;
	CameraProfile = nullptr;
}

void SDofCameraDesc::Load(const shared_str& Section)
{
	LoadFrom(DofSettings::Get(), Section);
}

void SDofCameraDesc::LoadFrom(const CInifile& Config, const shared_str& Section)
{
	this->Section = Section;
	DofConfig::ReadCameraBool(Section, "enabled", Enabled, Config);
	Fields.set(DofConfig::CameraManualFocalLength, DofConfig::ReadCameraBool(Section, "manual_focal_length", Camera.ManualFocalLength, Config));
	for (const DofConfig::SCameraFloatField& Field : DofConfig::CameraFloats)
	{
		if (!Config.line_exist(Section.c_str(), Field.Key))
		{
			continue;
		}
		float Value = 0.0f;
		char Extra = 0;
		const char* Text = Config.r_string(Section, Field.Key);
		if (!Text || sscanf(Text, "%f %c", &Value, &Extra) != 1 ||
			!_valid(Value) || Value < Field.Min || Value > Field.Max)
		{
			Msg("! DOF: invalid [%s] %s, inheriting the base camera", Section.c_str(), Field.Key);
			continue;
		}
		Camera.*(Field.Member) = Value;
		Fields.set(Field.Field, true);
	}
	if (Config.line_exist(Section.c_str(), "ndof_mode"))
	{
		const char* Text = Config.r_string(Section, "ndof_mode");
		const xr_string_view Mode = Text ? Text : "";
		if (Mode == "autofocus" || Mode == "auto" || Mode == "manual")
		{
			Camera.Mode = Mode == "manual" ? EDofMode::Manual : EDofMode::Autofocus;
			Fields.set(DofConfig::CameraMode, true);
		}
		else
		{
			Msg("! DOF: invalid [%s] ndof_mode, inheriting the base camera", Section.c_str());
		}
	}
	if (Config.line_exist(Section.c_str(), "focus_point"))
	{
		Fvector2 Point = {};
		char Extra = 0;
		const char* Text = Config.r_string(Section, "focus_point");
		if (Text && sscanf(Text, "%f , %f %c", &Point.x, &Point.y, &Extra) == 2 &&
			_valid(Point.x) && _valid(Point.y) && Point.x >= 0.0f && Point.x <= 1.0f && Point.y >= 0.0f && Point.y <= 1.0f)
		{
			Camera.FocusPoint = Point;
			Fields.set(DofConfig::CameraFocusPoint, true);
		}
		else
		{
			Msg("! DOF: invalid [%s] focus_point, inheriting the base camera", Section.c_str());
		}
	}
}

void SDofCameraDesc::Apply(SDofCameraSettings& Settings) const
{
	for (const DofConfig::SCameraFloatField& Field : DofConfig::CameraFloats)
	{
		if (Fields.test(Field.Field))
		{
			Settings.*(Field.Member) = Camera.*(Field.Member);
		}
	}
	if (Fields.test(DofConfig::CameraMode))
	{
		Settings.Mode = Camera.Mode;
	}
	if (Fields.test(DofConfig::CameraManualFocalLength))
	{
		Settings.ManualFocalLength = Camera.ManualFocalLength;
	}
	if (Fields.test(DofConfig::CameraFocusPoint))
	{
		Settings.FocusPoint = Camera.FocusPoint;
	}
}

void SHudDofDefaults::Load(const shared_str& Section)
{
	LoadFrom(DofSettings::Get(), Section, true);
}

void SHudDofDefaults::LoadFrom(const CInifile& Config, const shared_str& Section, bool LoadCameras)
{
	for (u32 Index = 0; LoadCameras && Index < static_cast<u32>(EDofEvent::Count); ++Index)
	{
		Cameras[Index] = DofConfig::ReadCameraProfile("ndof_events", DofConfig::CameraEvents[Index]);
	}
	Zoom = DofConfig::ReadVector(Config, Section, "default_zoom_dof", Zoom);
	Action.Request.Values = DofConfig::ReadVector(Config, Section, "default_action_dof", Action.Request.Values);
	Speed = DofConfig::ReadSpeed(Config, Section, "default_dof_speed", Speed);
	Action.Request.SpeedIn = DofConfig::ReadSpeed(Config, Section, "default_dof_speed_in", Action.Request.SpeedIn);
	Action.Request.SpeedOut = DofConfig::ReadSpeed(Config, Section, "default_dof_speed_out", Action.Request.SpeedOut);
	Inventory = {DofConfig::ReadVector(Config, Section, "inventory_dof", Action.Request.Values), DofConfig::ReadSpeed(Config, Section, "inventory_dof_speed_in", Action.Request.SpeedIn), DofConfig::ReadSpeed(Config, Section, "inventory_dof_speed_out", Action.Request.SpeedOut)};
	Action.TimeOffset = DofConfig::ReadFloat(Config, Section, "default_dof_time_offset", Action.TimeOffset);
	PickNear = std::min(DofConfig::ReadFloat(Config, "zone_pick_dof", "near", PickNear), 0.0f);
	PickFar = std::max(DofConfig::ReadFloat(Config, "zone_pick_dof", "far", PickFar), 0.0f);
}

void SHudDofDesc::Load(const shared_str& Section)
{
	LoadFrom(DofSettings::Get(), Section, SHudDofDefaults::Registry::Get("gunslinger_base"), true);
}

void SHudDofDesc::LoadFrom(const CInifile& Config, const shared_str& Section, const SHudDofDefaults& Defaults, bool LoadCameras)
{
	for (u32 Index = 0; LoadCameras && Index < static_cast<u32>(EDofEvent::Count); ++Index)
	{
		Cameras[Index] = DofConfig::ReadCameraProfile(Section, shared_str().printf("ndof_profile_%s", DofConfig::CameraEvents[Index]).c_str());
	}
	Zoom = {DofConfig::ReadVector(Config, Section, "zoom_dof", Defaults.Zoom), DofConfig::ReadSpeed(Config, Section, "zoom_in_dof_speed", Defaults.Action.Request.SpeedIn), DofConfig::ReadSpeed(Config, Section, "zoom_out_dof_speed", Defaults.Action.Request.SpeedOut)};
	AlterZoom = {DofConfig::ReadVector(Config, Section, "alter_zoom_dof", Zoom.Values), DofConfig::ReadSpeed(Config, Section, "alter_zoom_in_dof_speed", Defaults.Action.Request.SpeedIn), Zoom.SpeedOut};
	Grenade = {DofConfig::ReadVector(Config, Section, "gl_dof", Defaults.Zoom), DofConfig::ReadSpeed(Config, Section, "gl_in_dof_speed", Defaults.Action.Request.SpeedIn), Zoom.SpeedOut};
	Lens = {DofConfig::ReadVector(Config, Section, "lens_dof", Defaults.Zoom, &LensFields, &LensOverrides), Zoom.SpeedIn, Zoom.SpeedOut};
	Pda = {DofConfig::ReadVector(Config, Section, "zoom_dof", Defaults.Action.Request.Values), Zoom.SpeedIn, Zoom.SpeedOut};
	DisableZoom = Config.read_if_exists<bool>(Section.c_str(), "disable_zoom_dof", false);
	Reload = Defaults.Action;
	DofConfig::ReadLegacy(Config, Section, "reload_dof", Reload);

	if (Section.size() && Config.section_exist(Section.c_str()))
	{
		xr_hash_set<shared_str> Motions;
		for (const CInifile::Item& Entry : Config.r_section(Section).Data)
		{
			const xr_string_view Key = Entry.first.c_str();
			if (LoadCameras && Key.starts_with("ndof_profile_anm_"))
			{
				const SDofCameraDesc* Profile = DofConfig::ReadCameraProfile(Section, Entry.first.c_str());
				if (Profile)
				{
					const shared_str Motion = xr_string(Key.data() + 13, static_cast<u32>(Key.size() - 13)).c_str();
					CameraActions.emplace(Motion, Profile);
				}
			}
			const shared_str Motion = DofConfig::MotionFromKey(Entry.first.c_str());
			if (Motion.size())
			{
				Motions.insert(Motion);
			}
		}
		for (const shared_str& Motion : Motions)
		{
			SHudDofActionDesc Default = Defaults.Action;
			Default.Fields.set(DofEnabled, DofConfig::DefaultAction(Motion.c_str()));
			Actions.emplace(Motion, DofConfig::ReadAction(Config, Section, Motion, Default));
		}
	}
	Usable = Defaults.Action;
	DofConfig::ReadLegacy(Config, Section, "dof", Usable);
	Usable.Fields.set(DofEnabled, true);
	const Fvector UsableFallback = Usable.Request.Values;
	// Resolve native and original aliases from cached fields, without reading them again.
	for (const shared_str& Motion : {shared_str("anm_show"), shared_str("anm_use")})
	{
		const auto It = Actions.find(Motion);
		if (It != Actions.end())
		{
			DofConfig::MergeAction(Usable, It->second);
		}
	}
	if (Usable.Request.Values.x > Usable.Request.Values.y || Usable.Request.Values.y > Usable.Request.Values.z)
	{
		Usable.Request.Values = UsableFallback;
	}
}

void CDofManager::Initialize()
{
	Defaults = &SHudDofDefaults::Registry::Get("gunslinger_base");
	Talk = EngineExternal().GetTalkDof();
	Reset();
}

void CDofManager::LoadHud(SHudDofSource& Source, const shared_str& Section, bool Usable)
{
	if (Source.HudSection == Section && Source.Usable == Usable)
	{
		return;
	}
	StopAction(Source);
	Source.HudSection = Section;
	Source.Hud = Section.size() ? &SHudDofDesc::Registry::Get(Section) : nullptr;
	Source.Usable = Usable;
}

void CDofManager::LoadItem(SHudDofSource& Source, const shared_str& Section)
{
	if (Source.ItemSection == Section)
	{
		return;
	}
	Source.ItemSection = Section;
	Source.Item = Section.size() ? &SHudDofDesc::Registry::Get(Section) : nullptr;
}

void CDofManager::LoadScope(SHudDofSource& Source, const shared_str& Section)
{
	if (Source.ScopeSection == Section)
	{
		return;
	}
	Source.ScopeSection = Section;
	Source.Scope = Section.size() ? &SHudDofDesc::Registry::Get(Section) : nullptr;
}

void CDofManager::StartAction(SHudDofSource& Source, const shared_str& Motion, u32 Duration)
{
	StopAction(Source);
	Source.StartTime = Device.dwTimeGlobal;
	Source.Duration = Duration;
	Source.Playing = true;
	Source.Motion = Motion;
	CacheAction(Source);
}

void CDofManager::CacheAction(SHudDofSource& Source)
{
	const shared_str& Motion = Source.Motion;
	const u32 Duration = Source.Duration;
	Source.Revision = DofSettings::GetRevision();
	Source.HasAction = false;
	Source.CameraAction = nullptr;
	Source.CameraActionSection = nullptr;
	Source.ReleaseAfter = 0.0f;
	Source.ReloadAction = DofConfig::DefaultAction(Motion.c_str());
	Source.UsableAction = Source.Usable && (Motion == "anm_show" || Motion == "anm_use");
	for (const SHudDofDesc* Desc : {Source.Hud, Source.Item})
	{
		if (!Desc)
		{
			continue;
		}
		auto It = Desc->CameraActions.find(Motion);
		if (It == Desc->CameraActions.end() && Source.Usable && Motion == "anm_show")
		{
			It = Desc->CameraActions.find("anm_use");
		}
		if (It != Desc->CameraActions.end())
		{
			Source.CameraAction = It->second;
			Source.CameraActionSection = Desc == Source.Hud ? Source.HudSection : Source.ItemSection;
			break;
		}
	}
	Source.CameraReleaseAfter = Duration != 0 ? Duration / 1000.0f : flt_max;
	if (!Source.Hud)
	{
		return;
	}
	if (Source.Usable && Motion == "anm_show")
	{
		Source.Action = Source.Hud->Usable;
		Source.HasAction = true;
	}
	else
	{
		const auto It = Source.Hud->Actions.find(Motion);
		if (It != Source.Hud->Actions.end())
		{
			Source.Action = It->second;
			Source.HasAction = true;
		}
	}
	if (Source.HasAction)
	{
		Source.ReleaseAfter = DofConfig::ReleaseTime(Source.Action, Duration);
	}
	const SHudDofActionDesc* Timing = Source.HasAction ? &Source.Action : &Source.Hud->Reload;
	if (Source.UsableAction)
	{
		Timing = &Source.Hud->Usable;
	}
	else if (Source.ReloadAction && Source.Item && !Timing->Fields.test(DofTimeOffset))
	{
		Timing = &Source.Item->Reload;
	}
	Source.CameraReleaseAfter = DofConfig::ReleaseTime(*Timing, Duration);
}

void CDofManager::RefreshSource(SHudDofSource& Source)
{
	if (Source.Playing && Source.Revision != DofSettings::GetRevision())
	{
		CacheAction(Source);
	}
}

void CDofManager::StopAction(SHudDofSource& Source)
{
	Source.Action = {};
	Source.HasAction = false;
	Source.Motion = nullptr;
	Source.CameraAction = nullptr;
	Source.CameraActionSection = nullptr;
	Source.CameraReleaseAfter = 0.0f;
	Source.ReloadAction = false;
	Source.UsableAction = false;
	Source.Duration = 0;
	Source.Playing = false;
}

bool CDofManager::GetTimedAction(const SHudDofSource& Source, const SHudDofActionDesc& Action, SHudDofRequest& Request) const
{
	if (!Source.Playing || !Action.Fields.test(DofEnabled) || (Device.dwTimeGlobal - Source.StartTime) / 1000.0f >= DofConfig::ReleaseTime(Action, Source.Duration))
	{
		return false;
	}
	Request = Action.Request;
	return true;
}

bool CDofManager::GetAction(const SHudDofSource& Source, bool Animator, SHudDofRequest& Request) const
{
	if (!(Animator ? g_DofOptions.HudAnimator : g_DofOptions.Dynamic) || !Source.HasAction ||
		!Source.Action.Fields.test(DofEnabled) || (Device.dwTimeGlobal - Source.StartTime) / 1000.0f >= Source.ReleaseAfter)
	{
		return false;
	}
	Request = Source.Action.Request;
	return true;
}

Fvector CDofManager::PickFocus() const
{
	const float Focus = HUD().GetCurrentRayQuery().range;
	return Fvector().set(Focus + Defaults->PickNear, Focus, Focus + Defaults->PickFar);
}

const SDofCameraDesc* CDofManager::GetCameraProfile(EDofEvent Event, const SHudDofSource* Source, bool UseAction) const
{
	const u32 Index = static_cast<u32>(Event);
	const SDofCameraDesc* Profile = nullptr;
	if (Source)
	{
		if (UseAction)
		{
			Profile = Source->CameraAction;
		}
		if (!Profile && Event == EDofEvent::AimLens && Source->Scope)
		{
			Profile = Source->Scope->Cameras[Index];
		}
		if (!Profile && Source->Hud)
		{
			Profile = Source->Hud->Cameras[Index];
		}
		if (!Profile && Source->Item)
		{
			Profile = Source->Item->Cameras[Index];
		}
	}
	if (!Profile)
	{
		Profile = Defaults->Cameras[Index];
	}
	// An explicit disabled profile suppresses this event, including its lower-priority bindings.
	if (Profile && Profile->Enabled)
	{
		ActiveEvent = Event;
		ActiveProfile = Profile->Section;
		ActiveBinding = "ndof_events";
		if (Source)
		{
			if (UseAction && Source->CameraAction == Profile)
			{
				ActiveBinding = Source->CameraActionSection;
			}
			else if (Event == EDofEvent::AimLens && Source->Scope && Source->Scope->Cameras[Index] == Profile)
			{
				ActiveBinding = Source->ScopeSection;
			}
			else if (Source->Hud && Source->Hud->Cameras[Index] == Profile)
			{
				ActiveBinding = Source->HudSection;
			}
			else if (Source->Item && Source->Item->Cameras[Index] == Profile)
			{
				ActiveBinding = Source->ItemSection;
			}
		}
		return Profile;
	}
	return nullptr;
}

const SDofCameraDesc* CDofManager::SelectCameraProfile() const
{
	CActor* Actor = g_pGameLevel && Level().bReady && Level().CurrentViewEntity() ? Level().CurrentViewEntity()->cast_actor() : nullptr;
	if (!Actor || !Actor->g_Alive() || !Actor->HUDview())
	{
		return HasEffector ? GetCameraProfile(EDofEvent::Effector) : nullptr;
	}
	CInventoryItem* Item = Actor->inventory().ActiveItem();
	CHudItem* HudItem = Item ? Item->cast_hud_item() : nullptr;
	const SHudDofSource* Source = HudItem && !HudItem->IsHidden() && HudItem->HudItemData() ? &HudItem->DofSource : nullptr;
	const SDofCameraDesc* Fallback = HasEffector ? GetCameraProfile(EDofEvent::Effector, Source) : nullptr;
	ActiveEvent = EDofEvent::Count;
	ActiveProfile = nullptr;
	ActiveBinding = nullptr;
	if (Actor->IsTalking())
	{
		CInventoryOwner* Partner = Actor->GetTalkPartner();
		if (Partner && Partner->GetFocusingOnNpc())
		{
			if (const SDofCameraDesc* Profile = GetCameraProfile(EDofEvent::Talk, Source))
			{
				return Profile;
			}
		}
	}
	if (!Zones.empty())
	{
		if (const SDofCameraDesc* Profile = GetCameraProfile(EDofEvent::Zone, Source))
		{
			return Profile;
		}
	}
	if (Pickable)
	{
		if (const SDofCameraDesc* Profile = GetCameraProfile(EDofEvent::Pickable, Source))
		{
			return Profile;
		}
	}
	CUIGameCustom* GameUI = CurrentGameUI();
	if (GameUI && GameUI->GetActiveInventoryWindow())
	{
		if (const SDofCameraDesc* Profile = GetCameraProfile(EDofEvent::Inventory, Source))
		{
			return Profile;
		}
	}
	CHudAnimatorManager* AnimatorManager = Actor->HudAnimator();
	CHudAnimatorBase* Animator = AnimatorManager ? AnimatorManager->CurrentAnimator() : nullptr;
	const bool HasPdaAnimator = Animator && Animator->cast_pda_animator();
	if (Animator)
	{
		const SHudDofSource& AnimatorSource = Animator->DofSource;
		const bool HasAction = DofConfig::CameraActionActive(AnimatorSource);
		if (HasPdaAnimator || HasAction)
		{
			const EDofEvent Event = HasPdaAnimator ? EDofEvent::Pda : EDofEvent::HudAnimator;
			if (const SDofCameraDesc* Profile = GetCameraProfile(Event, &AnimatorSource, HasAction))
			{
				return Profile;
			}
		}
	}
	if (!HasPdaAnimator && GameUI && GameUI->PdaMenu() && GameUI->PdaMenu()->IsShown())
	{
		if (const SDofCameraDesc* Profile = GetCameraProfile(EDofEvent::Pda, Source))
		{
			return Profile;
		}
	}
	if (!Source)
	{
		return Fallback ? GetCameraProfile(EDofEvent::Effector, Source) : nullptr;
	}
	CWeapon* Weapon = Item->cast_weapon();
	const bool IsReloading = Weapon && Weapon->GetState() == CWeapon::eReload;
	if (DofConfig::CameraActionActive(*Source) && (IsReloading || Source->ReloadAction || Source->UsableAction || Source->CameraAction))
	{
		EDofEvent Event = EDofEvent::WeaponAction;
		if (IsReloading || Source->ReloadAction)
		{
			Event = EDofEvent::Reload;
		}
		else if (Source->UsableAction)
		{
			Event = EDofEvent::HudAnimator;
		}
		if (const SDofCameraDesc* Profile = GetCameraProfile(Event, Source, true))
		{
			return Profile;
		}
	}
	if (Weapon && Weapon->IsZoomed())
	{
		EDofEvent Event = EDofEvent::Aim;
		if (Weapon->IsGrenadeMode())
		{
			Event = EDofEvent::AimGrenade;
		}
		else if (Weapon->IsAltZoomed())
		{
			Event = EDofEvent::AimAlt;
		}
		else if (Weapon->IsLensedScopeInstalled() && Weapon->Allow3DScopes())
		{
			Event = EDofEvent::AimLens;
		}
		if (const SDofCameraDesc* Profile = GetCameraProfile(Event, Source))
		{
			return Profile;
		}
	}
	return Fallback ? GetCameraProfile(EDofEvent::Effector, Source) : nullptr;
}

bool CDofManager::SelectRequest(SHudDofRequest& Request) const
{
	CActor* Actor = g_pGameLevel && Level().bReady && Level().CurrentViewEntity() ? Level().CurrentViewEntity()->cast_actor() : nullptr;
	if (!Actor || !Actor->g_Alive() || !Actor->HUDview())
	{
		return false;
	}
	if (Actor->IsTalking())
	{
		CInventoryOwner* Partner = Actor->GetTalkPartner();
		if (g_DofOptions.Talk && Talk.w >= 0.0f && Partner && Partner->GetFocusingOnNpc())
		{
			Fvector Position;
			Partner->cast_game_object()->Center(Position);
			Position.sub(Device.vCameraPosition);
			const float Focus = std::max(Position.dotproduct(Device.vCameraDirection), EPS_L);
			ActiveEvent = EDofEvent::Talk;
			ActiveBinding = "dialogue focus";
			Request = {Fvector().set(Focus + Talk.x - Talk.y, Focus, Focus + Talk.z - Talk.y), Defaults->Speed, Defaults->Speed};
			return true;
		}
		return false;
	}
	if (Pickable || !Zones.empty())
	{
		ActiveEvent = !Zones.empty() ? EDofEvent::Zone : EDofEvent::Pickable;
		ActiveBinding = "zone_pick_dof";
		Request = {PickFocus(), Defaults->Speed, Defaults->Speed};
		return true;
	}
	const bool IsNewDof = psDeviceFlags.test(rsR4);
	CUIGameCustom* GameUI = CurrentGameUI();
	if (IsNewDof && g_DofOptions.Inventory && GameUI && GameUI->GetActiveInventoryWindow())
	{
		ActiveEvent = EDofEvent::Inventory;
		ActiveBinding = "gunslinger_base";
		Request = Defaults->Inventory;
		return true;
	}
	CHudAnimatorManager* AnimatorManager = Actor->HudAnimator();
	if (CHudAnimatorBase* Animator = AnimatorManager ? AnimatorManager->CurrentAnimator() : nullptr)
	{
		const SHudDofSource& Source = Animator->DofSource;
		if (GetAction(Source, true, Request))
		{
			ActiveEvent = Animator->cast_pda_animator() ? EDofEvent::Pda : EDofEvent::HudAnimator;
			ActiveBinding = Source.HudSection;
			return true;
		}
		if (Animator->cast_pda_animator() && g_DofOptions.HudAnimator && Source.Hud && !Source.Hud->DisableZoom)
		{
			ActiveEvent = EDofEvent::Pda;
			ActiveBinding = Source.HudSection;
			Request = Source.Hud->Pda;
			return true;
		}
		return false;
	}
	if (IsNewDof && g_DofOptions.HudAnimator && GameUI && GameUI->PdaMenu() && GameUI->PdaMenu()->IsShown())
	{
		ActiveEvent = EDofEvent::Pda;
		ActiveBinding = "gunslinger_base";
		Request = Defaults->Action.Request;
		return true;
	}
	CInventoryItem* Item = Actor->inventory().ActiveItem();
	CHudItem* HudItem = Item ? Item->cast_hud_item() : nullptr;
	if (!HudItem || HudItem->IsHidden() || !HudItem->HudItemData())
	{
		return false;
	}
	const SHudDofSource& Source = HudItem->DofSource;
	CWeapon* Weapon = Item->cast_weapon();
	const bool IsReloading = Weapon && Weapon->GetState() == CWeapon::eReload;
	if ((!IsReloading || g_DofOptions.Reload) && GetAction(Source, false, Request))
	{
		ActiveEvent = IsReloading || Source.ReloadAction ? EDofEvent::Reload : EDofEvent::WeaponAction;
		ActiveBinding = Source.Motion;
		return true;
	}
	if (!Weapon)
	{
		return false;
	}
	if (!g_DofOptions.Dynamic && g_DofOptions.Reload && Weapon->GetState() == CWeapon::eReload && Source.Item &&
		GetTimedAction(Source, Source.Item->Reload, Request))
	{
		ActiveEvent = EDofEvent::Reload;
		ActiveBinding = Source.ItemSection;
		return true;
	}
	if (!Weapon->IsZoomed() || !Source.Hud || Source.Hud->DisableZoom || (Source.Item && Source.Item->DisableZoom))
	{
		return false;
	}
	const SHudDofDesc& Desc = *Source.Hud;
	if (Weapon->IsLensedScopeInstalled() && Weapon->Allow3DScopes() && !Weapon->IsAltZoomed() && !Weapon->IsGrenadeMode())
	{
		ActiveEvent = EDofEvent::AimLens;
		ActiveBinding = Source.HudSection;
		Request = Desc.Lens;
		if (Source.Scope)
		{
			const SHudDofDesc& Scope = *Source.Scope;
			if (Scope.LensFields.test(DofNear))
			{
				Request.Values.x = Scope.LensOverrides.x;
			}
			if (Scope.LensFields.test(DofFocus))
			{
				Request.Values.y = Scope.LensOverrides.y;
			}
			if (Scope.LensFields.test(DofFar))
			{
				Request.Values.z = Scope.LensOverrides.z;
			}
			if (Request.Values.x > Request.Values.y || Request.Values.y > Request.Values.z)
			{
				Request.Values = Desc.Lens.Values;
			}
		}
		return true;
	}
	if (g_DofOptions.ConstantZoom)
	{
		if (Weapon->IsGrenadeMode())
		{
			ActiveEvent = EDofEvent::AimGrenade;
			ActiveBinding = Source.HudSection;
			Request = Desc.Grenade;
		}
		else if (Weapon->IsAltZoomed())
		{
			ActiveEvent = EDofEvent::AimAlt;
			ActiveBinding = Source.HudSection;
			Request = Desc.AlterZoom;
		}
		else if (!Weapon->IsScopeAttached())
		{
			ActiveEvent = EDofEvent::Aim;
			ActiveBinding = Source.HudSection;
			Request = Desc.Zoom;
		}
		else
		{
			return false;
		}
		return true;
	}
	if (!Weapon->IsAltZoomed() && (Weapon->IsCollimatorInstalled() || Weapon->IsLensedScopeInstalled()))
	{
		return false;
	}
	ActiveEvent = Weapon->IsGrenadeMode() ? EDofEvent::AimGrenade : Weapon->IsAltZoomed() ? EDofEvent::AimAlt
																						  : EDofEvent::Aim;
	ActiveBinding = Source.HudSection;
	Request = {PickFocus(), Defaults->Speed, Desc.Zoom.SpeedOut};
	return true;
}

void CDofManager::SetBase(const Fvector& Values)
{
	Base = Values;
}

void CDofManager::SetEffector(const Fvector& Values)
{
	Effector = Values;
	HasEffector = true;
}

void CDofManager::RestoreEffector()
{
	HasEffector = false;
}

void CDofManager::SetPickable(bool Active)
{
	Pickable = Active;
}

void CDofManager::SetZoneActive(ALife::_OBJECT_ID Id, bool Active)
{
	if (Active)
	{
		Zones.insert(Id);
	}
	else
	{
		Zones.erase(Id);
	}
}

void CDofManager::Blend(const Fvector& Values, float Weight, float Rate, float Delta)
{
	if (!Target.similar(Values) || TargetWeight != Weight || Speed != Rate)
	{
		From = Current;
		FromWeight = OverrideWeight;
		Target = Values;
		TargetWeight = Weight;
		Speed = Rate;
		Progress = 0.0f;
	}
	Progress = clampr(Progress + std::max(Delta, 0.0f) * Speed, 0.0f, 1.0f);
	Current.lerp(From, Target, Progress);
	OverrideWeight = FromWeight + (TargetWeight - FromWeight) * Progress;
}

void CDofManager::Update(float Delta)
{
	if (!Defaults || g_dedicated_server || HasCinema)
	{
		return;
	}
	KernelFrom = Progress < 1.0f ? KernelFrom : -1.0f;
	CameraProfile = nullptr;
	ActiveEvent = EDofEvent::Count;
	ActiveProfile = nullptr;
	ActiveBinding = nullptr;
	CActor* Actor = g_pGameLevel && Level().bReady && Level().CurrentViewEntity() ? Level().CurrentViewEntity()->cast_actor() : nullptr;
	if (Actor)
	{
		CInventoryItem* Item = Actor->inventory().ActiveItem();
		if (CHudItem* Hud = Item ? Item->cast_hud_item() : nullptr)
		{
			RefreshSource(Hud->DofSource);
		}
		if (CHudAnimatorManager* Manager = Actor->HudAnimator())
		{
			if (CHudAnimatorBase* Animator = Manager->CurrentAnimator())
			{
				RefreshSource(Animator->DofSource);
			}
		}
	}
	if (IsCameraPreview && psDeviceFlags.test(rsR4))
	{
		CameraProfile = &PreviewCamera;
		ActiveProfile = PreviewCamera.Section;
		ActiveBinding = "Preview";
		return;
	}
	if (IsStaticPreview)
	{
		ActiveBinding = "Preview";
		ReturnSpeed = PreviewStatic.SpeedOut;
		HadRequest = true;
		Blend(PreviewStatic.Values, 1.0f, PreviewStatic.SpeedIn, Delta);
		return;
	}
	if (psDeviceFlags.test(rsR4) && g_NewDofMode != static_cast<u32>(EDofMode::Static))
	{
		CameraProfile = SelectCameraProfile();
		return;
	}
	SHudDofRequest Request;
	if (SelectRequest(Request))
	{
		ReturnSpeed = Request.SpeedOut;
		HadRequest = true;
		Blend(Request.Values, 1.0f, Request.SpeedIn, Delta);
	}
	else
	{
		ActiveEvent = HasEffector ? EDofEvent::Effector : EDofEvent::Count;
		ActiveBinding = HasEffector ? "external effector" : "base";
		const Fvector& Values = HasEffector ? Effector : Base;
		Blend(Values, HasEffector ? 1.0f : 0.0f, HadRequest ? ReturnSpeed : Defaults->Speed, Delta);
		if (Progress >= 1.0f)
		{
			HadRequest = false;
		}
	}
}

void CDofManager::SetCinema(const Fvector4* Values)
{
	const bool WasCinema = HasCinema;
	HasCinema = Values && _valid(*Values) && Values->x < Values->y && Values->y < Values->z && Values->w >= 0.0f;
	if (!HasCinema)
	{
		KernelFrom = WasCinema ? Cinema.w : KernelFrom;
		Progress = WasCinema ? 0.0f : Progress;
		return;
	}
	Cinema = *Values;
	Current = From = Target = Fvector().set(Cinema.x, Cinema.y, Cinema.z);
	Progress = 1.0f;
	HadRequest = false;
}

void CDofManager::Reset()
{
	HasCinema = false;
	KernelFrom = -1.0f;
	StopPreview();
	CameraProfile = nullptr;
	Zones.clear();
	Pickable = false;
	HasEffector = false;
	HadRequest = false;
	Current = From = Target = Base;
	Progress = 0.0f;
	OverrideWeight = FromWeight = TargetWeight = 0.0f;
	Speed = Defaults ? Defaults->Speed : 5.0f;
	ReturnSpeed = 1.0f;
}

void CDofManager::GetCurrent(Fvector& Result) const
{
	Result = Current;
}

void CDofManager::GetCameraSettings(SDofCameraSettings& Settings) const
{
	if (CameraProfile)
	{
		CameraProfile->Apply(Settings);
	}
}
