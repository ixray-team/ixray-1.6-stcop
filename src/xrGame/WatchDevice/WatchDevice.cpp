#include "StdAfx.h"
#include "WatchDevice.h"
#include "WatchInternal.h"
#include "WatchLogic.h"
#include "../Include/xrRender/Kinematics.h"
#include "../../xrEngine/date_time.h"

CWatchDevice::CWatchDevice()
{
	Env = CreateGameWatchEnvironment();
}

CWatchDevice::~CWatchDevice()
{
	DestroyHudLights();
	Ui.reset();
}

void CWatchDevice::SetEnvironment(xr_unique_ptr<IWatchEnvironment> NewEnvironment)
{
	Env = std::move(NewEnvironment);
}

void CWatchDevice::Load(const shared_str& RootSection)
{
	Section = RootSection;
	Profile = nullptr;
	Loaded = false;
	DestroyHudLights();
	Config = SWatchConfig{};
	State = SWatchState{};
	Preview = SWatchDebugPreview{};
	Leds.clear();
	Gauges.clear();
	ResetRuntime();

	if (!pSettings || !pSettings->section_exist(Section.c_str()))
	{
		return;
	}

	Loaded = true;
	ApplyConfigLayers();
}

void CWatchDevice::ApplyConfigLayers()
{
	DestroyHudLights();
	Config = SWatchConfig{};

	LoadElementLists(*pSettings, Section);
	if (Profile.size())
	{
		LoadElementLists(*pSettings, Profile);
	}
	BuildElements();

	LoadConfig(*pSettings, Section, false);
	if (Profile.size())
	{
		LoadConfig(*pSettings, Profile, false);
	}
	ApplyOverrideFromDisk();

	Leds.assign(Config.Leds.size(), SWatchLedRuntime{});
	Gauges.assign(Config.Gauges.size(), SWatchGaugeRuntime{});
	CompileConfig(true);
	ConfigBaseline = Config;
}

void CWatchDevice::CompileConfig(bool Report)
{
	if (Leds.size() != Config.Leds.size())
	{
		DestroyHudLights();
		Leds.assign(Config.Leds.size(), SWatchLedRuntime{});
	}
	if (Gauges.size() != Config.Gauges.size())
	{
		Gauges.assign(Config.Gauges.size(), SWatchGaugeRuntime{});
	}

	RefreshCompassSettings(Report);
	UpdateDisplayFormats();
	UpdateLightSchedule(Report);
	UpdateAnomalyExclude();

	for (size_t Index = 0; Index < Leds.size(); ++Index)
	{
		const SWatchLedElement& Element = Config.Leds[Index];
		SWatchLedRuntime& Led = Leds[Index];
		Led.Enabled = Element.Master && Element.Present.Enabled;
		const shared_str& SourceName = Element.Present.Source.size() ? Element.Present.Source : Element.Name;
		if (!Led.SourceName.equal(SourceName) && (Report || !Led.SourceName.size()))
		{
			Led.Source = ResolveSource(SourceName, Report);
			Led.SourceName = SourceName;
		}
	}

	for (size_t Index = 0; Index < Gauges.size(); ++Index)
	{
		const SWatchGaugeElement& Element = Config.Gauges[Index];
		SWatchGaugeRuntime& Gauge = Gauges[Index];
		Gauge.Enabled = Element.Master && Element.Present.Enabled;
		const shared_str& SourceName = Element.Present.Source.size() ? Element.Present.Source : Element.Name;
		if (!Gauge.SourceName.equal(SourceName) && (Report || !Gauge.SourceName.size()))
		{
			Gauge.Source = ResolveSource(SourceName, Report);
			Gauge.SourceName = SourceName;
		}
		for (u32 Severity = 0; Severity < WatchSeverityCount; ++Severity)
		{
			const Fvector4* Rgba = WatchDetail::ConditionSeverityRgba(EWatchConditionSeverity(Severity), Element.Present);
			Gauge.HasColor[Severity] = Rgba && Rgba->w > EPS;
			Gauge.Colors[Severity] = Gauge.HasColor[Severity] ? WatchDetail::ConditionColorFromRgba(*Rgba) : 0;
		}
	}

	UpdateUILayout();
	UpdateLedMeshes();
	Anchors.Frame = u32(-1);
}

SWatchSource CWatchDevice::ResolveSource(const shared_str& Name, bool Report)
{
	SWatchSource Source;
	const char* Text = Name.size() ? Name.c_str() : "";
	const size_t PrefixLength = xr_strlen(WatchDetail::ScriptSourcePrefix);

	if (!xr_strcmp(Text, "anomaly"))
	{
		Source.Kind = EWatchSourceKind::Anomaly;
	}
	else if (!xr_strcmp(Text, "motion"))
	{
		Source.Kind = EWatchSourceKind::Motion;
	}
	else if (!xr_strcmp(Text, "noise"))
	{
		Source.Kind = EWatchSourceKind::Noise;
	}
	else if (WatchDetail::FindCondition(Text, Source.Condition))
	{
		Source.Kind = EWatchSourceKind::Condition;
	}
	else if (!strncmp(Text, WatchDetail::ScriptSourcePrefix, PrefixLength))
	{
		Source.Kind = EWatchSourceKind::Script;
		Source.Script = Env->ResolveScriptSource(Text + PrefixLength);
	}
	else if (Report)
	{
		Msg("! [watch] source [%s] is unknown, expected anomaly|motion|noise|<condition>|script:<function>", Text);
	}

	return Source;
}

shared_str CWatchDevice::ResolveProfileSection(IKinematics* NewModel) const
{
	if (!Loaded || !NewModel || !pSettings)
	{
		return shared_str();
	}

	const shared_str VisualName = NewModel->getDebugName();
	if (!VisualName.size())
	{
		return shared_str();
	}

	const char* Name = VisualName.c_str();
	const char* Base = std::max(strrchr(Name, '\\'), strrchr(Name, '/'));
	string128 Stem = {};
	xr_strcpy(Stem, Base ? Base + 1 : Name);
	if (char* Dot = strrchr(Stem, '.'))
	{
		*Dot = 0;
	}

	string256 ProfileName = {};
	xr_sprintf(ProfileName, "%s_%s", Section.c_str(), Stem);
	return pSettings->section_exist(ProfileName) ? shared_str(ProfileName) : shared_str();
}

void CWatchDevice::ResetRuntime()
{
	ResetCompassRuntime();
	Display = SWatchDisplayRuntime{};
	Surge = SWatchSurgeRuntime{};
	Light.Timer = 0.0f;
	Zones.SampleTimer = 0.0f;
	Zones.Gravity = 0.0f;
	Zones.AnomalyTarget = 0.0f;
	Anchors = SWatchAnchors{};
	Hidden = true;

	for (SWatchLedRuntime& Led : Leds)
	{
		Led.Intensity = 0.0f;
		Led.BlinkPhase = 0.0f;
		Led.Value = 0.0f;
	}

	for (SWatchGaugeRuntime& Gauge : Gauges)
	{
		Gauge.Value = 0.0f;
		Gauge.DisplayValue = 0.0f;
		Gauge.Badness = 0.0f;
		Gauge.Visible = false;
		Gauge.Severity = EWatchConditionSeverity::None;
	}
}

void CWatchDevice::ResetCompassRuntime()
{
	Compass = SWatchCompassRuntime{};
	Rig.SetCompassAngle(0.0f);
	Zones.Gravity = 0.0f;
	Zones.SampleTimer = 0.0f;
	State.CompassAngle = 0.0f;
	State.CompassError = 0.0f;
	State.TargetValid = false;
	State.CompassSourceKind = EWatchCompassSourceKind::None;
}

void CWatchDevice::BindModel(IKinematics* NewModel)
{
	Unbind();

	if (NewModel)
	{
		AttachModel(NewModel);
	}
}

void CWatchDevice::AttachModel(IKinematics* NewModel)
{
	const shared_str NewProfile = ResolveProfileSection(NewModel);
	if (!NewProfile.equal(Profile))
	{
		Profile = NewProfile;
		Msg("[watch] profile [%s]", Profile.size() ? Profile.c_str() : "base");
		ApplyConfigLayers();
	}

	xr_vector<shared_str> LedBones;
	LedBones.reserve(Config.Leds.size());
	for (const SWatchLedElement& Element : Config.Leds)
	{
		if (Element.Present.Bone.size())
		{
			LedBones.push_back(Element.Present.Bone);
		}
		else
		{
			LedBones.push_back(Element.Name.equal("anomaly") ? Config.Bones.AnomIndicator : Config.Bones.RadIndicator);
		}
	}

	Rig.Bind(NewModel, Config.Bones, LedBones);
	if (!Rig.BonesOk())
	{
		const SWatchBoneIds& Ids = Rig.Bones();
		Msg("! [watch] model is missing bones: compass=%d light=%d display=%d rad=%d anom=%d",
			Ids.Compass != BI_NONE, Ids.CompassLight != BI_NONE, Ids.Display != BI_NONE, Ids.RadIndicator != BI_NONE, Ids.AnomIndicator != BI_NONE);
	}
	Rig.SetCompassAngle(Compass.Angle);
	UpdateLedMeshes();
	CreateUI();
	Anchors.Frame = u32(-1);
}

void CWatchDevice::Unbind()
{
	DestroyHudLights();
	Ui.reset();
	Rig.Unbind();
	ResetCompassRuntime();
	Anchors = SWatchAnchors{};
}

void CWatchDevice::UpdateLedMeshes()
{
	for (size_t Index = 0; Index < Leds.size(); ++Index)
	{
		const SWatchChannelPresent& Present = Config.Leds[Index].Present;
		const bool Hide = Rig.Kinematics() && Leds[Index].Enabled && Present.PresentShader && Present.HideMesh;
		Rig.SetLedMeshHidden(u32(Index), Hide);
	}
}

void CWatchDevice::CommitCompassAngle(float Angle, bool ResetVelocity)
{
	Compass.Desired = Angle;
	if (ResetVelocity || !Compass.SpringActive)
	{
		Compass.Angle = Angle;
		Compass.Velocity = 0.0f;
	}
	Compass.SpringActive = true;
	SyncCompassState();
}

void CWatchDevice::SyncCompassState()
{
	Rig.SetCompassAngle(Compass.Angle);
	State.CompassAngle = Compass.Angle;
	State.CompassError = angle_difference_signed(Compass.Desired, Compass.Angle);
}

bool CWatchDevice::IsLedActive(u32 Index) const
{
	const SWatchChannelPresent& Present = Config.Leds[Index].Present;
	return Leds[Index].Enabled &&
		   ((Present.PresentShader && Rig.HasLedBone(Index)) ||
			(Present.PresentUi && Rig.HasDisplay()));
}

bool CWatchDevice::IsGaugeActive(u32 Index) const
{
	const SWatchGaugeRuntime& Gauge = Gauges[Index];
	if (!Gauge.Enabled || !Config.Gauges[Index].Present.PresentUi)
	{
		return false;
	}

	return Gauge.Source.Kind != EWatchSourceKind::Condition || Env->IsConditionAvailable(Gauge.Source.Condition);
}

bool CWatchDevice::UsesSource(EWatchSourceKind Kind) const
{
	for (u32 Index = 0; Index < Leds.size(); ++Index)
	{
		if (Leds[Index].Source.Kind == Kind && IsLedActive(Index))
		{
			return true;
		}
	}

	for (u32 Index = 0; Index < Gauges.size(); ++Index)
	{
		if (Gauges[Index].Source.Kind == Kind && IsGaugeActive(Index))
		{
			return true;
		}
	}

	return false;
}

float CWatchDevice::SampleSource(const SWatchSource& Source, float Normalize) const
{
	switch (Source.Kind)
	{
		case EWatchSourceKind::Anomaly: return Zones.AnomalyTarget;
		case EWatchSourceKind::Motion: return Env->SampleMotion();
		case EWatchSourceKind::Noise: return Env->SampleNoise();
		case EWatchSourceKind::Condition: return Env->SampleCondition(Source.Condition, Normalize);
		case EWatchSourceKind::Script: return Env->SampleScriptSource(Source.Script);
		default: return 0.0f;
	}
}

void CWatchDevice::SampleZones(float Dt)
{
	const bool GravityActive = !Preview.PreviewGravity && Config.Compass.GravityStrength > 0.0f && Rig.HasCompass();
	const bool AnomalyActive = UsesSource(EWatchSourceKind::Anomaly);
	if (Preview.PreviewGravity)
	{
		Zones.Gravity = clampr(Preview.PreviewGravityValue, 0.0f, 1.0f);
	}

	if (!Env->CanSampleZones() || (!GravityActive && !AnomalyActive))
	{
		if (!Preview.PreviewGravity)
		{
			Zones.Gravity = 0.0f;
		}
		Zones.AnomalyTarget = 0.0f;
		Zones.SampleTimer = 0.0f;
		return;
	}

	Zones.SampleTimer -= Dt;
	if (Zones.SampleTimer > 0.0f)
	{
		return;
	}

	Zones.SampleTimer = 1.0f / WatchDetail::ZoneSampleHz;

	float Gravity = 0.0f;
	float Anomaly = 0.0f;
	Env->SampleZones(GravityActive, AnomalyActive, Config.Anomaly, Zones.AnomalyExclude, Gravity, Anomaly);

	if (GravityActive)
	{
		Zones.Gravity = Gravity;
	}
	Zones.AnomalyTarget = Anomaly;
}

bool CWatchDevice::UpdateCompassGravityChaos(float Dt)
{
	const float Intensity = Zones.Gravity;
	if (Intensity <= WatchDetail::CompassGravityChaosEnter || Dt <= 0.0f)
	{
		if (Intensity <= WatchDetail::CompassGravityChaosEnter)
		{
			Compass.ChaosTime = 0.0f;
		}
		return false;
	}

	SetCompassVisible(true);
	Compass.ChaosTime += Dt;

	const float Spin = WatchLogic::CompassChaosSpin(Compass.ChaosTime);
	const float Speed = deg2rad(Config.Compass.GravityStrength) * (3.0f + Intensity * 14.0f);
	CommitCompassAngle(
		angle_normalize_signed(Compass.Angle + Spin * Speed * Intensity * Dt),
		true
	);
	State.CompassError = 0.0f;
	return true;
}

void CWatchDevice::SetCompassVisible(bool Visible)
{
	Rig.SetCompassHidden(!Visible);
}

void CWatchDevice::RefreshCompassSettings(bool Report)
{
	const SWatchCompass& Source = Config.Compass;
	SWatchCompassSettings& Settings = CompassSettings;
	Settings = SWatchCompassSettings{};

	if (Report && !Source.Mode.equal("north") && !Source.Mode.equal("quest") && !Source.Mode.equal("target") && !Source.Mode.equal("nav"))
	{
		Msg("! [watch] compass mode [%s] is unknown, expected north|quest|nav, using quest", Source.Mode.size() ? Source.Mode.c_str() : "");
	}
	if (Report && !Source.NoTarget.equal("hide") && !Source.NoTarget.equal("north"))
	{
		Msg("! [watch] compass no_target [%s] is unknown, expected hide|north, using hide", Source.NoTarget.size() ? Source.NoTarget.c_str() : "");
	}

	if (Source.Mode.equal("north"))
	{
		Settings.Mode = EWatchCompassMode::North;
	}
	else if (Source.Mode.equal("nav"))
	{
		Settings.Mode = EWatchCompassMode::Nav;
	}

	if (Source.NoTarget.equal("north"))
	{
		Settings.NoTarget = EWatchCompassNoTarget::North;
	}

	Settings.AngleOffsetRad = deg2rad(Source.AngleOffset);
	Settings.Colors[u32(EWatchCompassSourceKind::North)] = WatchDetail::ConditionColorFromRgba(Source.NorthColor);
	Settings.Colors[u32(EWatchCompassSourceKind::Story)] = WatchDetail::ConditionColorFromRgba(Source.StoryColor);
	Settings.Colors[u32(EWatchCompassSourceKind::Additional)] = WatchDetail::ConditionColorFromRgba(Source.AdditionalColor);
	Settings.Colors[u32(EWatchCompassSourceKind::UserNav)] = WatchDetail::ConditionColorFromRgba(Source.NavColor);
}

u32 CWatchDevice::CompassColorForKind(EWatchCompassSourceKind Kind) const
{
	if (Kind == EWatchCompassSourceKind::None)
	{
		return Ui ? Ui->GetCompassColor() : color_rgba(255, 30, 20, 255);
	}
	return CompassSettings.Colors[u32(Kind)];
}

void CWatchDevice::ApplyCompassNoTarget()
{
	State.TargetValid = false;
	State.CompassSourceKind = EWatchCompassSourceKind::None;

	if (CompassSettings.NoTarget == EWatchCompassNoTarget::Hide)
	{
		SetCompassVisible(false);
		Compass.SpringActive = false;
		Compass.Velocity = 0.0f;
		SyncCompassState();
		return;
	}

	SetCompassVisible(true);
	SWatchCompassSample Sample;
	Sample.Valid = true;
	Sample.Kind = EWatchCompassSourceKind::North;
	ApplyCompassSample(Sample, false);
}

void CWatchDevice::ApplyCompassSample(const SWatchCompassSample& Sample, bool HadTarget)
{
	if (!Sample.Valid)
	{
		ApplyCompassNoTarget();
		return;
	}

	const bool PointerTarget = WatchLogic::IsPointerKind(Sample.Kind);

	if (Sample.WorldDir.square_magnitude() < EPS_L)
	{
		if (HadTarget && PointerTarget)
		{
			SetCompassVisible(true);
			State.TargetValid = true;
			State.CompassSourceKind = Sample.Kind;
			Compass.SpringActive = true;
			SyncCompassState();
			return;
		}

		ApplyCompassNoTarget();
		return;
	}

	float Angle = 0.0f;
	if (!WatchLogic::WorldDirToCompassAngle(Sample.WorldDir, Env->ViewMatrix(), CompassSettings.AngleOffsetRad, Angle))
	{
		ApplyCompassNoTarget();
		return;
	}

	CommitCompassAngle(Angle, !HadTarget);
	SetCompassVisible(true);
	State.TargetValid = PointerTarget;
	State.CompassSourceKind = Sample.Kind;
}

void CWatchDevice::UpdateCompassSpring(float Dt)
{
	if (!Compass.SpringActive || !Rig.HasCompass() || Dt <= 0.0f)
	{
		SyncCompassState();
		return;
	}

	WatchLogic::StepCompassSpring(Compass.Angle, Compass.Velocity, Compass.Desired, Config.Compass.Stiffness, Config.Compass.Damping, Dt);
	SyncCompassState();
}

void CWatchDevice::UpdateCompass(float Dt)
{
	if (!Rig.HasCompass())
	{
		State.TargetValid = false;
		State.CompassSourceKind = EWatchCompassSourceKind::None;
		Compass.SpringActive = false;
		SyncCompassState();
		return;
	}

	bool AllowChaos = false;
	if (Preview.ForceAngle)
	{
		State.TargetValid = false;
		State.CompassSourceKind = EWatchCompassSourceKind::None;
		SetCompassVisible(true);
		CommitCompassAngle(deg2rad(Preview.ForceAngleDeg), true);
	}
	else
	{
		const bool HadTarget = State.TargetValid;
		SWatchCompassSample Sample;
		Env->SampleCompassTarget(CompassSettings.Mode, Sample);
		AllowChaos = CompassSettings.IsChaosEnabled();
		ApplyCompassSample(Sample, HadTarget);
	}

	if (AllowChaos && UpdateCompassGravityChaos(Dt))
	{
		return;
	}

	UpdateCompassSpring(Dt);
}

void CWatchDevice::Update(float Dt)
{
	if (!IsEnabled() || !Rig.Kinematics())
	{
		return;
	}

	if (Env->FrameId() > LastVisibleFrame + 1)
	{
		Hidden = true;
		return;
	}

	if (Hidden)
	{
		Hidden = false;
		Zones.SampleTimer = 0.0f;
		Display.Timer = 0.0f;
		Surge.Timer = 0.0f;
		Light.Timer = 0.0f;
	}

	const float SafeDt = (Dt > 0.0f) ? Dt : 0.0f;
	SampleZones(SafeDt);
	UpdateCompass(SafeDt);
	UpdateSurge(SafeDt);
	UpdateLeds(SafeDt);
	UpdateGauges(SafeDt);
	UpdateDisplayLag(SafeDt);
	UpdateDisplay(SafeDt);
	UpdateLight(SafeDt);
}

void CWatchDevice::CreateUI()
{
	Ui.reset();
	State.TimeText.clear();
	State.DateText.clear();
	State.BarometerText.clear();
	Display.Timer = 0.0f;
	Surge.Timer = 0.0f;

	if (!Loaded || !Rig.HasDisplay())
	{
		return;
	}

	Ui.reset(new CUIWatchWnd());
	if (!Ui->Init(Config))
	{
		Ui.reset();
		return;
	}
	Ui->SetIntensity(Display.Intensity);
}

void CWatchDevice::UpdateUILayout()
{
	const SWatchDisplay& DisplayConfig = Config.Display;
	Fvector Rotation = DisplayConfig.UiRotation;
	Rotation.mul(PI / 180.0f);
	UiOffset.setHPB(Rotation.x, Rotation.y, Rotation.z);
	UiOffset.i.mul(DisplayConfig.UiScale);
	UiOffset.j.mul(DisplayConfig.UiScale);
	UiOffset.k.mul(DisplayConfig.UiScale);
	UiOffset.translate_over(DisplayConfig.UiPosition);

	WatchLogic::MakeOffsetXform(Config.Light.UiPosition, Config.Light.UiRotation, Light.Offset);
	for (size_t Index = 0; Index < Leds.size(); ++Index)
	{
		const SWatchChannelPresent& Present = Config.Leds[Index].Present;
		WatchLogic::MakeOffsetXform(Present.UiPosition, Present.UiRotation, Leds[Index].Offset);
	}
	for (size_t Index = 0; Index < Gauges.size(); ++Index)
	{
		const SWatchConditionPresent& Present = Config.Gauges[Index].Present;
		WatchLogic::MakeOffsetXform(Present.UiIconPosition, Present.UiIconRotation, Gauges[Index].IconOffset);
		WatchLogic::MakeOffsetXform(Present.UiBarPosition, Present.UiBarRotation, Gauges[Index].BarOffset);
	}

	if (Ui)
	{
		Ui->SetLayout(DisplayConfig);
	}
	Anchors.Frame = u32(-1);
}

void CWatchDevice::UpdateDisplayFormats()
{
	WatchLogic::ParseFormat(Config.Display.TimeFormat.c_str(), false, Display.TimeTokens);
	WatchLogic::ParseFormat(Config.Display.DateFormat.c_str(), true, Display.DateTokens);
}

void CWatchDevice::UpdateDisplay(float Dt)
{
	if (!Ui || !Env->HasLevel())
	{
		return;
	}

	Display.Timer -= Dt;
	if (Display.Timer > 0.0f)
	{
		return;
	}

	Display.Timer = (Display.Hz > EPS) ? 1.0f / Display.Hz : 0.0f;
	if (Display.Freeze > 0.0f)
	{
		return;
	}

	if (Display.FreezeChance > EPS && WatchLogic::HashUnit(++Display.Tick) < Display.FreezeChance)
	{
		Display.Freeze = std::max(Display.Timer, WatchDetail::DisplayMinFreeze);
		return;
	}

	const u64 GameTime = Env->GameTimeMs();
	const u64 DelayMs = u64(Display.Delay * 1000.0f);

	SWatchGameTime Time;
	u32 Milliseconds = 0;
	split_time(GameTime > DelayMs ? GameTime - DelayMs : 0, Time.Year, Time.Month, Time.Day, Time.Hour, Time.Minute, Time.Second, Milliseconds);

	string64 Text = {};
	WatchLogic::FormatText(Display.TimeTokens, Time, Text);
	if (State.TimeText != Text)
	{
		State.TimeText = Text;
		Ui->SetTime(Text);
	}

	WatchLogic::FormatText(Display.DateTokens, Time, Text);
	if (State.DateText != Text)
	{
		State.DateText = Text;
		Ui->SetDate(Text);
	}
}

void CWatchDevice::UpdateSurge(float Dt)
{
	if (!Ui || !Env->HasLevel())
	{
		return;
	}

	Surge.Timer -= Dt;
	if (Surge.Timer > 0.0f)
	{
		return;
	}

	Surge.Timer = 1.0f / WatchDetail::SurgeSampleHz;
	Surge.Valid = Env->SampleSurge(Surge.Seconds, Surge.Progress);

	if (Preview.PreviewSurge)
	{
		State.SurgeFactor = Preview.PreviewSurgeValue;
	}
	else if (Surge.Valid && Surge.Progress >= 0.0f)
	{
		State.SurgeFactor = sinf(PI * clampr(Surge.Progress, 0.0f, 1.0f));
	}
	else
	{
		State.SurgeFactor = 0.0f;
	}

	UpdateBarometer();
}

void CWatchDevice::UpdateDisplayLag(float Dt)
{
	const SWatchLag& SurgeLag = Config.SurgeLag;
	const SWatchLag& AnomalyLag = Config.AnomalyLag;
	Surge.Lag = WatchLogic::SmoothTowards(Surge.Lag, State.SurgeFactor, Dt, WatchDetail::LagSmoothing);
	Surge.AnomalyLag = WatchLogic::SmoothTowards(Surge.AnomalyLag, State.AnomalyLevel, Dt, WatchDetail::LagSmoothing);

	const float BaseHz = Config.Display.UpdateHz;
	const float SurgeHz = WatchLogic::EvaluateLagUpdateHz(BaseHz, SurgeLag.MinUpdateHz, Surge.Lag);
	const float AnomalyHz = WatchLogic::EvaluateLagUpdateHz(BaseHz, AnomalyLag.MinUpdateHz, Surge.AnomalyLag);
	const float SurgeFreeze = clampr(SurgeLag.FreezeChance * Surge.Lag, 0.0f, 1.0f);
	const float AnomalyFreeze = clampr(AnomalyLag.FreezeChance * Surge.AnomalyLag, 0.0f, 1.0f);

	State.DisplayQuality = clampr(1.0f - Surge.Lag - Surge.AnomalyLag, 0.0f, 1.0f);
	Display.Delay = SurgeLag.MaxDelay * Surge.Lag + AnomalyLag.MaxDelay * Surge.AnomalyLag;
	Display.Hz = std::min(SurgeHz, AnomalyHz);
	Display.FreezeChance = clampr(1.0f - (1.0f - SurgeFreeze) * (1.0f - AnomalyFreeze), 0.0f, 1.0f);
	Display.Freeze = std::max(0.0f, Display.Freeze - Dt);

	float Intensity = clampr(0.20f + 0.80f * State.DisplayQuality, 0.0f, 1.0f);
	const float Flicker = clampr(SurgeLag.Flicker * Surge.Lag + AnomalyLag.Flicker * Surge.AnomalyLag, 0.0f, 1.0f);
	if (Display.Freeze > 0.0f)
	{
		Intensity = 0.0f;
	}
	else if (Flicker > EPS)
	{
		Display.GlitchPhase += Dt * (4.0f + 12.0f * Flicker);
		if (Display.GlitchPhase > WatchDetail::DisplayGlitchWrap)
		{
			Display.GlitchPhase -= WatchDetail::DisplayGlitchWrap;
		}
		const float Noise = WatchLogic::DisplayFlickerNoise(Display.GlitchPhase);
		Intensity *= 1.0f - Flicker * (0.35f + 0.65f * Noise);
		if (Noise > 0.82f && Flicker > 0.20f)
		{
			Intensity = 0.0f;
		}
	}
	else
	{
		Display.GlitchPhase = 0.0f;
	}

	State.DisplayGlitched = Display.Freeze > 0.0f || Intensity < WatchDetail::DisplayGlitchLevel;
	if (Ui && !fsimilar(Intensity, Display.Intensity))
	{
		Ui->SetIntensity(Intensity);
	}
	Display.Intensity = Intensity;
}

void CWatchDevice::UpdateBarometer()
{
	const SWatchDisplay& DisplayConfig = Config.Display;
	if (!DisplayConfig.BarometerEnabled)
	{
		SetBarometerText("");
		return;
	}

	if (Preview.PreviewSurge)
	{
		State.Barometer = Preview.PreviewSurgeValue;
	}
	else if (Surge.Valid)
	{
		float Target = 1.0f;
		if (Surge.Progress < 0.0f)
		{
			const float Minutes = Surge.Seconds / 60.0f;
			const float FarMinutes = DisplayConfig.BarometerLookaheadFar;
			const float NearMinutes = DisplayConfig.BarometerLookaheadNear;
			if (FarMinutes > NearMinutes)
			{
				Target = clampr((FarMinutes - Minutes) / (FarMinutes - NearMinutes), 0.0f, 1.0f);
			}
			else
			{
				Target = (Minutes <= NearMinutes) ? 1.0f : 0.0f;
			}
		}

		State.Barometer += (Target - State.Barometer) * WatchDetail::BarometerResponse;
	}
	else
	{
		State.Barometer = 0.0f;
		SetBarometerText("");
		return;
	}

	if (Display.Freeze > 0.0f)
	{
		return;
	}

	const float DayMinutes = float(Env->GameTimeMs() % 86400000ull) / 60000.0f;
	const float Drift = WatchDetail::BarometerDrift * (0.6f * sinf(DayMinutes * 0.21f) + 0.4f * sinf(DayMinutes * 0.047f));
	string16 Text = {};
	xr_sprintf(Text, "%.0f", DisplayConfig.BarometerNormal - State.Barometer * DisplayConfig.BarometerDrop + Drift);
	SetBarometerText(Text);
}

void CWatchDevice::SetBarometerText(const char* Text)
{
	if (State.BarometerText == Text)
	{
		return;
	}

	State.BarometerText = Text;
	if (Ui)
	{
		Ui->SetBarometer(Text);
	}
}

void CWatchDevice::UpdateLightSchedule(bool Report)
{
	if (!WatchLogic::ParseLightSchedule(Config.Light.Schedule.c_str(), Light.Keys) && Report)
	{
		Msg("! [watch] light schedule [%s] is invalid, expected ascending 'hour:day|dusk|night|dark' items", Config.Light.Schedule.size() ? Config.Light.Schedule.c_str() : "");
	}
	Light.Timer = 0.0f;
}

void CWatchDevice::UpdateLight(float Dt)
{
	if (Preview.PreviewEmission)
	{
		State.Emission = Preview.PreviewEmissionValue;
		return;
	}

	if (!Env->HasLevel())
	{
		return;
	}

	Light.Timer -= Dt;
	if (Light.Timer > 0.0f)
	{
		return;
	}

	Light.Timer = 1.0f / WatchDetail::LightSampleHz;
	const float Hour = float(Env->GameTimeMs() % 86400000ull) / 3600000.0f;
	State.Emission = WatchLogic::EvaluateLightSchedule(Light.Keys, Config.Light, Hour) * State.DisplayQuality;
}

void CWatchDevice::CreateHudPointLight(ref_light& PointLight, float Range)
{
	DestroyHudPointLight(PointLight);
	PointLight = ::Render->light_create();
	PointLight->set_type(IRender_Light::POINT);
	PointLight->set_shadow(false);
	PointLight->set_hud_mode(true);
	PointLight->set_occq_mode(false);
	PointLight->set_range(std::max(0.0f, Range));
	PointLight->set_active(false);
}

void CWatchDevice::DestroyHudPointLight(ref_light& PointLight)
{
	if (!PointLight)
	{
		return;
	}

	PointLight->set_active(false);
	PointLight.destroy();
}

void CWatchDevice::TurnOffHudPointLight(ref_light& PointLight)
{
	if (PointLight && PointLight->get_active())
	{
		PointLight->set_active(false);
	}
}

void CWatchDevice::DestroyHudLights()
{
	for (SWatchLedRuntime& Led : Leds)
	{
		DestroyHudPointLight(Led.Light);
	}
}

void CWatchDevice::TurnOffHudLights()
{
	for (SWatchLedRuntime& Led : Leds)
	{
		TurnOffHudPointLight(Led.Light);
	}
}

void CWatchDevice::PrepareAnchors(const Fmatrix& WatchesXform)
{
	const u32 Frame = Env->FrameId();
	if (Anchors.Frame == Frame)
	{
		return;
	}

	Anchors.Frame = Frame;
	Anchors.DisplayValid = Rig.DisplayXform(WatchesXform, Anchors.Display);
	if (Anchors.DisplayValid)
	{
		Anchors.Display.mulB_43(UiOffset);
	}

	const u16 GlowBone = Rig.HasCompassLight() ? Rig.Bones().CompassLight : Rig.Bones().Compass;
	Anchors.CompassValid = Rig.BoneXform(GlowBone, WatchesXform, Anchors.Compass);
	if (Anchors.CompassValid)
	{
		Anchors.Compass.mulB_43(Light.Offset);
	}

	for (u32 Index = 0; Index < Leds.size(); ++Index)
	{
		SWatchLedRuntime& Led = Leds[Index];
		Led.AnchorValid = Rig.LedXform(Index, WatchesXform, Led.Anchor);
		if (Led.AnchorValid)
		{
			Led.Anchor.mulB_43(Led.Offset);
		}
	}
}

void CWatchDevice::SyncLedLight(u32 Index)
{
	const SWatchLedElement& Element = Config.Leds[Index];
	const SWatchChannelPresent& Present = Element.Present;
	SWatchLedRuntime& Led = Leds[Index];

	if (!Led.Enabled || !Present.PresentShader || !Present.LightEnabled)
	{
		DestroyHudPointLight(Led.Light);
		return;
	}

	if (Led.Value <= EPS || !Led.AnchorValid)
	{
		TurnOffHudPointLight(Led.Light);
		return;
	}

	const float Strength = clampr(Led.Value * Present.LightBrightness * (Present.Brightness / WatchLedMaxBrightness), 0.0f, WatchDetail::HudLightMaxStrength);
	if (Strength <= EPS)
	{
		TurnOffHudPointLight(Led.Light);
		return;
	}

	if (!Led.Light)
	{
		CreateHudPointLight(Led.Light, Present.LightRange);
		if (!Led.Light)
		{
			return;
		}
	}

	const u32 Rgba = Ui ? Ui->GetLedColor(Index) : WatchDetail::LedFallbackColor(Element.Name);
	Fcolor Color;
	Color.set(
		float(color_get_R(Rgba)) * (1.0f / 255.0f) * Strength,
		float(color_get_G(Rgba)) * (1.0f / 255.0f) * Strength,
		float(color_get_B(Rgba)) * (1.0f / 255.0f) * Strength,
		1.0f
	);

	Led.Light->set_position(Led.Anchor.c);
	Led.Light->set_color(Color);
	Led.Light->set_range(std::max(0.0f, Present.LightRange));
	if (!Led.Light->get_active())
	{
		Led.Light->set_active(true);
	}
}

void CWatchDevice::SyncHudLights(const Fmatrix& WatchesXform)
{
	if (!IsEnabled() || !Rig.Kinematics())
	{
		TurnOffHudLights();
		return;
	}

	LastVisibleFrame = Env->FrameId();
	PrepareAnchors(WatchesXform);
	for (u32 Index = 0; Index < Leds.size(); ++Index)
	{
		SyncLedLight(Index);
	}
}

void CWatchDevice::UpdateAnomalyExclude()
{
	Zones.AnomalyExclude.clear();
	const char* List = Config.Anomaly.Exclude.c_str();
	const int Count = _GetItemCount(List);
	for (int Index = 0; Index < Count; ++Index)
	{
		string128 Item = {};
		_GetItem(List, Index, Item);
		if (Item[0])
		{
			Zones.AnomalyExclude.emplace_back(Item);
		}
	}
	Zones.SampleTimer = 0.0f;
}

void CWatchDevice::UpdateLeds(float Dt)
{
	float AnomalyLevel = 0.0f;
	for (u32 Index = 0; Index < Leds.size(); ++Index)
	{
		const SWatchLedElement& Element = Config.Leds[Index];
		SWatchLedRuntime& Led = Leds[Index];
		if (Led.Preview)
		{
			Led.Intensity = clampr(Led.PreviewValue, 0.0f, 1.0f);
		}
		else
		{
			const float Target = IsLedActive(Index) ? SampleSource(Led.Source, 1.0f) : 0.0f;
			Led.Intensity = WatchLogic::SmoothTowards(Led.Intensity, Target, Dt, Element.Present.Smoothing);
		}

		WatchLogic::UpdateLedBlink(Led.BlinkPhase, Led.Value, Element.Blink, Led.Intensity, Dt);
		if (Led.Source.Kind == EWatchSourceKind::Anomaly)
		{
			AnomalyLevel = std::max(AnomalyLevel, Led.Intensity);
		}
	}
	State.AnomalyLevel = AnomalyLevel;
}

void CWatchDevice::UpdateGauges(float Dt)
{
	for (u32 Index = 0; Index < Gauges.size(); ++Index)
	{
		const SWatchConditionPresent& Present = Config.Gauges[Index].Present;
		SWatchGaugeRuntime& Gauge = Gauges[Index];

		if (!IsGaugeActive(Index))
		{
			Gauge.Value = 0.0f;
			Gauge.DisplayValue = 0.0f;
			Gauge.Badness = 0.0f;
			Gauge.Visible = false;
			Gauge.Severity = EWatchConditionSeverity::None;
			continue;
		}

		const float Target = clampr(Gauge.Preview ? Gauge.PreviewValue : SampleSource(Gauge.Source, Present.Normalize), 0.0f, 1.0f);
		Gauge.Value = WatchLogic::SmoothTowards(Gauge.Value, Target, Dt, Present.Smoothing);
		Gauge.DisplayValue = clampr(Present.Invert ? (1.0f - Gauge.Value) : Gauge.Value, 0.0f, 1.0f);
		Gauge.Visible = Gauge.DisplayValue >= Present.VisibleMin && Gauge.DisplayValue <= Present.VisibleMax;
		Gauge.Badness = WatchDetail::ConditionBadness(Gauge.DisplayValue, Present.SeverityHigher);
		Gauge.Severity = WatchDetail::EvaluateConditionSeverity(Gauge.Badness, Present);
	}
}

u32 CWatchDevice::GaugeColor(const SWatchGaugeRuntime& Gauge, u32 Fallback) const
{
	const u32 Severity = u32(Gauge.Severity);
	return Gauge.HasColor[Severity] ? Gauge.Colors[Severity] : Fallback;
}

bool CWatchDevice::RenderUIQuery() const
{
	return Ui && Rig.Kinematics() && IsEnabled();
}

void CWatchDevice::RenderUI(const Fmatrix& WatchesXform)
{
	PrepareAnchors(WatchesXform);
	if (!Anchors.DisplayValid)
	{
		return;
	}

	LastVisibleFrame = Env->FrameId();
	Ui->Render(Anchors.Display);

	const float CompassIntensity = State.TargetValid
		? Config.Compass.TargetGlow * State.DisplayQuality
		: State.Emission;
	if (Anchors.CompassValid && CompassIntensity > 0.0f)
	{
		Ui->RenderCompassGlow(Anchors.Compass, Config.Light.UiSize, CompassIntensity, CompassColorForKind(State.CompassSourceKind));
	}

	for (u32 Index = 0; Index < Leds.size(); ++Index)
	{
		const SWatchChannelPresent& Present = Config.Leds[Index].Present;
		const SWatchLedRuntime& Led = Leds[Index];
		if (!Led.Enabled || Led.Value <= 0.0f)
		{
			continue;
		}

		if (Present.PresentShader && !Rig.IsLedMeshHidden(Index) && Led.AnchorValid)
		{
			Ui->RenderLedGlow(Index, Led.Anchor, Present.UiSize, Led.Value);
		}

		if (Present.PresentUi && Led.Value > EPS)
		{
			Fmatrix Xform = Anchors.Display;
			Xform.mulB_43(Led.Offset);
			Ui->RenderLedGlow(Index, Xform, Present.UiSize, Led.Value);
		}
	}

	for (u32 Index = 0; Index < Gauges.size(); ++Index)
	{
		RenderGauge(Index);
	}

	for (u32 Index = 0; Index < Leds.size(); ++Index)
	{
		const SWatchChannelPresent& Present = Config.Leds[Index].Present;
		const SWatchLedRuntime& Led = Leds[Index];
		if (Led.Enabled && Present.PresentShader && Rig.IsLedMeshHidden(Index) && Led.AnchorValid)
		{
			Ui->RenderLed(Index, Led.Anchor, Present.GlassSize, Present.UiSize, Led.Value, Present.Brightness);
		}
	}
}

void CWatchDevice::RenderGauge(u32 Index)
{
	const SWatchConditionPresent& Present = Config.Gauges[Index].Present;
	const SWatchGaugeRuntime& Gauge = Gauges[Index];
	if (!Gauge.Visible || !Present.PresentUi || Gauge.Severity == EWatchConditionSeverity::None)
	{
		return;
	}

	const float Glow = Present.Glow * WatchDetail::ConditionSeverityGlow(Gauge.Severity, Present);
	const float Intensity = clampr(Display.Intensity * Glow, 0.0f, 1.0f);
	if (Intensity <= EPS)
	{
		return;
	}

	if (Present.PresentIcon && Ui->HasGaugeIcon(Index))
	{
		Fmatrix IconXform;
		IconXform.mul(Anchors.Display, Gauge.IconOffset);
		Ui->RenderGaugeIcon(Index, IconXform, Present.UiIconSize, Intensity, GaugeColor(Gauge, Ui->GetGaugeIconColor(Index)));
	}

	if (Present.PresentBar && Ui->HasGaugeBar(Index))
	{
		Fmatrix BarXform;
		BarXform.mul(Anchors.Display, Gauge.BarOffset);
		Ui->RenderGaugeBar(Index, BarXform, Present.UiBarSize, Gauge.Badness, Intensity, GaugeColor(Gauge, Ui->GetGaugeBarColor(Index)));
	}
}

void CWatchDevice::Revert()
{
	if (!Loaded)
	{
		return;
	}

	Config = ConfigBaseline;
	for (SWatchLedRuntime& Led : Leds)
	{
		Led.Preview = false;
	}
	for (SWatchGaugeRuntime& Gauge : Gauges)
	{
		Gauge.Preview = false;
	}
	Preview = SWatchDebugPreview{};
	DestroyHudLights();
	CompileConfig(true);
	CreateUI();
}

void CWatchDevice::Reload()
{
	if (!Section.size())
	{
		return;
	}

	IKinematics* BoundModel = Rig.Kinematics();
	Unbind();
	Load(Section);

	if (BoundModel)
	{
		AttachModel(BoundModel);
	}

	Preview = SWatchDebugPreview{};
}
