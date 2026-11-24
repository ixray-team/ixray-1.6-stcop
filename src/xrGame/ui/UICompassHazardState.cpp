#include "StdAfx.h"
#include "pch_script.h"
#include "UICompassHazardState.h"

#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrEngine/device.h"
#include "../../xrScripts/script_engine.h"
#include "../ai_space.h"
#include "../Actor.h"
#include "../ActorCondition.h"
#include "../Level.h"
#include "../AnomalyZoneRegistry.h"

namespace CompassHazardStateDefaults
{
constexpr const char* StateNodes[] = {"safe", "anomaly", "radiation", "emission"};
constexpr u32 StateColors[] = {0xFF78B9A0, 0xFFC6A7E8, 0xFFD6C36A, 0xFFE06B32};
constexpr float StateLevels[] = {0.25f, 0.5f, 0.75f, 1.f};
}

void CUICompassHazardState::InitFromXml(CUIXml& Xml, const char* Path)
{
	RelPos.set(Xml.ReadAttribFlt(Path, 0, "x", 0.f), Xml.ReadAttribFlt(Path, 0, "y", 0.f));
	RelSize.set(Xml.ReadAttribFlt(Path, 0, "width", 1.f), Xml.ReadAttribFlt(Path, 0, "height", 1.f));
	ScanRadius = Xml.ReadAttribFlt(Path, 0, "scan_radius", ScanRadius);
	RadiationThreshold = Xml.ReadAttribFlt(Path, 0, "radiation_threshold", RadiationThreshold);
	BlendSpeed = std::max(0.1f, Xml.ReadAttribFlt(Path, 0, "blend_speed", BlendSpeed));
	EmissionFunction = Xml.ReadAttrib(Path, 0, "emission_check", "surge_manager.is_started");
	EmissionPollInterval = std::max(0.05f, Xml.ReadAttribFlt(Path, 0, "emission_poll", EmissionPollInterval));

	string256 StatePath;
	for (u32 Index = 0; Index < u32(EState::Count); ++Index)
	{
		xr_strconcat(StatePath, Path, ":", CompassHazardStateDefaults::StateNodes[Index]);
		const u32 Packed = Xml.NavigateToNode(StatePath, 0) ? CUIXmlInit::GetColor(Xml, StatePath, 0, CompassHazardStateDefaults::StateColors[Index]) : CompassHazardStateDefaults::StateColors[Index];
		Styles[Index].Color.set(color_get_R(Packed) / 255.f, color_get_G(Packed) / 255.f, color_get_B(Packed) / 255.f);
		Styles[Index].Level = Xml.NavigateToNode(StatePath, 0) ? Xml.ReadAttribFlt(StatePath, 0, "level", CompassHazardStateDefaults::StateLevels[Index]) : CompassHazardStateDefaults::StateLevels[Index];
	}

	string256 GlowPath;
	xr_strconcat(GlowPath, Path, ":glow");
	if (Xml.NavigateToNode(GlowPath, 0))
	{
		Glow = UIHelper::CreateStatic(Xml, GlowPath, this, false);
	}

	CurrentColor = Styles[0].Color;
	CurrentLevel = Styles[0].Level;
}

void CUICompassHazardState::ApplyLayout()
{
	CUIWindow* Parent = GetParent();
	if (!Parent)
	{
		return;
	}
	const Fvector2 ParentSize = Parent->GetWndSize();
	const Fvector2 Size = Fvector2().set(RelSize.x * ParentSize.x, RelSize.y * ParentSize.y);
	SetWndPos(Fvector2().set(RelPos.x * ParentSize.x, RelPos.y * ParentSize.y));
	SetWndSize(Size);
	if (Glow)
	{
		Glow->SetWndPos(Fvector2().set(0.f, 0.f));
		Glow->SetWndSize(Size);
	}
}

bool CUICompassHazardState::IsEmissionActive()
{
	EmissionPollTimer -= Device.fTimeDelta;
	if (EmissionPollTimer > 0.f)
	{
		return EmissionCached;
	}
	EmissionPollTimer = EmissionPollInterval;
	EmissionCached = false;
	if (!EmissionFunction.size())
	{
		return false;
	}
	luabind::functor<bool> Check;
	if (!ai().script_engine().functor(EmissionFunction.c_str(), Check))
	{
		return false;
	}
	try
	{
		EmissionCached = Check();
	}
	catch (...)
	{
		EmissionCached = false;
	}
	return EmissionCached;
}

CUICompassHazardState::EState CUICompassHazardState::EvaluateState()
{
	CActor* Player = Actor();
	if (!Player || !g_pGameLevel)
	{
		return EState::Safe;
	}
	if (IsEmissionActive())
	{
		return EState::Emission;
	}
	const AnomalyZoneRegistry::SNearbyHazards Hazards = AnomalyZoneRegistry::Query(Player->Position(), ScanRadius);
	if (Hazards.Radiation || Player->conditions().GetRadiation() > RadiationThreshold)
	{
		return EState::Radiation;
	}
	if (Hazards.Anomaly)
	{
		return EState::Anomaly;
	}
	return EState::Safe;
}

void CUICompassHazardState::Update()
{
	ApplyLayout();
	State = EvaluateState();

	const SStateStyle& Target = Styles[u32(State)];
	const float Blend = 1.f - expf(-BlendSpeed * Device.fTimeDelta);
	CurrentColor.lerp(CurrentColor, Target.Color, Blend);
	CurrentLevel += (Target.Level - CurrentLevel) * Blend;

	if (Glow)
	{
		Glow->SetTextureColor(color_rgba(iFloor(CurrentColor.x * 255.f + 0.5f), iFloor(CurrentColor.y * 255.f + 0.5f), iFloor(CurrentColor.z * 255.f + 0.5f), iFloor(clampr(CurrentLevel, 0.f, 1.f) * 255.f + 0.5f)));
	}
	inherited::Update();
}
