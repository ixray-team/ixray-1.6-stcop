#include "StdAfx.h"
#include "UIMapActorCompass.h"
#include "UIMap.h"

#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/Widgets/UIStatic.h"

#include "../../xrEngine/device.h"
#include "../../xrEngine/string_table.h"
#include "../Level.h"

namespace MapActorCompassDefaults
{
constexpr const char* SectorSuffixes[CUIMapActorCompass::SectorCount] = {"n", "ne", "e", "se", "s", "sw", "w", "nw"};
constexpr const char* DefaultPrefix = "pda_compass_";
constexpr const char* DefaultFormat = "pda_compass_format";
constexpr const char* FallbackFormat = "%s %d";
}

void CUIMapActorCompass::InitFromXml(CUIXml& Xml, const char* Path)
{
	CUIXmlInit::InitWindow(Xml, Path, 0, this);

	string512 Buffer;
	xr_strconcat(Buffer, Path, ":arrow");
	Arrow = UIHelper::CreateStatic(Xml, Buffer, this, false);
	if (Arrow)
	{
		Arrow->EnableHeading(true);
	}

	xr_strconcat(Buffer, Path, ":label");
	Label = UIHelper::CreateStatic(Xml, Buffer, this, false);

	SmoothSpeed = std::max(0.0f, Xml.ReadAttribFlt(Path, 0, "smooth_speed", 12.0f));
	FollowActor = Xml.ReadAttribInt(Path, 0, "follow_actor", 0) != 0;
	HideActorSpot = FollowActor && Xml.ReadAttribInt(Path, 0, "hide_actor_spot", 0) != 0;

	const char* Prefix = Xml.ReadAttrib(Path, 0, "sector_prefix", MapActorCompassDefaults::DefaultPrefix);
	for (u32 Index = 0; Index < SectorCount; ++Index)
	{
		xr_strconcat(Buffer, Prefix, MapActorCompassDefaults::SectorSuffixes[Index]);
		SectorIds[Index] = Buffer;
	}

	const char* FormatId = Xml.ReadAttrib(Path, 0, "format", MapActorCompassDefaults::DefaultFormat);
	const char* Translated = g_pStringTable->translate(FormatId).c_str();
	const bool Valid = Translated && xr_strcmp(Translated, FormatId) && strstr(Translated, "%s") && strstr(Translated, "%d");
	Format = Valid ? Translated : MapActorCompassDefaults::FallbackFormat;

	Show(false);
}

void CUIMapActorCompass::UpdateFromMap(CUICustomMap* Map, const Fvector2& OwnerAbsPos)
{
	const bool HasActor = g_pGameLevel && Level().CurrentEntity();
	if (!HasActor || (FollowActor && (!Map || !Map->IsShown())))
	{
		Show(false);
		HasHeading = false;
		return;
	}

	if (FollowActor)
	{
		const Fvector& Position = Level().CurrentEntity()->Position();
		const Fvector2 Local = Map->ConvertRealToLocal(Fvector2().set(Position.x, Position.z), !Map->Heading());
		Fvector2 MapAbs;
		Map->GetAbsolutePos(MapAbs);
		SetWndPos(Fvector2().set(MapAbs.x + Local.x - OwnerAbsPos.x - GetWidth() * 0.5f, MapAbs.y + Local.y - OwnerAbsPos.y - GetHeight() * 0.5f));
	}
	Show(true);

	Fvector2 Flat;
	Flat.set(Device.vCameraDirection.x, Device.vCameraDirection.z);
	if (Flat.square_magnitude() > EPS)
	{
		const float Target = Flat.getH();
		if (!HasHeading || SmoothSpeed <= 0.0f)
		{
			ShownHeading = Target;
			HasHeading = true;
		}
		else
		{
			const float Diff = angle_normalize_signed(Target - ShownHeading);
			const float Blend = 1.0f - expf(-SmoothSpeed * Device.fTimeDeltaContinual);
			ShownHeading = fis_zero(Diff, EPS_L) ? Target : angle_normalize_signed(ShownHeading + Diff * Blend);
		}
		UpdateLabel(rad2deg(angle_normalize(-Target)));
	}

	if (Arrow)
	{
		Arrow->SetHeading(ShownHeading + (FollowActor && Map->Heading() ? Map->GetHeading() : 0.0f));
	}
}

void CUIMapActorCompass::UpdateLabel(float AzimuthDegrees)
{
	if (!Label)
	{
		return;
	}

	const int Degrees = iFloor(AzimuthDegrees + 0.5f) % 360;
	const int Sector = iFloor((AzimuthDegrees + 22.5f) / 45.0f) % int(SectorCount);
	if (Degrees == LastDegrees && Sector == LastSector)
	{
		return;
	}
	LastDegrees = Degrees;
	LastSector = Sector;

	string128 Text;
	xr_sprintf(Text, Format.c_str(), g_pStringTable->translate(SectorIds[Sector]).c_str(), Degrees);
	Label->TextItemControl()->SetText(Text);
}
