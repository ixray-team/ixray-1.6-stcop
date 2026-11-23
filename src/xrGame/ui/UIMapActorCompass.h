#pragma once

#include "../../xrUI/Widgets/UIWindow.h"

class CUIXml;
class CUIStatic;
class CUICustomMap;

class CUIMapActorCompass final : public CUIWindow
{
	using inherited = CUIWindow;

public:
	static constexpr u32 SectorCount = 8;

	void InitFromXml(CUIXml& Xml, const char* Path);
	void UpdateFromMap(CUICustomMap* Map, const Fvector2& OwnerAbsPos);
	bool HidesActorSpot() const
	{
		return HideActorSpot;
	}

private:
	void UpdateLabel(float AzimuthDegrees);

	CUIStatic* Arrow = nullptr;
	CUIStatic* Label = nullptr;
	shared_str SectorIds[SectorCount];
	xr_string Format;
	float SmoothSpeed = 12.0f;
	float ShownHeading = 0.0f;
	bool HasHeading = false;
	bool HideActorSpot = false;
	bool FollowActor = false;
	int LastDegrees = -1;
	int LastSector = -1;
};
