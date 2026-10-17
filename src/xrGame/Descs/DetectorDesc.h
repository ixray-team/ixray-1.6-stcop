#pragma once
#include "DescRegistry.h"

struct SDetectTypeDesc
{
	Fvector2 Freq = {};
	shared_str SoundLine;
};

struct SDetectListDesc
{
	xr_map<shared_str, SDetectTypeDesc> Types;

	void Load(const shared_str& Section, const char* Prefix);
};

XREFLECT(IniRead)
struct SCustomDetectorDesc
{
	using Registry = TDescRegistry<SCustomDetectorDesc>;

	XPROPERTY(IniReadExist, Name="af_radius")
	float AfDetectRadius = 30.0f;
	XPROPERTY(IniReadExist, Name="af_vis_radius")
	float AfVisRadius = 2.0f;
	SDetectListDesc Artefacts;

	void Load(const shared_str& Section);
};

XREFLECT(IniRead)
struct SEliteDetectorDesc : SCustomDetectorDesc
{
	using Registry = TDescRegistry<SEliteDetectorDesc>;

	XPROPERTY(IniReadExist, Name="ui_xml_tag")
	shared_str UiXmlTag = "elite";
	Fmatrix UiAttachOffset = Fidentity;

	void Load(const shared_str& Section);
};

struct SScientificDetectorDesc final : SEliteDetectorDesc
{
	using Registry = TDescRegistry<SScientificDetectorDesc>;

	SDetectListDesc Zones;

	SScientificDetectorDesc()
	{
		UiXmlTag = "scientific";
	}

	void Load(const shared_str& Section);
};
