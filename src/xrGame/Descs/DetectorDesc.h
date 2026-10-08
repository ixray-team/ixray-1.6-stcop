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

struct SCustomDetectorDesc
{
	using Registry = TDescRegistry<SCustomDetectorDesc>;

	float AfDetectRadius = 30.0f;
	float AfVisRadius = 2.0f;
	SDetectListDesc Artefacts;

	void Load(const shared_str& Section);
};

struct SEliteDetectorDesc : SCustomDetectorDesc
{
	using Registry = TDescRegistry<SEliteDetectorDesc>;

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
