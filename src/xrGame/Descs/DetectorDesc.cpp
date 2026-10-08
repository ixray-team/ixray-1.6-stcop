#include "StdAfx.h"
#include "DetectorDesc.h"

void SDetectListDesc::Load(const shared_str& Section, const char* Prefix)
{
	string256 Temp = {};

	for (u32 Index = 1;; ++Index)
	{
		xr_sprintf(Temp, "%s_class_%d", Prefix, Index);
		if (!pSettings->line_exist(Section, Temp))
		{
			break;
		}

		SDetectTypeDesc& Type = Types[pSettings->r_string(Section, Temp)];

		xr_sprintf(Temp, "%s_freq_%d", Prefix, Index);
		Type.Freq = pSettings->r_fvector2(Section, Temp);
		Type.SoundLine.printf("%s_sound_%d_", Prefix, Index);
	}
}

void SCustomDetectorDesc::Load(const shared_str& Section)
{
	AfDetectRadius = READ_IF_EXISTS(pSettings, r_float, Section, "af_radius", 30.0f);
	AfVisRadius = READ_IF_EXISTS(pSettings, r_float, Section, "af_vis_radius", 2.0f);
	Artefacts.Load(Section, "af");
}

void SEliteDetectorDesc::Load(const shared_str& Section)
{
	SCustomDetectorDesc::Load(Section);

	UiXmlTag = READ_IF_EXISTS(pSettings, r_string, Section, "ui_xml_tag", *UiXmlTag);

	Fvector AttachPos = pSettings->r_fvector3(Section, "ui_p");
	Fvector AttachRot = pSettings->r_fvector3(Section, "ui_r");

	AttachRot.mul(PI / 180.f);
	UiAttachOffset.setHPB(AttachRot.x, AttachRot.y, AttachRot.z);
	UiAttachOffset.translate_over(AttachPos);
}

void SScientificDetectorDesc::Load(const shared_str& Section)
{
	SEliteDetectorDesc::Load(Section);
	Zones.Load(Section, "zone");
}
