#include "stdafx.h"
#include "UIScrollBarProfiles.h"
#include "../ui_base.h"

namespace ScrollBarProfilesFile
{
constexpr const char* Name = "scroll_bar.xml";
}

CUIScrollBarProfiles& CUIScrollBarProfiles::Get()
{
	static CUIScrollBarProfiles Instance;
	return Instance;
}

CUIXml* CUIScrollBarProfiles::Document()
{
	const shared_str Resolved = UI().get_xml_name(ScrollBarProfilesFile::Name);
	if (Xml && Resolved == ResolvedName)
	{
		return Xml.get();
	}

	xr_unique_ptr<CUIXml> Loaded = xr_make_unique<CUIXml>();
	if (!Loaded->Load(CONFIG_PATH, UI_PATH, ScrollBarProfilesFile::Name))
	{
		return nullptr;
	}

	Xml = std::move(Loaded);
	ResolvedName = Resolved;
	return Xml.get();
}

CUIXml* CUIScrollBarProfiles::Find(const char* Profile)
{
	if (!Profile || !Profile[0])
	{
		return nullptr;
	}

	xrCriticalSectionGuard Guard(Lock);
	CUIXml* Doc = Document();
	return Doc && Doc->NavigateToNode(Profile, 0) ? Doc : nullptr;
}

void CUIScrollBarProfiles::Invalidate()
{
	xrCriticalSectionGuard Guard(Lock);
	Xml.reset();
	ResolvedName = nullptr;
}
