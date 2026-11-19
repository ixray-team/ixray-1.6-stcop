#pragma once

#include "../xrUIXmlParser.h"

class UI_API CUIScrollBarProfiles final
{
public:
	static CUIScrollBarProfiles& Get();

	CUIXml* Find(const char* Profile);
	void Invalidate();

private:
	CUIXml* Document();

	xr_unique_ptr<CUIXml> Xml;
	shared_str ResolvedName;
	xrCriticalSection Lock;
};
