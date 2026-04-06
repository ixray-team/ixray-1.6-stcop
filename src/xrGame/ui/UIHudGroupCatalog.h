#pragma once

#include "../../xrCore/_stl_extensions.h"
#include "../../xrCore/xrstring.h"

struct SHudGroupDesc final
{
    shared_str _shortLabel;
    shared_str _iconTexture;
};

struct SHudSectionDesc final
{
    u32 _groupIndex = 0;
    shared_str _label;
};

class CUIHudGroupCatalog final
{
public:
    CUIHudGroupCatalog();
    ~CUIHudGroupCatalog();

    CUIHudGroupCatalog(const CUIHudGroupCatalog&) = delete;
    CUIHudGroupCatalog& operator=(const CUIHudGroupCatalog&) = delete;

    bool Load(LPCSTR fileName);
    bool IsLoaded() const { return _isLoaded; }
    bool IsEmpty() const { return _groups.empty(); }

    bool TryGetSection(const shared_str& sectionName, u32& outGroupIndex, shared_str& outSectionLabel) const;
    bool FindGroupIndex(const shared_str& sectionName, u32& outGroupIndex) const;
    bool FindSectionLabel(const shared_str& sectionName, shared_str& outLabel) const;
    const SHudGroupDesc* GetGroup(u32 index) const;

private:
    xr_vector<SHudGroupDesc> _groups;
    xr_map<shared_str, SHudSectionDesc> _sectionToDesc;
    bool _isLoaded = false;
};
