#include "StdAfx.h"
#include "UIHudGroupCatalog.h"

#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrCore/FormatParsers/XML/xrXMLParser.h"

namespace
{
XML_NODE* FindCatalogRoot(CUIXml& xml)
{
    static const char* roots[] = {"hud_group_catalog", "ammo_caliber_groups"};
    XML_NODE* const docRoot = xml.GetRoot();
    if (docRoot)
    {
        if (const tinyxml2::XMLElement* docEl = docRoot->ToElement())
        {
            const char* const elementName = docEl->Name();
            for (const char* rootTag : roots)
            {
                if (xr_strcmp(elementName, rootTag) == 0)
                {
                    return docRoot;
                }
            }
        }
    }
    for (const char* rootTag : roots)
    {
        if (XML_NODE* node = xml.NavigateToNode(rootTag, 0))
        {
            return node;
        }
    }
    return nullptr;
}

shared_str ReadGroupShortLabel(CUIXml& xml, XML_NODE* groupNode)
{
    shared_str groupLabel = xml.ReadAttrib(groupNode, "short", "");
    if (!groupLabel.size())
    {
        groupLabel = xml.ReadAttrib(groupNode, "short_text", "");
    }
    if (!groupLabel.size())
    {
        XML_NODE* nameNode = xml.NavigateToNode(groupNode, "name", 0);
        if (nameNode)
        {
            const char* nameText = nullptr;
            if (const tinyxml2::XMLElement* nameEl = nameNode->ToElement())
            {
                nameText = nameEl->GetText();
            }
            if (!nameText || !nameText[0])
            {
                nameText = xml.Read(nameNode, nullptr);
            }
            if (nameText && nameText[0])
            {
                groupLabel._set(nameText);
            }
        }
    }
    return groupLabel;
}

bool InsertSectionFromNode(
    CUIXml& xml,
    XML_NODE* sectionNode,
    u32 groupIndex,
    const char* fileName,
    int groupIndexForLog,
    xr_map<shared_str, SHudSectionDesc>& sectionToDesc)
{
    shared_str sectionName = xml.ReadAttrib(sectionNode, "name", "");
    if (!sectionName.size())
    {
        Msg("! CUIHudGroupCatalog::Load: section without name in group[%d] [%s]", groupIndexForLog, fileName);
        return false;
    }

    if (sectionToDesc.find(sectionName) != sectionToDesc.end())
    {
        Msg("! CUIHudGroupCatalog::Load: duplicate section [%s], keeping first binding [%s]",
            sectionName.c_str(),
            fileName);
        return false;
    }

    shared_str sectionLabel;
    if (const tinyxml2::XMLElement* sectionEl = sectionNode->ToElement())
    {
        const char* labelText = sectionEl->GetText();
        if (labelText && labelText[0])
        {
            sectionLabel._set(labelText);
        }
    }

    SHudSectionDesc sectionDesc;
    sectionDesc._groupIndex = groupIndex;
    sectionDesc._label = sectionLabel;
    sectionToDesc[sectionName] = sectionDesc;
    return true;
}
} // namespace

CUIHudGroupCatalog::CUIHudGroupCatalog() = default;

CUIHudGroupCatalog::~CUIHudGroupCatalog() = default;

bool CUIHudGroupCatalog::Load(const char* fileName)
{
    _groups.clear();
    _sectionToDesc.clear();
    _isLoaded = false;

    if (!fileName || !fileName[0])
    {
        Msg("! CUIHudGroupCatalog::Load: empty file name");
        return false;
    }

    CUIXml xml;
    string_path uiRelativePath{};
    {
        shared_str correctedName = xml.correct_file_name(UI_PATH, fileName);
        xr_sprintf(uiRelativePath, "%s\\%s", UI_PATH, correctedName.c_str());
    }
    CXml::RemoveFromCache(CONFIG_PATH, uiRelativePath);

    if (!xml.Load(CONFIG_PATH, UI_PATH, fileName))
    {
        Msg("! CUIHudGroupCatalog::Load: cannot load [%s]", fileName);
        return false;
    }

    XML_NODE* const root = FindCatalogRoot(xml);
    if (!root)
    {
        Msg("! CUIHudGroupCatalog::Load: missing <hud_group_catalog> or <ammo_caliber_groups> in [%s]", fileName);
        return false;
    }

    const int groupCount = xml.GetNodesNum(root, "group");
    for (int groupIndex = 0; groupIndex < groupCount; ++groupIndex)
    {
        XML_NODE* groupNode = xml.NavigateToNode(root, "group", groupIndex);
        if (!groupNode)
        {
            continue;
        }

        SHudGroupDesc desc;
        desc._shortLabel = ReadGroupShortLabel(xml, groupNode);
        desc._iconTexture = xml.ReadAttrib(groupNode, "icon", "");
        const u32 newGroupIndex = (u32)_groups.size();
        _groups.push_back(desc);

        const int sectionCount = xml.GetNodesNum(groupNode, "section");
        if (sectionCount <= 0)
        {
            Msg("! CUIHudGroupCatalog::Load: group[%d] has no <section> children [%s]", groupIndex, fileName);
        }

        for (int sectionIndex = 0; sectionIndex < sectionCount; ++sectionIndex)
        {
            XML_NODE* sectionNode = xml.NavigateToNode(groupNode, "section", sectionIndex);
            if (!sectionNode)
            {
                continue;
            }
            InsertSectionFromNode(xml, sectionNode, newGroupIndex, fileName, groupIndex, _sectionToDesc);
        }
    }

    if (_groups.empty())
    {
        Msg("! CUIHudGroupCatalog::Load: no valid groups in [%s]", fileName);
        return false;
    }

    _isLoaded = true;
    return true;
}

bool CUIHudGroupCatalog::TryGetSection(const shared_str& sectionName, u32& outGroupIndex, shared_str& outSectionLabel) const
{
    const auto sectionIt = _sectionToDesc.find(sectionName);
    if (sectionIt == _sectionToDesc.end())
    {
        return false;
    }
    outGroupIndex = sectionIt->second._groupIndex;
    outSectionLabel = sectionIt->second._label;
    return true;
}

bool CUIHudGroupCatalog::FindGroupIndex(const shared_str& sectionName, u32& outGroupIndex) const
{
    shared_str unusedLabel;
    return TryGetSection(sectionName, outGroupIndex, unusedLabel);
}

bool CUIHudGroupCatalog::FindSectionLabel(const shared_str& sectionName, shared_str& outLabel) const
{
    u32 unusedGroup = 0;
    return TryGetSection(sectionName, unusedGroup, outLabel);
}

const SHudGroupDesc* CUIHudGroupCatalog::GetGroup(u32 index) const
{
    if (index >= _groups.size())
    {
        return nullptr;
    }
    return &_groups[index];
}
