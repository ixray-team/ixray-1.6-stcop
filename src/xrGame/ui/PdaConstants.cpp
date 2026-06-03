#include "stdafx.h"
#include "PdaConstants.h"
#include "../../xrUI/xrUIXmlParser.h"

namespace
{
const char* ResolveAlias(const char* defaultId)
{
    CInifile* aliasFile = nullptr;
    if (pSettings && pSettings->section_exist(PdaConfig::TabAliasesSection))
    {
        aliasFile = pSettings;
    }
    else if (pGameGlobals && pGameGlobals->section_exist(PdaConfig::TabAliasesSection))
    {
        aliasFile = pGameGlobals;
    }

    if (!aliasFile)
    {
        return defaultId;
    }

    if (!aliasFile->line_exist(PdaConfig::TabAliasesSection, defaultId))
    {
        return defaultId;
    }

    const char* alias = aliasFile->r_string(PdaConfig::TabAliasesSection, defaultId);
    if (!alias || !alias[0])
    {
        return defaultId;
    }

    return alias;
}
}

namespace PdaSectionId
{
const char* Resolve(const char* defaultId)
{
    return ResolveAlias(defaultId);
}

bool Equals(const shared_str& sectionId, const char* defaultId)
{
    const char* resolvedId = Resolve(defaultId);
    return sectionId == defaultId || sectionId == resolvedId;
}
}

namespace
{
bool ReadNodeSize(CUIXml& xml, const char* path, float& outWidth, float& outHeight)
{
	if (!xml.NavigateToNode(path, 0))
	{
		return false;
	}

	outWidth = xml.ReadAttribFlt(path, 0, "width", 0.f);
	outHeight = xml.ReadAttribFlt(path, 0, "height", 0.f);
	return true;
}

bool SizesMismatch(float widthA, float heightA, float widthB, float heightB)
{
	return std::abs(widthA - widthB) > PdaXml::ContactsFrameSizeTolerance
		|| std::abs(heightA - heightB) > PdaXml::ContactsFrameSizeTolerance;
}
} // namespace

SPdaContactsLayoutInfo InspectPdaContactsLayout(CUIXml& xml)
{
	SPdaContactsLayoutInfo info;

	XML_NODE* storedRoot = xml.GetLocalRoot();
	const XML_NODE* documentRoot = xml.GetRoot();
	if (documentRoot)
	{
		xml.SetLocalRoot(const_cast<XML_NODE*>(documentRoot));
	}

	info.hasBackground = xml.NavigateToNode(PdaXml::ContactsBackground, 0) != nullptr;
	info.hasDialogNode = xml.NavigateToNode(PdaXml::ContactsDialog, 0) != nullptr;

	if (info.hasDialogNode)
	{
		const char* dialogMainPath = "pda_dialog:main";
		const char* dialogAnswersPath = "pda_dialog:answers_list";
		const char* dialogQuestionsPath = "pda_dialog:questions_list";

		info.hasDialogMain = xml.NavigateToNode(dialogMainPath, 0) != nullptr;
		info.hasAnswersList = xml.NavigateToNode(dialogAnswersPath, 0) != nullptr;
		info.hasQuestionsList = xml.NavigateToNode(dialogQuestionsPath, 0) != nullptr;
		info.hasDialogFonts = xml.GetNodesNum(PdaXml::ContactsDialog, 0, "font") >= 2;

		ReadNodeSize(xml, PdaXml::ContactsRightFrame, info.rightFrameWidth, info.rightFrameHeight);
		ReadNodeSize(xml, dialogMainPath, info.dialogMainWidth, info.dialogMainHeight);

		if (info.hasDialogMain && info.rightFrameWidth > 0.f && info.dialogMainWidth > 0.f)
		{
			info.frameSizeMismatch = SizesMismatch(
				info.rightFrameWidth,
				info.rightFrameHeight,
				info.dialogMainWidth,
				info.dialogMainHeight
			);
		}
	}

	if (storedRoot)
	{
		xml.SetLocalRoot(storedRoot);
	}
	else if (documentRoot)
	{
		xml.SetLocalRoot(const_cast<XML_NODE*>(documentRoot));
	}

	return info;
}

bool IsPdaContactsLayoutValid(const SPdaContactsLayoutInfo& info)
{
	return info.hasDialogNode
		&& info.hasDialogMain
		&& info.hasAnswersList
		&& info.hasQuestionsList
		&& info.hasDialogFonts;
}

void LogPdaContactsLayoutIssues(const SPdaContactsLayoutInfo& info, const char* xmlFileName)
{
	const char* fileName = (xmlFileName && xmlFileName[0]) ? xmlFileName : PdaXml::ContactsNew;

	if (!info.hasDialogNode)
	{
		Msg("! [PDA] %s: missing <%s> (embedded talk UI will fallback to talk.xml)", fileName, PdaXml::ContactsDialog);
	}
	else if (!info.hasDialogMain)
	{
		Msg("! [PDA] %s: <%s> has no <%s> node", fileName, PdaXml::ContactsDialog, PdaXml::DialogMain);
	}

	if (info.hasDialogNode && !info.hasAnswersList)
	{
		Msg("! [PDA] %s: <%s> missing <%s>", fileName, PdaXml::ContactsDialog, PdaXml::DialogAnswersList);
	}

	if (info.hasDialogNode && !info.hasQuestionsList)
	{
		Msg("! [PDA] %s: <%s> missing <%s>", fileName, PdaXml::ContactsDialog, PdaXml::DialogQuestionsList);
	}

	if (info.hasDialogNode && !info.hasDialogFonts)
	{
		Msg("! [PDA] %s: <%s> needs two <font> nodes", fileName, PdaXml::ContactsDialog);
	}

	if (info.hasBackground)
	{
		Msg(
			"! [PDA] %s: optional <%s> is present; texture must exist in UI atlas (HD HUD: use pda_16 tab backdrop instead)",
			fileName,
			PdaXml::ContactsBackground
		);
	}

	if (info.frameSizeMismatch)
	{
		Msg(
			"! [PDA] %s: <%s> size %.0fx%.0f differs from <%s:%s> %.0fx%.0f (sync widths for embed)",
			fileName,
			PdaXml::ContactsRightFrame,
			info.rightFrameWidth,
			info.rightFrameHeight,
			PdaXml::ContactsDialog,
			PdaXml::DialogMain,
			info.dialogMainWidth,
			info.dialogMainHeight
		);
	}
}

STaskWndFeatures DetectTaskWndFeatures(CUIXml& xml)
{
    STaskWndFeatures features;
    features.panelStoryline = xml.NavigateToNode(PdaTaskXml::PanelStorylineItem) != nullptr;
    features.filterTabs = xml.NavigateToNode(PdaTaskXml::PanelFilterTabs)
        && xml.GetNodesNum(PdaTaskXml::PanelFilterTabs, 0, "button") > 0;

    const bool hasLegacyStoryline = xml.NavigateToNode(PdaTaskXml::LegacyStorylineItem) != nullptr;
    if (features.panelStoryline && hasLegacyStoryline)
    {
        Msg(
            "! [PDA] pda_tasks.xml: both %s and %s are present; panel storyline takes precedence",
            PdaTaskXml::PanelStorylineItem,
            PdaTaskXml::LegacyStorylineItem
        );
    }

    features.legacyHeader = !features.panelStoryline && hasLegacyStoryline;
    return features;
}
