#include "StdAfx.h"

#include "PdaTalkDialogPolicy.h"

#include "pda_communication.h"
#include "pda_talk_reward_guard.h"
#include "Phrase.h"

#include "../xrCore/EngineExternal.h"
#include "../xrCore/xr_ini.h"
#include "../xrCore/FS.h"

namespace
{
constexpr const char* kPdaTalkSection = "pda_talk";
constexpr const char* kPolicyFilesKey = "policy_files";
constexpr const char* kDialogPolicySection = "pda_talk_dialog";
constexpr const char* kPhrasePolicySection = "pda_talk_phrase";

EPdaTalkDialogPolicy ParsePolicyValue(const char* value)
{
	if (value == nullptr || value[0] == 0)
	{
		return EPdaTalkDialogPolicy::Inherit;
	}

	if (!_stricmp(value, "inherit"))
	{
		return EPdaTalkDialogPolicy::Inherit;
	}
	if (!_stricmp(value, "blocked"))
	{
		return EPdaTalkDialogPolicy::Blocked;
	}
	if (!_stricmp(value, "info_only"))
	{
		return EPdaTalkDialogPolicy::InfoOnly;
	}
	if (!_stricmp(value, "meet_required"))
	{
		return EPdaTalkDialogPolicy::MeetRequired;
	}
	if (!_stricmp(value, "allowed"))
	{
		return EPdaTalkDialogPolicy::Allowed;
	}
	if (!_stricmp(value, "pda_only"))
	{
		return EPdaTalkDialogPolicy::PdaOnly;
	}

	Msg("! [PDA] unknown dialog policy [%s], using inherit", value);
	return EPdaTalkDialogPolicy::Inherit;
}

bool IsPhysicalActionName(const char* actionName)
{
	return PdaTalkRewardGuard::IsPhysicalPhraseAction(actionName);
}

bool SplitPhrasePolicyKey(const shared_str& key, shared_str& dialogIdOut, shared_str& phraseIdOut)
{
	const char* keyText = key.c_str();
	if (keyText == nullptr)
	{
		return false;
	}

	const char* colon = strchr(keyText, ':');
	if (colon == nullptr || colon == keyText)
	{
		return false;
	}

	const ptrdiff_t dialogLen = colon - keyText;
	if (dialogLen <= 0 || dialogLen >= static_cast<ptrdiff_t>(sizeof(string256)))
	{
		return false;
	}

	string256 dialogBuffer = {};
	strncpy_s(dialogBuffer, sizeof(dialogBuffer), keyText, static_cast<size_t>(dialogLen));
	dialogIdOut = dialogBuffer;
	phraseIdOut = colon + 1;
	return phraseIdOut.size() > 0;
}

void LoadDialogPoliciesFromIni(const CInifile* ini, xr_map<shared_str, EPdaTalkDialogPolicy>& out)
{
	if (ini == nullptr || !ini->section_exist(kDialogPolicySection))
	{
		return;
	}

	const CInifile::Sect& section = ini->r_section(kDialogPolicySection);
	for (const CInifile::Item& item : section.Data)
	{
		if (!item.first.size() || !item.second.size())
		{
			continue;
		}

		out[item.first] = ParsePolicyValue(item.second.c_str());
	}
}

void LoadPhrasePoliciesFromIni(const CInifile* ini, xr_map<shared_str, EPdaTalkDialogPolicy>& out)
{
	if (ini == nullptr || !ini->section_exist(kPhrasePolicySection))
	{
		return;
	}

	const CInifile::Sect& section = ini->r_section(kPhrasePolicySection);
	for (const CInifile::Item& item : section.Data)
	{
		if (!item.first.size() || !item.second.size())
		{
			continue;
		}

		shared_str dialogId;
		shared_str phraseId;
		if (!SplitPhrasePolicyKey(item.first, dialogId, phraseId))
		{
			Msg("! [PDA] %s: invalid key [%s], expected dialog_id:phrase_id", kPhrasePolicySection, item.first.c_str());
			continue;
		}

		string256 phraseKey = {};
		xr_sprintf(phraseKey, "%s:%s", dialogId.c_str(), phraseId.c_str());
		out[phraseKey] = ParsePolicyValue(item.second.c_str());
	}
}

void AppendPolicyFile(const char* fileStem, xr_map<shared_str, EPdaTalkDialogPolicy>& dialogOut, xr_map<shared_str, EPdaTalkDialogPolicy>& phraseOut)
{
	if (fileStem == nullptr || fileStem[0] == 0)
	{
		return;
	}

	string_path fileName = {};
	if (strchr(fileStem, '.') != nullptr)
	{
		xr_strcpy(fileName, fileStem);
	}
	else
	{
		xr_sprintf(fileName, "%s.ltx", fileStem);
	}

	string_path configPath = {};
	FS.update_path(configPath, "$game_config$", fileName);
	if (!FS.exist(configPath))
	{
		Msg("! [PDA] policy file not found: %s", configPath);
		return;
	}

	const CInifile policyIni(configPath, true);
	LoadDialogPoliciesFromIni(&policyIni, dialogOut);
	LoadPhrasePoliciesFromIni(&policyIni, phraseOut);
}

void ParsePolicyFileList(const char* fileList, xr_map<shared_str, EPdaTalkDialogPolicy>& dialogOut, xr_map<shared_str, EPdaTalkDialogPolicy>& phraseOut)
{
	if (fileList == nullptr || fileList[0] == 0)
	{
		return;
	}

	string4096 buffer = {};
	xr_strcpy(buffer, fileList);

	char* context = nullptr;
	for (char* token = strtok_s(buffer, ",;", &context); token != nullptr; token = strtok_s(nullptr, ",;", &context))
	{
		AppendPolicyFile(token, dialogOut, phraseOut);
	}
}
} // namespace

CPdaTalkDialogPolicy& CPdaTalkDialogPolicy::Get()
{
	static CPdaTalkDialogPolicy registry;
	return registry;
}

void CPdaTalkDialogPolicy::loadFromIni(const CInifile* ini) const
{
	LoadDialogPoliciesFromIni(ini, _dialogPolicies);
	LoadPhrasePoliciesFromIni(ini, _phrasePolicies);
}

void CPdaTalkDialogPolicy::loadPolicyFiles(const CInifile* pdaTalkIni) const
{
	if (pdaTalkIni == nullptr || !pdaTalkIni->section_exist(kPdaTalkSection))
	{
		return;
	}

	if (!pdaTalkIni->line_exist(kPdaTalkSection, kPolicyFilesKey))
	{
		return;
	}

	const char* fileList = pdaTalkIni->r_string(kPdaTalkSection, kPolicyFilesKey);
	ParsePolicyFileList(fileList, _dialogPolicies, _phrasePolicies);
}

void CPdaTalkDialogPolicy::EnsureLoaded() const
{
	if (_loaded)
	{
		return;
	}

	_loaded = true;

	if (pSettings != nullptr)
	{
		loadFromIni(pSettings);
		loadPolicyFiles(pSettings);
	}

	if (pGameGlobals != nullptr)
	{
		loadFromIni(pGameGlobals);
		loadPolicyFiles(pGameGlobals);
	}

	CInifile* engineIni = EngineExternal().GetIniFile();
	if (engineIni != nullptr)
	{
		loadPolicyFiles(engineIni);
	}
}

EPdaTalkDialogPolicy CPdaTalkDialogPolicy::GetDialogPolicy(const shared_str& dialogId) const
{
	EnsureLoaded();

	const auto it = _dialogPolicies.find(dialogId);
	if (it == _dialogPolicies.end())
	{
		return EPdaTalkDialogPolicy::Inherit;
	}

	return it->second;
}

EPdaTalkDialogPolicy CPdaTalkDialogPolicy::GetPhrasePolicy(const shared_str& dialogId, const shared_str& phraseId) const
{
	string256 phraseKey = {};
	xr_sprintf(phraseKey, "%s:%s", dialogId.c_str(), phraseId.c_str());
	const auto it = _phrasePolicies.find(phraseKey);
	if (it == _phrasePolicies.end())
	{
		return EPdaTalkDialogPolicy::Inherit;
	}

	return it->second;
}

EPdaTalkDialogPolicy CPdaTalkDialogPolicy::ResolvePhrasePolicy(const shared_str& dialogId, const shared_str& phraseId) const
{
	const EPdaTalkDialogPolicy phrasePolicy = GetPhrasePolicy(dialogId, phraseId);
	if (phrasePolicy != EPdaTalkDialogPolicy::Inherit)
	{
		return phrasePolicy;
	}

	return GetDialogPolicy(dialogId);
}

bool CPdaTalkDialogPolicy::IsDialogAllowed(const shared_str& dialogId, bool isPdaMode) const
{
	EnsureLoaded();

	const EPdaTalkDialogPolicy policy = GetDialogPolicy(dialogId);

	if (isPdaMode)
	{
		return policy != EPdaTalkDialogPolicy::Blocked;
	}

	return policy != EPdaTalkDialogPolicy::PdaOnly;
}

bool CPdaTalkDialogPolicy::IsPhraseAllowed(const shared_str& dialogId, const CPhrase* phrase, bool isPdaMode) const
{
	if (phrase == nullptr || !phrase->IsEnabled())
	{
		return false;
	}

	if (!IsDialogAllowed(dialogId, isPdaMode))
	{
		return false;
	}

	if (!isPdaMode)
	{
		return true;
	}

	const EPdaTalkDialogPolicy phrasePolicy = ResolvePhrasePolicy(dialogId, phrase->GetID());
	return phrasePolicy != EPdaTalkDialogPolicy::Blocked;
}

bool CPdaTalkDialogPolicy::ShouldDeferPhraseAction(const shared_str& dialogId, const shared_str& phraseId, const char* actionName) const
{
	EnsureLoaded();

	if (!PdaCommunication().IsRemotePhraseContext())
	{
		return false;
	}

	const EPdaTalkDialogPolicy phrasePolicy = ResolvePhrasePolicy(dialogId, phraseId);
	const EPdaTalkDialogPolicy dialogPolicy = GetDialogPolicy(dialogId);

	if (phrasePolicy == EPdaTalkDialogPolicy::Allowed)
	{
		return false;
	}

	if (phrasePolicy == EPdaTalkDialogPolicy::MeetRequired)
	{
		return IsPhysicalActionName(actionName);
	}

	if (phrasePolicy == EPdaTalkDialogPolicy::Blocked)
	{
		return IsPhysicalActionName(actionName);
	}

	if (dialogPolicy == EPdaTalkDialogPolicy::Allowed)
	{
		return false;
	}

	if (dialogPolicy == EPdaTalkDialogPolicy::MeetRequired || dialogPolicy == EPdaTalkDialogPolicy::InfoOnly)
	{
		return IsPhysicalActionName(actionName);
	}

	return IsPhysicalActionName(actionName);
}
