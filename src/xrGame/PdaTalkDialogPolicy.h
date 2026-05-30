#pragma once

#include "../xrCore/_types.h"

class CInifile;
class CPhrase;

enum class EPdaTalkDialogPolicy : u8
{
	Inherit = 0,
	Blocked,
	InfoOnly,
	MeetRequired,
	Allowed,
	PdaOnly,
};

// External LTX overlay for legacy dialog PDA rules ([pda_talk_dialog], [pda_talk_phrase], optional policy_files).
class CPdaTalkDialogPolicy final
{
public:
	static CPdaTalkDialogPolicy& Get();

	void EnsureLoaded() const;

	EPdaTalkDialogPolicy GetDialogPolicy(const shared_str& dialogId) const;
	EPdaTalkDialogPolicy GetPhrasePolicy(const shared_str& dialogId, const shared_str& phraseId) const;
	EPdaTalkDialogPolicy ResolvePhrasePolicy(const shared_str& dialogId, const shared_str& phraseId) const;

	bool IsDialogAllowed(const shared_str& dialogId, bool isPdaMode) const;
	bool IsPhraseAllowed(const shared_str& dialogId, const CPhrase* phrase, bool isPdaMode) const;
	bool ShouldDeferPhraseAction(const shared_str& dialogId, const shared_str& phraseId, const char* actionName) const;

private:
	CPdaTalkDialogPolicy() = default;

	void loadFromIni(const CInifile* ini) const;
	void loadPolicyFiles(const CInifile* pdaTalkIni) const;

	mutable bool _loaded = false;
	mutable xr_map<shared_str, EPdaTalkDialogPolicy> _dialogPolicies;
	mutable xr_map<shared_str, EPdaTalkDialogPolicy> _phrasePolicies;
};

IC CPdaTalkDialogPolicy& PdaTalkDialogPolicy() { return CPdaTalkDialogPolicy::Get(); }
