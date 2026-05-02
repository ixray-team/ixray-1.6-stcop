#pragma once

class CDialogScriptHelper;
class CGameObject;

namespace PdaTalkRewardGuard
{
bool IsPhysicalPhraseAction(const char* actionName);

void RunPhraseActions(
    const CDialogScriptHelper& helper,
    const CGameObject* speakerGo1,
    const CGameObject* speakerGo2,
    const char* dialogId,
    const char* phraseId
);

void NotifyMeetRewardPending();
} // namespace PdaTalkRewardGuard
