#include "StdAfx.h"

#include "pda_talk_reward_guard.h"

#include "PhraseScript.h"
#include "pda_communication.h"
#include "Actor.h"
#include "../xrEngine/string_table.h"
#include "ai_space.h"
#include "../xrScripts/script_engine.h"
#include "script_game_object.h"
#include "UIGameCustom.h"
#include "ui/UITalkWnd.h"

namespace
{
bool ContainsSubstringI(const char* haystack, const char* needle)
{
    if (haystack == nullptr || needle == nullptr || needle[0] == 0)
    {
        return false;
    }

    const size_t needleLen = xr_strlen(needle);
    for (const char* cursor = haystack; *cursor != 0; ++cursor)
    {
        if (_strnicmp(cursor, needle, needleLen) == 0)
        {
            return true;
        }
    }

    return false;
}
} // namespace

namespace PdaTalkRewardGuard
{
bool IsPhysicalPhraseAction(const char* actionName)
{
    if (actionName == nullptr || actionName[0] == 0)
    {
        return false;
    }

    static constexpr const char* kPhysicalTokens[] = {
        "give_item",
        "give_items",
        "give_money",
        "relocate_item",
        "relocate_money",
        "transfer_item",
        "transfer_money",
    };

    for (const char* token : kPhysicalTokens)
    {
        if (ContainsSubstringI(actionName, token))
        {
            return true;
        }
    }

    return false;
}

void NotifyMeetRewardPending()
{
    CUIGameCustom* gameUi = CurrentGameUI();
    if (gameUi == nullptr || gameUi->TalkMenu == nullptr || !gameUi->TalkMenu->IsActiveTalkUi())
    {
        return;
    }

    const char* caption = g_pStringTable->translate("st_pda_talk_reward_meet_title").c_str();
    const char* text = g_pStringTable->translate("st_pda_talk_reward_meet_text").c_str();
    gameUi->TalkMenu->AddIconedMessage(
        caption,
        text,
        "ui_inGame2_Predmet_poluchen",
        "iconed_answer_item"
    );
}

void RunPhraseActions(
    const CDialogScriptHelper& helper,
    const CGameObject* speakerGo1,
    const CGameObject* speakerGo2,
    const char* dialogId,
    const char* phraseId
)
{
    bool deferredPhysical = false;

    for (u32 i = 0; i < helper.Actions().size(); ++i)
    {
        const shared_str& actionName = helper.Actions()[i];
        THROW(*actionName);

        if (PdaCommunication().IsRemotePhraseContext() && IsPhysicalPhraseAction(*actionName))
        {
            deferredPhysical = true;
            continue;
        }

        luabind::functor<void> luaFunction;
        const bool functorExists = ai().script_engine().functor(*actionName, luaFunction);
        if (!functorExists)
        {
            Msg("[ERROR] Cannot find phrase dialog script function %s", *actionName);
            return;
        }

        if (speakerGo2 != nullptr)
        {
            luaFunction(speakerGo1->lua_game_object(), speakerGo2->lua_game_object(), dialogId, phraseId);
        }
        else
        {
            luaFunction(speakerGo1->lua_game_object(), dialogId);
        }
    }

    if (deferredPhysical)
    {
        NotifyMeetRewardPending();
    }
}
} // namespace PdaTalkRewardGuard
