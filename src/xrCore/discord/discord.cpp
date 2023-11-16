#include "stdafx.h"

#include "discord.h"

XRCORE_API DiscordShared g_Discord;
discord::Core* core{};

// Called when the game starts or when spawned
void DiscordShared::Init() noexcept {
    auto result = discord::Core::Create(1174634951715594311, DiscordCreateFlags_Default, &core);
}

// Called every frame
void DiscordShared::Update() noexcept {
    if (NeedSync)
    {
        SyncActivity();
        NeedSync = false;
    }
    ::core->RunCallbacks();
}

void DiscordShared::SetStatus(const char* Name) noexcept {
    activity.SetDetails(Name);

    NeedSync = true;
}

void DiscordShared::SetPhase(const char* Name) noexcept {
    activity.SetState(Name);

    NeedSync = true;
}

void DiscordShared::SyncActivity() noexcept {
    static bool isCorrect = true;
    core->ActivityManager().UpdateActivity(activity, 
        [](discord::Result result) 
        {
            if (isCorrect && result != discord::Result::Ok)
            {
                Msg("! [ERROR] Discord API: Invalid request");
                isCorrect = false;
            }
        }
    );


}
