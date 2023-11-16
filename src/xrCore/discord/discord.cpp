#include "stdafx.h"

#include "discord.h"

XRCORE_API DiscordShared g_Discord;

DiscordShared::~DiscordShared()
{
    delete Core;
}

// Called when the game starts or when spawned
void DiscordShared::Init() noexcept 
{
    auto result = discord::Core::Create(1174634951715594311, DiscordCreateFlags_Default, &Core);
    Core->SetLogHook
    (
        discord::LogLevel::Error,
        [](discord::LogLevel, char const* msg)
        {
            Msg("! [Discord]: %s", msg);
        }
    );
}

// Called every frame
void DiscordShared::Update() noexcept 
{
    if (NeedSync)
    {
        SyncActivity();
        NeedSync = false;
    }

    Core->RunCallbacks();
}

void DiscordShared::SetStatus(const char* Name) noexcept 
{
    Activity.SetDetails(Name);
    NeedSync = true;
}

void DiscordShared::SetPhase(const char* Name) noexcept 
{
    Activity.SetState(Name);
    NeedSync = true;
}

void DiscordShared::SyncActivity() noexcept 
{
    static bool isCorrect = true;
    Core->ActivityManager().UpdateActivity
    (
        Activity, 
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
