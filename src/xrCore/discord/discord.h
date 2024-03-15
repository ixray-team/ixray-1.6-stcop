#pragma once
#include <discord_gamesdk/cpp/discord.h>

class XRCORE_API DiscordShared
{
private:
    volatile bool NeedSync = false;

protected:
    discord::Activity activity;

public:
    constexpr DiscordShared() : activity({}) {}
    virtual ~DiscordShared() = default;

    virtual void Init() noexcept;
    virtual void Update() noexcept;

    void SetStatus(const char* Name) noexcept;
    void SetPhase(const char* Name) noexcept;

protected:
    void SyncActivity() noexcept;
};

extern XRCORE_API DiscordShared g_Discord;
