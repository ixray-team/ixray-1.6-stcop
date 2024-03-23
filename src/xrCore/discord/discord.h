#pragma once
#include <discord_gamesdk/cpp/discord.h>

class XRCORE_API DiscordShared
{
private:
    volatile bool NeedSync = false;
    xr_string Status;
    xr_string Phase;

protected:
    discord::Activity Activity; 
    discord::Core* Core = nullptr;

public:
    DiscordShared() : Activity({}) {}
    virtual ~DiscordShared();

    virtual void Init() noexcept;
    virtual void Update() noexcept;

    void SetStatus(const xr_string&) noexcept;
    void SetPhase(const xr_string&) noexcept;

protected:
    void SyncActivity() noexcept;
};

extern XRCORE_API DiscordShared g_Discord;
