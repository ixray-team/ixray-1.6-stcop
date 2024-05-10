#pragma once
#ifdef IXR_WINDOWS
#include <discord_gamesdk/cpp/discord.h>
#endif

class XRCORE_API DiscordShared
{
private:
    volatile bool NeedSync = false;
    xr_string Status;
    xr_string Phase;

#ifdef IXR_WINDOWS
protected:
    discord::Activity Activity; 
    discord::Core* Core = nullptr;
#endif

public:
    DiscordShared();
    virtual ~DiscordShared();

    virtual void Init() noexcept;
    virtual void Update() noexcept;

    void SetStatus(const xr_string&) noexcept;
    void SetPhase(const xr_string&) noexcept;

protected:
    void SyncActivity() noexcept;
};

extern XRCORE_API DiscordShared g_Discord;
