#include "stdafx.h"

#include "discord.h"

XRCORE_API DiscordShared g_Discord;

DiscordShared::~DiscordShared()
{
	xr_delete(Core);
}

// Called when the game starts or when spawned
void DiscordShared::Init() noexcept 
{
#ifndef IXR_WINDOWS
	return;
#endif
	auto result = discord::Core::Create(1174634951715594311, DiscordCreateFlags_Default, &Core);

	if (Core == nullptr)
		return;

	Core->SetLogHook
	(
		discord::LogLevel::Error,
		[](discord::LogLevel MsgLvl, char const* msg)
		{
			const char* Mark = nullptr;

			switch (MsgLvl)
			{
				case discord::LogLevel::Info: Mark = "(Info)"; break;
				case discord::LogLevel::Warn: Mark = "(Warning)"; break;
				case discord::LogLevel::Error: Mark = "(Error)"; break;
				case discord::LogLevel::Debug: Mark = "(Dbg)"; break;
			}

			Msg("! [Discord] %s: %s", Mark, msg);
		}
	);

	Activity.GetAssets().SetLargeImage("logo");
	Activity.SetInstance(true);
	Activity.SetType(discord::ActivityType::Playing);

	xr_time_t start_time = xr_chrono_to_time_t(std::chrono::system_clock::now());
	Activity.GetTimestamps().SetStart(start_time);
}

// Called every frame
void DiscordShared::Update() noexcept 
{
	if (Core == nullptr)
		return;

	if (NeedSync)
	{
		SyncActivity();
		NeedSync = false;
	}

	Core->RunCallbacks();
}

void DiscordShared::SetStatus(const xr_string& Name) noexcept
{
	Status = Name;
	NeedSync = true;
}

void DiscordShared::SetPhase(const xr_string& Name) noexcept
{
	Phase = Name;
	NeedSync = true;
}

void DiscordShared::SyncActivity() noexcept 
{
	static bool isCorrect = true;

	Activity.SetDetails(ANSI_TO_UTF8(Status).c_str());
	Activity.SetState(ANSI_TO_UTF8(Phase).c_str());

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
