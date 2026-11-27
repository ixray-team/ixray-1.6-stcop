#pragma once

#include "../../../xrEngine/device.h"

class CViewerSessionManager
{
public:
	static constexpr u32 MaxSessions = 2;

	CViewerSessionManager()
	{
		const u32 dimension = static_cast<u32>(std::max(1, std::min(Device.Width, Device.Height))) / 2u;
		for (u32 index = 0; index < MaxSessions; ++index)
		{
			Sessions[index].SessionIndex = index;
			Sessions[index].ViewerState.ViewportWidth = dimension;
			Sessions[index].ViewerState.ViewportHeight = dimension;
		}
		Sessions[0].AutorunOwner = true;
	}

	CViewerSession& SelectSession(u32 index = MaxSessions)
	{
		if (index < MaxSessions)
			SelectedSession = index;
		return Sessions[SelectedSession];
	}

	CViewerSession Sessions[MaxSessions];
	CAI_Space* PrimaryAiSpace = nullptr;
	CLevel* PrimaryLevel = nullptr;
	CActor* PrimaryActor = nullptr;
	player_hud* PrimaryPlayerHud = nullptr;
	::CCustomHUD* PrimaryHud = nullptr;
	::IGame_CustomUI* PrimaryGameUI = nullptr;
	u32 SelectedSession = 0;
	static constexpr u32 EditorWindowCount = 5;
	bool SecondaryWindowVisible = false;
	bool WindowsVisible[EditorWindowCount] = {};
	bool Accessed = false;
};

namespace ViewerSessionDetail
{
	extern CViewerSessionManager* Manager;
}

class CViewerSessionTicker : public pureFrame, public pureRender, public pureAppEnd
{
public:
	CViewerSessionManager Manager;

	void OnFrame() override;
	void OnRender() override;
	void OnAppEnd() override;
};
