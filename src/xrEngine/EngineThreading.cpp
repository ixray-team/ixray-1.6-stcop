#include "stdafx.h"
#include "EngineThreading.h"
#include "CustomHUD.h"
#include "IGame_Persistent.h"
#include "IGame_Level.h"
#include "Rain.h"
#include "../Include/xrRender/RenderVisual.h"
#include "../Include/xrRender/ParticleCustom.h"
#include "../xrCore/discord/discord.h"
#include "../xrCore/Save/SaveManager.h"


#if !defined(MASTER_GOLD) && defined(IXR_WINDOWS) && (defined(_MSC_VER) || (defined(__clang__) && defined(_MSC_EXTENSIONS)))
#define ALLOW_SEH_EXCEPTIONS
#include <windows.h> // for EXCEPTION_ACCESS_VIOLATION
#include <excpt.h>

int ex_filter(unsigned int code, struct _EXCEPTION_POINTERS *ep)
{
	if (IsDebuggerPresent())
	{
		DebugBreak();
	}
	ProcessStackTrace(ep);
	return EXCEPTION_EXECUTE_HANDLER;
}
#endif

void XRay::Engine::PreRenderThread()
{
#ifdef ALLOW_SEH_EXCEPTIONS
	__try
#endif
	{
		{
			PROF_EVENT("Discord Sync");
			g_Discord.Update();
		}
	
		{
			PROF_EVENT("seqParallelRender");
			for (auto& it : Device.seqParallelRender)
				it();
		}
	
		if(g_pGamePersistent && !g_pGamePersistent->m_pMainMenu->IsActive())
		{
			if (g_pGamePersistent->pEnvironment && g_pGamePersistent->pEnvironment->eff_Rain)
				g_pGamePersistent->pEnvironment->eff_Rain->UpdateItems();
	
			g_pGamePersistent->UpdateParticles();
		}
		PROF_STOP_THREAD();
		Platform::SetThreadName("X-Ray Empty Task");
#ifdef ALLOW_SEH_EXCEPTIONS
	} __except(ex_filter(GetExceptionCode(), GetExceptionInformation()))
	{
		FATAL("Unhandled exception in PreRenderThread!");
#endif
	}
}

void XRay::Engine::GameThread()
{
#ifdef ALLOW_SEH_EXCEPTIONS
	__try
#endif
	{
		// we has granted permission to execute
		{
			PROF_EVENT("g_hud OnFrameMT");
			if (g_hud)
				g_hud->OnFrameMT();
		}

	{
		PROF_EVENT("SoundEvent_Dispatch");
		if (g_pGameLevel && g_pGameLevel->bReady)
		{
			g_pGameLevel->SoundEvent_Dispatch();
		}
	}

		{
			PROF_EVENT("Save Writing");
			while (auto task = CSaveManager::GetInstance().PopSaveTask())
			{
				Device.async_tasks.run([=]()
				{
					PROF_START_THREAD("Async Task");
					PROF_EVENT("Save writing task");
					task->WriteSavedDataImpl();
					xr_delete(task);
					PROF_STOP_THREAD();
				});
			}
		}

	{
		PROF_EVENT("Sheduler");
		if (!Device.Paused())
		{
			::Engine.Sheduler.Update();
		}
	}

	{
		PROF_EVENT("seqParallel");
		for (u32 pit = 0; pit < Device.seqParallel.size(); pit++)
		{
			Device.seqParallel[pit]();
		}
		Device.seqParallel.clear();
	}

		{
			PROF_EVENT("seqFrameMT");
			Device.seqFrameMT.Process<&pureFrame::OnFrame>();
		}

	// Device.GCThread.Run();

	if (Device.LuaGC)
	{
		Device.LuaGC();
	}

	if (::Sound != nullptr)
	{
		Device.Statistic->Sound.Begin();
		::Sound->update(Device.mView_saved, Device.vCameraPosition_saved, Device.vCameraDirection_saved, Device.vCameraTop_saved);
		Device.Statistic->Sound.End();
	}
#ifdef ALLOW_SEH_EXCEPTIONS
	} __except(ex_filter(GetExceptionCode(), GetExceptionInformation()))
	{
		FATAL("Unhandled exception in GameThread!");
#endif
	}
}