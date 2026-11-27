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
#include "../xrCore/StackTrace/StackTrace.h"

void XRay::Engine::PreRenderThread()
{
	// A parallel session borrows the legacy service pointers on the primary
	// thread. No worker callback may observe that temporary ownership.
	xrCriticalSectionGuard contextGuard(&::Engine.Sheduler.UpdateMutex());
	{
		PROF_EVENT("Discord Sync");
		g_Discord.Update();
	}

	{
		PROF_EVENT("seqParallelRender");
		for (auto& it : Device.seqParallelRender)
			it();
	}

	if (g_pGamePersistent && !g_pGamePersistent->m_pMainMenu->IsActive())
	{
		if (g_pGamePersistent->pEnvironment && g_pGamePersistent->pEnvironment->eff_Rain)
			g_pGamePersistent->pEnvironment->eff_Rain->UpdateItems();

		const IGame_Level* owner = g_pGameLevel;
		g_pGamePersistent->UpdateParticles(owner != nullptr && owner->IsParallel() ? nullptr : owner);
	}
}

static LONG LogGameThreadCrash(EXCEPTION_POINTERS* ep)
{
	Msg("! GameThread crashed: code 0x%08lX at %p (xrGame %p, xrEngineCore %p, xrNetServer %p, xrPhysics %p, xrScripts %p)",
		ep->ExceptionRecord->ExceptionCode, ep->ExceptionRecord->ExceptionAddress,
		GetModuleHandleA("xrGame.dll"), GetModuleHandleA("xrEngineCore.dll"), GetModuleHandleA("xrNetServer.dll"), GetModuleHandleA("xrPhysics.dll"), GetModuleHandleA("xrScripts.dll"));
	// Symbolize immediately: by the time the engine handler runs, the heap may
	// be too damaged for its own stack walk to survive.
	CONTEXT save = *ep->ContextRecord;
	for (const std::string& frame : StackTrace::BuildStackTrace(ep->ContextRecord, 64))
		Msg("! GameThread   %s", frame.c_str());
	*ep->ContextRecord = save;
	xrLogger::FlushLog();
	return EXCEPTION_CONTINUE_SEARCH;
}

void XRay::Engine::GameThread()
{
	__try
	{
	GameThreadBody();
	}
	__except (LogGameThreadCrash(GetExceptionInformation()))
	{
	}
}

void XRay::Engine::GameThreadBody()
{
	xrCriticalSectionGuard contextGuard(&::Engine.Sheduler.UpdateMutex());
	// Device.GCThread.Wait();

	{
		PROF_EVENT("g_hud OnFrameMT");
		if (g_pGameLevel != nullptr && g_pGameLevel->IsParallel())
		{
			Msg("! [viewer] skipped parallel HUD work on GameThread: hud=%p global=%p level=%p", g_hud, g_hud, g_pGameLevel);
		}
		else if (g_hud)
		{
			g_hud->OnFrameMT();
		}
	}

	{
		PROF_EVENT("SoundEvent_Dispatch");
		if (g_pGameLevel && g_pGameLevel->bReady && !g_pGameLevel->IsParallel())
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
		if (!Device.Paused() && !Device.IsPrecacheSpawning)
		{
			::Engine.Sheduler.Update();
		}
	}

	{
		PROF_EVENT("seqParallel");
		xrCriticalSectionGuard guard(&::Engine.Sheduler.UpdateMutex());
		for (u32 pit = 0; pit < Device.seqParallel.size(); pit++)
		{
			Device.seqParallel[pit]();
		}
		Device.seqParallel.clear();
	}

	{
		PROF_EVENT("seqFrameMT");
		xrCriticalSectionGuard guard(&::Engine.Sheduler.UpdateMutex());
		Device.seqFrameMT.Process<&pureFrame::OnFrame>();
	}

	// Device.GCThread.Run();

	if (g_pGamePersistent != nullptr)
	{
		g_pGamePersistent->OnFrameMT();
	}

	if (Device.LuaGC)
	{
		PROF_EVENT("LuaGC");
		Device.LuaGC();
	}

	if (::Sound != nullptr)
	{
		Device.Statistic->Sound.Begin();
		::Sound->update(Device.vCameraPosition_saved, Device.vCameraDirection_saved, Device.vCameraTop_saved);
		Device.Statistic->Sound.End();
	}

	Device.SecondaryTasks.wait();
}

extern volatile bool quiting;

void XRay::Engine::GCThread()
{
	if (Device.LuaGC != nullptr)
	{
		PROF_EVENT("LuaGC");
		Device.LuaGC();
	}
}
