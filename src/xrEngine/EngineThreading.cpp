#include "stdafx.h"
#include "EngineThreading.h"
#include "IGame_Persistent.h"
#include "IGame_Level.h"
#include "Rain.h"
#include "../Include/xrRender/RenderVisual.h"
#include "../Include/xrRender/ParticleCustom.h"
#include "../xrCore/discord/discord.h"

void XRay::Engine::PreRenderThread()
{
	Platform::SetThreadName("X-Ray Pre-Render");
	PROF_START_THREAD("X-Ray Pre-Render");
	{
		PROF_EVENT("Discord Sync");
		g_Discord.Update();
	}

	{
		PROF_EVENT("seqParallelRender");
		for (auto& it : Device.seqParallelRender)
			it();
	}

	if (g_pGamePersistent && (g_pGamePersistent->m_pMainMenu == nullptr || !g_pGamePersistent->m_pMainMenu->IsActive()))
	{
		if (g_pGamePersistent->pEnvironment && g_pGamePersistent->pEnvironment->eff_Rain)
			g_pGamePersistent->pEnvironment->eff_Rain->UpdateItems();

		g_pGamePersistent->UpdateParticles();
	}
	PROF_STOP_THREAD();
	Platform::SetThreadName("X-Ray Empty Task");
}

void XRay::Engine::GameThread()
{
	Platform::SetThreadName("X-Ray Game Thread");
	PROF_START_THREAD("X-Ray Game Thread");
	{
		PROF_EVENT("SoundEvent_Dispatch");
		if (g_pGameLevel && g_pGameLevel->bReady)
			g_pGameLevel->SoundEvent_Dispatch();
	}

	{
		PROF_EVENT("Sheduler");
		if (!Device.Paused())
			::Engine.Sheduler.Update();
	}

	{
		PROF_EVENT("seqParallel");
		for (u32 pit = 0; pit < Device.seqParallel.size(); pit++)
			Device.seqParallel[pit]();
		Device.seqParallel.clear();
	}

	{
		PROF_EVENT("seqFrameMT");
		Device.seqFrameMT.Process<&pureFrame::OnFrame>();
	}
	PROF_STOP_THREAD();
	Platform::SetThreadName("X-Ray Empty Task");

	if (g_pGamePersistent != nullptr)
	{
		g_pGamePersistent->OnFrameMT();
	}
}