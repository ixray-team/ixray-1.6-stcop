#include "stdafx.h"

#include "ImUtils.h"

clsid_manager* g_pClsidManager;

void RegisterImGuiInGame()
{
	if (!Device.IsEditorMode())
	{
		CImGuiManager::Instance().Subscribe("Time Manager", CImGuiManager::ERenderPriority::eMedium, RenderTimeManagerWindow);
		CImGuiManager::Instance().Subscribe("Spawn Manager", CImGuiManager::ERenderPriority::eMedium, RenderSpawnManagerWindow);
		CImGuiManager::Instance().Subscribe("Weapon Manager", CImGuiManager::ERenderPriority::eMedium, RenderWeaponManagerWindow);
		CImGuiManager::Instance().Subscribe("Search Manager", CImGuiManager::ERenderPriority::eMedium, RenderSearchManagerWindow);

		InitImGuiCLSIDInGame();
		InitImGuiSearchInGame();
		InitSections();
	}
}