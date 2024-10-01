#include "stdafx.h"

void InitImGuiCLSIDInGame();
void InitImGuiSearchInGame();

void RenderTimeManagerWindow();
void RenderSpawnManagerWindow();
void RenderWeaponManagerWindow();
void RenderSearchManagerWindow();

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
	}
}