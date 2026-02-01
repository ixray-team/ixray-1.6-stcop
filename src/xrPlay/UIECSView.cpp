#include "UIEditorMain.h"
#include "../xrScripts/stdafx.h"
#include "../xrCore/ECS/EntityManager.h"

void ECSViewDraw()
{
#ifdef DEBUG_DRAW
	CImGuiManager::Instance().Subscribe
	(
		"ECS Viewer", CImGuiManager::ERenderPriority::eMedium,
		[]()
		{
			if (!Engine.External.EditorStates[static_cast<std::uint8_t>(EditorUI::ECSViewer)])
				return;

			ImGui::Begin("ECS Debugger");

			for (auto& [TypeID, StorageBase] : GECSManager->ComponentStorages)
			{
				auto Iter = GECSManager->ECS_DrawFuncs.find(TypeID);
				if (Iter != GECSManager->ECS_DrawFuncs.end())
				{
					IECSComponentStorage* storageBase = StorageBase;
					const char* compName = storageBase->ECS_GetName();

					if (ImGui::CollapsingHeader(compName))
					{
						Iter->second(storageBase);
					}
				}
			}

			ImGui::End();
		}
	);
#endif
}
