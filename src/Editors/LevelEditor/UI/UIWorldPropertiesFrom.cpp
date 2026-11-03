#include "stdafx.h"

UIWorldPropertiesFrom::UIWorldPropertiesFrom()
{
}

UIWorldPropertiesFrom::~UIWorldPropertiesFrom()
{
}

void UIWorldPropertiesFrom::Draw()
{
	if (bOpen)
	{
		if (ImGui::Begin("World Properties", &bOpen))
		{
			if (XRay::ImGui::BeginDarkChild("WorldPropertiesBorder"))
			{
				// Holds the settings of the selected tool
				g_LevelAutoBackup.BeginToolWidgets();
				LTools->GetWorldProperties()->Draw();
				g_LevelAutoBackup.EndToolWidgets();

				XRay::ImGui::EndDarkChild();
			}
		}
		ImGui::End();
	}
}
