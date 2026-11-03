#include "stdafx.h"

UILPropertiesForm::UILPropertiesForm()
{
}

UILPropertiesForm::~UILPropertiesForm()
{
}

void UILPropertiesForm::Draw()
{
	if (bOpen)
	{
		if (ImGui::Begin("Properties", &bOpen))
		{
			if (XRay::ImGui::BeginDarkChild("WorldPropertiesBorder"))
			{
				if (PropUpdateIsCompleted)
				{
					g_LevelAutoBackup.BeginToolWidgets();
					LTools->GetProperties()->Draw();
					g_LevelAutoBackup.EndToolWidgets();
				}
				else
				{
					ImGui::Text("Async loading...");
				}

				XRay::ImGui::EndDarkChild();
			}
		}
		ImGui::End();
	}
}