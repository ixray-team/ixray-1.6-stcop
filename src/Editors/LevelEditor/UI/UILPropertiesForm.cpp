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
			bool Opened = XRay::ImGui::BeginDarkChild("WorldPropertiesBorder");
			if (LTools->PropUpdateIsCompleted)
			{
				LTools->GetProperties()->Draw();
			}
			else
			{
				ImGui::Text("Async loading...");
			}
			XRay::ImGui::EndDarkChild(Opened);
		}
		ImGui::End();
	}
}