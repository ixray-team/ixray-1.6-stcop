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
            bool Opened = XRay::ImGui::BeginDarkChild("WorldPropertiesBorder");
			LTools->GetWorldProperties()->Draw();
            XRay::ImGui::EndDarkChild(Opened);
		}
		ImGui::End();
	}
}
