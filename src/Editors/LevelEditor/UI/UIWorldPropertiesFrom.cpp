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
				LTools->GetWorldProperties()->Draw();

				XRay::ImGui::EndDarkChild();
			}
		}
		ImGui::End();
	}
}
