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
		if (ImGui::Begin(g_pStringTable->translate("ed_st_world_properties").c_str(), &bOpen))
		{
			LTools->GetWorldProperties()->Draw();
		}
		ImGui::End();
	}
}
