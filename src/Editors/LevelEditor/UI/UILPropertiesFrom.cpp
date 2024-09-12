#include "stdafx.h"

UILPropertiesFrom::UILPropertiesFrom()
{
}

UILPropertiesFrom::~UILPropertiesFrom()
{
}

void UILPropertiesFrom::Draw()
{
	if (bOpen)
	{
		if (ImGui::Begin(g_pStringTable->translate("ed_st_properties").c_str(), &bOpen))
		{
			if (LTools->PropUpdateIsCompleted)
			{
				LTools->GetProperties()->Draw();
			}
			else
			{
				ImGui::Text(g_pStringTable->translate("ed_st_async_loading").c_str());
			}
		}
		ImGui::End();
	}
}