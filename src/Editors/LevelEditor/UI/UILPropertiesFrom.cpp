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
		if (ImGui::Begin("Properties", &bOpen))
		{
			if (LTools->PropUpdateIsCompleted)
			{
				LTools->GetProperties()->Draw();
			}
			else
			{
				ImGui::Text("Async loading...");
			}
		}
		ImGui::End();
	}
}