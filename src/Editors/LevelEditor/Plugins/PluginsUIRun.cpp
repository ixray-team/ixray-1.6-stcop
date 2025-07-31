#include "stdafx.h"
#include "PluginsUIRun.h"
#include "PluginManager.h"

CPluginUIRun::CPluginUIRun(IPluginBase* Plug):
	InputPlugin(Plug)
{
	bOpen = true;
}

void CPluginUIRun::Draw()
{
	if (ImGui::Begin("Plugin Run", &bOpen, ImGuiWindowFlags_::ImGuiWindowFlags_AlwaysAutoResize))
	{
		for (auto& [Arg, Desc] : InputPlugin->InputArgsName)
		{
			ImGui::Text(Desc.c_str());
			//ImGui::SameLine();
			ImGui::InputText(("##" + Arg).c_str(), InputPlugin->InputArgsValues[Arg], 256);
		}

		if (ImGui::Button("Run"))
		{
			bOpen = false;
			InputPlugin->Run();
		}
		ImGui::SameLine();


		if (ImGui::Button("Cancel"))
		{
			bOpen = false;
		}
	}
	ImGui::End();
}