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
			if (InputPlugin->Type == EPluginType::Lua && Arg == "level")
			{
				memset(InputPlugin->InputArgsValues[Arg], 0, sizeof(InputPlugin->InputArgsValues[Arg]));
				continue;
			}

			ImGui::Text(Desc.c_str());
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