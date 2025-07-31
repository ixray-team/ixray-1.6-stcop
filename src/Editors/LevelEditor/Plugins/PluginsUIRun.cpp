#include "stdafx.h"
#include "PluginsUIRun.h"
#include "PluginManager.h"

CPluginUIRun::CPluginUIRun(IPluginBase* Plug):
	InputPlugin(Plug)
{
	bOpen = true;
	OpenImage = EDevice->Resources->_CreateTexture("ed\\bar\\open_gamedata");
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

			xr_string TestPath = Desc;
			TestPath = TestPath.RemoveWhitespaces();
			if (FS.pathes.contains(TestPath.c_str()))
			{
				string_path FullPath = {};
				FS.update_path(FullPath, TestPath.c_str(), "");
				if (FullPath[strlen(FullPath) - 1] == '\\')
				{
					FullPath[strlen(FullPath) - 1] = '\0';
				}

				xr_string FixedPath = std::filesystem::absolute(FullPath).string().c_str();
				xr_strcpy(InputPlugin->InputArgsValues[Arg], FixedPath.c_str());
				continue;
			}

			ImGui::Text(Desc.c_str());
			ImGui::InputText(("##" + Arg).c_str(), InputPlugin->InputArgsValues[Arg], 256);

			ImGui::SameLine();
			if (ImGui::ImageButton(("##Open" + Arg).c_str(), OpenImage->pSurface, { 10, 14 }))
			{
				xr_string TempPath;
				EFS.GetOpenName("$fs_root$", TempPath, false, nullptr, -1, "*.*");
				xr_strcpy(InputPlugin->InputArgsValues[Arg], TempPath.c_str());
			}
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