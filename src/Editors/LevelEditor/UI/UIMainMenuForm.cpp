#include "stdafx.h"
#include "../../xrEUI/imgui_IconMenuItem.h"

#include "../Nodes/UIMacroView.h"
#include "../Editor/Utils/ReferenceReplacer.h"

#include "../../xrEUI/xrUITheme.h"
#include "../../xrEUI/Windows/Help.h"
#include "../../xrECore/Editor/EThumbnail.h"

#include "../../xrEUI/imgui_EditorEx.h"

#include "Plugins/PluginManager.h"
#include "Plugins/PluginsUIRun.h"

UIMainMenuForm::UIMainMenuForm()
{
	PlugPy = EDevice->Resources->_CreateTexture("ed\\plugins\\python");
	PlugLua = EDevice->Resources->_CreateTexture("ed\\plugins\\lua");
}

UIMainMenuForm::~UIMainMenuForm()
{
}

shared_str UIMainMenuForm::GetCommandShortcat(int CommandID) const
{
	ECommandVec& CommandVec = GetEditorCommands();

	if (CommandVec[CommandID] == nullptr)
		return {};

	ESubCommandVec& SubCommandVec = CommandVec[CommandID]->sub_commands;

	if (SubCommandVec.empty())
		return {};

	const xr_shortcut& Cat = SubCommandVec[0]->shortcut;

	xr_string txt;
	if (Cat.key == 0)
	{
		return {};
	}

	if (Cat.ext.test(xr_shortcut::flCtrl))
	{
		txt.append("Ctrl+");
	}
	if (Cat.ext.test(xr_shortcut::flShift))
	{
		txt.append("Shift+");
	}
	if (Cat.ext.test(xr_shortcut::flAlt))
	{
		txt.append("Alt+");
	}

	txt += SDL_GetScancodeName((SDL_Scancode)Cat.key);


	return txt.c_str();
}

void UIMainMenuForm::DrawMenuItem(const char* label, int command, const xr_string& param, int flag)
{
	if (ImGui::MenuItem(label, *GetCommandShortcat(command)))
	{
		ExecCommand(command, param, flag);
	}
}

void UIMainMenuForm::DrawMenuItem(const char* label, int command, int param, int flag)
{
	if (ImGui::MenuItem(label, *GetCommandShortcat(command)))
	{
		ExecCommand(command, param, flag);
	}
}

void UIMainMenuForm::Draw()
{
	if (IXBeginMainMenuBar())
	{
		DrawLevelName();

		if (ImGui::BeginMenu("File"))
		{
			DrawMenuItem("Clear", COMMAND_CLEAR);
			DrawMenuItem("Open", COMMAND_LOAD);
			DrawMenuItem("Save", COMMAND_SAVE, LTools->m_LastFileName);
			DrawMenuItem("Save As ...", COMMAND_SAVE, 0, 1);
			ImGui::Separator();

			DrawMenuItem("Open Selection", COMMAND_LOAD_SELECTION);
			DrawMenuItem("Save Selection", COMMAND_SAVE_SELECTION);
			ImGui::Separator();

			if (ImGui::BeginMenu("Open Recent", *GetCommandShortcat(COMMAND_LOAD)))
			{
				for (auto& str : EPrefs->scene_recent_list)
				{
					if (ImGui::MenuItem(str.c_str(), "")) { ExecCommand(COMMAND_LOAD, str); }
				}
				ImGui::EndMenu();
			}
			ImGui::Separator();

			DrawMenuItem("Quit", COMMAND_QUIT);
			ImGui::EndMenu();
		}

		if (ImGui::BeginMenu("Scene"))
		{
			{
				bool selected = !MainForm->GetWorldPropertiesFrom()->IsClosed();
				if (ImGui::MenuItem("World Properties", "", &selected))
				{
					if (selected)
						MainForm->GetWorldPropertiesFrom()->Open();
					else
						MainForm->GetWorldPropertiesFrom()->Close();
				}

				if (ImGui::MenuItem("Export as archive"))
				{
					ExportLevelAsArchive();

				}
			}
			ImGui::Separator();

			DrawMenuItem("Validate", COMMAND_VALIDATE_SCENE);
			ImGui::Separator();
			if (ImGui::MenuItem("Summary Info", "")) 
			{
				ExecCommand(COMMAND_CLEAR_SCENE_SUMMARY);
				ExecCommand(COMMAND_COLLECT_SCENE_SUMMARY);
				ExecCommand(COMMAND_SHOW_SCENE_SUMMARY);
			}
			DrawMenuItem("Highlight Texture", COMMAND_SCENE_HIGHLIGHT_TEXTURE);
			ImGui::Separator();

			DrawMenuItem("Clear Debug Draw", COMMAND_CLEAR_DEBUG_DRAW);
			ImGui::Separator();
			if (ImGui::MenuItem("Export entire Scene as Obj", ""))
			{
				Scene->ExportObj(false);
			}
			if (ImGui::MenuItem("Export selection as Obj", ""))
			{
				Scene->ExportObj(true);
			}
			ImGui::EndMenu();
		}
		if (ImGui::BeginMenu("Compile"))
		{
			if (ImGui::BeginMenu("Make"))
			{
				DrawMenuItem("Make All", COMMAND_BUILD);
				ImGui::Separator();
				DrawMenuItem("Make Game", COMMAND_MAKE_GAME);
				DrawMenuItem("Make Puddles", COMMAND_MAKE_PUDDLES);
				DrawMenuItem("Make Details", COMMAND_MAKE_DETAILS);
				DrawMenuItem("Make Hom", COMMAND_MAKE_HOM);
				DrawMenuItem("Make SOM", COMMAND_MAKE_SOM);
				DrawMenuItem("Make AI-Map", COMMAND_MAKE_AIMAP);
				ImGui::EndMenu();
			}
			bool bDisable = false;
			if (LTools->IsCompilerRunning() || LTools->IsGameRunning())
			{
				ImGui::BeginDisabled();
				bDisable = true;
			}
			if (ImGui::BeginMenu("Compile"))
			{
				if (ImGui::MenuItem("Geometry & Light", ""))
				{
					LTools->RunXrLC();
				}
				if (ImGui::MenuItem("Detail Object Light", ""))
				{
					LTools->RunXrDO();
				}
				if (ImGui::BeginMenu("AI-Map"))
				{
					if (ImGui::MenuItem("High", ""))
					{
						LTools->RunXrAI_AIMap(false);
					}
					if (ImGui::MenuItem("Low", ""))
					{
						LTools->RunXrAI_AIMap(true);
					}
					if (ImGui::MenuItem("Verify", ""))
					{
						LTools->RunXrAI_Verify();
					}
					ImGui::EndMenu();
				}
				if (ImGui::BeginMenu("Spawn"))
				{
					if (ImGui::MenuItem("Only current level", ""))
					{
						LTools->RunXrAI_Spawn(true);
					}
					if (ImGui::MenuItem("All levels", ""))
					{
						LTools->RunXrAI_Spawn(false);
					}
					ImGui::EndMenu();
				}

				ImGui::EndMenu();
			}
			if (bDisable)
			{
				ImGui::EndDisabled();
			}
			ImGui::Separator();

			DrawMenuItem("Import Error List", COMMAND_IMPORT_COMPILER_ERROR);
			DrawMenuItem("Import xrAI Error List", COMMAND_IMPORT_AICOMPILER_ERROR);
			DrawMenuItem("Export Error List", COMMAND_EXPORT_COMPILER_ERROR);
			DrawMenuItem("Clear Error List", COMMAND_CLEAR_DEBUG_DRAW);
			ImGui::EndMenu();
		}
		if (ImGui::BeginMenu("Objects"))
		{
			DrawMenuItem("Library Editor", COMMAND_LIBRARY_EDITOR);
			ImGui::Separator();

			DrawMenuItem("Clip Editor", COMMAND_SHOW_CLIP_EDITOR);
			DrawMenuItem("Multi Rename", COMMAND_MULTI_RENAME_OBJECTS);

			if (ImGui::MenuItem("Multi Replace")) 
			{ 
				UIReferenceReplacer* RefUI = new UIReferenceReplacer;
				RefUI->UpdateReferences();

				UI->Push(RefUI);
			}
			DrawMenuItem("Reload", COMMAND_RELOAD_OBJECTS);
			ImGui::EndMenu();
		}
		if (ImGui::BeginMenu("Images"))
		{
			DrawMenuItem("Image Editor", COMMAND_IMAGE_EDITOR);
			ImGui::Separator();

			DrawMenuItem("Reload Textures", COMMAND_RELOAD_TEXTURES);
			ImGui::Separator();

			DrawMenuItem("Synchronize Textures", COMMAND_REFRESH_TEXTURES);
			DrawMenuItem("Check New Textures", COMMAND_CHECK_TEXTURES);
			ImGui::Separator();

			DrawMenuItem("Edit minimap", COMMAND_MINIMAP_EDITOR);
			if (ImGui::MenuItem("Sync THM", ""))
			{
				FS_FileSet      files;
				FS.file_list(files, _textures_, FS_ListFiles, "*.thm");
				FS_FileSet::iterator I = files.begin();
				FS_FileSet::iterator E = files.end();

				for (; I != E; ++I)
				{
					ETextureThumbnail* TH = new ETextureThumbnail((*I).name.c_str(), false);
					TH->Load((*I).name.c_str(), _textures_);
					TH->Save();
					xr_delete(TH);
				}
			}
			ImGui::EndMenu();
		}
		if (ImGui::BeginMenu("Sounds"))
		{
			DrawMenuItem("Sound Editor", COMMAND_SOUND_EDITOR, "");
			ImGui::Separator();

			DrawMenuItem("Synchronize Sounds (Soft)", COMMAND_SYNC_SOUNDS);
			DrawMenuItem("Synchronize Sounds (Hard)", COMMAND_SYNC_SOUNDS_HARD);
			ImGui::Separator();

			DrawMenuItem("Refresh Environment Library", COMMAND_REFRESH_SOUND_ENVS);
			DrawMenuItem("Refresh Environment Geometry", COMMAND_REFRESH_SOUND_ENV_GEOMETRY);
			ImGui::EndMenu();

		}

		if (ImGui::BeginMenu("Options"))
		{
			if (ImGui::BeginMenu("Render"))
			{
				if (ImGui::BeginMenu("Quality"))
				{
					static bool selected[4] = { false,false,true,false };
					if (ImGui::MenuItem("25%", "", &selected[0]))
					{
						selected[1] = selected[2] = selected[3] = false;
						UI->SetRenderQuality(1 / 4.f);
						UI->RedrawScene();
					}
					if (ImGui::MenuItem("50%", "", &selected[1]))
					{
						selected[0] = selected[2] = selected[3] = false;
						UI->SetRenderQuality(1 / 2.f);
						UI->RedrawScene();
					}
					if (ImGui::MenuItem("100%", "", &selected[2]))
					{
						selected[1] = selected[0] = selected[3] = false;
						UI->SetRenderQuality(1.f);
						UI->RedrawScene();
					}
					if (ImGui::MenuItem("200%", "", &selected[3]))
					{
						selected[1] = selected[2] = selected[0] = false;
						UI->SetRenderQuality(2.f);
						UI->RedrawScene();
					}
					ImGui::EndMenu();
				}
				if (ImGui::BeginMenu("Fill Mode"))
				{
					bool selected[3] = { EDevice->dwFillMode == D3DFILL_POINT,EDevice->dwFillMode == D3DFILL_WIREFRAME,EDevice->dwFillMode == D3DFILL_SOLID };
					if (ImGui::MenuItem("Point", "", &selected[0]))
					{
						EDevice->dwFillMode = D3DFILL_POINT;
						UI->RedrawScene();
					}
					if (ImGui::MenuItem("Wireframe", "", &selected[1]))
					{
						EDevice->dwFillMode = D3DFILL_WIREFRAME;
						UI->RedrawScene();
					}
					if (ImGui::MenuItem("Solid", "", &selected[2]))
					{
						EDevice->dwFillMode = D3DFILL_SOLID;
						UI->RedrawScene();
					}
					ImGui::EndMenu();
				}
				if (ImGui::BeginMenu("Shader Mode"))
				{
					bool selected[2] = { EDevice->dwShadeMode == D3DSHADE_FLAT,EDevice->dwShadeMode == D3DSHADE_GOURAUD };
					if (ImGui::MenuItem("Flat", "", &selected[0]))
					{
						EDevice->dwShadeMode = D3DSHADE_FLAT;
						UI->RedrawScene();
					}
					if (ImGui::MenuItem("Gouraud", "", &selected[1]))
					{
						EDevice->dwShadeMode = D3DSHADE_GOURAUD;
						UI->RedrawScene();
					}
					ImGui::EndMenu();
				}
				{
					bool selected = psDeviceFlags.test(rsEdgedFaces);
					if (ImGui::MenuItem("Edged Faces", "", &selected))
					{
						psDeviceFlags.set(rsEdgedFaces, selected);
						UI->RedrawScene();
					}
				}
				ImGui::Separator();
				{
					bool selected = !Caps.bForceGPU_SW;
					if (ImGui::MenuItem("RenderHW", "", &selected))
					{
						Caps.bForceGPU_SW = !selected;
						UI->Resize();
					}
				}
				ImGui::Separator();
				{
					bool selected = psDeviceFlags.test(rsFilterLinear);
					if (ImGui::MenuItem("Filter Linear", "", &selected))
					{
						psDeviceFlags.set(rsFilterLinear, selected);
						UI->RedrawScene();
					}
				}
				{
					bool selected = psDeviceFlags.test(rsRenderTextures);
					if (ImGui::MenuItem("Textures", "", &selected))
					{
						psDeviceFlags.set(rsRenderTextures, selected);
						UI->RedrawScene();
					}
				}
				ImGui::EndMenu();
			}
			/*
			* FX: нахуй - так нахуй
			ImGui::Separator();
			{
				bool selected = psDeviceFlags.test(rsDrawSafeRect);
				if (ImGui::MenuItem("Draw Safe Rect", "", &selected))
				{
					psDeviceFlags.set(rsDrawSafeRect, selected);
					UI->RedrawScene();
				}
			}
			*/
			{
				bool selected = psDeviceFlags.test(rsDrawGrid);
				if (ImGui::MenuItem("Draw Grid", "", &selected))
				{
					psDeviceFlags.set(rsDrawGrid, selected);
					UI->RedrawScene();
				}
			}
			{
				if (ImGui::BeginMenu("Coordinate Axes"))
				{
					bool disabled = psDeviceFlags.test(rsDisableAxisCube);

					if (ImGui::MenuItem("None", "", &disabled))
					{
						psDeviceFlags.set(rsDisableAxisCube, disabled);
					}

					ImGui::BeginDisabled(disabled);

					bool selected_a = false;
					bool selected_c = false;

					(!psDeviceFlags.test(rsDrawAxis) ? selected_c : selected_a) = true;

					if (ImGui::MenuItem("Axis", "", &selected_a))
					{
						psDeviceFlags.set(rsDrawAxis, true);
					}
					if (ImGui::MenuItem("Cube", "", &selected_c))
					{
						psDeviceFlags.set(rsDrawAxis, false);
					}

					ImGui::EndDisabled();
					ImGui::EndMenu();
				}
			}
			ImGui::Separator();
			{
				bool selected = psDeviceFlags.test(rsFog);
				if (ImGui::MenuItem("Fog", "", &selected))
				{
					psDeviceFlags.set(rsFog, selected);
					UI->RedrawScene();
				}
			}
            // Погода
			{
				if (ImGui::BeginMenu("Environment"))
				{
                    {
                        if (ImGui::Button("Weather properties"))
                        {
                            ExecCommand(COMMAND_WEATHER_PROPERTIES);
                        }
                        if (ImGui::IsItemHovered())
                            ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
                    }
					bool selected = !psDeviceFlags.test(rsEnvironment);
					if (ImGui::MenuItem("None", "", &selected))
					{
						psDeviceFlags.set(rsEnvironment, false);
						UI->RedrawScene();
					}
                    if (ImGui::IsItemHovered())
                        ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
					ImGui::Separator();
					for (auto& i : g_pGamePersistent->Environment().WeatherCycles)
					{
						selected = psDeviceFlags.test(rsEnvironment) && i.first == g_pGamePersistent->Environment().CurrentCycleName;
						if (ImGui::MenuItem(i.first.c_str(), "", &selected))
						{
							psDeviceFlags.set(rsEnvironment, true);
							g_pGamePersistent->Environment().SetWeather(i.first.c_str(), true);
							UI->RedrawScene();
						}
                        if (ImGui::IsItemHovered())
                            ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
					}

					ImGui::EndMenu();
				}
			}
			ImGui::Separator();
			{
				bool selected = psDeviceFlags.test(rsMuteSounds);
				if (ImGui::MenuItem("Mute Sounds", "", &selected))
				{
					psDeviceFlags.set(rsMuteSounds, selected);
				}
			}
			{
				bool selected = psDeviceFlags.test(rsRenderRealTime);
				if (ImGui::MenuItem("Real Time", "", &selected))
				{
					psDeviceFlags.set(rsRenderRealTime, selected);
				}
			}
			ImGui::Separator();
			{
				bool selected = psDeviceFlags.test(rsStatistic);
				if (ImGui::MenuItem("Stats", "", &selected)) { psDeviceFlags.set(rsStatistic, selected);  UI->RedrawScene(); }

			}
			ImGui::EndMenu();
		}

		if (ImGui::BeginMenu("Windows"))
		{
			DrawMenuItem("Light Anim Editor", COMMAND_LIGHTANIM_EDITOR);

			if (ImGui::MenuItem("Macro Editor", ""))
			{
				static CUIMacroView* View = nullptr;
				if (View == nullptr)
				{
					View = new CUIMacroView;
					UI->Push(View);
				}

				View->Show(true);
			}

			ImGui::Separator();

			{
				bool selected = MainForm->GetLeftBarForm()->IsUseSnapList();
				if (ImGui::MenuItem("Snap List", "", &selected))
				{
					MainForm->GetLeftBarForm()->ShowSnapList(selected);
				}
			}
			{
				bool selected = MainForm->GetLeftBarForm()->IsUseObjectsTool();
				if (ImGui::MenuItem("Objects Tools", "", &selected))
				{
					MainForm->GetLeftBarForm()->ShowObjectsTool(selected);
				}
				{
					bool selected = !MainForm->GetPropertiesFrom()->IsClosed();
					if (ImGui::MenuItem("Properties", "", &selected)) { if (selected)MainForm->GetPropertiesFrom()->Open(); else MainForm->GetPropertiesFrom()->Close(); }
				}
			}
			{
				bool selected = AllowLogCommands();

				if (ImGui::MenuItem("Log", *GetCommandShortcat(COMMAND_LOG_COMMANDS), &selected)) { ExecCommand(COMMAND_LOG_COMMANDS); }

				CUIThemeManager& ThemeInstance = CUIThemeManager::Get();
				bool selected2 = !ThemeInstance.IsClosed();
				if (ImGui::MenuItem("Theme", "", &selected2))
				{
					if (selected2)
					{
						if (!UI->HasWindow<CUIThemeManager>())
						{
							UI->Push(&ThemeInstance);
						}
						ThemeInstance.Show(true);
					}
					else
						ThemeInstance.Show(false);
				}
			}

			ImGui::EndMenu();
		}

		if (ImGui::BeginMenu("Help", ""))
		{
			if (ImGui::MenuItem("Wiki", ""))
			{

			}
			ImGui::Separator();
			if (ImGui::MenuItem("About...", ""))
			{
				CUIHelp::Instance().Show();
				UI->Push(&CUIHelp::Instance(), false);
			}

			ImGui::EndMenu();
		}

		if (ImGui::BeginMenu("Plugins", ""))
		{
			CPluginsManagers& PlugMngr = CPluginsManagers::Instance();

			bool NeedReinit = false;
			if (ImGui::MenuItem("Reinit"))
			{
				PlugMngr.Reinit();
			}

			ImGui::Separator();
			PlugPy->Load();
			PlugLua->Load();

			for (IPluginBase* Plug : PlugMngr.Plugins)
			{
				if (Plug->Type == EPluginType::Lua)
				{
					ImGui::Image(PlugLua->pSurface, { 15, 15 });
				}
				else
				{
					ImGui::Image(PlugPy->pSurface, { 15, 15 });
				}
				ImGui::SameLine();
				if (ImGui::MenuItem(Plug->Name.c_str(), ""))
				{
					if (Plug->IsSimple())
					{
						Plug->Run();
					}
					else
					{
						UI->Push(new CPluginUIRun(Plug));
					}
				}

				if (ImGui::IsItemHovered())
				{
					ImGui::SetTooltip(Plug->Desc.c_str());
				}
			}
			ImGui::EndMenu();
		}

		ImGui::Separator();

		{
			bool selected = UIObjectList::IsOpen();
			if (ImGui::MenuItem("Object List", "", &selected))
			{
				if (selected)
					UIObjectList::Show();
				else
					UIObjectList::Close();
			}
		}

		IXEndMainMenuBar();
	}
}

void UIMainMenuForm::DrawLevelName()
{
	if (Scene->full_name.empty())
		return;

	ImVec4 CheckMarkColor = ImGui::GetStyle().Colors[ImGuiCol_CheckMark];

	if (Scene->IsUnsaved())
	{
		CheckMarkColor = ImColor(255, 22, 22);
	}

	shared_str LevelPath = xr_path(Scene->full_name).stem().string().c_str();
	

	ImGui::SameLine();
	ImGui::SetCursorPosX(ImGui::GetCursorPosX() - 7);
	ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 5);
	ImGui::PushStyleColor(ImGuiCol_Text, CheckMarkColor);
	ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(3, 0));
	ImGui::Button(*LevelPath, ImVec2(0, 20));
	ImGui::PopStyleVar();
	ImGui::PopStyleColor();
	ImGui::SameLine();
	//ImGui::SetCursorPosY(ImGui::GetCursorPosY() - 5);
}

void UIMainMenuForm::ExportLevelAsArchive()
{
	if (!std::filesystem::exists("export"))
	{
		std::filesystem::create_directory("export");
	}

	xr_path File = LTools->m_LastFileName;
	const xr_string LevelName = File.xfilename();

	xr_string FullPath = "export\\" + LevelName;
	if (std::filesystem::exists(FullPath.data()))
	{
		std::filesystem::remove_all(FullPath.data());
	}
	std::filesystem::create_directory(FullPath.data());

	ObjectIt _F = Scene->FirstObj(OBJCLASS_SCENEOBJECT);
	ObjectIt _E = Scene->LastObj(OBJCLASS_SCENEOBJECT);

	xr_path RawPath = (FullPath + "\\rawdata").data();
	xr_path GamePath = (FullPath + "\\gamedata").data();
	xr_path RawObjectPath = (RawPath.xstring() + "\\objects").data();
	xr_path RawGroupPath = (RawPath.xstring() + "\\groups").data();
	xr_path LevelPath = (RawPath.xstring() + "\\levels").data();
	xr_path TexturesObjectPath = (GamePath.xstring() + "\\textures").data();
	xr_path TextureLodsObjectPath = (TexturesObjectPath.xstring() + "\\lods").data();


	std::filesystem::create_directory(RawPath);
	std::filesystem::create_directory(RawObjectPath);
	std::filesystem::create_directory(RawGroupPath);
	std::filesystem::create_directory(LevelPath);
	std::filesystem::create_directory(GamePath);
	std::filesystem::create_directory(TexturesObjectPath);
	std::filesystem::create_directory(TextureLodsObjectPath);


	string_path GameTextures = {};
	FS.update_path(GameTextures, "$game_textures$", "");
	string_path Rawdata = {};
	FS.update_path(Rawdata, "$server_data_root$", "");

	auto ParseBumpFromTexture = [&](const xr_string& InFileThm)
	{
		ETextureThumbnail* pThmTexture = (ETextureThumbnail*)CreateThumbnail(InFileThm.c_str(), ECustomThumbnail::ETTexture, false);
		pThmTexture->Load(InFileThm.c_str(), "");

		if (pThmTexture != nullptr)
		{
			shared_str Temp = *pThmTexture->_Format().bump_name;

			if (Temp.size() > 0)
			{
				xr_string BumpTextureIn = xr_string(GameTextures) + *Temp + ".dds";
				xr_string BumpTextureOut = TexturesObjectPath.xstring() + "\\" + *Temp + ".dds";
				FS.file_copy(BumpTextureIn.c_str(), BumpTextureOut.c_str());

				BumpTextureIn = xr_string(GameTextures) + *Temp + "#.dds";
				BumpTextureOut = TexturesObjectPath.xstring() + "\\" + *Temp + "#.dds";
				FS.file_copy(BumpTextureIn.c_str(), BumpTextureOut.c_str());
			}
		}
	};
	
	auto ParseObject = [&](auto Obj, xr_path FilePath, xr_path DirFilePath)
	{
		auto Dirs = FilePath.xstring().Split('\\');
		for (size_t Iter = 0; Iter < Dirs.size() - 1; Iter++)
		{
			DirFilePath += ("\\" + Dirs[Iter]).data();
			if (!std::filesystem::exists(DirFilePath))
			{
				std::filesystem::create_directories(DirFilePath);
			}
		}

		xr_string InFile = xr_string(Rawdata) + "objects\\" + FilePath.xstring() + ".object";
		xr_string OutFile = DirFilePath.xstring() + "\\" + Dirs[Dirs.size() - 1] + ".object";
		FS.file_copy(InFile.c_str(), OutFile.c_str());

		// Parse textures
		for (CSurface* Surface : Obj->m_Surfaces)
		{
			DirFilePath = TexturesObjectPath;
			xr_string TextureName = Surface->m_Texture.c_str();
			TextureName += ".dds";

			auto Dirs = TextureName.Split('\\');

			for (size_t Iter = 0; Iter < Dirs.size() - 1; Iter++)
			{
				DirFilePath += ("\\" + Dirs[Iter]).data();
				if (!std::filesystem::exists(DirFilePath))
				{
					std::filesystem::create_directories(DirFilePath);
				}
			}

			InFile = GameTextures + TextureName;
			OutFile = DirFilePath.xstring() + "\\" + Dirs[Dirs.size() - 1];
			xr_string BaseFileName = DirFilePath.xstring() + "\\" + Dirs[Dirs.size() - 1];
			xr_string OutFileThm = BaseFileName.substr(0, BaseFileName.size() - 3) + "thm";
			xr_string InFileThm = InFile.substr(0, InFile.size() - 3) + "thm";
			FS.file_copy(InFile.c_str(), OutFile.c_str());
			FS.file_copy(InFileThm.c_str(), OutFileThm.c_str());

			// Parse bumps
			ParseBumpFromTexture(InFileThm);
		}
	};
	
	// LOD's
	auto CopyLodTexture = [&TextureLodsObjectPath](const xr_string& InputFile)
	{
		string_path LodTexturePath = {};
		FS.update_path(LodTexturePath, _game_textures_, (InputFile + ".dds").data());
		string_path LodTexturePathThm = {};
		FS.update_path(LodTexturePathThm, _game_textures_, (InputFile + ".thm").data());

		if (std::filesystem::exists(LodTexturePath))
		{
			xr_string OutLodTexture = TextureLodsObjectPath;
			OutLodTexture += "\\" + xr_path(InputFile).xfilename() + ".dds";

			std::filesystem::copy(LodTexturePath, OutLodTexture.data(), std::filesystem::copy_options::skip_existing);
		}

		if (!std::filesystem::exists(LodTexturePathThm))
			return;

		xr_string OutLodTextureThm = TextureLodsObjectPath;
		OutLodTextureThm += "\\" + xr_path(InputFile).xfilename() + ".thm";

		std::filesystem::copy(LodTexturePathThm, OutLodTextureThm.data(), std::filesystem::copy_options::skip_existing);
	};

	// Parse objects
	for (; _F != _E; _F++)
	{
		CSceneObject* Obj = (CSceneObject *)*_F;
		ParseObject(Obj, Obj->GetReference()->GetName(), RawObjectPath);
		
		const xr_string& LodTexture = Obj->GetReference()->GetLODTextureName();

		if (LodTexture.empty())
			continue;

		CopyLodTexture(LodTexture);
		CopyLodTexture(LodTexture + "_nm");
	}

	// Parse levels
	FS.file_copy(File.xstring().c_str(), (LevelPath / LevelName).xstring().c_str());

	xr_path LevelDir = File.xstring().substr(0, File.xstring().size() - xr_strlen(".level")).c_str();
	xr_path OutLevelDir = LevelPath / File.xfilename().substr(0, File.xfilename().size() - xr_strlen(".level")).c_str();
	std::filesystem::copy(LevelDir, OutLevelDir, std::filesystem::copy_options::update_existing | std::filesystem::copy_options::recursive);

	// Parse details 
	EDetailManager* pDetTool = (EDetailManager*)Scene->GetTool(OBJCLASS_DO);

	if (pDetTool->m_Base.name.size() > 0)
	{

		xr_string TexturePath = GameTextures;
		TexturePath += *pDetTool->m_Base.name;
		TexturePath += ".dds";

		xr_string TexturePathOut = TexturesObjectPath;
		TexturePathOut += "\\";
		TexturePathOut += *pDetTool->m_Base.name;
		TexturePathOut += ".dds";

		xr_path TextureMask = TexturePathOut;
		TextureMask = TextureMask.parent_path();

		if (!std::filesystem::exists(TextureMask))
		{
			std::filesystem::create_directories(TextureMask);
		}

		std::filesystem::copy(TexturePath.data(), TexturePathOut.data(), std::filesystem::copy_options::update_existing);
	}

	for (CDetail* DetObjPtr : pDetTool->objects)
	{
		EDetail* NormalPtr = (EDetail*)DetObjPtr;
		ParseObject(NormalPtr->m_pRefs, NormalPtr->m_pRefs->GetName(), RawObjectPath);
	}
}

