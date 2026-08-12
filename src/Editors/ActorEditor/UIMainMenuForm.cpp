#include "stdafx.h"
#include "CustomTools/UIBatchTools.h"
#include "CustomTools/UIPostProcess.h"
#include "../xrEUI/xrUITheme.h"
#include "../xrECore/Editor/imgui_EditorEx.h"
#include "../../xrEngine/IGame_Persistent.h"

#include "IconsFontAwesome6.h"

UIMainMenuForm::UIMainMenuForm()
{
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

void UIMainMenuForm::DrawMenuItem(const char* label, int command, int param, int flag)
{
	if (ImGui::MenuItem(label, *GetCommandShortcat(command)))
	{
		ExecCommand(command, param, flag);
	}
}

void UIMainMenuForm::DrawMenuItemI(const char* label, const char* icon, int command, const xr_string& param, int flag)
{
	if (ImGui::MenuItemI(label, icon, *GetCommandShortcat(command)))
	{
		ExecCommand(command, param, flag);
	}
}

void UIMainMenuForm::DrawMenuItemI(const char* label, const char* icon, int command, int param, int flag)
{
	if (ImGui::MenuItemI(label, icon, *GetCommandShortcat(command)))
	{
		ExecCommand(command, param, flag);
	}
}

void UIMainMenuForm::Draw()
{
	ImGui::PushStyleColor(ImGuiCol_Button, XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::BackgroundTint).Value);
	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));

	if (IXBeginMainMenuBar())
	{
		ImVec2 cp;
		if (ImGui::BeginMenu("File")) {
			DrawMenuItemI("Clear", ICON_FA_FILE, COMMAND_CLEAR);
			DrawMenuItemI("Load", ICON_FA_FILE_IMPORT, COMMAND_LOAD);
			DrawMenuItemI("Save", ICON_FA_FLOPPY_DISK, COMMAND_SAVE, ATools->m_LastFileName, 0);
			DrawMenuItemI("Save as...", ICON_FA_FLOPPY_DISK, COMMAND_SAVE, 0, 1);
			ImGui::Separator();

			if (ImGui::BeginMenu("Open Recent", *GetCommandShortcat(COMMAND_LOAD)))
			{
				for (auto& str : EPrefs->scene_recent_list)
				{
					if (ImGui::MenuItem(str.c_str(), ""))
					{
						ExecCommand(COMMAND_LOAD, str);
					}
				}
				ImGui::EndMenu();
			}
			ImGui::Separator();

			DrawMenuItemI("Import...", ICON_FA_FILE_IMPORT, COMMAND_IMPORT);
			DrawMenuItemI("Import OMF...", ICON_FA_FILE_IMPORT, COMMAND_IMPORT_OMF);
			ImGui::Separator();

			DrawMenuItem("Optimize Motions", COMMAND_OPTIMIZE_MOTIONS);
			ImGui::Separator();

			DrawMenuItem("Batch Convert...", COMMAND_BATCH_CONVERT);
			ImGui::Separator();

			if (ImGui::BeginMenuI("Export", ICON_FA_FILE_EXPORT))
			{
				DrawMenuItemI("Export OGF...", ICON_FA_FILE_EXPORT, COMMAND_EXPORT_OGF);
				DrawMenuItemI("Export OMF...", ICON_FA_FILE_EXPORT, COMMAND_EXPORT_OMF);
				DrawMenuItemI("Export OBJ...", ICON_FA_FILE_EXPORT, COMMAND_EXPORT_OBJ);
				DrawMenuItemI("Export DM...", ICON_FA_FILE_EXPORT, COMMAND_EXPORT_DM);
				DrawMenuItemI("Export C++...", ICON_FA_FILE_EXPORT, COMMAND_EXPORT_CPP);
				ImGui::EndMenu();
			}
			ImGui::Separator();

			DrawMenuItemI("Quit", ICON_FA_POWER_OFF, COMMAND_QUIT);
			ImGui::EndMenu();
		};

		if (ImGui::BeginMenu("Preview Object")) {
			DrawMenuItem("Custom...", COMMAND_SELECT_PREVIEW_OBJ, false);
			DrawMenuItem("Clear", COMMAND_SELECT_PREVIEW_OBJ, true);
			ImGui::Separator();

			DrawMenuItem("Preferences", COMMAND_PREVIEW_OBJ_PREF);
			ImGui::EndMenu();
		};

		if (ImGui::BeginMenu("Tools"))
		{
			if (ImGui::MenuItem("Batch Tools"))
				CUIBatchTools::Instance().Open();

			ImGui::EndMenu();
		}

		if (ImGui::BeginMenu("Editors")) {
			if (ImGui::BeginMenu("Images"))
			{
				DrawMenuItemI("Image Editor", ICON_FA_IMAGE, COMMAND_IMAGE_EDITOR);
				ImGui::Separator();

				DrawMenuItemI("Synchronize Textures", ICON_FA_REPEAT, COMMAND_REFRESH_TEXTURES);
				DrawMenuItemI("Check New Textures", ICON_FA_CHECK, COMMAND_CHECK_TEXTURES);
				ImGui::Separator();

				DrawMenuItemI("Minimap Editor", ICON_FA_MAP, COMMAND_MINIMAP_EDITOR);
				ImGui::EndMenu();
			}
			if (ImGui::BeginMenu("Sounds"))
			{
				DrawMenuItemI("Sound Editor", ICON_FA_MUSIC, COMMAND_SOUND_EDITOR);
				ImGui::Separator();

				DrawMenuItemI("Synchronize Sounds (Soft)", ICON_FA_REPEAT, COMMAND_SYNC_SOUNDS);
				DrawMenuItemI("Synchronize Sounds (Hard)", ICON_FA_REPEAT, COMMAND_SYNC_SOUNDS_HARD);
				ImGui::EndMenu();
			}

			DrawMenuItemI("Light Anim Editor", ICON_FA_LIGHTBULB, COMMAND_LIGHTANIM_EDITOR);

			auto& PPE = CMainPPE::Instance();
			if (ImGui::MenuItem("Post Process Editor", nullptr))
			{
				PPE.OpenState() = true;
			}
			ImGui::EndMenu();
		};

		if (ImGui::BeginMenu("Options")) {
			if (ImGui::BeginMenu("Render"))
			{
				if (ImGui::BeginMenuI("Environment", ICON_FA_CLOUD_SUN))
				{
					DrawMenuItem("Properties", COMMAND_WEATHER_PROPERTIES);

					bool selected = !psDeviceFlags.test(rsEnvironment);
					if (ImGui::MenuItem("None", "", &selected))
					{
						psDeviceFlags.set(rsEnvironment, false);
						UI->RedrawScene();
					}

					ImGui::Separator();
					auto& WeatherCycles = g_pGamePersistent->Environment().WeatherCycles;
					for (auto& Cycle : WeatherCycles)
					{
						selected = psDeviceFlags.test(rsEnvironment) && Cycle.first == g_pGamePersistent->Environment().CurrentCycleName;

						if (ImGui::MenuItem(Cycle.first.c_str(), "", &selected))
						{
							psDeviceFlags.set(rsEnvironment, true);
							g_pGamePersistent->Environment().SetWeather(Cycle.first.c_str(), true);
							UI->RedrawScene();
						}

						if (ImGui::IsItemHovered())
						{
							ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
						}
					}
					ImGui::EndMenu();
				}
				if (ImGui::BeginMenu("Quality"))
				{
					static bool Selected[4] = { false,false,true,false };
					if (ImGui::MenuItem("25%", "", &Selected[0]))
					{
						Selected[1] = Selected[2] = Selected[3] = false;
						UI->SetRenderQuality(1 / 4.f);
						UI->RedrawScene();
					}
					if (ImGui::MenuItem("50%", "", &Selected[1]))
					{
						Selected[0] = Selected[2] = Selected[3] = false;
						UI->SetRenderQuality(1 / 2.f);
						UI->RedrawScene();
					}
					if (ImGui::MenuItem("100%", "", &Selected[2]))
					{
						Selected[1] = Selected[0] = Selected[3] = false;
						UI->SetRenderQuality(1.f);
						UI->RedrawScene();
					}
					if (ImGui::MenuItem("200%", "", &Selected[3]))
					{
						Selected[1] = Selected[2] = Selected[0] = false;
						UI->SetRenderQuality(2.f);
						UI->RedrawScene();
					}
					ImGui::EndMenu();
				}
				if (ImGui::BeginMenu("Fill Mode"))
				{
					bool Selected[3] = { false ,EDevice->dwFillMode == D3DFILL_WIREFRAME,EDevice->dwFillMode == D3DFILL_SOLID };
					if (ImGui::MenuItem("Wireframe", "", &Selected[1]))
					{
						EDevice->dwFillMode = D3DFILL_WIREFRAME;
						UI->RedrawScene();
					}
					if (ImGui::MenuItem("Solid", "", &Selected[2]))
					{
						EDevice->dwFillMode = D3DFILL_SOLID;
						UI->RedrawScene();
					}
					ImGui::EndMenu();
				}
				{
					bool Selected = psDeviceFlags.test(rsEdgedFaces);
					if (ImGui::MenuItem("Edged Faces", "", &Selected))
					{
						psDeviceFlags.set(rsEdgedFaces, Selected);
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
				ImGui::EndMenu();
			}
			ImGui::Separator();
			{
				bool selected = psDeviceFlags.test(rsDrawSafeRect);
				if (ImGui::MenuItem("Draw Safe Rect", "", &selected))
				{
					psDeviceFlags.set(rsDrawSafeRect, selected);
					UI->RedrawScene();
				}
			}
			{
				bool selected = psDeviceFlags.test(rsDrawGrid);
				if (ImGui::MenuItemI("Draw Grid", ICON_FA_TABLE_CELLS, "", &selected))
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
				if (ImGui::MenuItemI("Fog", ICON_FA_CLOUD, "", &selected))
				{
					psDeviceFlags.set(rsFog, selected);
					UI->RedrawScene();
				}
			}
			{
				bool selected = psDeviceFlags.test(rsMuteSounds);
				if (ImGui::MenuItemI("Mute Sounds", ICON_FA_VOLUME_XMARK, "", &selected))
				{
					psDeviceFlags.set(rsMuteSounds, selected);
				}
			}
			{
				bool selected = psDeviceFlags.test(rsRenderRealTime);
				if (ImGui::MenuItemI("Real Time", ICON_FA_HOURGLASS_HALF, "", &selected))
				{
					psDeviceFlags.set(rsRenderRealTime, selected);
				}
			}
			ImGui::Separator();
			{
				bool selected = psDeviceFlags.test(rsStatistic);
				if (ImGui::MenuItem("Stats", "", &selected))
				{
					psDeviceFlags.set(rsStatistic, selected);
					UI->RedrawScene();
				}

			}
			ImGui::EndMenu();
		};

		if (ImGui::BeginMenu("Windows")) {
		{
			bool selected = AllowLogCommands();

			if (ImGui::MenuItem("Log", "", &selected))
			{
				ExecCommand(COMMAND_LOG_COMMANDS);
			}

			CUIThemeManager& ThemeInstance = CUIThemeManager::Get();
			bool selected2 = !ThemeInstance.IsClosed();
			if (ImGui::MenuItemI("Theme", ICON_FA_PAINT_ROLLER, "", &selected2))
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

		IXEndMainMenuBar();
	}

	ImGui::PopStyleColor();
	ImGui::PopStyleVar();
}
