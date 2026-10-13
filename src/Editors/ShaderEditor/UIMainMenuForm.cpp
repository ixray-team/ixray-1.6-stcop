#include "stdafx.h"
#include "../xrEUI/xrUITheme.h"
#include "../xrECore/Editor/imgui_EditorEx.h"

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
			DrawMenuItemI("Save", ICON_FA_FLOPPY_DISK, COMMAND_SAVE);
			DrawMenuItemI("Reload", ICON_FA_ARROWS_ROTATE, COMMAND_LOAD);
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
			ImGui::EndMenu();
		}

		if (ImGui::BeginMenu("Options")) {
			if (ImGui::BeginMenu("Render"))
			{
				EditorRenderMenu::DrawItems();
				ImGui::EndMenu();
			}
			ImGui::Separator();
			{
				bool selected = psDeviceFlags.test(rsDrawSafeRect);
				if (ImGui::MenuItem("Draw Safe Rect", "", &selected))
				{
					psDeviceFlags.set(rsDrawSafeRect, selected);
					EContext.UI->RedrawScene();
				}
			}
			{
				bool selected = psDeviceFlags.test(rsDrawGrid);
				if (ImGui::MenuItemI("Draw Grid", ICON_FA_TABLE_CELLS, "", &selected))
				{
					psDeviceFlags.set(rsDrawGrid, selected);
					EContext.UI->RedrawScene();
				}
			}
			ImGui::Separator();
			{
				bool selected = psDeviceFlags.test(rsFog);
				if (ImGui::MenuItemI("Fog", ICON_FA_CLOUD, "", &selected))
				{
					psDeviceFlags.set(rsFog, selected);
					EContext.UI->RedrawScene();
				}
			}
			ImGui::Separator();
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
					EContext.UI->RedrawScene();
				}

			}
			ImGui::EndMenu();
		}

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
						if (!EContext.UI->HasWindow<CUIThemeManager>())
						{
							EContext.UI->Push(&ThemeInstance);
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
