#include "stdafx.h"
#include "../../Layers/xrRender/dxRenderDeviceRender.h"
#include "../xrEUI/xrUITheme.h"
#include "../xrEUI/imgui_EditorEx.h"

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

void UIMainMenuForm::DrawMenuItemI(const char* label, const char* icon, int command, int param, int flag)
{
    if (ImGui::MenuItemI(label, icon, *GetCommandShortcat(command)))
    {
        ExecCommand(command, param, flag);
    }
}

void UIMainMenuForm::Draw()
{
    if (IXBeginMainMenuBar())
    {
        if (ImGui::BeginMenu("File"))
        {
            DrawMenuItemI("Save", ICON_FA_FLOPPY_DISK, COMMAND_SAVE);  
            DrawMenuItemI("Reload", ICON_FA_ARROWS_ROTATE, COMMAND_LOAD);
            ImGui::Separator();

            DrawMenuItemI("Save .xr", ICON_FA_FLOPPY_DISK, COMMAND_SAVE_XR);
            DrawMenuItemI("Load .xr", ICON_FA_FILE_IMPORT, COMMAND_LOAD_XR);
            ImGui::Separator();

            DrawMenuItemI("Validate", ICON_FA_CIRCLE_CHECK, COMMAND_VALIDATE);
            DrawMenuItem("Compact", COMMAND_COMPACT_PARTICLES);
            ImGui::EndMenu();
        }
        if (ImGui::BeginMenu("Editors"))
        {
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
            ImGui::Separator();
            {
                bool selected = psDeviceFlags.test(rsFog);
                if (ImGui::MenuItemI("Fog", ICON_FA_CLOUD, "", &selected))
                {
                    psDeviceFlags.set(rsFog, selected);
                    UI->RedrawScene();
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
                    UI->RedrawScene();
                }

            }
            ImGui::EndMenu();
        }
        if (ImGui::BeginMenu("Windows"))
        {
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
}
