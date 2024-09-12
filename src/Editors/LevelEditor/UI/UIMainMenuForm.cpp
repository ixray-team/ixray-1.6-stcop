#include "stdafx.h"
#include "../xrEUI/xrUITheme.h"

UIMainMenuForm::UIMainMenuForm()
{
}

UIMainMenuForm::~UIMainMenuForm()
{
}

void UIMainMenuForm::Draw()
{
  
	if (ImGui::BeginMainMenuBar())
	{
		if (ImGui::BeginMenu(g_pStringTable->translate("ed_st_file").c_str()))
		{
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_clear").c_str(), "")) { ExecCommand(COMMAND_CLEAR); }
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_open").c_str(), "")) { ExecCommand(COMMAND_LOAD); }
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_save").c_str(), "")) { ExecCommand(COMMAND_SAVE, xr_string(LTools->m_LastFileName.c_str())); }
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_save_as").c_str(), "")) { ExecCommand(COMMAND_SAVE, 0, 1); }
			ImGui::Separator();
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_open_selection").c_str(), "")) { ExecCommand(COMMAND_LOAD_SELECTION); }
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_save_selection_as").c_str(), "")) { ExecCommand(COMMAND_SAVE_SELECTION); }
			ImGui::Separator();
			if (ImGui::BeginMenu(g_pStringTable->translate("ed_st_open_recent").c_str(), ""))
			{
				for (auto& str : EPrefs->scene_recent_list)
				{
					if (ImGui::MenuItem(str.c_str(), "")) { ExecCommand(COMMAND_LOAD, str); }
				}
				ImGui::EndMenu();
			}
			ImGui::Separator();
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_quit").c_str(), "")) { ExecCommand(COMMAND_QUIT); }
			ImGui::EndMenu();
		}

		if (ImGui::BeginMenu(g_pStringTable->translate("ed_st_scene").c_str()))
		{
			{
				bool selected = !MainForm->GetWorldPropertiesFrom()->IsClosed();
				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_world_properties").c_str(), "", &selected)) { if (selected)MainForm->GetWorldPropertiesFrom()->Open(); else MainForm->GetWorldPropertiesFrom()->Close(); }
			}
			ImGui::Separator();

			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_validate").c_str(), "")) { ExecCommand(COMMAND_VALIDATE_SCENE); }
			ImGui::Separator();
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_summary_info").c_str(), "")) {
				ExecCommand(COMMAND_CLEAR_SCENE_SUMMARY);
				ExecCommand(COMMAND_COLLECT_SCENE_SUMMARY);
				ExecCommand(COMMAND_SHOW_SCENE_SUMMARY);
			}
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_highlight_texture").c_str(), ""))
			{
				ExecCommand(COMMAND_SCENE_HIGHLIGHT_TEXTURE);
			}
			ImGui::Separator();
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_clear_debug_draw").c_str(), ""))
			{
				ExecCommand(COMMAND_CLEAR_DEBUG_DRAW);
			}
			ImGui::Separator();
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_export_scene_as_obj").c_str(), ""))
			{
				Scene->ExportObj(false);
			}
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_export_selection_as_obj").c_str(), ""))
			{
				Scene->ExportObj(true);
			}
			ImGui::EndMenu();
		}
		if (ImGui::BeginMenu(g_pStringTable->translate("ed_st_compile").c_str()))
		{
			if (ImGui::BeginMenu(g_pStringTable->translate("ed_st_make").c_str()))
			{
				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_make_all").c_str(), ""))
				{
					ExecCommand(COMMAND_BUILD);
				}
				ImGui::Separator();
				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_make_game").c_str(), ""))
				{
					ExecCommand(COMMAND_MAKE_GAME);
				}
				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_make_puddles").c_str(), ""))
				{
					ExecCommand(COMMAND_MAKE_PUDDLES);
				}
				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_make_details").c_str(), ""))
				{
					ExecCommand(COMMAND_MAKE_DETAILS);
				}
				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_make_hom").c_str(), ""))
				{
					ExecCommand(COMMAND_MAKE_HOM);
				}
				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_make_som").c_str(), ""))
				{
					ExecCommand(COMMAND_MAKE_SOM);
				}
				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_make_ai").c_str(), ""))
				{
					ExecCommand(COMMAND_MAKE_AIMAP);
				}
				ImGui::EndMenu();
			}
			bool bDisable = false;
			if (LTools->IsCompilerRunning() || LTools->IsGameRunning())
			{
				ImGui::BeginDisabled();
				bDisable = true;
			}
			if (ImGui::BeginMenu(g_pStringTable->translate("ed_st_compile").c_str()))
			{
				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_geometry_and_light").c_str(), ""))
				{
					LTools->RunXrLC();
				}
				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_detail_obj_light").c_str(), ""))
				{
					LTools->RunXrDO();
				}
				if (ImGui::BeginMenu(g_pStringTable->translate("ed_st_ai_map").c_str()))
				{
					if (ImGui::MenuItem(g_pStringTable->translate("ed_st_high").c_str(), ""))
					{
						LTools->RunXrAI_AIMap(false);
					}
					if (ImGui::MenuItem(g_pStringTable->translate("ed_st_low").c_str(), ""))
					{
						LTools->RunXrAI_AIMap(false);
					}
					if (ImGui::MenuItem(g_pStringTable->translate("ed_st_verify").c_str(), ""))
					{
						LTools->RunXrAI_Verify();
					}
					ImGui::EndMenu();
				}
				if (ImGui::BeginMenu(g_pStringTable->translate("ed_st_spawn").c_str()))
				{
					if (ImGui::MenuItem(g_pStringTable->translate("ed_st_only_current_lvl").c_str(), ""))
					{
						LTools->RunXrAI_Spawn(true);
					}
					if (ImGui::MenuItem(g_pStringTable->translate("ed_st_all_levels").c_str(), ""))
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
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_import_error_list").c_str(), ""))
			{
				ExecCommand(COMMAND_IMPORT_COMPILER_ERROR);
			}
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_import_xrai_error_list").c_str(), ""))
			{
				ExecCommand(COMMAND_IMPORT_AICOMPILER_ERROR);
			}
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_export_error_list").c_str(), ""))
			{
				ExecCommand(COMMAND_EXPORT_COMPILER_ERROR);
			}
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_clear_error_list").c_str(), ""))
			{
				ExecCommand(COMMAND_CLEAR_DEBUG_DRAW);
			}
			ImGui::EndMenu();
		}
		if (ImGui::BeginMenu(g_pStringTable->translate("ed_st_objects").c_str()))
		{
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_lib_editor").c_str())) { ExecCommand(COMMAND_LIBRARY_EDITOR); }
			ImGui::Separator();
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_reload").c_str())) { ExecCommand(COMMAND_RELOAD_OBJECTS); }
			ImGui::EndMenu();
		}
		if (ImGui::BeginMenu(g_pStringTable->translate("ed_st_images").c_str()))
		{
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_image_editor").c_str(), "")) { ExecCommand(COMMAND_IMAGE_EDITOR); }
			ImGui::Separator();
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_reload_textures").c_str(), "")) { ExecCommand(COMMAND_RELOAD_TEXTURES); }
			ImGui::Separator();
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_sync_textures").c_str(), "")) { ExecCommand(COMMAND_REFRESH_TEXTURES); }
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_check_new_textures").c_str(), "")) { ExecCommand(COMMAND_CHECK_TEXTURES); }
			ImGui::Separator();
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_edit_minimap").c_str(), "")) { ExecCommand(COMMAND_MINIMAP_EDITOR); }
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_sync_thm").c_str(), ""))
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
		if (ImGui::BeginMenu(g_pStringTable->translate("ed_st_sounds").c_str()))
		{
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_sound_editor").c_str(), "")) { ExecCommand(COMMAND_SOUND_EDITOR); }
			ImGui::Separator();
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_sync_sounds").c_str(), "")) { ExecCommand(COMMAND_SYNC_SOUNDS); }
			ImGui::Separator();
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_refresh_env_lib").c_str(), "")) { ExecCommand(COMMAND_REFRESH_SOUND_ENVS); }
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_refresh_env_geom").c_str(), "")) { ExecCommand(COMMAND_REFRESH_SOUND_ENV_GEOMETRY); }
			ImGui::EndMenu();

		}
		
		if (ImGui::BeginMenu(g_pStringTable->translate("ed_st_options").c_str()))
		{
			if (ImGui::BeginMenu(g_pStringTable->translate("ed_st_render").c_str()))
			{
				if (ImGui::BeginMenu(g_pStringTable->translate("ed_st_quality").c_str()))
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
				if (ImGui::BeginMenu(g_pStringTable->translate("ed_st_fill_mode").c_str()))
				{
					bool selected[3] = { EDevice->dwFillMode == D3DFILL_POINT,EDevice->dwFillMode == D3DFILL_WIREFRAME,EDevice->dwFillMode == D3DFILL_SOLID };
					if (ImGui::MenuItem(g_pStringTable->translate("ed_st_point").c_str(), "", &selected[0]))
					{
						EDevice->dwFillMode = D3DFILL_POINT;
						UI->RedrawScene();
					}
					if (ImGui::MenuItem(g_pStringTable->translate("ed_st_wireframe").c_str(), "", &selected[1]))
					{
						EDevice->dwFillMode = D3DFILL_WIREFRAME;
						UI->RedrawScene();
					}
					if (ImGui::MenuItem(g_pStringTable->translate("ed_st_solid").c_str(), "", &selected[2]))
					{
						EDevice->dwFillMode = D3DFILL_SOLID;
						UI->RedrawScene();
					}
					ImGui::EndMenu();
				}
				if (ImGui::BeginMenu(g_pStringTable->translate("ed_st_shader_mode").c_str()))
				{
					bool selected[2] = { EDevice->dwShadeMode == D3DSHADE_FLAT,EDevice->dwShadeMode == D3DSHADE_GOURAUD };
					if (ImGui::MenuItem(g_pStringTable->translate("ed_st_flat").c_str(), "", &selected[0]))
					{
						EDevice->dwShadeMode = D3DSHADE_FLAT;
						UI->RedrawScene();
					}
					if (ImGui::MenuItem(g_pStringTable->translate("ed_st_gouraud").c_str(), "", &selected[1]))
					{
						EDevice->dwShadeMode = D3DSHADE_GOURAUD;
						UI->RedrawScene();
					}
					ImGui::EndMenu();
				}
				{
					bool selected = psDeviceFlags.test(rsEdgedFaces);
					if (ImGui::MenuItem(g_pStringTable->translate("ed_st_edged_faces").c_str(), "", &selected))
					{
						psDeviceFlags.set(rsEdgedFaces, selected);
						UI->RedrawScene();
					}
				}
				ImGui::Separator();
				{
					bool selected = !Caps.bForceGPU_SW;
					if (ImGui::MenuItem(g_pStringTable->translate("ed_st_hw_render").c_str(), "", &selected))
					{
						Caps.bForceGPU_SW = !selected;
						UI->Resize();
					}
				}
				ImGui::Separator();
				{
					bool selected = psDeviceFlags.test(rsFilterLinear);
					if (ImGui::MenuItem(g_pStringTable->translate("ed_st_linear_filtering").c_str(), "", &selected))
					{
						psDeviceFlags.set(rsFilterLinear, selected);
						UI->RedrawScene();
					}
				}
				{
					bool selected = psDeviceFlags.test(rsRenderTextures);
					if (ImGui::MenuItem(g_pStringTable->translate("ed_st_textures").c_str(), "", &selected))
					{
						psDeviceFlags.set(rsRenderTextures, selected);
						UI->RedrawScene();
					}
				}
				ImGui::EndMenu();
			}
			{
				bool selected = psDeviceFlags.test(rsDrawGrid);
				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_draw_grid").c_str(), "", &selected))
				{
					psDeviceFlags.set(rsDrawGrid, selected);
					UI->RedrawScene();
				}
			}
			ImGui::Separator();
			{
				bool selected = psDeviceFlags.test(rsFog);
				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_fog").c_str(), "", &selected))
				{
					psDeviceFlags.set(rsFog, selected);
					UI->RedrawScene();
				}
			}
			{
				if (ImGui::BeginMenu(g_pStringTable->translate("ed_st_environment").c_str()))
				{
					bool selected = !psDeviceFlags.test(rsEnvironment);
					if (ImGui::MenuItem(g_pStringTable->translate("ed_st_none").c_str(), "", &selected))
					{
						psDeviceFlags.set(rsEnvironment, false);
						UI->RedrawScene();
					}
					ImGui::Separator();
					for (auto& i : g_pGamePersistent->Environment().WeatherCycles)
					{
						selected = psDeviceFlags.test(rsEnvironment) && i.first == g_pGamePersistent->Environment().CurrentCycleName;
						if (ImGui::MenuItem(i.first.c_str(), "", &selected))
						{
							psDeviceFlags.set(rsEnvironment, true);
							g_pGamePersistent->Environment().SetWeather(i.first.c_str(),true);
							UI->RedrawScene();
						}
					}
				   
					ImGui::EndMenu();
				}
			}
			ImGui::Separator();
			{
				bool selected = psDeviceFlags.test(rsLighting);
				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_lighting").c_str(), "", &selected))
				{
					psDeviceFlags.set(rsLighting, selected);
					UI->RedrawScene();
				}
			}
			{
				bool selected = psDeviceFlags.test(rsMuteSounds);
				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_mute_sounds").c_str(), "", &selected))
				{
					psDeviceFlags.set(rsMuteSounds, selected);
				}
			}
			{
				bool selected = psDeviceFlags.test(rsRenderRealTime);
				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_real_time").c_str(), "", &selected))
				{
					psDeviceFlags.set(rsRenderRealTime, selected);
				}
			}
			ImGui::Separator();
			{
				bool selected = psDeviceFlags.test(rsStatistic);
				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_stats").c_str(), "", &selected)) { psDeviceFlags.set(rsStatistic, selected);  UI->RedrawScene(); }

			}
			ImGui::EndMenu();
		}

		if (ImGui::BeginMenu(g_pStringTable->translate("ed_st_windows").c_str()))
		{
			{
				bool selected = MainForm->GetLeftBarForm()->IsUseSnapList();
				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_snap_list").c_str(), "", &selected))
				{
					MainForm->GetLeftBarForm()->ShowSnapList(selected);
				}
			}
			{
				bool selected = MainForm->GetLeftBarForm()->IsUseObjectsTool();
				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_obj_tools").c_str(), "", &selected))
				{
					MainForm->GetLeftBarForm()->ShowObjectsTool(selected);
				}
				{
					bool selected = !MainForm->GetPropertiesFrom()->IsClosed();
					if (ImGui::MenuItem(g_pStringTable->translate("ed_st_properties").c_str(), "", &selected)) { if (selected)MainForm->GetPropertiesFrom()->Open(); else MainForm->GetPropertiesFrom()->Close(); }
				}
			}
			{
				bool selected = AllowLogCommands();

				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_log").c_str(), "", &selected)) { ExecCommand(COMMAND_LOG_COMMANDS); }

				CUIThemeManager& ThemeInstance = CUIThemeManager::Get();
				bool selected2 = !ThemeInstance.IsClosed();
				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_theme").c_str(), "", &selected2))
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

		ImGui::Separator();

		if (ImGui::MenuItem(g_pStringTable->translate("ed_st_la_editor").c_str(), ""))
		{ 
			ExecCommand(COMMAND_LIGHTANIM_EDITOR);
		}

		{
			bool selected = UIObjectList::IsOpen();
			if (ImGui::MenuItem(g_pStringTable->translate("ed_st_obj_list").c_str(), "", &selected)) { if (selected) UIObjectList::Show(); else UIObjectList::Close(); }
		}
		
		if (ImGui::MenuItem(g_pStringTable->translate("ed_st_preferences").c_str(), "")) { ExecCommand(COMMAND_EDITOR_PREF); }

		ImGui::EndMainMenuBar();
	}
}

