#include "stdafx.h"
#include "../Level.h"
#include "../Actor.h"
#include "../alife_simulator.h"
#include "../alife_object_registry.h"
#include "../Inventory.h"
#include "../inventory_item.h"
#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/string_table.h"
#include "../player_hud.h"
#include "ai_space.h"

#include "ImUtils.h"

extern bool hud_adj_crosshair;

void ImGui_Render2DWidget(float grid_step=24.0f)
{
	static ImVec2 circlePos(100.0f, 100.0f);
	static float circleRadius = 20.0f;
	const float squareSize = 200.0f;
	const float minRadius = 5.0f;
	const float maxRadius = 50.0f;
	const auto color_hovered = IM_COL32(255, 255, 0, 200);
	const auto color_nothovered = IM_COL32(255, 0, 0, 200);
	const auto color_active = IM_COL32(50, 200, 50, 200);
	ImGui::BeginChild("SquareArea", ImVec2(squareSize, squareSize), true,
		ImGuiWindowFlags_NoScrollbar | ImGuiWindowFlags_NoMove);
	{
		const ImVec2 squareMin = ImGui::GetWindowPos();
		const ImVec2 squareMax(squareMin.x + squareSize, squareMin.y + squareSize);
		ImDrawList* drawList = ImGui::GetWindowDrawList();

		// Draw square border
		drawList->AddRectFilled(squareMin, squareMax, IM_COL32(25,25,25,255));
		const ImU32 gridColor = IM_COL32(100, 100, 100, 255); // Gray with 50% alpha
		for (float x = 0; x <= squareSize; x += grid_step) {
			ImVec2 start(squareMin.x + x, squareMin.y);
			ImVec2 end(squareMin.x + x, squareMax.y);
			drawList->AddLine(start, end, gridColor);
		}
		for (float y = 0; y <= squareSize; y += grid_step) {
			ImVec2 start(squareMin.x, squareMin.y + y);
			ImVec2 end(squareMax.x, squareMin.y + y);
			drawList->AddLine(start, end, gridColor);
		}

		// Calculate circle position in screen space
		const ImVec2 circleCenter(squareMin.x + circlePos.x, squareMin.y + circlePos.y);

		// Create invisible button over the circle area
		ImGui::SetCursorScreenPos(ImVec2(circleCenter.x - circleRadius, circleCenter.y - circleRadius));
		ImGui::InvisibleButton("##CircleDrag", ImVec2(circleRadius * 2, circleRadius * 2));

		// Handle dragging only when clicking inside the circle
		if (ImGui::IsItemActive() && ImGui::IsMouseDragging(ImGuiMouseButton_Left))
		{
			ImVec2 mouseDelta = ImGui::GetIO().MouseDelta;
			circlePos.x = std::clamp(circlePos.x + mouseDelta.x,
				circleRadius, squareSize - circleRadius);
			circlePos.y = std::clamp(circlePos.y + mouseDelta.y,
				circleRadius, squareSize - circleRadius);
		}

		ImU32 cursor_color = color_nothovered;
		if (ImGui::IsItemHovered() && !ImGui::IsItemActive())
		{
			cursor_color = color_hovered;
		}

		if (ImGui::IsItemActive())
		{
			cursor_color = color_active;
		}

		// Handle mouse wheel for radius adjustment
		if (ImGui::IsWindowHovered())
		{
			const float wheel = ImGui::GetIO().MouseWheel;
			if (wheel != 0.0f)
			{
				circleRadius = std::clamp(circleRadius + wheel * 2.0f, minRadius, maxRadius);
				circlePos.x = std::clamp(circlePos.x, circleRadius, squareSize - circleRadius);
				circlePos.y = std::clamp(circlePos.y, circleRadius, squareSize - circleRadius);
			}
		}

		// Draw the circle
		drawList->AddCircle(circleCenter, circleRadius, cursor_color);
	}
	ImGui::EndChild();
}


void RenderHUDAdjustManager()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_HudAdjustManager)])
		return;

	if (!g_pGameLevel)
		return;

	if (!ai().get_alife())
		return;

	if (imgui_hud_adjust_manager.is_initialized == false)
		return;

	if (!g_actor)
		return;

	if (!g_player_hud)
		return;

	CInventoryItem* p_item = g_actor->inventory().ActiveItem();

	ImGui::PushStyleColor(ImGuiCol_WindowBg, ImVec4(0.0f, 0.0f, 0.0f, kGeneralAlphaLevelForImGuiWindows));

	ImGui::BeginDisabled(!p_item);

	if (ImGui::Begin("Hud Adjust", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_HudAdjustManager)]))
	{
		if (ImGui::BeginTabBar("Header"))
		{
			if (ImGui::BeginTabItem("General"))
			{
				xr_string p_active_weapon_name = "NO ACTIVE WEAPON";

				if (p_item)
				{
					p_active_weapon_name = Platform::ANSI_TO_UTF8(p_item->NameShort());
				}
				ImGui::Text("Active weapon: %s", p_active_weapon_name.c_str());

				if (p_item)
				{
					ImGui::Text("Item Section: %s", p_item->m_section_id.c_str());
					
					if (g_player_hud)
					{
						const char* p_hand = "single hand";
						bool two_hands = false;
						if (g_player_hud->attached_item(0) && g_player_hud->attached_item(1))
						{
							p_hand = "two hands";
							two_hands = true;
						}

						ImGui::Text("Mode: %s", p_hand);

						ImGui::Checkbox("Show crosshair", &hud_adj_crosshair);

						auto p_draw_info_hud_item = [](attachable_hud_item* p_item, u8 index) -> void {
							if (p_item)
							{
								char name[16] = "";
								sprintf_s(name, "attached_item#%d", index);
								ImGui::SeparatorText(name);

							//	ImGui::Text("Hands hud: %s", p_item->m_parent->section_name().c_str());
							//	ImGui::Text("Item hud: %s", p_item->m_sect_name.c_str());
								R_ASSERT2(p_item->m_parent, "must be valid!");

								char hud_header_name[32] = "";
								char item_header_name[32] = "";

								std::sprintf(hud_header_name, "Hud = %s##hh%d", p_item->m_parent->section_name().c_str(),index);
								std::sprintf(item_header_name, "Item = %s##hh%d", p_item->m_sect_name.c_str(), index);

								if (ImGui::CollapsingHeader(hud_header_name))
								{


									ImGui::SeparatorText("Position##HUD");

									if (ImGui::Button("Reset##HPosition"))
									{
										// todo: implement
									}

									if (ImGui::BeginTable("Data##HUDP", 2))
									{
										ImGui::TableNextRow();

										ImGui::TableNextColumn();

										Fvector& position = p_item->hands_offset_pos();

										ImGui::SliderFloat("X##HUDP", &position.x, -1.0f, 1.0f);

										ImGui::SliderFloat("Y##HUDP", &position.y, -1.0f, 1.0f);

										ImGui::SliderFloat("Z##HUDP", &position.z, -1.0f, 1.0f);
										
										
										auto test = ImGui::GetContentRegionAvail();
										ImGui::TableNextColumn();
										ImGui_Render2DWidget(16.0f);

										ImGui::EndTable();
									}




									ImGui::SeparatorText("Rotation##HUD");

									if (ImGui::Button("Reset##HRotation"))
									{
										// todo: implement
									}

									if (ImGui::BeginTable("Data##HUDR", 2))
									{
										ImGui::TableNextRow();

										ImGui::TableNextColumn();

										Fvector& rotation = p_item->hands_offset_rot();

										ImGui::SliderFloat("X##HUDR", &rotation.x, -360.0f, 360.0f);

										ImGui::SliderFloat("Y##HUDR", &rotation.y, -360.0f, 360.0f);

										ImGui::SliderFloat("Z##HUDR", &rotation.z, -360.0f, 360.0f);

										ImGui::TableNextColumn();

										ImGui::EndTable();
									}
								}

								if (ImGui::CollapsingHeader(item_header_name))
								{
									ImGui::SeparatorText("Position##Item");

									if (ImGui::BeginTable("Data##HUDPI", 2))
									{
										ImGui::TableNextRow();

										ImGui::TableNextColumn();

										Fvector& position = p_item->m_measures.m_item_attach[0];

										ImGui::SliderFloat("X##HUDP", &position.x, -1.0f, 1.0f);

										ImGui::SliderFloat("Y##HUDP", &position.y, -1.0f, 1.0f);

										ImGui::SliderFloat("Z##HUDP", &position.z, -1.0f, 1.0f);


										auto test = ImGui::GetContentRegionAvail();
										ImGui::TableNextColumn();
										ImGui_Render2DWidget(16.0f);

										ImGui::EndTable();
									}

									ImGui::SeparatorText("Rotation##Item");

									if (ImGui::BeginTable("Data##HUDR", 2))
									{
										ImGui::TableNextRow();

										ImGui::TableNextColumn();

										Fvector& rotation = p_item->m_measures.m_item_attach[1];

										ImGui::SliderFloat("X##HUDR", &rotation.x, -360.0f, 360.0f);

										ImGui::SliderFloat("Y##HUDR", &rotation.y, -360.0f, 360.0f);

										ImGui::SliderFloat("Z##HUDR", &rotation.z, -360.0f, 360.0f);

										ImGui::TableNextColumn();

										ImGui::EndTable();
									}

								}
							}
						};

						attachable_hud_item* p_hud_item_first = g_player_hud->attached_item(0);

						p_draw_info_hud_item(p_hud_item_first,0);

						if (two_hands)
						{
							attachable_hud_item* p_hud_item_second = g_player_hud->attached_item(1);
							p_draw_info_hud_item(p_hud_item_second,1);
						}
					}
				}

				ImGui::EndTabItem();
			}


			if (ImGui::BeginTabItem("Settings"))
			{
				int casted = imgui_hud_adjust_manager.settings.history_command_max_count;

				if (ImGui::Button("Save"))
				{
					// todo: implement
				}

				ImGui::SeparatorText("Params");

				if (ImGui::SliderInt("max history command count", &casted, 0, 1000))
				{
					imgui_hud_adjust_manager.settings.history_command_max_count = static_cast<decltype(imgui_hud_adjust_manager.settings.history_command_max_count)>(casted);
				}

				ImGui::EndTabItem();
			}

			ImGui::EndTabBar();
		}

		ImGui::End();
	}

	ImGui::EndDisabled();
	ImGui::PopStyleColor(1);
}