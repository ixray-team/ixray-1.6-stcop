#include "StdAfx.h"
#include "ImUtils.h"
#include "FDemoRecord.h"

extern float dr_cam_inert;
extern float dr_cam_pos_inert;
extern bool dr_disable_time_factor_influence;
extern float g_base_fov;

static int s_help_marker_counter = 0;
static float s_slider_step = 0.01f;

static void tip(const char* desc)
{
	ImGui::PushID(s_help_marker_counter++);
	ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(0.35f, 0.55f, 0.85f, 1.0f));
	ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(2, 0));
	ImGui::Text("[?]");
	bool hovered = ImGui::IsItemHovered();
	ImGui::PopStyleVar();
	ImGui::PopStyleColor();
	ImGui::PopID();

	if (hovered)
	{
		ImGui::BeginTooltip();
		ImGui::PushTextWrapPos(ImGui::GetFontSize() * 40.0f);
		ImGui::TextWrapped("%s", desc);
		ImGui::PopTextWrapPos();
		ImGui::EndTooltip();
	}
}

static void drag_float3_reset(const char* label, Fvector& v, float step, const Fvector& def, float min = -10000.f, float max = 10000.f)
{
	ImGui::DragFloat3(label, &v.x, step, min, max, "%.4f");
	ImGui::SameLine();
	if (ImGui::SmallButton(("R##" + xr_string(label)).c_str()))
	{
		v.set(def);
	}
}

static void render_bone_tree(IKinematics* kinematics, u16 bone_id, CDemoRecord* rec, CObject* obj)
{
	if (!kinematics || bone_id >= kinematics->LL_BoneCount())
	{
		return;
	}

	const char* bone_name = kinematics->LL_BoneName_dbg(bone_id);
	bool selected = (bone_id == rec->bone_id && rec->view_from_bone_mode);
	bool has_children = false;

	for (u16 j = 0; j < kinematics->LL_BoneCount(); j++)
	{
		if (j == bone_id)
		{
			continue;
		}

		if (kinematics->GetBoneData(j).GetParentID() == bone_id)
		{
			has_children = true;
			break;
		}
	}

	string128 label;
	if (selected)
	{
		sprintf_s(label, "[*] %d: %s", bone_id, bone_name);
	}
	else
	{
		sprintf_s(label, "%d: %s", bone_id, bone_name);
	}

	if (has_children)
	{
		ImGuiTreeNodeFlags flags = 0;
		if (selected)
		{
			flags |= ImGuiTreeNodeFlags_Selected;
		}

		bool open = ImGui::TreeNodeEx((void*)(intptr_t)bone_id, flags, "%s", label);

		if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
		{
			rec->bone_holder = obj;
			rec->bone_holder_kinematics = kinematics;
			rec->bone_id = bone_id;
			rec->view_from_bone_mode = true;
			rec->look_at_point_mode = false;
		}

		if (open)
		{
			for (u16 j = 0; j < kinematics->LL_BoneCount(); j++)
			{
				if (j == bone_id)
				{
					continue;
				}
				if (kinematics->GetBoneData(j).GetParentID() == bone_id)
				{
					render_bone_tree(kinematics, j, rec, obj);
				}
			}
			ImGui::TreePop();
		}
	}
	else
	{
		ImGuiTreeNodeFlags flags = ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen;
		if (selected)
		{
			flags |= ImGuiTreeNodeFlags_Selected;
		}

		ImGui::TreeNodeEx((void*)(intptr_t)bone_id, flags, "%s", label);

		if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
		{
			rec->bone_holder = obj;
			rec->bone_holder_kinematics = kinematics;
			rec->bone_id = bone_id;
			rec->view_from_bone_mode = true;
			rec->look_at_point_mode = false;
		}
	}
}

void RenderDemoRecordEditorWindow()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_DemoRecord)])
	{
		return;
	}

	ImGui::PushStyleColor(ImGuiCol_WindowBg, ImVec4(0.0f, 0.0f, 0.0f, kGeneralAlphaLevelForImGuiWindows));

	if (!ImGui::Begin("Demo Record", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_DemoRecord)]))
	{
		ImGui::End();
		ImGui::PopStyleColor(1);
		return;
	}

	if (demo_record == nullptr)
	{
		ImGui::TextDisabled("No active demo recording");
		ImGui::End();
		ImGui::PopStyleColor(1);
		return;
	}

	s_help_marker_counter = 0;

	ImGui::SeparatorText("Global");
	ImGui::Text("Slider step:");
	ImGui::SameLine();
	ImGui::SetNextItemWidth(120);
	ImGui::InputFloat("##step", &s_slider_step, 0.001f, 0.1f, "%.4f");
	ImGui::SameLine();
	tip("Controls the precision of all DragFloat sliders below. "
				"Lower values = finer control. Use Ctrl+click on a slider to type exact value.");

	if (ImGui::CollapsingHeader("Options", ImGuiTreeNodeFlags_DefaultOpen))
	{
		ImGui::Checkbox("Disable time factor influence", &dr_disable_time_factor_influence);
		ImGui::SameLine();
		tip("When enabled, camera movement is not affected by the game's time factor (e.g. slowdown/pause). "
					"Movement speed remains constant regardless of game speed.");

		ImGui::Checkbox("Draw skeleton", &demo_record->draw_skeleton);
		ImGui::SameLine();
		tip("Renders the bone skeleton of the object under the crosshair. "
					"Useful for debugging bone attachments and camera look-at points.");
	}

	if (ImGui::CollapsingHeader("Input", ImGuiTreeNodeFlags_DefaultOpen))
	{
		ImGui::Checkbox("New input schema", &demo_record->new_input_schema);
		ImGui::SameLine();
		tip("Toggles between old and new camera input schemes.\n\n"
					"OLD: separate speed multipliers for Shift (slow), Alt (fast), Ctrl (accel).\n"
					"NEW: single unified 'Camera movement speed' multiplier for all directions.");

		if (demo_record->new_input_schema)
		{
			ImGui::SliderFloat("Camera movement speed", &demo_record->camera_transform_speed, 1.0f, 100.0f, "%.1f");
			ImGui::SameLine();
			tip("Unified movement speed multiplier. Applied to all camera movement axes equally. "
						"Higher values = faster camera. Default: 3.0");
			ImGui::SameLine();
			if (ImGui::SmallButton("R##cam_speed"))
			{
				demo_record->camera_transform_speed = demo_record->stored_camera_transform_speed;
			}
		}
		else
		{
			ImGui::SliderFloat("Speed 0 (slow)", &demo_record->m_fSpeed0, 0.0f, 50.0f, "%.1f");
			ImGui::SameLine();
			tip("Speed multiplier when holding Shift. Used for slow, precise camera movement. "
						"Value is read from [demo_record] speed0 in user.ltx.");
			ImGui::SameLine();
			if (ImGui::SmallButton("R##speed0"))
			{
				demo_record->m_fSpeed0 = demo_record->stored_fSpeed0;
			}

			ImGui::SliderFloat("Speed 2 (fast)", &demo_record->m_fSpeed2, 0.0f, 500.0f, "%.1f");
			ImGui::SameLine();
			tip("Speed multiplier when holding Alt. Used for fast camera traversal. "
						"Value is read from [demo_record] speed2 in user.ltx.");
			ImGui::SameLine();
			if (ImGui::SmallButton("R##speed2"))
			{
				demo_record->m_fSpeed2 = demo_record->stored_fSpeed2;
			}

			ImGui::SliderFloat("Speed 3 (accel)", &demo_record->m_fSpeed3, 0.0f, 1000.0f, "%.1f");
			ImGui::SameLine();
			tip("Speed multiplier when holding Ctrl (acceleration). "
						"Value is read from [demo_record] speed3 in user.ltx.");
			ImGui::SameLine();
			if (ImGui::SmallButton("R##speed3"))
			{
				demo_record->m_fSpeed3 = demo_record->stored_fSpeed3;
			}

			ImGui::TextDisabled("Base speed: 10.0 (hardcoded)");
			ImGui::SameLine();
			tip("Default movement speed when no modifier key is held. Hardcoded to 10.0 in source code.");
		}

		if (ImGui::TreeNode("Controls reference"))
		{
			if (demo_record->new_input_schema)
			{
				ImGui::BulletText("W/A/S/D        - Move forward / left / backward / right");
				ImGui::BulletText("Q/E             - Roll camera left / right");
				ImGui::BulletText("Mouse           - Look around (pitch / yaw)");
				ImGui::BulletText("R/T             - Increase / decrease FOV");
				ImGui::BulletText("MWHEEL          - Increase camera speed");
				ImGui::BulletText("SHIFT+MWHEEL    - Decrease camera speed");
				ImGui::BulletText("F               - Record keyframe");
				ImGui::BulletText("U               - Toggle bone attach / detach");
				ImGui::BulletText("J               - Toggle look-at lock");
				ImGui::BulletText("K               - Toggle skeleton rendering");
				ImGui::BulletText("Z               - Reset FOV to original");
				ImGui::BulletText("0               - Toggle redirect input to level");
				ImGui::BulletText("F1              - Show help overlay");
				ImGui::BulletText("F11             - Level map screenshot");
				ImGui::BulletText("Ctrl+F11        - Level map screenshot (HQ, 4 tiles)");
				ImGui::BulletText("F12             - Screenshot");
				ImGui::BulletText("Backspace       - Cubemap capture");
				ImGui::BulletText("ESC             - Quit demo record");
				ImGui::BulletText("Pause           - Pause / unpause");
				ImGui::BulletText("`               - Show console");
			}
			else
			{
				ImGui::BulletText("W/A/S/D        - Move forward / left / backward / right");
				ImGui::BulletText("Q/E             - Roll camera left / right");
				ImGui::BulletText("Mouse           - Look around (pitch / yaw)");
				ImGui::BulletText("R/T             - Increase / decrease FOV");
				ImGui::BulletText("SHIFT (hold)    - Slow movement (Speed 0)");
				ImGui::BulletText("ALT (hold)      - Fast movement (Speed 2)");
				ImGui::BulletText("CTRL (hold)     - Acceleration (Speed 3)");
				ImGui::BulletText("LMB (hold)      - Move forward");
				ImGui::BulletText("RMB (hold)      - Move backward");
				ImGui::BulletText("MWHEEL          - Increase camera speed");
				ImGui::BulletText("SHIFT+MWHEEL    - Decrease camera speed");
				ImGui::BulletText("SPACE           - Record keyframe");
				ImGui::BulletText("U               - Toggle bone attach / detach");
				ImGui::BulletText("J               - Toggle look-at lock");
				ImGui::BulletText("K               - Toggle skeleton rendering");
				ImGui::BulletText("Z               - Reset FOV to original");
				ImGui::BulletText("0               - Toggle redirect input to level");
				ImGui::BulletText("F1              - Show help overlay");
				ImGui::BulletText("F11             - Level map screenshot");
				ImGui::BulletText("Ctrl+F11        - Level map screenshot (HQ, 4 tiles)");
				ImGui::BulletText("F12             - Screenshot");
				ImGui::BulletText("Backspace       - Cubemap capture");
				ImGui::BulletText("ESC             - Quit demo record");
				ImGui::BulletText("Pause           - Pause / unpause");
				ImGui::BulletText("`               - Show console");
			}
			ImGui::TreePop();
		}
	}

	if (ImGui::CollapsingHeader("Camera", ImGuiTreeNodeFlags_DefaultOpen))
	{
		ImGui::SeparatorText("Mode");

		int current_mode = demo_record->view_from_bone_mode ? 2 : (demo_record->look_at_point_mode ? 1 : 0);
		const char* mode_names[] = {"FreeLook", "LookAtPoint", "LookFromBone"};
		ImGui::Combo("Camera mode", &current_mode, mode_names, 3);

		if (current_mode == 0)
		{
			if (demo_record->view_from_bone_mode || demo_record->look_at_point_mode)
			{
				demo_record->detach_bone();
				demo_record->look_at_point_mode = false;
			}
		}
		else if (current_mode == 1)
		{
			if (!demo_record->look_at_point_mode)
			{
				if (demo_record->rq_result.range > EPS_S)
				{
					demo_record->view_from_bone_mode = false;

					if (demo_record->rq_result.O != nullptr)
					{
						if (IRenderVisual* v = demo_record->rq_result.O->Visual())
						{
							if (IKinematics* k = v->dcast_PKinematics())
							{
								demo_record->bone_holder = demo_record->rq_result.O;
								demo_record->bone_holder_kinematics = k;
								demo_record->bone_id = (u16)demo_record->rq_result.element;
							}
							else
							{
								Fvector cam_pos;
								demo_record->GetGlobalPosition(cam_pos);
								demo_record->look_at_point.set(cam_pos.mad(Device.vCameraDirection, demo_record->rq_result.range));
							}
						}
					}
					else
					{
						Fvector cam_pos;
						CDemoRecord::GetGlobalPosition(cam_pos);
						demo_record->look_at_point.set(cam_pos.mad(Device.vCameraDirection, demo_record->rq_result.range));
					}

					Fvector current_eulers;
					demo_record->get_camera_hpb(current_eulers);
					demo_record->hpb.set(current_eulers);

					demo_record->look_at_point_mode = true;
				}
			}
		}
		else if (current_mode == 2)
		{
			if (!demo_record->view_from_bone_mode)
			{
				demo_record->look_at_point_mode = false;
				demo_record->try_attach_bone();
			}
		}

		if (demo_record->view_from_bone_mode)
		{
			ImGui::SameLine();
			tip("ViewFromBone: camera is attached to a bone.\n"
						"Use the offset controls below to adjust rotation and position\n"
						"relative to the bone. Press U to detach (or switch mode above).");
		}
		else if (demo_record->look_at_point_mode)
		{
			ImGui::SameLine();
			tip("LookAtPoint: camera always faces a locked target point.\n"
						"Press J to unlock (or switch mode above).");
		}
		else
		{
			ImGui::SameLine();
			tip("FreeLook: standard free camera. Use mouse and WASD to navigate.\n"
						"Press J to lock look-at, U to attach to bone.");
		}

		ImGui::SeparatorText("Smoothing");

		ImGui::DragFloat("Rotation inertia", &dr_cam_inert, s_slider_step, 0.0f, .999f, "%.4f");
		ImGui::SameLine();
		tip("Rotation inertia (smoothing). Controls how quickly the camera orientation catches up "
					"to the target angle. 0.0 = instant (no smoothing), 1.0 = very slow. Default: 0.0");
		ImGui::SameLine();
		if (ImGui::SmallButton("R##inert_rot"))
		{
			dr_cam_inert = 0.f;
		}

		ImGui::DragFloat("Position inertia", &dr_cam_pos_inert, s_slider_step, 0.0f, .999f, "%.4f");
		ImGui::SameLine();
		tip("Position inertia (smoothing). Controls how quickly the camera position catches up "
					"to the target point. 0.0 = instant (no smoothing), 1.0 = very slow. Default: 0.0");
		ImGui::SameLine();
		if (ImGui::SmallButton("R##inert_pos"))
		{
			dr_cam_pos_inert = 0.f;
		}

		if (!demo_record->view_from_bone_mode)
		{
			ImGui::SeparatorText("Orientation");
			tip("Camera heading/pitch/roll angles. Updated from mouse input each frame.");

			Fvector hpb_zero = {0.f, 0.f, 0.f};
			drag_float3_reset("##hpb", demo_record->hpb, s_slider_step, hpb_zero);
		}

		if (demo_record->view_from_bone_mode)
		{
			ImGui::SeparatorText("Bone View Offsets");
			tip("Offsets applied on top of the bone's world transform.\n"
						"Rotation offset: added to bone HPB angles (negated on X/Y).\n"
						"Position offset: transformed by camera basis and added to bone world position.\n"
						"Press Z to reset both to zero.");

			Fvector hpb_zero = {0.f, 0.f, 0.f};
			drag_float3_reset("##bone_hpb_off", demo_record->hpb_view_from_bone_offset, s_slider_step, hpb_zero);
			ImGui::SameLine();
			ImGui::Text("Rotation offset");

			Fvector pos_zero = {0.f, 0.f, 0.f};
			drag_float3_reset("##bone_pos_off", demo_record->p_cam_pos_view_from_bone_offset, s_slider_step, pos_zero);
			ImGui::SameLine();
			ImGui::Text("Position offset");
		}
		
		ImGui::SeparatorText("Field of View");
		ImGui::DragFloat("FOV", &g_base_fov, s_slider_step * 10.f, 5.f, 179.f, "%.1f");
		ImGui::SameLine();
		tip("Current field of view. Use R/T keys or mouse wheel to change.\n"
					"Press Reset to restore to the value saved when demo recording started.");
		ImGui::SameLine();
		if (ImGui::SmallButton("R##fov"))
		{
			g_base_fov = demo_record->stored_fov;
		}

		ImGui::Checkbox("Auto FOV change speed", &demo_record->fov_auto_scale);
		ImGui::SameLine();
		tip("When enabled, R/T key FOV change speed scales automatically based on camera movement speed.\n"
					"When disabled, FOV change speed is set manually via the slider below.");

		if (!demo_record->fov_auto_scale)
		{
			ImGui::SliderFloat("FOV change speed", &demo_record->fov_scale_speed, 0.1f, 100.0f, "%.1f");
			ImGui::SameLine();
			tip("Manual FOV change speed (degrees per second) when pressing R/T.\n"
						"Higher values = faster FOV change. Default: 5.0");
			ImGui::SameLine();
			if (ImGui::SmallButton("R##fov_scale_speed"))
			{
				demo_record->fov_scale_speed = demo_record->stored_fov_scale_speed;
			}
		}
	}

	if (ImGui::CollapsingHeader("Bone Browser", ImGuiTreeNodeFlags_DefaultOpen))
	{
		CObject* target_obj = nullptr;
		IKinematics* kin = nullptr;

		if (demo_record->view_from_bone_mode && demo_record->bone_holder_kinematics != nullptr)
		{
			target_obj = demo_record->bone_holder;
			kin = demo_record->bone_holder_kinematics;
		}
		else if (demo_record->rq_result.O != nullptr)
		{
			if (IRenderVisual* v = demo_record->rq_result.O->Visual())
			{
				kin = v->dcast_PKinematics();
			}
			target_obj = demo_record->rq_result.O;
		}

		if (demo_record->view_from_bone_mode && demo_record->bone_holder_kinematics != nullptr && demo_record->bone_id != BI_NONE)
		{
			ImGui::SeparatorText("Attached to bone:");
			ImGui::Text("Bone: %d - %s", demo_record->bone_id, demo_record->bone_holder_kinematics->LL_BoneName_dbg(demo_record->bone_id));
			ImGui::SameLine();
			if (ImGui::SmallButton("Detach##bone"))
			{
				demo_record->detach_bone();
			}
		}
		
		if (target_obj == nullptr || kin == nullptr)
		{
			ImGui::TextDisabled("Point crosshair at an object with bones");
		}
		else
		{
			ImGui::Text("Object: %s", target_obj->cName().c_str());
			ImGui::Text("Bones: %d", kin->LL_BoneCount());

			if (demo_record->view_from_bone_mode)
			{
				ImGui::SameLine();
				ImGui::TextDisabled("(attached)");
			}

			if (ImGui::TreeNode("Bone tree"))
			{
				u16 root = kin->LL_GetBoneRoot();
				render_bone_tree(kin, root, demo_record, target_obj);
				ImGui::TreePop();
			}
		}
	}

	if (ImGui::CollapsingHeader("State", ImGuiTreeNodeFlags_DefaultOpen))
	{
		ImGui::Text("Keyframes: %d", (int)demo_record->keyframes.size());
		ImGui::SameLine();
		tip("Number of recorded keyframes. Press F (new schema) or Space (old schema) to record a new keyframe.");

		ImGui::Text("Acceleration: %s", demo_record->enable_acceleration ? "on" : "off");
		ImGui::SameLine();
		tip("Holding Ctrl enables acceleration mode (uses Speed 3 multiplier in old schema). "
					"Automatically engaged while Ctrl is held.");

		ImGui::Text("Redirect input: %s", demo_record->redirect_input_to_level ? "LEVEL" : "DEMO RECORD");
		ImGui::SameLine();
		tip("When enabled, keyboard/mouse input is forwarded to the game entity instead of the demo camera. "
					"Press 0 to toggle.");
	}

	if (ImGui::CollapsingHeader("Actions", ImGuiTreeNodeFlags_DefaultOpen))
	{
		if (ImGui::Button("Screenshot", ImVec2(-1, 0)))
		{
			demo_record->make_screenshot();
		}
		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("Takes a screenshot without HUD. Press F12 for the same action.");
		}

		if (ImGui::Button("Cubemap", ImVec2(-1, 0)))
		{
			demo_record->make_cubemap();
		}
		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("Captures a 6-face cubemap. Press Backspace for the same action.");
		}

		if (ImGui::Button("Level Map (Low Quality)", ImVec2(-1, 0)))
		{
			demo_record->make_level_map_screenshot(false);
		}
		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("Captures a full level map screenshot at default resolution. Press F11 for the same action.");
		}

		if (ImGui::Button("Level Map (High Quality)", ImVec2(-1, 0)))
		{
			demo_record->make_level_map_screenshot(true);
		}
		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("Captures a high-quality level map screenshot split into 4 tiles. Press Ctrl+F11 for the same action.");
		}
	}

	ImGui::End();
	ImGui::PopStyleColor(1);
}
