#include "stdafx.h"
#include "FDemoRecord.h"

#include "Actor.h"
#include "HUDManager.h"
#include "../xrEngine/CameraBase.h"
#include "../xrEngine/Effector.h"
#include "../xrEngine/xr_level_controller.h"
#include "../xrEngine/IGame_Actor.h"
#include "../xrEngine/xr_collide_form.h"
#include "../xrEngine/xr_input.h"
#include "../xrEngine/xr_ioc_cmd.h"

extern ENGINE_API bool g_bDisableRedText;
static Flags32 s_hud_flag = {0};
static Flags32 s_dev_flags = {0};

CDemoRecord* demo_record = nullptr;
CDemoRecord::force_position CDemoRecord::g_position = {false, {0, 0, 0}};

float dr_cam_inert = 0.f;
float dr_cam_pos_inert = 0.f;
bool dr_disable_time_factor_influence = false;

Fbox curr_lm_fbox;

void setup_lm_screenshot_matrices()
{
	psHUD_Flags.assign(0);

	Fbox bb = curr_lm_fbox;
	bb.getcenter(Device.vCameraPosition);

	Device.vCameraDirection.set(0.f, -1.f, 0.f);
	Device.vCameraRight.set(1.f, 0.f, 0.f);
	Device.vCameraTop.set(0.f, 0.f, 1.f);

	Device.mView.build_camera_dir(Device.vCameraPosition, Device.vCameraDirection, Device.vCameraTop);

	bb.xform(Device.mView);

	Device.mProject.build_projection_ortho(
		bb.max.x - bb.min.x,
		bb.max.y - bb.min.y,
		bb.min.z,
		bb.max.z
	);

	Device.mProject_saved = Device.mProject;
	Device.mProject_old = Device.mProject;
	Device.mView_saved = Device.mView;
	Device.mView_old = Device.mView;
}

static void update_whith_timescale(Fvector& v, const Fvector& v_delta)
{
	float scale = 1.f / Device.time_factor();
	v.mad(v, v_delta, scale);
}

Fbox get_level_screenshot_bound()
{
	Fbox res = g_pGameLevel->ObjectSpace.GetBoundingVolume();

	if (g_pGameLevel->pLevel->section_exist("level_map"))
	{
		Fvector4 res2d = g_pGameLevel->pLevel->r_fvector4("level_map", "bound_rect");

		res.min.x = res2d.x;
		res.min.z = res2d.y;

		res.max.x = res2d.z;
		res.max.z = res2d.w;
	}

	return res;
}

void GetLM_BBox(Fbox& bb, int Step)
{
	float half_x = bb.min.x + (bb.max.x - bb.min.x) / 2;
	float half_z = bb.min.z + (bb.max.z - bb.min.z) / 2;

	switch (Step)
	{
		case 0:
		{
			bb.max.x = half_x;
			bb.min.z = half_z;
		}
		break;
		case 1:
		{
			bb.min.x = half_x;
			bb.min.z = half_z;
		}
		break;
		case 2:
		{
			bb.max.x = half_x;
			bb.max.z = half_z;
		}
		break;
		case 3:
		{
			bb.min.x = half_x;
			bb.max.z = half_z;
		}
		break;
	}
};


//								+X,				-X,				+Y,				-Y,			+Z,				-Z
static Fvector cmNorm[6] = {{0.f, 1.f, 0.f}, {0.f, 1.f, 0.f}, {0.f, 0.f, -1.f}, {0.f, 0.f, 1.f}, {0.f, 1.f, 0.f}, {0.f, 1.f, 0.f}};
static Fvector cmDir[6] = {{1.f, 0.f, 0.f}, {-1.f, 0.f, 0.f}, {0.f, 1.f, 0.f}, {0.f, -1.f, 0.f}, {0.f, 0.f, 1.f}, {0.f, 0.f, -1.f}};

CDemoRecord::CDemoRecord(const char* name, float life_time) : CEffectorCam(cefDemo, life_time)
{
	demo_record = this;

	Device.seqRender.Add(this, REG_PRIORITY_LOW);

	HUD().world_prims.hud_mode = false;

	stored_fov = g_base_fov;

	bone_id = BI_NONE;
	bone_holder_kinematics = nullptr;

	g_bDisableRedText = true;

	m_iLMScreenshotFragment = -1;
	redirect_input_to_level = false;

	Platform::Unlink(name);
	file = FS.w_open(name);

	if (file)
	{
		g_position.set_position = false;

		IR_Capture();
		parse_actor_cam();

		frame_pos_delta.set(0.f, 0.f, 0.f);
		frame_hpb_delta.set(0.f, 0.f, 0.f);
		m_bMakeCubeMap = false;
		m_bMakeScreenshot = false;
		m_bMakeLevelMap = false;
		camera_transform_speed = 3.f;
		look_at_point.set(0.f, 0.f, 0.f);
		look_at_point_mode = false;

		m_fSpeed0 = pSettings->r_float("demo_record", "speed0");
		m_fSpeed1 = pSettings->r_float("demo_record", "speed1");
		m_fSpeed2 = pSettings->r_float("demo_record", "speed2");
		m_fSpeed3 = pSettings->r_float("demo_record", "speed3");

		stored_camera_transform_speed = camera_transform_speed;
		stored_fSpeed0 = m_fSpeed0;
		stored_fSpeed1 = m_fSpeed1;
		stored_fSpeed2 = m_fSpeed2;
		stored_fSpeed3 = m_fSpeed3;
		stored_fov_scale_speed = fov_scale_speed;

		new_input_schema = EngineExternal()[EEngineExternalGame::NewDemoRecordInputSchema];
	}
	else
	{
		fLifeTime = -1;
	}
}

CDemoRecord::~CDemoRecord()
{
	demo_record = nullptr;
	Device.seqRender.Remove(this);

	if (file)
	{
		IR_Release();
		FS.w_close(file);
	}

	g_base_fov = stored_fov;
}

void CDemoRecord::make_screenshot_face()
{
	switch (Stage)
	{
		case 0:
			s_hud_flag.assign(psHUD_Flags);
			psHUD_Flags.assign(0);
			break;
		case 1:
			Render->Screenshot();
			psHUD_Flags.assign(s_hud_flag);
			m_bMakeScreenshot = false;
			break;
	}
	Stage++;
}

void CDemoRecord::make_level_map_process()
{
	static float psOldVidMode[2];

	switch (Stage)
	{
		case 0:
		{
			s_dev_flags = psDeviceFlags;
			s_hud_flag.assign(psHUD_Flags);

			psDeviceFlags.zero();
			psDeviceFlags.set(rsFullscreen, false);
			psDeviceFlags.set(rsClearBB | rsDrawStatic, true);

			psOldVidMode[0] = psCurrentVidMode[0];
			psOldVidMode[1] = psCurrentVidMode[1];

			psCurrentVidMode[0] = psCurrentVidMode[1] = 4096.0f;

			Device.Reset();
		}
		break;

		case DEVICE_RESET_PRECACHE_FRAME_COUNT + 60:
		{
			setup_lm_screenshot_matrices();

			string_path tmp;
			if (m_iLMScreenshotFragment == -1)
			{
				xr_sprintf(tmp, sizeof(tmp), "map_%s", *g_pGameLevel->name());
			}
			else
			{
				xr_sprintf(tmp, sizeof(tmp), "map_%s#%d", *g_pGameLevel->name(), m_iLMScreenshotFragment);
			}

			if (m_iLMScreenshotFragment != -1)
			{
				++m_iLMScreenshotFragment;

				if (m_iLMScreenshotFragment != 4)
				{
					curr_lm_fbox = get_level_screenshot_bound();
					GetLM_BBox(curr_lm_fbox, m_iLMScreenshotFragment);
					Stage -= 20;
				}
			}

			Render->Screenshot(IRender_interface::SM_FOR_LEVELMAP, tmp);

			if (m_iLMScreenshotFragment == -1 || m_iLMScreenshotFragment == 4)
			{
				psHUD_Flags.assign(s_hud_flag);
				psDeviceFlags = s_dev_flags;

				psCurrentVidMode[0] = psOldVidMode[0];
				psCurrentVidMode[1] = psOldVidMode[1];

				Device.Reset();

				m_bMakeLevelMap = false;
				m_iLMScreenshotFragment = -1;
			}
		}
		break;
		default:
		{
			setup_lm_screenshot_matrices();
		}
		break;
	}
	Stage++;
}

void CDemoRecord::make_cube_map_face(Fvector& D, Fvector& N)
{
	string32 buf;
	switch (Stage)
	{
		case 0:
			N.set(cmNorm[Stage]);
			D.set(cmDir[Stage]);
			s_hud_flag.assign(psHUD_Flags);
			psHUD_Flags.assign(0);
			break;
		case 1:
		case 2:
		case 3:
		case 4:
		case 5:
			N.set(cmNorm[Stage]);
			D.set(cmDir[Stage]);
			Render->Screenshot(IRender_interface::SM_FOR_CUBEMAP, _itoa(Stage, buf, 10));
			break;
		case 6:
			Render->Screenshot(IRender_interface::SM_FOR_CUBEMAP, _itoa(Stage, buf, 10));
			N.set(camera.j);
			D.set(camera.k);
			psHUD_Flags.assign(s_hud_flag);
			m_bMakeCubeMap = false;
			break;
	}
	Stage++;
}

bool CDemoRecord::ProcessCam(SCamEffectorInfo& info)
{
	if (CObject* cce = Level().CurrentControlEntity())
	{
		if (CEntityAlive* ea = smart_cast<CEntityAlive*>(cce); ea != nullptr && !ea->g_Alive())
		{
			fLifeTime = -1;
		}
	}

	dt = dr_disable_time_factor_influence ? Device.fRealTimeDelta : Device.fTimeDelta;

	info.dont_apply = false;

	if (nullptr == file)
	{
		return true;
	}

	if (m_bMakeScreenshot)
	{
		make_screenshot_face();
		// update camera
		info.n.set(camera.j);
		info.d.set(camera.k);
		info.p.set(camera.c);
	}
	else if (m_bMakeLevelMap)
	{
		make_level_map_process();
		info.dont_apply = true;
	}
	else if (m_bMakeCubeMap)
	{
		make_cube_map_face(info.d, info.n);
		info.p.set(camera.c);
		info.fAspect = 1.f;
	}
	else
	{
		if (new_input_schema)
		{
			if (IR_GetKeyState(SDL_SCANCODE_F1))
			{
				CGameFont* SystemFont = g_FontManager->pFontSystem;

				SystemFont->SetColor(color_rgba(255, 0, 0, 255));
				SystemFont->SetAligment(CGameFont::alCenter);
				SystemFont->OutSetI(0, -.05f);

				SystemFont->OutNext("RECORDING");
				SystemFont->OutNext("Key frames count: %d", keyframes.size());

				SystemFont->SetAligment(CGameFont::alLeft);
				SystemFont->OutSetI(-0.2f, +.05f);

				SystemFont->OutNext("%s%s%s%s", dik_to_keyname(get_action_dik(kFWD)), dik_to_keyname(get_action_dik(kBACK)), dik_to_keyname(get_action_dik(kL_STRAFE)), dik_to_keyname(get_action_dik(kR_STRAFE)));
				SystemFont->OutNext("%s", dik_to_keyname(get_action_dik(kCROUCH)));
				SystemFont->OutNext("%s", dik_to_keyname(get_action_dik(kJUMP)));
				SystemFont->OutNext("MWUP");
				SystemFont->OutNext("MWDOWN");
				SystemFont->OutNext("SHIFT + MW");
				SystemFont->OutNext("F");
				SystemFont->OutNext("ESC");
				SystemFont->OutNext("F11");
				SystemFont->OutNext("Left Ctrl + F11");
				SystemFont->OutNext("F12");
				SystemFont->OutNext("Left Ctrl + F12");
				SystemFont->OutNext("J");
				SystemFont->OutNext(" ");
				SystemFont->OutNext("K");
				SystemFont->OutNext("U");
				SystemFont->SetAligment(CGameFont::alLeft);
				SystemFont->OutSetI(0, +.05f);
				SystemFont->OutNext("= Forward, Backward, Left, Right");
				SystemFont->OutNext("= Move down");
				SystemFont->OutNext("= Move up");
				SystemFont->OutNext("= Increase camera speed");
				SystemFont->OutNext("= Decrease camera speed");
				SystemFont->OutNext("= Decrease camera speed by 3 times");
				SystemFont->OutNext("= Append keyframe");
				SystemFont->OutNext("= Quit");
				SystemFont->OutNext("= Cube Map");
				SystemFont->OutNext("= Level Map Screenshot");
				SystemFont->OutNext("= Level Map Screenshot (High Quality)");
				SystemFont->OutNext("= ScreenShot");
				SystemFont->OutNext("= Lock camera look-at to a point in space or to a model bone — ");
				SystemFont->OutNext("  the camera is always forced to face the selected target.");
				SystemFont->OutNext("= Toggle model skeleton rendering");
				SystemFont->OutNext("= Attach camera to a model bone");
			}
		}
		else
		{
			if (IR_GetKeyState(SDL_SCANCODE_F1))
			{
				CGameFont* SystemFont = g_FontManager->pFontSystem;

				SystemFont->SetColor(color_rgba(255, 0, 0, 255));
				SystemFont->SetAligment(CGameFont::alCenter);
				SystemFont->OutSetI(0, -.05f);

				SystemFont->OutNext("RECORDING");
				SystemFont->OutNext("Key frames count: %d", keyframes.size());

				SystemFont->SetAligment(CGameFont::alLeft);
				SystemFont->OutSetI(-0.2f, +.05f);
				SystemFont->OutNext("SPACE");
				SystemFont->OutNext("BACK");
				SystemFont->OutNext("ESC");
				SystemFont->OutNext("F11");
				SystemFont->OutNext("Left Ctrl + F11");
				SystemFont->OutNext("F12");
				SystemFont->OutNext("J");
				SystemFont->OutNext(" ");
				SystemFont->OutNext("K");
				SystemFont->OutNext("U");
				SystemFont->SetAligment(CGameFont::alLeft);
				SystemFont->OutSetI(0, +.05f);
				SystemFont->OutNext("= Append keyframe");
				SystemFont->OutNext("= Cube Map");
				SystemFont->OutNext("= Quit");
				SystemFont->OutNext("= Level Map Screenshot");
				SystemFont->OutNext("= Level Map Screenshot (HQ)");
				SystemFont->OutNext("= ScreenShot");
				SystemFont->OutNext("= Lock camera look-at to a point in space or to a model bone — ");
				SystemFont->OutNext("  the camera is always forced to face the selected target.");
				SystemFont->OutNext("= Toggle model skeleton rendering");
				SystemFont->OutNext("= Attach camera to a model bone");
			}
		}

		if (!new_input_schema)
		{
			if (IR_GetKeyState(SDL_SCANCODE_LSHIFT) || IR_GetKeyState(SDL_SCANCODE_RSHIFT))
			{
				frame_pos_delta.mul(m_fSpeed0);
			}
			else if (IR_GetKeyState(SDL_SCANCODE_LALT) || IR_GetKeyState(SDL_SCANCODE_RALT))
			{
				frame_pos_delta.mul(m_fSpeed2);
			}
			else if (enable_acceleration)
			{
				frame_pos_delta.mul(m_fSpeed3);
			}
			else
			{
				frame_pos_delta.mul(10.f);
			}
		}
		else
		{
			frame_pos_delta.mul(camera_transform_speed);
		}

		float pos_dt_magnitude = frame_pos_delta.magnitude();
		float scaled_fov = fov_auto_scale
							   ? (pos_dt_magnitude > EPS_S ? pos_dt_magnitude : 5.f) * dt
							   : fov_scale_speed * dt;

		if (pInput->iGetAsyncKeyState(SDL_SCANCODE_R))
		{
			clamp(g_base_fov += scaled_fov, 5.f, 179.f);
		}

		if (pInput->iGetAsyncKeyState(SDL_SCANCODE_T))
		{
			clamp(g_base_fov -= scaled_fov, 5.f, 179.f);
		}

		frame_pos_delta.mul(dt);
		frame_hpb_delta.mul(1.f);

		if (g_position.set_position)
		{
			p_cam_pos.set(g_position.p);
			g_position.set_position = false;
		}
		else
		{
			g_position.p.set(p_cam_pos);
		}

		Level().ObjectSpace.RayPick(camera.c, camera.k, 1000.f, collide::rq_target::rqtBoth, rq_result, nullptr);
		view_from_bone_mode ? update_look_from_bone() : look_at_point_mode ? update_look_at_point()
																		   : update_free_look();

		Fvector new_pos;
		camera.transform_dir(new_pos, frame_pos_delta);
		p_cam_pos.add(new_pos);

		p_cam_pos_current.inertion(p_cam_pos, dr_cam_pos_inert);
		camera.translate_over(p_cam_pos_current);

		info.n.set(camera.j);
		info.d.set(camera.k);
		info.p.set(camera.c);

		fLifeTime -= Device.fTimeDelta;

		frame_pos_delta.set(0.f, 0.f, 0.f);
		frame_hpb_delta.set(0.f, 0.f, 0.f);
	}
	return true;
}

void CDemoRecord::update_look_at_point()
{
	Fvector dir;

	if (bone_holder_kinematics != nullptr && bone_holder != nullptr && bone_id != BI_NONE)
	{
		bone_holder_kinematics->LL_GetBoneWorldPosition(bone_id, bone_holder->XFORM(), look_at_point);
	}

	dir.sub(look_at_point, camera.c);

	Fmatrix basis;
	basis.identity();
	basis.k.normalize_safe(dir);
	Fvector::generate_orthonormal_basis(basis.k, basis.j, basis.i);

	Fvector target_eulers;
	basis.getHPB(target_eulers);

	target_eulers = {
		hpb_current.x + angle_difference_signed(target_eulers.x, hpb_current.x),
		hpb_current.y + angle_difference_signed(target_eulers.y, hpb_current.y),
		0.f
	};

	hpb_current.inertion(target_eulers, dr_cam_inert);
	camera.setHPB(hpb_current.x, hpb_current.y, hpb_current.z);
}

void CDemoRecord::update_free_look()
{
	if (!rq_result.IsStatic())
	{
		if (IKinematics* kinematics = rq_result.GetDynamic()->Visual()->dcast_PKinematics(); kinematics != nullptr && draw_skeleton)
		{
			Flags32 old_flags = HUD().world_prims.m_skeleton_flags;

			HUD().world_prims.m_skeleton_flags.set(LevelInspector::ESI_BONES | LevelInspector::ESI_BONES_LINKS, TRUE);
			HUD().world_prims.DrawSkeleton(kinematics, rq_result.GetDynamic()->XFORM());

			HUD().world_prims.m_skeleton_flags = old_flags;
		}
	}

	hpb.x -= frame_hpb_delta.y;
	hpb.y -= frame_hpb_delta.x;
	hpb.z += frame_hpb_delta.z;

	hpb_current.inertion(hpb, dr_cam_inert);
	camera.setHPB(hpb_current.x, hpb_current.y, hpb_current.z);
}

void CDemoRecord::update_look_from_bone()
{
	Fvector bone_world_pos;
	bone_holder_kinematics->LL_GetBoneWorldPosition(bone_id, bone_holder->XFORM(), bone_world_pos);

	Fmatrix bone_world_xfrom;
	bone_holder_kinematics->LL_GetBoneWorldTransform(bone_id, bone_holder->XFORM(), bone_world_xfrom);

	Fvector bone_world_hpb;
	bone_world_xfrom.getHPB(bone_world_hpb);

	Fvector r, n, d;

	d = bone_world_xfrom.k;
	r.crossproduct(d, {0.f, 1.f, 0.f}).normalize_safe();
	n.crossproduct(r, d).normalize_safe();

	Fmatrix restored_basis;
	restored_basis.i = r;
	restored_basis.j = n;
	restored_basis.k = d;

	restored_basis.getHPB(bone_world_hpb);

	Fvector blend_view_offset = {
		bone_world_hpb.x + -hpb_view_from_bone_offset.x,
		bone_world_hpb.y + -hpb_view_from_bone_offset.y,
		bone_world_hpb.z + hpb_view_from_bone_offset.z,
	};

	Fvector target_eulers = {
		hpb_current.x + angle_difference_signed(blend_view_offset.x, hpb_current.x),
		hpb_current.y + angle_difference_signed(blend_view_offset.y, hpb_current.y),
		hpb_current.z + angle_difference_signed(blend_view_offset.z, hpb_current.z),
	};

	hpb_current.inertion(target_eulers, dr_cam_inert);
	p_cam_pos.set(bone_world_pos);

	Fvector blend_pos_offset;
	camera.transform_dir(blend_pos_offset, p_cam_pos_view_from_bone_offset);

	p_cam_pos.add(blend_pos_offset);
	camera.setHPB(hpb_current.x, hpb_current.y, hpb_current.z);
}

void CDemoRecord::parse_actor_cam()
{
	camera.invert(Device.mView);

	p_cam_pos.set(Device.vCameraPosition);
	p_cam_pos_current.set(Device.vCameraPosition);

	Fvector hpb_actor;
	camera.getHPB(hpb_actor);
	hpb_actor.z = 0.f;

	hpb.set(hpb_actor);
	hpb_current.set(hpb_actor);
}

bool CDemoRecord::try_attach_bone()
{
	if (rq_result.O == nullptr)
	{
		return false;
	}

	if (IRenderVisual* v = rq_result.O->Visual())
	{
		if (IKinematics* k = v->dcast_PKinematics())
		{
			bone_holder = rq_result.O;
			bone_holder_kinematics = k;
			bone_id = (u16)rq_result.element;
			view_from_bone_mode = true;
			look_at_point_mode = false;
			return true;
		}
	}
	return false;
}

void CDemoRecord::detach_bone()
{
	bone_id = BI_NONE;
	bone_holder = nullptr;
	bone_holder_kinematics = nullptr;
	view_from_bone_mode = false;

	Fvector cur_eulers;
	camera.getHPB(cur_eulers);
	hpb.set(cur_eulers);
	hpb_current.set(cur_eulers);
}

void CDemoRecord::IR_OnKeyboardPress(int dik)
{
	if (view_from_bone_mode)
	{
		switch (dik)
		{
			case K_RESET_FOV:
			{
				hpb_view_from_bone_offset.set(zero_vel);
				p_cam_pos_view_from_bone_offset.set(zero_vel);
			}
			break;
		}
	}
	else
	{
		switch (dik)
		{
			case K_RESET_FOV:
			{
				g_base_fov = stored_fov;
			}
			break;
		}
	}

	switch (dik)
	{
		case K_TOGGLE_BONE_ATTACH:
			if (!look_at_point_mode)
			{
				if (view_from_bone_mode)
				{
					detach_bone();
				}
				else
				{
					try_attach_bone();
				}
			}
			break;

		case K_TOGGLE_SKELETON:
			draw_skeleton = !draw_skeleton;
			break;

		case K_TOGGLE_LOOKAT_LOCK:
			if (!view_from_bone_mode)
			{
				if (look_at_point_mode && look_at_point != zero_vel)
				{
					Fvector cur_eulers;
					camera.getHPB(cur_eulers);

					hpb.set(cur_eulers);
					hpb_current.set(cur_eulers);

					if (bone_id != BI_NONE)
					{
						bone_id = BI_NONE;
					}

					look_at_point.set(zero_vel);
					look_at_point_mode = false;
				}
				else
				{
					if (rq_result.range > EPS_S)
					{
						Fvector current_eulers;

						if (!rq_result.IsStatic())
						{
							if (IRenderVisual* v = rq_result.GetDynamic()->Visual())
							{
								if (IKinematics* k = v->dcast_PKinematics())
								{
									bone_holder = const_cast<CObject*>(rq_result.GetDynamic());
									bone_holder_kinematics = k;
									bone_id = (u16)rq_result.element;
								}
								else
								{
									look_at_point.set(camera.c.mad(camera.k, rq_result.range));
								}
							}
						}
						else
						{
							look_at_point.set(camera.c.mad(camera.k, rq_result.range));
						}

						camera.getHPB(current_eulers);
						hpb.set(current_eulers);
						hpb_current.set(current_eulers);

						look_at_point_mode = true;
					}
				}
			}
			break;

		case K_TOGGLE_REDIRECT_INPUT:
			redirect_input_to_level = !redirect_input_to_level;
			break;

		case K_SHOW_CONSOLE:
			Console->Show();
			break;

		case K_TOGGLE_PAUSE:
			Device.Pause(!Device.Paused(), true, true, "demo_record");
			break;

		case K_ENABLE_ACCELERATION:
		case K_ENABLE_ACCELERATION_R:
			enable_acceleration = true;
			break;

		case K_RECORD_KEYFRAME:
			if (new_input_schema)
			{
				record_keyframe();
			}
			break;

		case K_RECORD_KEYFRAME_OLD:
			if (!new_input_schema)
			{
				record_keyframe();
			}
			break;

		case K_MAKE_CUBEMAP:
			make_cubemap();
			break;

		case K_MAKE_LEVELMAP:
			make_level_map_screenshot(IR_GetKeyState(SDL_SCANCODE_LCTRL));
			break;

		case K_MAKE_SCREENSHOT:
			make_screenshot();
			break;

		case K_QUIT:
			fLifeTime = -1;
			break;

#ifndef MASTER_GOLD
		case K_FORCE_TRANSFORM:
			if (g_pGameLevel->CurrentEntity())
			{
				g_pGameLevel->CurrentEntity()->ForceTransform(camera);
				fLifeTime = -1;
			}
			break;
#endif
	}

	if (redirect_input_to_level)
	{
		if (IInputReceiver* ControlEntityIR = smart_cast<IInputReceiver*>(g_pGameLevel->CurrentControlEntity()))
		{
			ControlEntityIR->IR_OnKeyboardPress(dik);
		}
		return;
	}
}

void CDemoRecord::IR_OnKeyboardHold(int dik)
{
	if (redirect_input_to_level)
	{
		if (IInputReceiver* ControlEntityIR = smart_cast<IInputReceiver*>(g_pGameLevel->CurrentControlEntity()))
		{
			ControlEntityIR->IR_OnKeyboardHold(dik);
		}
		return;
	}

	float roll_angle_per_second = CCC_Float::FastCommand("roll_angle_per_second", 1.f);

	if (view_from_bone_mode)
	{
		switch (dik)
		{
			case K_MOVE_FORWARD:
				p_cam_pos_view_from_bone_offset.z += 1.f * dt;
				break;

			case K_MOVE_LEFT:
				p_cam_pos_view_from_bone_offset.x -= 1.f * dt;
				break;

			case K_MOVE_BACKWARD:
				p_cam_pos_view_from_bone_offset.z -= 1.f * dt;
				break;

			case K_MOVE_RIGHT:
				p_cam_pos_view_from_bone_offset.x += 1.f * dt;
				break;

			case K_ROLL_LEFT:
				hpb_view_from_bone_offset.z -= 1.f * dt;
				break;

			case K_ROLL_RIGHT:
				hpb_view_from_bone_offset.z += 1.f * dt;
				break;
		}
	}
	else
	{
		switch (dik)
		{
			case K_ROLL_LEFT:
				frame_hpb_delta.z -= roll_angle_per_second * dt;
				break;

			case K_ROLL_RIGHT:
				frame_hpb_delta.z += roll_angle_per_second * dt;
				break;
		}
	}

	if (new_input_schema)
	{
		switch (get_binded_action(dik))
		{
			case kFWD:
				frame_pos_delta.z += 1.0f;
				break;

			case kBACK:
				frame_pos_delta.z -= 1.0f;
				break;

			case kL_STRAFE:
				frame_pos_delta.x -= 1.0f;
				break;

			case kR_STRAFE:
				frame_pos_delta.x += 1.0f;
				break;

			case kCROUCH:
				frame_pos_delta.y -= 1.0f;
				break;

			case kJUMP:
				frame_pos_delta.y += 1.0f;
				break;
		}
	}
	else
	{
		switch (dik)
		{
			case K_MOVE_FORWARD:
				frame_pos_delta.y += 1.0f;
				break;

			case K_MOVE_LEFT:
				frame_pos_delta.x -= 1.0f;
				break;

			case K_MOVE_BACKWARD:
				frame_pos_delta.y -= 1.0f;
				break;

			case K_MOVE_RIGHT:
				frame_pos_delta.x += 1.0f;
				break;
		}
	}
}

void CDemoRecord::IR_OnMousePress(int btn)
{
	if (redirect_input_to_level)
	{
		g_pGameLevel->IR_OnMousePress(btn);
		return;
	}
}

void CDemoRecord::IR_OnMouseMove(int dx, int dy)
{
	if (redirect_input_to_level)
	{
		if (IInputReceiver* ControlEntityIR = smart_cast<IInputReceiver*>(g_pGameLevel->CurrentControlEntity()))
		{
			ControlEntityIR->IR_OnMouseMove(dx, dy);
		}
		return;
	}

	if (view_from_bone_mode && IR_GetKeyState(SDL_SCANCODE_LSHIFT))
	{
		float d_scale = Actor()->cam_Active()->f_fov / g_fov * psMouseSens * psMouseSensScale / 50.f;

		if (dx)
		{
			float d = static_cast<float>(dx) * d_scale;
			hpb_view_from_bone_offset.x += d < 0.f ? -std::abs(d) : std::abs(d);
		}

		if (dy)
		{
			float d = (psMouseInvert ? -1.f : 1.f) * static_cast<float>(dy) * d_scale * (3.f / 4.f);
			hpb_view_from_bone_offset.y += d > 0.f ? std::abs(d) : -std::abs(d);
		}
	}

	float ensitivity = .5f;

	if (IGame_Actor* IGameActor = smart_cast<IGame_Actor*>(g_pGameLevel->CurrentControlEntity()))
	{
		float fov = IGameActor->cam_Active()->Fov();
		ensitivity = fov / 67.5f * psMouseSens * psMouseSensScale / 50.0f;
	}

	if (dx)
	{
		frame_hpb_delta.y += static_cast<float>(dx) * ensitivity;
	}

	if (dy)
	{
		frame_hpb_delta.x += (psMouseInvert ? -1 : 1) * static_cast<float>(dy) * ensitivity * 3.0f / 4.0f;
	}
}

void CDemoRecord::IR_OnMouseRelease(int btn)
{
	if (redirect_input_to_level)
	{
		g_pGameLevel->IR_OnMouseRelease(btn);
		return;
	}
}

void CDemoRecord::IR_OnMouseWheel(int direction)
{
	bool ModifierIncluded = pInput->iGetAsyncKeyState(SDL_SCANCODE_LSHIFT) || pInput->iGetAsyncKeyState(SDL_SCANCODE_RSHIFT);

	switch (direction)
	{
		case -1:
			camera_transform_speed -= ModifierIncluded ? 15.f : 5.f;
			break;

		case 1:
			camera_transform_speed += ModifierIncluded ? 15.f : 5.f;
			break;
	}
	clamp(camera_transform_speed, 1.f, FLT_MAX);
}

void CDemoRecord::IR_OnMouseHold(int btn)
{
	if (redirect_input_to_level)
	{
		if (IInputReceiver* ControlEntityIR = smart_cast<IInputReceiver*>(g_pGameLevel->CurrentControlEntity()))
		{
			ControlEntityIR->IR_OnMouseHold(btn);
		}
	}

	if (!new_input_schema)
	{
		switch (btn)
		{
			case M_MOVE_FORWARD:
				frame_pos_delta.z += 1.f;
				break;

			case M_MOVE_BACKWARD:
				frame_pos_delta.z -= 1.f;
				break;
		}
	}
}

void CDemoRecord::record_keyframe()
{
	Fmatrix ViewMatrix;

	ViewMatrix.invert(camera);
	file->w(&ViewMatrix, sizeof(Fmatrix));

	keyframes.emplace_back(p_cam_pos_current);
}

void CDemoRecord::make_cubemap()
{
	m_bMakeCubeMap = true;
	Stage = 0;
}

void CDemoRecord::make_screenshot()
{
	m_bMakeScreenshot = true;
	Stage = 0;
}

void CDemoRecord::make_level_map_screenshot(bool bHQ)
{
	//	Console->Execute("run_string level.set_weather(\"map\",true)");

	if (!bHQ)
	{
		m_iLMScreenshotFragment = -1;
	}
	else
	{
		m_iLMScreenshotFragment = 0;
	}

	curr_lm_fbox = get_level_screenshot_bound();
	GetLM_BBox(curr_lm_fbox, m_iLMScreenshotFragment);

	m_bMakeLevelMap = true;
	Stage = 0;
}

void CDemoRecord::OnRender()
{
	if (!keyframes.empty())
	{
		for (size_t i = 0; i < keyframes.size(); i++)
		{
			HUD().world_prims.append_sphere(keyframes.at(i), .1f, color_rgba(10, 10, 10, 255), color_rgba(255, 70, 70, 50));
			HUD().world_prims.append_text3d(keyframes.at(i), shared_str().printf("Keyframe #%d", i));
		}
	}
}

void CDemoRecord::IR_GamepadUpdateStick(int id, Fvector2 value)
{
	Fvector vR_delta = Fvector().set(0, 0, 0);
	Fvector vT_delta = Fvector().set(0, 0, 0);
	// Left stick
	switch (id)
	{
		case 0:
		{
			if (!fis_zero(value.x))
			{
				vT_delta.x += value.x;
			}

			if (!fis_zero(value.y))
			{
				vT_delta.z += value.y;
			}
		}
		break;
		// Right stick
		case 1:
		{
			float scale = Device.fTimeDelta * psGamepadSens * psMouseSensScale;

			if (!fis_zero(value.x))
			{
				float d = value.x * scale * 8;
				vR_delta.y += d;
			}

			if (!fis_zero(value.y))
			{
				float d = (psGamepadInvert ? -1 : 1) * value.y * scale * 3.f / 4.f;
				d *= 8;

				vR_delta.x += d;
			}
		}
		break;
		// Triggers
		case 2:
		{
			// Left
			if (!fis_zero(value.x))
			{
				vT_delta.y -= value.x;
			}
			// Right
			if (!fis_zero(value.y))
			{
				vT_delta.y += value.y;
			}
		}
		break;
	}
	update_whith_timescale(frame_hpb_delta, vR_delta);
	update_whith_timescale(frame_pos_delta, vT_delta);
}

void CDemoRecord::IR_GamepadKeyPress(int id)
{
	switch (id)
	{
		case GP_QUIT:
		{
			fLifeTime = -1;
			break;
		}

		case GP_TOGGLE_ACCELERATION:
		{
			enable_acceleration = !enable_acceleration;
			break;
		}

		case GP_RECORD_KEYFRAME:
		{
			record_keyframe();
			break;
		}
	}
}

void CDemoRecord::IR_OnKeyboardRelease(int dik)
{
	switch (dik)
	{
		case K_ENABLE_ACCELERATION:
		case K_ENABLE_ACCELERATION_R:
		{
			enable_acceleration = false;
			break;
		}
	}
}
