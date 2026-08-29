#include "StdAfx.h"
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

bool stored_weapon;
bool stored_cross;
bool stored_red_text;

CDemoRecord* xrDemoRecord = nullptr;
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
	Device.seqRender.Add(this, REG_PRIORITY_LOW);

	HUD().world_prims.hud_mode = false;

	bone_id = BI_NONE;
	bone_holder_kinematics = nullptr;

	stored_red_text = g_bDisableRedText;
	g_bDisableRedText = true;

	m_iLMScreenshotFragment = -1;
	m_b_redirect_input_to_level = false;

	Platform::Unlink(name);
	file = FS.w_open(name);

	if (file)
	{
		g_position.set_position = false;

		IR_Capture();
		ParseActorCam();

		Velocity.set(0.f, 0.f, 0.f);
		AngularVelocity.set(0.f, 0.f, 0.f);

		frame_pos_delta.set(0.f, 0.f, 0.f);
		frame_hpb_delta.set(0.f, 0.f, 0.f);
		m_bMakeCubeMap = false;
		m_bMakeScreenshot = false;
		m_bMakeLevelMap = false;
		CameraTransformFactor = 3.f;
		p_lap.set(0.f, 0.f, 0.f);
		lap_lock = false;

		m_fSpeed0 = pSettings->r_float("demo_record", "speed0");
		m_fSpeed1 = pSettings->r_float("demo_record", "speed1");
		m_fSpeed2 = pSettings->r_float("demo_record", "speed2");
		m_fSpeed3 = pSettings->r_float("demo_record", "speed3");

		NewInputSchema = EngineExternal()[EEngineExternalGame::NewDemoRecordInputSchema];
	}
	else
	{
		fLifeTime = -1;
	}
}

CDemoRecord::~CDemoRecord()
{
	Device.seqRender.Remove(this);

	if (file)
	{
		IR_Release();
		FS.w_close(file);
	}

	g_bDisableRedText = stored_red_text;
}

void CDemoRecord::MakeScreenshotFace()
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

void CDemoRecord::MakeLevelMapProcess()
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

void CDemoRecord::MakeCubeMapFace(Fvector& D, Fvector& N)
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
			N.set(Camera.j);
			D.set(Camera.k);
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
	
	info.dont_apply = false;

	if (nullptr == file)
	{
		return true;
	}

	if (m_bMakeScreenshot)
	{
		MakeScreenshotFace();
		// update camera
		info.n.set(Camera.j);
		info.d.set(Camera.k);
		info.p.set(Camera.c);
	}
	else if (m_bMakeLevelMap)
	{
		MakeLevelMapProcess();
		info.dont_apply = true;
	}
	else if (m_bMakeCubeMap)
	{
		MakeCubeMapFace(info.d, info.n);
		info.p.set(Camera.c);
		info.fAspect = 1.f;
	}
	else
	{
		if (NewInputSchema)
		{
			if (IR_GetKeyState(SDL_SCANCODE_F1))
			{
				CGameFont* SystemFont = g_FontManager->pFontSystem;

				SystemFont->SetColor(color_rgba(255, 0, 0, 255));
				SystemFont->SetAligment(CGameFont::alCenter);
				SystemFont->OutSetI(0, -.05f);

				SystemFont->OutNext("RECORDING");
				SystemFont->OutNext("Key frames count: %d", KeyframesPositions.size());

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
				SystemFont->OutNext("Key frames count: %d", KeyframesPositions.size());

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

		if (!NewInputSchema)
		{
			if (IR_GetKeyState(SDL_SCANCODE_LSHIFT) || IR_GetKeyState(SDL_SCANCODE_RSHIFT))
			{
				frame_pos_delta.mul(m_fSpeed0);
			}
			else if (IR_GetKeyState(SDL_SCANCODE_LALT) || IR_GetKeyState(SDL_SCANCODE_RALT))
			{
				frame_pos_delta.mul(m_fSpeed2);
			}
			else if (m_bEnableAcceleration)
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
			frame_pos_delta.mul(CameraTransformFactor);
		}

		dr_disable_time_factor_influence ? frame_pos_delta.mul(Device.fRealTimeDelta) : frame_pos_delta.mul(Device.fTimeDelta);
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

		MovePosition(frame_pos_delta);

		Level().ObjectSpace.RayPick(Camera.c, Camera.k, 1000.f, collide::rq_target::rqtBoth, rq_result, nullptr);
		view_from_bone_mode ? UpdateLookFromBone() : lap_lock ? UpdateLookAtPoint()
															  : UpdateFreeLook();

		p_cam_pos_current.inertion(p_cam_pos, dr_cam_pos_inert);
		Camera.translate_over(p_cam_pos_current);

		info.n.set(Camera.j);
		info.d.set(Camera.k);
		info.p.set(Camera.c);

		fLifeTime -= Device.fTimeDelta;

		frame_pos_delta.set(0.f, 0.f, 0.f);
		frame_hpb_delta.set(0.f, 0.f, 0.f);
	}
	return true;
}

void CDemoRecord::UpdateLookAtPoint()
{
	Fvector dir;

	if (bone_holder_kinematics != nullptr && bone_holder != nullptr && bone_id != BI_NONE)
	{
		bone_holder_kinematics->LL_GetBoneWorldPosition(bone_id, bone_holder->XFORM(), p_lap);
	}

	dir.sub(p_lap, Camera.c);

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
	Camera.setHPB(hpb_current.x, hpb_current.y, hpb_current.z);
}

void CDemoRecord::UpdateFreeLook()
{
	if (rq_result.O != nullptr)
	{
		if (IKinematics* kinematics = rq_result.O->Visual()->dcast_PKinematics(); kinematics != nullptr && draw_skeleton)
		{
			Flags32 old_flags = HUD().world_prims.m_skeleton_flags;

			HUD().world_prims.m_skeleton_flags.set(LevelInspector::ESI_BONES | LevelInspector::ESI_BONES_LINKS, TRUE);
			HUD().world_prims.DrawSkeleton(kinematics, rq_result.O->XFORM());

			HUD().world_prims.m_skeleton_flags = old_flags;
		}
	}

	hpb.x -= frame_hpb_delta.y;
	hpb.y -= frame_hpb_delta.x;
	hpb.z += frame_hpb_delta.z;

	hpb_current.inertion(hpb, dr_cam_inert);
	Camera.setHPB(hpb_current.x, hpb_current.y, hpb_current.z);
}

void CDemoRecord::UpdateLookFromBone()
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
	Camera.transform_dir(blend_pos_offset, p_cam_pos_view_from_bone_offset);

	p_cam_pos.add(blend_pos_offset);
	Camera.setHPB(hpb_current.x, hpb_current.y, hpb_current.z);
}

void CDemoRecord::ParseActorCam()
{
	Camera.invert(Device.mView);

	p_cam_pos.set(Device.vCameraPosition);
	p_cam_pos_current.set(Device.vCameraPosition);

	Fvector hpb_actor;
	Camera.getHPB(hpb_actor);
	hpb_actor.z = 0.f;

	hpb.set(hpb_actor);
	hpb_current.set(hpb_actor);
}

void CDemoRecord::MovePosition(Fvector d)
{
	Fvector new_pos;

	new_pos.set(Camera.k);
	new_pos.normalize_safe();
	new_pos.mul(d.z);
	p_cam_pos.add(new_pos);

	new_pos.set(Camera.i);
	new_pos.normalize_safe();
	new_pos.mul(d.x);
	p_cam_pos.add(new_pos);

	new_pos.set(Camera.j);
	new_pos.normalize_safe();
	new_pos.mul(d.y);
	p_cam_pos.add(new_pos);
}

void CDemoRecord::IR_OnKeyboardPress(int dik)
{
	if (dik == SDL_SCANCODE_Z && view_from_bone_mode)
	{
		hpb_view_from_bone_offset.set(zero_vel);
		p_cam_pos_view_from_bone_offset.set(zero_vel);
	}

	if (dik == SDL_SCANCODE_U && !lap_lock)
	{
		if (view_from_bone_mode && bone_id != BI_NONE && bone_holder_kinematics != nullptr)
		{
			bone_id = BI_NONE;
			bone_holder = nullptr;
			bone_holder_kinematics = nullptr;

			view_from_bone_mode = false;
		}
		else
		{
			if (rq_result.O != nullptr)
			{
				if (IRenderVisual* v = rq_result.O->Visual())
				{
					if (IKinematics* k = v->dcast_PKinematics())
					{
						bone_holder = rq_result.O;
						bone_holder_kinematics = k;
						bone_id = (u16)rq_result.element;

						view_from_bone_mode = true;
					}
				}
			}
		}
	}

	if (dik == SDL_SCANCODE_K)
	{
		draw_skeleton = !draw_skeleton;
	}

	if (dik == SDL_SCANCODE_J && !view_from_bone_mode)
	{
		if (lap_lock && p_lap != zero_vel)
		{
			Fvector cur_eulers;
			Camera.getHPB(cur_eulers);

			hpb.set(cur_eulers);
			hpb_current.set(cur_eulers);

			if (bone_id != BI_NONE)
			{
				bone_id = BI_NONE;
			}

			p_lap.set(zero_vel);
			lap_lock = false;
		}
		else
		{
			if (rq_result.range > EPS_S)
			{
				Fvector current_eulers;

				if (rq_result.O != nullptr)
				{
					if (IRenderVisual* v = rq_result.O->Visual())
					{
						if (IKinematics* k = v->dcast_PKinematics())
						{
							bone_holder = rq_result.O;
							bone_holder_kinematics = k;
							bone_id = (u16)rq_result.element;
						}
						else
						{
							p_lap.set(Camera.c.mad(Camera.k, rq_result.range));
						}
					}
				}
				else
				{
					p_lap.set(Camera.c.mad(Camera.k, rq_result.range));
				}

				Camera.getHPB(current_eulers);
				hpb.set(current_eulers);
				hpb_current.set(current_eulers);

				lap_lock = true;
			}
		}
	}

	if (dik == SDL_SCANCODE_0)
	{
		m_b_redirect_input_to_level = !m_b_redirect_input_to_level;
	}

	if (dik == SDL_SCANCODE_GRAVE)
	{
		Console->Show();
	}
	
	if (dik == SDL_SCANCODE_PAUSE)
	{
		Device.Pause(!Device.Paused(), true, true, "demo_record");
	}

	if (m_b_redirect_input_to_level)
	{
		if (IInputReceiver* ControlEntityIR = smart_cast<IInputReceiver*>(g_pGameLevel->CurrentControlEntity()))
		{
			ControlEntityIR->IR_OnKeyboardPress(dik);
		}
		return;
	}

	if (dik == SDL_SCANCODE_LCTRL || dik == SDL_SCANCODE_RCTRL)
	{
		m_bEnableAcceleration = true;
	}

	if (NewInputSchema && dik == SDL_SCANCODE_F || !NewInputSchema && dik == SDL_SCANCODE_SPACE)
	{
		RecordKey();
	}

	if (dik == SDL_SCANCODE_BACKSPACE)
	{
		MakeCubemap();
	}

	if (dik == SDL_SCANCODE_F11)
	{
		MakeLevelMapScreenshot(IR_GetKeyState(SDL_SCANCODE_LCTRL));
	}

	if (dik == SDL_SCANCODE_F12)
	{
		MakeScreenshot();
	}

	if (dik == SDL_SCANCODE_ESCAPE)
	{
		fLifeTime = -1;
	}

#ifndef MASTER_GOLD
	if (dik == SDL_SCANCODE_RETURN)
	{
		if (g_pGameLevel->CurrentEntity())
		{
			g_pGameLevel->CurrentEntity()->ForceTransform(Camera);
			fLifeTime = -1;
		}
	}
#endif
}

void CDemoRecord::IR_OnKeyboardHold(int dik)
{
	if (m_b_redirect_input_to_level)
	{
		if (IInputReceiver* ControlEntityIR = smart_cast<IInputReceiver*>(g_pGameLevel->CurrentControlEntity()))
		{
			ControlEntityIR->IR_OnKeyboardHold(dik);
		}
		return;
	}

	if (view_from_bone_mode)
	{
		float dt = dr_disable_time_factor_influence ? Device.fRealTimeDelta : Device.fTimeDelta;
		
		switch (dik)
		{
			case SDL_SCANCODE_W:
				p_cam_pos_view_from_bone_offset.z += 1.f * dt;
				break;

			case SDL_SCANCODE_A:
				p_cam_pos_view_from_bone_offset.x -= 1.f * dt;
				break;

			case SDL_SCANCODE_S:
				p_cam_pos_view_from_bone_offset.z -= 1.f * dt;
				break;

			case SDL_SCANCODE_D:
				p_cam_pos_view_from_bone_offset.x += 1.f * dt;
				break;

			case SDL_SCANCODE_Q:
				hpb_view_from_bone_offset.z -= 1.f * dt;
				break;

			case SDL_SCANCODE_E:
				hpb_view_from_bone_offset.z += 1.f * dt;
				break;
		}
	}

	if (NewInputSchema)
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
			case SDL_SCANCODE_W:
				frame_pos_delta.y += 1.0f;
				break;

			case SDL_SCANCODE_A:
				frame_pos_delta.x -= 1.0f;
				break;

			case SDL_SCANCODE_S:
				frame_pos_delta.y -= 1.0f;
				break;

			case SDL_SCANCODE_D:
				frame_pos_delta.x += 1.0f;
				break;
		}
	}
}

void CDemoRecord::IR_OnMousePress(int btn)
{
	if (m_b_redirect_input_to_level)
	{
		g_pGameLevel->IR_OnMousePress(btn);
		return;
	}
}

void CDemoRecord::IR_OnMouseMove(int dx, int dy)
{
	if (m_b_redirect_input_to_level)
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

	Fvector RightDelta = Fvector();
	float Sensitivity = .5f;

	if (IGame_Actor* IGameActor = smart_cast<IGame_Actor*>(g_pGameLevel->CurrentControlEntity()))
	{
		float fov = Actor()->cam_Active()->Fov();
		Sensitivity = fov / 67.5f * psMouseSens * psMouseSensScale / 50.0f;
	}

	if (dx)
	{
		RightDelta.y += static_cast<float>(dx) * Sensitivity;
	}

	if (dy)
	{
		RightDelta.x += (psMouseInvert ? -1 : 1) * static_cast<float>(dy) * Sensitivity * 3.0f / 4.0f;
	}

	frame_hpb_delta.add(RightDelta);
}

void CDemoRecord::IR_OnMouseRelease(int btn)
{
	if (m_b_redirect_input_to_level)
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
			CameraTransformFactor -= ModifierIncluded ? 15.f : 5.f;
			break;

		case 1:
			CameraTransformFactor += ModifierIncluded ? 15.f : 5.f;
			break;
	}
	clamp(CameraTransformFactor, 1.f, FLT_MAX);
}

void CDemoRecord::IR_OnMouseHold(int btn)
{
	if (m_b_redirect_input_to_level)
	{
		if (IInputReceiver* ControlEntityIR = smart_cast<IInputReceiver*>(g_pGameLevel->CurrentControlEntity()))
		{
			ControlEntityIR->IR_OnMouseHold(btn);
		}
	}

	Fvector Delta = Fvector();

	if (!NewInputSchema)
	{
		switch (btn)
		{
			case 0:
				Delta.z += 1.f;
				break;

			case 1:
				Delta.z -= 1.f;
				break;
		}
	}

	frame_pos_delta.add(Delta);
}

void CDemoRecord::RecordKey()
{
	Fmatrix ViewMatrix;

	ViewMatrix.invert(Camera);
	file->w(&ViewMatrix, sizeof(Fmatrix));

	KeyframesPositions.emplace_back(p_cam_pos_current);
}

void CDemoRecord::MakeCubemap()
{
	m_bMakeCubeMap = true;
	Stage = 0;
}

void CDemoRecord::MakeScreenshot()
{
	m_bMakeScreenshot = true;
	Stage = 0;
}

void CDemoRecord::MakeLevelMapScreenshot(bool bHQ)
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
	if (!KeyframesPositions.empty())
	{
		for (size_t i = 0; i < KeyframesPositions.size(); i++)
		{
			HUD().world_prims.append_sphere(KeyframesPositions.at(i), .1f, color_rgba(10, 10, 10, 255), color_rgba(255, 70, 70, 50));
			HUD().world_prims.append_text3d(KeyframesPositions.at(i), shared_str().printf("Keyframe #%d", i));
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
		case SDL_GAMEPAD_BUTTON_EAST:
		{
			fLifeTime = -1;
			break;
		}
		case SDL_GAMEPAD_BUTTON_LEFT_STICK:
		{
			m_bEnableAcceleration = !m_bEnableAcceleration;
			break;
		}
		case SDL_GAMEPAD_BUTTON_RIGHT_SHOULDER:
		{
			RecordKey();
			break;
		}
	}
}

void CDemoRecord::IR_OnKeyboardRelease(int dik)
{
	switch (dik)
	{
		case SDL_SCANCODE_LCTRL:
		case SDL_SCANCODE_RCTRL:
		{
			m_bEnableAcceleration = false;
			break;
		}
	}
}
