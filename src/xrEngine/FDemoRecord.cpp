#include "stdafx.h"
#include "IGame_Level.h"
#include "x_ray.h"

#include "GameFont.h"
#include "FDemoRecord.h"

#include "CameraBase.h"
#include "XR_IOConsole.h"
#include "xr_input.h"
#include "xr_object.h"
#include "Render.h"
#include "CustomHUD.h"
#include "CameraManager.h"
#include "IGame_Actor.h"
#include "xr_level_controller.h"

extern bool g_bDisableRedText;
static Flags32	s_hud_flag	= {0};
static Flags32	s_dev_flags	= {0};

bool stored_weapon;
bool stored_cross;
bool stored_red_text;

CDemoRecord * xrDemoRecord = 0;
CDemoRecord::force_position CDemoRecord:: g_position = { false, { 0, 0, 0 } };

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

	Device.mProject.build_projection_ortho
	(
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

void GetLM_BBox(Fbox &bb, int Step)
{
	float half_x = bb.min.x + (bb.max.x - bb.min.x) / 2;
	float half_z = bb.min.z + (bb.max.z - bb.min.z) / 2;

	switch (Step)
	{
		case 0:
		{
			bb.max.x = half_x;
			bb.min.z = half_z;
		}break;
		case 1:
		{
			bb.min.x = half_x;
			bb.min.z = half_z;
		}break;
		case 2:
		{
			bb.max.x = half_x;
			bb.max.z = half_z;
		}break;
		case 3:
		{
			bb.min.x = half_x;
			bb.max.z = half_z;
		}break;
	}
};


//								+X,				-X,				+Y,				-Y,			+Z,				-Z
static Fvector cmNorm[6]	= {{0.f,1.f,0.f}, {0.f,1.f,0.f}, {0.f,0.f,-1.f},{0.f,0.f,1.f}, {0.f,1.f,0.f}, {0.f,1.f,0.f}};
static Fvector cmDir[6]		= {{1.f,0.f,0.f}, {-1.f,0.f,0.f},{0.f,1.f,0.f}, {0.f,-1.f,0.f},{0.f,0.f,1.f}, {0.f,0.f,-1.f}};

CDemoRecord::CDemoRecord(const char* name, float life_time) : CEffectorCam(cefDemo, life_time /*,false*/)
{
	stored_red_text = g_bDisableRedText;
	g_bDisableRedText = true;
	m_iLMScreenshotFragment = -1;

	m_b_redirect_input_to_level = false;
	Platform::Unlink(name);
	file = FS.w_open(name);
	if (file)
	{
		g_position.set_position = false;
		IR_Capture(); // capture input
		Camera.invert(Device.mView);

		// parse yaw
		Fvector& dir = Camera.k;
		Fvector DYaw;
		DYaw.set(dir.x, 0.f, dir.z);
		DYaw.normalize_safe();
		if (DYaw.x < 0)
		{
			HPB.x = acosf(DYaw.z);
		}
		else
		{
			HPB.x = 2 * PI - acosf(DYaw.z);
		}

		// parse pitch
		dir.normalize_safe();
		HPB.y = asinf(dir.y);
		HPB.z = 0;

		Position.set(Camera.c);

		Velocity.set(0, 0, 0);
		AngularVelocity.set(0, 0, 0);

		FrameTopDelta.set(0, 0, 0);
		FrameRightDelta.set(0, 0, 0);
		m_bMakeCubeMap = false;
		m_bMakeScreenshot = false;
		m_bMakeLevelMap = false;
		CameraTransformFactor = 5.f;
	}
	else
	{
		fLifeTime = -1;
	}
}

CDemoRecord::~CDemoRecord()
{
	if (file) 
	{
		IR_Release	();	// release input
		FS.w_close	(file);
	}
	g_bDisableRedText	= stored_red_text;

	Device.seqRender.Remove		( this		);
}

void CDemoRecord::MakeScreenshotFace()
{
	switch (Stage){
	case 0:
		s_hud_flag.assign	(psHUD_Flags);
		psHUD_Flags.assign	(0);
	break;
	case 1:
		Render->Screenshot	();
		psHUD_Flags.assign	(s_hud_flag);
		m_bMakeScreenshot= false;
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

		}break;

	case DEVICE_RESET_PRECACHE_FRAME_COUNT+60:
		{
			setup_lm_screenshot_matrices		();

			string_path					tmp;
			if(m_iLMScreenshotFragment==-1)
				xr_sprintf				(tmp, sizeof(tmp),"map_%s", *g_pGameLevel->name());
			else
				xr_sprintf				(tmp, sizeof(tmp),"map_%s#%d", *g_pGameLevel->name(), m_iLMScreenshotFragment);

			if(m_iLMScreenshotFragment!=-1)
			{
				++m_iLMScreenshotFragment;
				
				if(m_iLMScreenshotFragment!=4)
				{
					curr_lm_fbox		= get_level_screenshot_bound();
					GetLM_BBox			(curr_lm_fbox, m_iLMScreenshotFragment);
					Stage				-= 20;
				}
			}

			Render->Screenshot(IRender_interface::SM_FOR_LEVELMAP, tmp);

			if(m_iLMScreenshotFragment==-1 || m_iLMScreenshotFragment==4)
			{
				psHUD_Flags.assign(s_hud_flag);
				psDeviceFlags = s_dev_flags;

				psCurrentVidMode[0] = psOldVidMode[0];
				psCurrentVidMode[1] = psOldVidMode[1];

				Device.Reset();

				m_bMakeLevelMap = false;
				m_iLMScreenshotFragment = -1;
			}
		}break;
	default:
		{
			setup_lm_screenshot_matrices		();
		}break;
	}
	Stage++;
}

void CDemoRecord::MakeCubeMapFace(Fvector &D, Fvector &N)
{
	string32 buf;
	switch (Stage){
	case 0:
		N.set		(cmNorm[Stage]);
		D.set		(cmDir[Stage]);
		s_hud_flag.assign(psHUD_Flags);
		psHUD_Flags.assign	(0);
	break;
	case 1:
	case 2:
	case 3:
	case 4:
	case 5:
		N.set		(cmNorm[Stage]);
		D.set		(cmDir[Stage]);
		Render->Screenshot	(IRender_interface::SM_FOR_CUBEMAP,_itoa(Stage,buf,10));
	break;
	case 6:
		Render->Screenshot	(IRender_interface::SM_FOR_CUBEMAP,_itoa(Stage,buf,10));
		N.set		(Camera.j);
		D.set		(Camera.k);
		psHUD_Flags.assign(s_hud_flag);
		m_bMakeCubeMap = false;
	break;
	}
	Stage++;
}

bool CDemoRecord::ProcessCam(SCamEffectorInfo& info)
{
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
			SystemFont->OutNext("F");
			SystemFont->OutNext("BACK");
			SystemFont->OutNext("ESC");
			SystemFont->OutNext("F11");
			SystemFont->OutNext("LCONTROL+F11");
			SystemFont->OutNext("F12");
			SystemFont->SetAligment(CGameFont::alLeft);
			SystemFont->OutSetI(0, +.05f);
			SystemFont->OutNext("= Append keyframe");
			SystemFont->OutNext("= Cube Map");
			SystemFont->OutNext("= Quit");
			SystemFont->OutNext("= Level Map ScreenShot");
			SystemFont->OutNext("= Level Map ScreenShot(High Quality)");
			SystemFont->OutNext("= ScreenShot");
		}
		
		FrameTopDelta.mul(CameraTransformFactor);
		FrameTopDelta.mul(Device.fTimeDelta);
		FrameRightDelta.mul(1.f);

		HPB.x -= FrameRightDelta.y;
		HPB.y -= FrameRightDelta.x;
		HPB.z += FrameRightDelta.z;

		if (g_position.set_position)
		{
			Position.set(g_position.p);
			g_position.set_position = false;
		}
		else
		{
			g_position.p.set(Position);
		}

		Fvector CamMove;

		CamMove.set				(Camera.k);
		CamMove.normalize_safe	();
		CamMove.mul				(FrameTopDelta.z);
		Position.add			(CamMove);

		CamMove.set				(Camera.i);
		CamMove.normalize_safe	();
		CamMove.mul				(FrameTopDelta.x);
		Position.add			(CamMove);

		CamMove.set				(Camera.j);
		CamMove.normalize_safe	();
		CamMove.mul				(FrameTopDelta.y);
		Position.add			(CamMove);

		Camera.setHPB			(HPB.x,HPB.y,HPB.z);
		Camera.translate_over	(Position);

		// update camera
		info.n.set(Camera.j);
		info.d.set(Camera.k);
		info.p.set(Camera.c);

		fLifeTime -= Device.fTimeDelta;

		FrameTopDelta.set(0, 0, 0);
		FrameRightDelta.set(0, 0, 0);
	}
	return true;
}

void CDemoRecord::IR_OnKeyboardPress(int dik)
{
	if (dik == SDL_SCANCODE_0)
	{
		m_b_redirect_input_to_level = !m_b_redirect_input_to_level;
	}
	
	if (dik == SDL_SCANCODE_LCTRL)
	{
		m_bEnableAcceleration = true;
	}

	if (m_b_redirect_input_to_level)
	{
		if (IInputReceiver* ControlEntityIR = smart_cast<IInputReceiver*>(g_pGameLevel->CurrentControlEntity()))
		{
			ControlEntityIR->IR_OnKeyboardPress(dik);
		}
		return;
	}
	
	if (dik == SDL_SCANCODE_GRAVE)
	{
		Console->Show();
	}
	
	if (dik == SDL_SCANCODE_F)
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

	if (dik == SDL_SCANCODE_PAUSE)
	{
		Device.Pause(!Device.Paused(), true, true, "demo_record");
	}
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
	
	EGameActions action = get_binded_action(dik);
	Fvector TopDelta = Fvector();

	switch (action)
	{
		case kFWD:
			TopDelta.z += 1.0f;
			break;

		case kBACK:
			TopDelta.z -= 1.0f;
			break;

		case kL_STRAFE:
			TopDelta.x -= 1.0f;
			break;

		case kR_STRAFE:
			TopDelta.x += 1.0f;
			break;

		case kCROUCH:
			TopDelta.y -= 1.0f;
			break;

		case kJUMP:
			TopDelta.y += 1.0f;
			break;
	}

	FrameTopDelta.add(TopDelta);
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

	Fvector RightDelta = Fvector();
	float Sensitivity = .5f;
	
	if (IGame_Actor* IGameActor = smart_cast<IGame_Actor*>(g_pGameLevel->CurrentControlEntity()))
	{
		float fov = IGameActor->cam_Active()->f_fov;
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
	
	FrameRightDelta.add(RightDelta);
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
}

void CDemoRecord::RecordKey()
{
	Fmatrix ViewMatrix;

	ViewMatrix.invert(Camera);
	file->w(&ViewMatrix, sizeof(Fmatrix));
	
	KeyframesPositions.emplace_back(ViewMatrix.c);
}

void CDemoRecord::MakeCubemap()
{
	m_bMakeCubeMap	= true;
	Stage			= 0;
}

void CDemoRecord::MakeScreenshot()
{
	m_bMakeScreenshot = true;
	Stage = 0;
}

void CDemoRecord::MakeLevelMapScreenshot(bool bHQ)
{
//	Console->Execute("run_string level.set_weather(\"map\",true)");

	if(!bHQ)
		m_iLMScreenshotFragment = -1;
	else
		m_iLMScreenshotFragment	= 0;
	
	curr_lm_fbox		= get_level_screenshot_bound();
	GetLM_BBox			(curr_lm_fbox, m_iLMScreenshotFragment);

	m_bMakeLevelMap		= true;
	Stage				= 0;
}

void CDemoRecord::OnRender()
{
	//g_FontManager->OnRender();
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
				vT_delta.y += value.y;
			}
		}
		break;
		// Right stick
		case 1:
		{
			float scale = Device.fTimeDelta * psMouseSensScale;

			if (!fis_zero(value.x))
			{
				float d = value.x * scale * 160;
				vR_delta.y += d;
			}

			if (!fis_zero(value.y))
			{
				float d = (psGamepadInvert ? -1 : 1) * value.y * scale * 3.f / 4.f;
				d *= 160;

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
				vT_delta.z -= value.x;
			}
			// Right
			if (!fis_zero(value.y))
			{
				vT_delta.z += value.y;
			}
		}
		break;
	}
	update_whith_timescale(FrameRightDelta, vR_delta);
	update_whith_timescale(FrameTopDelta, vT_delta);
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
	}
}

void CDemoRecord::IR_OnKeyboardRelease(int dik)
{
	switch (dik)
	{
		case SDL_SCANCODE_LCTRL:
		{
			m_bEnableAcceleration = false;
			break;
		}
	}
}
