#include "StdAfx.h"
#include "script_game_object.h"
#include "HudItem.h"
#include "physic_item.h"
#include "Actor.h"
#include "ActorEffector.h"
#include "Missile.h"
#include "xrMessages.h"
#include "Level.h"
#include "Inventory.h"
#include "../xrEngine/CameraBase.h"
#include "player_hud.h"
#include "../xrEngine/SkeletonMotions.h"
#include "script_game_object.h"
#include "../../xrUI/ui_base.h"
#include "HUDManager.h"
#include "Torch.h"
#include "Weapon.h"
#include "WeaponBinoculars.h"
#include "WeaponKnife.h"
#include "HUDAnimItem.h"
#include "ActorEffector.h"

ENGINE_API extern float psHUD_FOV_def;

CHudItem::CHudItem()
{
	RenderHud					(TRUE);
	m_bStopAtEndAnimIsRunning	= false;
	m_current_motion_def		= nullptr;
	m_started_rnd_anim_idx		= u8(-1);
	m_nearwall_last_hud_fov		= psHUD_FOV_def;
	m_fLR_CameraFactor			= 0.f;
	m_fLR_MovingFactor			= 0.f;
	m_fLR_InertiaFactor			= 0.f;
	m_fUD_InertiaFactor			= 0.f;
	SwitchSprint				= false;
	SetAnimationCallback		(nullptr);
	mark						= 0;
	fDeviceFlags.zero();
	_action_ppe					= -1;
}

DLL_Pure *CHudItem::_construct	()
{
	m_object			= smart_cast<CPhysicItem*>(this);
	VERIFY				(m_object);

	m_item				= smart_cast<CInventoryItem*>(this);
	VERIFY				(m_item);

	return				(m_object);
}

CHudItem::~CHudItem()
{
}

void CHudItem::Load(LPCSTR section)
{
	hud_sect				= pSettings->r_string		(section,"hud");
	hud_sect_cache = hud_sect;

	m_animation_slot		= pSettings->r_u32			(section,"animation_slot");

	m_nearwall_dist_min = READ_IF_EXISTS(pSettings, r_float, section, "nearwall_dist_min", .2f);
	m_nearwall_dist_max = READ_IF_EXISTS(pSettings, r_float, section, "nearwall_dist_max", 1.f);
	m_nearwall_target_hud_fov = READ_IF_EXISTS(pSettings, r_float, section, "nearwall_target_hud_fov", 0.27f);
	m_nearwall_speed_mod = READ_IF_EXISTS(pSettings, r_float, section, "nearwall_speed_mod", 10.f);

	m_fHudFov = READ_IF_EXISTS(pSettings, r_float, hud_sect, "hud_fov", 0.0f);

	m_current_inertion.PitchOffsetR = READ_IF_EXISTS(pSettings, r_float, hud_sect, "inertion_pitch_offset_r", PITCH_OFFSET_R);
	m_current_inertion.PitchOffsetD = READ_IF_EXISTS(pSettings, r_float, hud_sect, "inertion_pitch_offset_d", PITCH_OFFSET_D);
	m_current_inertion.PitchOffsetN = READ_IF_EXISTS(pSettings, r_float, hud_sect, "inertion_pitch_offset_n", PITCH_OFFSET_N);

	bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];

	if (isGuns)
	{
		float origin_offset = READ_IF_EXISTS(pSettings, r_float, hud_sect, "inertion_origin_offset", -1.f);

		if (origin_offset < 0.0f)
			origin_offset = READ_IF_EXISTS(pSettings, r_float, hud_sect, "inertion_origin_offset", ORIGIN_OFFSET);
		else
			origin_offset = -READ_IF_EXISTS(pSettings, r_float, hud_sect, "inertion_origin_offset", -1.f);

		m_current_inertion.OriginOffset = origin_offset;
	}
	else
		m_current_inertion.OriginOffset = READ_IF_EXISTS(pSettings, r_float, hud_sect, "inertion_origin_offset", ORIGIN_OFFSET);

	m_current_inertion.TendtoSpeed = READ_IF_EXISTS(pSettings, r_float, hud_sect, "inertion_tendto_speed", TENDTO_SPEED);

	m_bDisableBore = READ_IF_EXISTS(pSettings, r_bool, hud_sect, "disable_bore", isGuns);

	if (!m_bDisableBore)
		m_sounds.LoadSound(section, "snd_bore", "sndBore", true);

	if (pSettings->line_exist(section, "snd_sprint_start"))
		m_sounds.LoadSound(section, "snd_sprint_start", "sndSprintStart", true);


	if (pSettings->line_exist(section, "snd_sprint_end"))
		m_sounds.LoadSound(section, "snd_sprint_end", "sndSprintEnd", true);

	if (pSettings->line_exist(section, "snd_headlamp_on"))
		m_sounds.LoadSound(section, "snd_headlamp_on", "sndHeadlampOn", true);

	if (pSettings->line_exist(section, "snd_headlamp_off"))
		m_sounds.LoadSound(section, "snd_headlamp_off", "sndHeadlampOff", true);

	if (pSettings->line_exist(section, "snd_nv_on"))
		m_sounds.LoadSound(section, "snd_nv_on", "sndNVOn", true);

	if (pSettings->line_exist(section, "snd_nv_off"))
		m_sounds.LoadSound(section, "snd_nv_off", "sndNVOff", true);
}


void CHudItem::PlaySound(LPCSTR alias, const Fvector& position, bool allowOverlap)
{
	m_sounds.PlaySound(alias, position, object().H_Root(), !!GetHUDmode(), false , allowOverlap);
}

void CHudItem::renderable_Render()
{
	UpdateXForm					();
	BOOL _hud_render			= ::Render->get_HUD() && GetHUDmode();
	
	if(_hud_render  && !IsHidden())
	{ 
	}
	else 
	{
		if (!object().H_Parent() || (!_hud_render && !IsHidden()))
		{
			on_renderable_Render		();
			debug_draw_firedeps			();
		}else
		if (object().H_Parent()) 
		{
			CInventoryOwner	*owner = smart_cast<CInventoryOwner*>(object().H_Parent());
			VERIFY			(owner);
			CInventoryItem	*self = smart_cast<CInventoryItem*>(this);
			if (owner->attached(self) ||
				(item().BaseSlot() == INV_SLOT_3 /*|| item().BaseSlot() == INV_SLOT_2*/))
				on_renderable_Render();
		}
	}
}

void CHudItem::SwitchState(u32 S)
{
	if (OnClient()) 
		return;

	SetNextState( S );

	if (object().Local() && !object().getDestroy())	
	{
		// !!! Just single entry for given state !!!
		NET_Packet				P;
		object().u_EventGen		(P,GE_WPN_STATE_CHANGE,object().ID());
		P.w_u8					(u8(S));
		object().u_EventSend	(P);
	}
}

void CHudItem::OnEvent(NET_Packet& P, u16 type)
{
	switch (type)
	{
	case GE_WPN_STATE_CHANGE:
		{
			u8				S;
			P.r_u8			(S);
			OnStateSwitch	(u32(S));
		}
		break;
	}
}

void CHudItem::OnStateSwitch(u32 S)
{
	SetState			(S);
	
	if(object().Remote()) 
		SetNextState	(S);

	switch (S)
	{
	case eBore:
	{
		SetPending(FALSE);

		PlayAnimBore();
		if (HudItemData())
		{
			Fvector P = HudItemData()->m_item_transform.c;
			m_sounds.PlaySound("sndBore", P, object().H_Root(), !!GetHUDmode(), false, m_started_rnd_anim_idx);
		}

	}break;
	case eSprintStart:
	{
		SetPending(FALSE);
		SwitchSprint = true;
		PlayHUDMotion("anm_idle_sprint_start", true, this, GetState());
		if (HudItemData() && m_sounds.FindSoundItem("sndSprintStart", false));
			PlaySound("sndSprintStart", HudItemData()->m_item_transform.c);
	}break;
	case eSprintEnd:
	{
		SetPending(FALSE);
		SwitchSprint = false;
		PlayHUDMotion("anm_idle_sprint_end", true, this, GetState());
		if (HudItemData() && m_sounds.FindSoundItem("sndSprintEnd", false));
			PlaySound("sndSprintEnd", HudItemData()->m_item_transform.c);
	}break;
	case eSwitchDevice:
	{
		SetPending(TRUE);
		if (fDeviceFlags.test(DF_HEADLAMP))
		{
			if (CTorch* torch = smart_cast<CTorch*>(Actor()->inventory().ItemFromSlot(TORCH_SLOT)))
			{
				string64 anm = "";
				xr_sprintf(anm, "anm_headlamp_%s", torch->IsSwitched() ? "off" : "on");
				PlayHUDMotion(anm, true, this, eSwitchDevice);
				SetAnimationCallback({ CHudItem::TAnimationEffector(Actor(), &CActor::HeadlampCallback) });
				if (CWeapon* wpn = smart_cast<CWeapon*>(this))
				{
					wpn->MakeLockByConfigParam("lock_time_start_" + GetActualCurrentAnim(), false, { CHudItem::TAnimationEffector(Actor(), &CActor::HeadlampCallback)});
				}

				StartCompanionAnimIfNeeded(GetActualCurrentAnim());
				xr_sprintf(anm, "sndHeadlamp%s", torch->IsSwitched() ? "Off" : "On");
				PlaySound(anm, HudItemData()->m_item_transform.c);
			}
		}
		else if (fDeviceFlags.test(DF_NIGHTVISION))
		{
			if (CTorch* torch = smart_cast<CTorch*>(Actor()->inventory().ItemFromSlot(TORCH_SLOT)))
			{
				string64 anm = "";
				xr_sprintf(anm, "anm_nv_%s", torch->GetNightVisionStatus() ? "off" : "on");
				PlayHUDMotion(anm, true, this, eSwitchDevice);
				SetAnimationCallback({ CHudItem::TAnimationEffector(Actor(), &CActor::NVCallback)});
				if (CWeapon* wpn = smart_cast<CWeapon*>(this))
				{
					wpn->MakeLockByConfigParam("lock_time_start_" + GetActualCurrentAnim(), false, { CHudItem::TAnimationEffector(Actor(), &CActor::NVCallback) });
				}

				StartCompanionAnimIfNeeded(GetActualCurrentAnim());
				xr_sprintf(anm, "sndNV%s", torch->GetNightVisionStatus() ? "Off" : "On");
				PlaySound(anm, HudItemData()->m_item_transform.c);
			}
		}
		fDeviceFlags.zero();
	}break;
	case eShowing:
	{
		if (Actor() && object().H_Parent() == Actor())
			g_player_hud->attach_item(this);
	}break;
	}

	if (S != eIdle && S != eSprintStart && S != eSprintEnd)
		SwitchSprint = false;
}

void CHudItem::OnAnimationEnd(u32 state)
{
	if (CActor* pActor = smart_cast<CActor*>(object().H_Parent()))
	{
		pActor->callback(GameObject::eActorHudAnimationEnd)(smart_cast<CGameObject*>(this)->lua_game_object(), hud_sect.c_str(), m_current_motion.c_str(), state, animation_slot());
	}

	switch(state)
	{
	case eSprintStart:
	case eSprintEnd:
	case eBore:
	case eSwitchDevice:
		{
			SwitchState	(eIdle);
			SetPending(FALSE);
		} break;
	}
}

void CHudItem::PlayAnimBore()
{
	PlayHUDMotion("anm_bore", TRUE, this, GetState());
}

bool CHudItem::ActivateItem() 
{
	OnActiveItem	();
	return			true;
}

void CHudItem::DeactivateItem() 
{
	OnHiddenItem	();
}
void CHudItem::OnMoveToRuck(const SInvItemPlace& prev)
{
	SwitchState(eHidden);
}

bool CHudItem::SendDeactivateItem()
{
	if (GetState() == eHiding)
		return false;

	SendHiddenItem();

	return true;
}

void CHudItem::SendHiddenItem()
{
	if (!object().getDestroy())
	{
		NET_Packet				P;
		object().u_EventGen		(P,GE_WPN_STATE_CHANGE,object().ID());
		P.w_u8					(u8(eHiding));
		object().u_EventSend	(P, net_flags(TRUE, TRUE, FALSE, TRUE));
	}
}

void CHudItem::UpdateHudAdditonal(Fmatrix& trans)
{
	const static bool isInertion = EngineExternal()[EEngineExternalGame::EnableWeaponInertion];
	if (!isInertion)
		return;

	CActor* pActor = smart_cast<CActor*>(object().H_Parent());
	if (!pActor)
		return;

	attachable_hud_item* hi = HudItemData();
	if(!hi)
		return;

	static float fAvgTimeDelta = Device.fTimeDelta;
	fAvgTimeDelta = _inertion(fAvgTimeDelta, Device.fTimeDelta, 0.8f);

	float fYMag = pActor->fFPCamYawMagnitude;
	float fPMag = pActor->fFPCamPitchMagnitude;

	float fStrafeMaxTime = hi->m_measures.m_strafe_offset[2][0].y;
	// Макс. время в секундах, за которое мы наклонимся из центрального положения
	if (fStrafeMaxTime <= EPS)
		fStrafeMaxTime = 0.01f;

	float fStepPerUpd = fAvgTimeDelta / fStrafeMaxTime; // Величина изменение фактора поворота

	// Добавляем боковой наклон от движения камеры
	float fCamReturnSpeedMod = 1.5f;
	// Восколько ускоряем нормализацию наклона, полученного от движения камеры (только от бедра)

	// Высчитываем минимальную скорость поворота камеры для начала инерции
	float fStrafeMinAngle = hi->m_measures.m_strafe_offset[3][0].y;

	// Высчитываем мксимальный наклон от поворота камеры
	float fCamLimitBlend = hi->m_measures.m_strafe_offset[3][0].x;

	// Считаем стрейф от поворота камеры
	if (abs(fYMag) > (m_fLR_CameraFactor == 0.0f ? fStrafeMinAngle : 0.0f))
	{
		//--> Камера крутится по оси Y
		m_fLR_CameraFactor -= (fYMag * fAvgTimeDelta * 0.75f);
		clamp(m_fLR_CameraFactor, -fCamLimitBlend, fCamLimitBlend);
	}
	else
	{
		//--> Камера не поворачивается - убираем наклон
		if (m_fLR_CameraFactor < 0.0f)
		{
			m_fLR_CameraFactor += fStepPerUpd * fCamReturnSpeedMod;
			clamp(m_fLR_CameraFactor, -fCamLimitBlend, 0.0f);
		}
		else
		{
			m_fLR_CameraFactor -= fStepPerUpd * fCamReturnSpeedMod;
			clamp(m_fLR_CameraFactor, 0.0f, fCamLimitBlend);
		}
	}

	// Добавляем боковой наклон от ходьбы вбок
	float fChangeDirSpeedMod = 3;
	// Восколько быстро меняем направление направление наклона, если оно в другую сторону от текущего
	u32 iMovingState = pActor->GetMovementState(eReal);
	if ((iMovingState & ACTOR_DEFS::EMoveCommand::mcLStrafe) != 0)
	{
		// Движемся влево
		float fVal = (m_fLR_MovingFactor > 0.f ? fStepPerUpd * fChangeDirSpeedMod : fStepPerUpd);
		m_fLR_MovingFactor -= fVal;
	}
	else if ((iMovingState & ACTOR_DEFS::EMoveCommand::mcRStrafe) != 0)
	{
		// Движемся вправо
		float fVal = (m_fLR_MovingFactor < 0.f ? fStepPerUpd * fChangeDirSpeedMod : fStepPerUpd);
		m_fLR_MovingFactor += fVal;
	}
	else
	{
		// Двигаемся в любом другом направлении - плавно убираем наклон
		if (m_fLR_MovingFactor < 0.0f)
		{
			m_fLR_MovingFactor += fStepPerUpd;
			clamp(m_fLR_MovingFactor, -1.0f, 0.0f);
		}
		else
		{
			m_fLR_MovingFactor -= fStepPerUpd;
			clamp(m_fLR_MovingFactor, 0.0f, 1.0f);
		}
	}
	clamp(m_fLR_MovingFactor, -1.0f, 1.0f); // Фактор боковой ходьбы не должен превышать эти лимиты

	// Вычисляем и нормализируем итоговый фактор наклона
	float fLR_Factor = m_fLR_MovingFactor;

	clamp(fLR_Factor, -1.0f, 1.0f); // Фактор боковой ходьбы не должен превышать эти лимиты

	Fvector curr_offs, curr_rot;
	Fmatrix hud_rotation;
	Fmatrix hud_rotation_y;

	if ((hi->m_measures.m_strafe_offset[2][0].x != 0.0f))
	{
		// Смещение позиции худа в стрейфе
		curr_offs = hi->m_measures.m_strafe_offset[0][0]; // pos
		curr_offs.mul(fLR_Factor); // Умножаем на фактор стрейфа

		// Поворот худа в стрейфе
		curr_rot = hi->m_measures.m_strafe_offset[1][0]; // rot
		curr_rot.mul(-PI / 180.f); // Преобразуем углы в радианы
		curr_rot.mul(fLR_Factor); // Умножаем на фактор стрейфа

		hud_rotation.identity();
		hud_rotation.rotateX(curr_rot.x);

		hud_rotation_y.identity();
		hud_rotation_y.rotateY(curr_rot.y);
		hud_rotation.mulA_43(hud_rotation_y);

		hud_rotation_y.identity();
		hud_rotation_y.rotateZ(curr_rot.z);
		hud_rotation.mulA_43(hud_rotation_y);

		hud_rotation.translate_over(curr_offs);
		trans.mulB_43(hud_rotation);
	}

	//============= Инерция оружия =============//
	// Параметры инерции
	float fInertiaSpeedMod = hi->m_measures.m_inertion_params.m_tendto_speed;

	float fInertiaReturnSpeedMod = hi->m_measures.m_inertion_params.m_tendto_ret_speed;

	float fInertiaMinAngle = hi->m_measures.m_inertion_params.m_min_angle;

	Fvector4 vIOffsets; // x = L, y = R, z = U, w = D
	vIOffsets.x = hi->m_measures.m_inertion_params.m_offset_LRUD.x;
	vIOffsets.y = hi->m_measures.m_inertion_params.m_offset_LRUD.y;
	vIOffsets.z = hi->m_measures.m_inertion_params.m_offset_LRUD.z;
	vIOffsets.w = hi->m_measures.m_inertion_params.m_offset_LRUD.w;

	// Высчитываем инерцию из поворотов камеры
	bool bIsInertionPresent = m_fLR_InertiaFactor != 0.0f || m_fUD_InertiaFactor != 0.0f;
	if (abs(fYMag) > fInertiaMinAngle || bIsInertionPresent)
	{
		float fSpeed = fInertiaSpeedMod;
		if (fYMag > 0.0f && m_fLR_InertiaFactor > 0.0f ||
			fYMag < 0.0f && m_fLR_InertiaFactor < 0.0f)
		{
			fSpeed *= 2.f; //--> Ускоряем инерцию при движении в противоположную сторону
		}

		m_fLR_InertiaFactor -= (fYMag * fAvgTimeDelta * fSpeed); // Горизонталь (м.б. > |1.0|)
	}

	if (abs(fPMag) > fInertiaMinAngle || bIsInertionPresent)
	{
		float fSpeed = fInertiaSpeedMod;
		if (fPMag > 0.0f && m_fUD_InertiaFactor > 0.0f ||
			fPMag < 0.0f && m_fUD_InertiaFactor < 0.0f)
		{
			fSpeed *= 2.f; //--> Ускоряем инерцию при движении в противоположную сторону
		}

		m_fUD_InertiaFactor -= (fPMag * fAvgTimeDelta * fSpeed); // Вертикаль (м.б. > |1.0|)
	}

	clamp(m_fLR_InertiaFactor, -1.0f, 1.0f);
	clamp(m_fUD_InertiaFactor, -1.0f, 1.0f);

	// Плавное затухание инерции (основное, но без линейной никогда не опустит инерцию до полного 0.0f)
	m_fLR_InertiaFactor *= clampr(1.f - fAvgTimeDelta * fInertiaReturnSpeedMod, 0.0f, 1.0f);
	m_fUD_InertiaFactor *= clampr(1.f - fAvgTimeDelta * fInertiaReturnSpeedMod, 0.0f, 1.0f);

	// Минимальное линейное затухание инерции при покое (горизонталь)
	if (fYMag == 0.0f)
	{
		float fRetSpeedMod = (fYMag == 0.0f ? 1.0f : 0.75f) * (fInertiaReturnSpeedMod * 0.075f);
		if (m_fLR_InertiaFactor < 0.0f)
		{
			m_fLR_InertiaFactor += fAvgTimeDelta * fRetSpeedMod;
			clamp(m_fLR_InertiaFactor, -1.0f, 0.0f);
		}
		else
		{
			m_fLR_InertiaFactor -= fAvgTimeDelta * fRetSpeedMod;
			clamp(m_fLR_InertiaFactor, 0.0f, 1.0f);
		}
	}

	// Минимальное линейное затухание инерции при покое (вертикаль)
	if (fPMag == 0.0f)
	{
		float fRetSpeedMod = (fPMag == 0.0f ? 1.0f : 0.75f) * (fInertiaReturnSpeedMod * 0.075f);
		if (m_fUD_InertiaFactor < 0.0f)
		{
			m_fUD_InertiaFactor += fAvgTimeDelta * fRetSpeedMod;
			clamp(m_fUD_InertiaFactor, -1.0f, 0.0f);
		}
		else
		{
			m_fUD_InertiaFactor -= fAvgTimeDelta * fRetSpeedMod;
			clamp(m_fUD_InertiaFactor, 0.0f, 1.0f);
		}
	}

	// Применяем инерцию к худу
	float fLR_lim = (m_fLR_InertiaFactor < 0.0f ? vIOffsets.x : vIOffsets.y);
	float fUD_lim = (m_fUD_InertiaFactor < 0.0f ? vIOffsets.z : vIOffsets.w);

	curr_offs = { fLR_lim * -1.f * m_fLR_InertiaFactor, fUD_lim * m_fUD_InertiaFactor, 0.0f };

	hud_rotation.identity();
	hud_rotation.translate_over(curr_offs);
	trans.mulB_43(hud_rotation);
}

void CHudItem::UpdateCL()
{
	if(m_current_motion_def)
	{
		if(m_bStopAtEndAnimIsRunning)
		{
			const xr_vector<motion_marks>&	marks = m_current_motion_def->marks;
			if(!marks.empty())
			{
				float motion_prev_time = ((float)m_dwMotionCurrTm - (float)m_dwMotionStartTm)/1000.0f;
				float motion_curr_time = ((float)Device.dwTimeGlobal - (float)m_dwMotionStartTm)/1000.0f;
				
				xr_vector<motion_marks>::const_iterator it = marks.begin();
				xr_vector<motion_marks>::const_iterator it_e = marks.end();
				for(;it!=it_e;++it)
				{
					const motion_marks&	M = (*it);
					if(M.is_empty())
						continue;
	
					const motion_marks::interval* Iprev = M.pick_mark(motion_prev_time);
					const motion_marks::interval* Icurr = M.pick_mark(motion_curr_time);
					if(Iprev==nullptr && Icurr!=nullptr /* || M.is_mark_between(motion_prev_time, motion_curr_time)*/)
					{
						OnMotionMark				(m_startedMotionState, M);
					}
				}
			
			}

			m_dwMotionCurrTm					= Device.dwTimeGlobal;
			if(m_dwMotionCurrTm > m_dwMotionEndTm)
			{
				m_current_motion_def				= nullptr;
				m_dwMotionStartTm					= 0;
				m_dwMotionEndTm						= 0;
				m_dwMotionCurrTm					= 0;
				m_bStopAtEndAnimIsRunning			= false;
				OnAnimationEnd						(m_startedMotionState);
			}
		}
	}

	if (object().H_Parent() == nullptr)
	{
		lock_time_callback = nullptr;
		mark = 0;
	}

	if (Actor() && object().H_Parent() == Actor())
	{
		u32 anim_time = Device.GetTimeDeltaSafe(m_dwMotionStartTm, m_dwMotionCurrTm);

		if (smart_cast<CWeaponBinoculars*>(this) != nullptr || smart_cast<CWeaponKnife*>(this) != nullptr || smart_cast<CMissile*>(this) != nullptr || smart_cast<CHUDAnimItem*>(this) != nullptr)
		{
			if (lock_time_callback != nullptr && mark > 0 && mark < anim_time)
			{
				lock_time_callback();
				SetAnimationCallback(nullptr);
				mark = 0;
			}
		}

		xr_string use_ppe_key = "use_ppe_effector_" + GetActualCurrentAnim();

		if (READ_IF_EXISTS(pSettings, r_bool, hud_sect, use_ppe_key.c_str(), false))
		{
			xr_string ppe_key = "ppe_effector_" + GetActualCurrentAnim();
			xr_string ppe = pSettings->r_string(hud_sect, ppe_key.c_str());
			xr_string ppe_start_key = "ppe_start_" + GetActualCurrentAnim();
			xr_string ppe_end_key = "ppe_end_" + GetActualCurrentAnim();
			int ppe_start = floor(READ_IF_EXISTS(pSettings, r_float, hud_sect, ppe_start_key.c_str(), 0.f) * 1000.f);
			int ppe_end = floor(READ_IF_EXISTS(pSettings, r_float, hud_sect, ppe_end_key.c_str(), 100.f) * 1000.f);

			if (anim_time > ppe_start && anim_time < ppe_end)
			{
				// Включим PPE, если он выключен
				if (_action_ppe < 0)
				{
					_action_ppe = 2014;
					add_pp_effector(ppe.c_str(), _action_ppe, false);
					set_pp_effector_factor2(_action_ppe, 1.f);
				}
			}
			else
			{
				// Выключим PPE, если он включен
				if (_action_ppe >= 0)
				{
					set_pp_effector_factor2(_action_ppe, 0.001f);
					remove_pp_effector(_action_ppe);
					_action_ppe = -1;
				}
			}
		}
		else
		{
			if (_action_ppe >= 0)
			{
				set_pp_effector_factor2(_action_ppe, 0.001f);
				remove_pp_effector(_action_ppe);
				_action_ppe = -1;
			}
		}
	}
}

void CHudItem::OnH_A_Chield		()
{}

void CHudItem::OnH_B_Chield		()
{
	StopCurrentAnimWithoutCallback();
}

void CHudItem::OnH_B_Independent	(bool just_before_destroy)
{
	m_sounds.StopAllSounds	();
	UpdateXForm				();
	
	// next code was commented 
	/*
	if(HudItemData() && !just_before_destroy)
	{
		object().XFORM().set( HudItemData()->m_item_transform );
	}
	
	if (HudItemData())
	{
		g_player_hud->detach_item(this);
		Msg("---Detaching hud item [%s][%d]", this->HudSection().c_str(), this->object().ID());
	}*/
	//SetHudItemData			(nullptr);
}

void CHudItem::OnH_A_Independent	()
{
	if(HudItemData())
		g_player_hud->detach_item(this);
	StopCurrentAnimWithoutCallback();
}

void CHudItem::on_b_hud_detach()
{
	m_sounds.StopAllSounds	();
}

void CHudItem::on_a_hud_attach()
{
	if(m_current_motion_def)
	{
		PlayHUDMotion_noCB(m_current_motion, FALSE);
#ifdef DEBUG
//		Msg("continue playing [%s][%d]",m_current_motion.c_str(), Device.dwFrame);
#endif // #ifdef DEBUG
	}else
	{
#ifdef DEBUG
//		Msg("no active motion");
#endif // #ifdef DEBUG
	}
}

//AVO: check if animation exists
bool CHudItem::HudAnimationExist(LPCSTR anim_name)
{
	if (HudItemData()) // First person
	{
		string256 anim_name_r;
		bool is_16x9 = UI().is_widescreen();
		u16 attach_place_idx = READ_IF_EXISTS(pSettings, r_u16, HudItemData()->m_sect_name, "attach_place_idx", 0);
		xr_sprintf(anim_name_r, "%s%s", anim_name, ((attach_place_idx == 1) && is_16x9) ? "_16x9" : "");
		player_hud_motion* anm = HudItemData()->m_hand_motions.find_motion(anim_name_r);
		if (anm)
			return true;
	}
	else // Third person
	{
		if (g_player_hud->motion_length(anim_name, HudSection(), m_current_motion_def) > 100)
			return true;

	}
#ifdef DEBUG
	Msg("~ [WARNING] ------ Animation [%s] does not exist in [%s]", anim_name, HudSection().c_str());
#endif
	return false;
}
//-AVO

u32 CHudItem::PlayHUDMotion(xr_string M, BOOL bMixIn, CHudItem*  W, u32 state, bool need_suffix)
{
	if (need_suffix)
		M = NeedAddSuffix(M);

	if (HudItemData() && !HudAnimationExist(M.c_str()))
	{
		Msg("! model [%s] has no motion alias defined [%s]", hud_sect.c_str(), M);
		return 0;
	}
	
	xr_string snd_name = "snd_" + M;
	if (pSettings->line_exist(hud_sect, snd_name.c_str()))
	{
		m_sounds.LoadSound(HudSection().c_str(), snd_name.c_str(), "sndByMotion", false);
		PlaySound("sndByMotion", object().Position());
	}

	if (smart_cast<CWeaponBinoculars*>(this) != nullptr || smart_cast<CWeaponKnife*>(this) != nullptr || smart_cast<CMissile*>(this) != nullptr || smart_cast<CHUDAnimItem*>(this) != nullptr)
	{
		if (pSettings->line_exist(hud_sect, ("mark_" + M).c_str()))
			mark = floor(READ_IF_EXISTS(pSettings, r_float, hud_sect, ("mark_" + M).c_str(), 100.f) * 1000.f);
	}

	u32 anim_time = PlayHUDMotion_noCB(M.c_str(), bMixIn);
	if (anim_time>0)
	{
		m_bStopAtEndAnimIsRunning	= true;
		m_dwMotionStartTm			= Device.dwTimeGlobal;
		m_dwMotionCurrTm			= m_dwMotionStartTm;
		m_dwMotionEndTm				= m_dwMotionStartTm + anim_time;
		m_startedMotionState		= state;
	}else
		m_bStopAtEndAnimIsRunning	= false;

	return anim_time;
}

u32 CHudItem::PlayHUDMotion_noCB(const shared_str& motion_name, BOOL bMixIn)
{
	m_current_motion					= motion_name;

	if(bDebug && item().m_pInventory)
	{
		Msg("-[%s] as[%d] [%d]anim_play [%s][%d]",
			HudItemData()?"HUD":"Simulating", 
			item().m_pInventory->GetActiveSlot(), 
			item().object_id(),
			motion_name.c_str(), 
			Device.dwFrame);
	}
	if( HudItemData() )
	{
		return HudItemData()->anim_play		(motion_name, bMixIn, m_current_motion_def, m_started_rnd_anim_idx);
	}else
	{
		m_started_rnd_anim_idx				= 0;
		return g_player_hud->motion_length	(motion_name, HudSection(), m_current_motion_def );
	}
}

void CHudItem::StopCurrentAnimWithoutCallback()
{
	m_dwMotionStartTm			= 0;
	m_dwMotionEndTm				= 0;
	m_dwMotionCurrTm			= 0;
	m_bStopAtEndAnimIsRunning	= false;
	m_current_motion_def		= nullptr;
}

BOOL CHudItem::GetHUDmode()
{
	if (object().H_Parent())
	{
		CActor* A = smart_cast<CActor*>(object().H_Parent());
		return (A && A->HUDview() && HudItemData());
	}
	else
		return FALSE;
}

void CHudItem::PlayAnimIdle()
{
	if (TryPlayAnimIdle())
		return;

	PlayHUDMotion("anm_idle", TRUE, nullptr, GetState());
}

bool CHudItem::TryPlayAnimIdle()
{
	if(MovingAnimAllowedNow())
	{
		CActor* pActor = smart_cast<CActor*>(object().H_Parent());
		if (pActor)
		{
			u32 state = pActor->GetMovementState(eReal);
			bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];
			if(state & ACTOR_DEFS::EMoveCommand::mcSprint)
			{
				if (!SwitchSprint && isGuns)
				{
					SwitchState(eSprintStart);
					return true;
				}

				PlayAnimIdleSprint();

				return true;
			}
			else if (SwitchSprint && isGuns)
			{
				SwitchState(eSprintEnd);
				return true;
			}
			else if (pActor->AnyMove())
			{
				if (state & ACTOR_DEFS::EMoveCommand::mcCrouch && isGuns)
				{
					if (state & ACTOR_DEFS::EMoveCommand::mcAccel)
						PlayAnimIdleMovingCrouchSlow();
					else
						PlayAnimIdleMovingCrouch();

					return true;
				}
				else
				{
					if (state & ACTOR_DEFS::EMoveCommand::mcAccel && isGuns)
						PlayAnimIdleMovingSlow();
					else
						PlayAnimIdleMoving();

					return true;
				}
			}
		}
	}
	return false;
}

void CHudItem::PlayAnimIdleMoving()
{
	PlayHUDMotion("anm_idle_moving", TRUE, nullptr, GetState());
}

void CHudItem::PlayAnimIdleMovingSlow()
{
	PlayHUDMotion("anm_idle_moving_slow", TRUE, NULL, GetState());
}

void CHudItem::PlayAnimIdleMovingCrouch()
{
	PlayHUDMotion("anm_idle_moving_crouch", TRUE, NULL, GetState());
}

void CHudItem::PlayAnimIdleMovingCrouchSlow()
{
	PlayHUDMotion("anm_idle_moving_crouch_slow", TRUE, NULL, GetState());
}

void CHudItem::PlayAnimIdleSprint()
{
	PlayHUDMotion("anm_idle_sprint", TRUE, nullptr,GetState());
}

void CHudItem::OnMovementChanged(ACTOR_DEFS::EMoveCommand cmd)
{
	if (GetState() == eIdle && !m_bStopAtEndAnimIsRunning)
	{
		PlayAnimIdle();
		ResetSubStateTime();
	}
}

attachable_hud_item* CHudItem::HudItemData()
{
	attachable_hud_item* hi = nullptr;
	if (!g_player_hud)
		return hi;

	hi = g_player_hud->attached_item(0);
	if (hi && hi->m_parent_hud_item == this)
		return hi;

	hi = g_player_hud->attached_item(1);
	if (hi && hi->m_parent_hud_item == this)
		return hi;

	return nullptr;
}

float CHudItem::GetHudFov()
{
	if (Level().CurrentViewEntity() == object().H_Parent())
	{
		float dist = HUD().GetCurrentRayQuery().range;

		clamp(dist, m_nearwall_dist_min, m_nearwall_dist_max);
		float fDistanceMod = ((dist - m_nearwall_dist_min) / (m_nearwall_dist_max - m_nearwall_dist_min));

		float fBaseFov = m_fHudFov ? m_fHudFov : psHUD_FOV_def;
		clamp(fBaseFov, 5.f, 180.f);
		const static bool isCollision = EngineExternal()[EEngineExternalGame::EnableWeaponCollision];
		if (isCollision)
		{

			float src = m_nearwall_speed_mod * Device.fTimeDelta;
			clamp(src, 0.f, 1.f);

			float fTrgFov = m_nearwall_target_hud_fov + fDistanceMod * (fBaseFov - m_nearwall_target_hud_fov);
			m_nearwall_last_hud_fov = m_nearwall_last_hud_fov * (1.f - src) + fTrgFov * src;
		}
		else
			m_nearwall_last_hud_fov = fBaseFov;
	}

	return m_nearwall_last_hud_fov;
}

xr_string CHudItem::GetActualCurrentAnim() const
{
	if (m_current_motion != nullptr)
		return m_current_motion.c_str();

	return "";
}

bool CHudItem::StartCompanionAnimIfNeeded(const xr_string anim_name)
{
	if (Actor() != object().H_Parent())
		return false;

	xr_string new_anm = anim_name;
	if (new_anm.Contains("anm_"))
		new_anm.erase(0, 4);

	if (Actor()->GetDetector() != nullptr)
	{
		string128 det_anm = "";
		xr_sprintf(det_anm, "anm_lefthand_%s_wpn_%s", Actor()->GetDetector()->m_section_id.c_str(), new_anm.c_str());
		if (!pSettings->line_exist(hud_sect, det_anm))
			return false;

		AssignDetectorAnim(det_anm, true, true);
		return true;
	}

	return false;
}

void CHudItem::AssignDetectorAnim(const xr_string anm_alias, bool bMixIn, bool use_companion_section)
{
	if (Actor()->GetDetector() == nullptr || Actor()->GetDetector()->GetState() != CCustomDetector::eIdle)
		return;

	LPCSTR section = Actor()->GetDetector()->HudSection().c_str();

	if (use_companion_section)
		section = HudSection().c_str();

	if (pSettings->line_exist(section, anm_alias.c_str()))
	{
		swap(this->HudItemData()->m_hand_motions, Actor()->GetDetector()->HudItemData()->m_hand_motions);
		Actor()->GetDetector()->PlayHUDMotion(anm_alias, true, Actor()->GetDetector(), eIdle);
		swap(this->HudItemData()->m_hand_motions, Actor()->GetDetector()->HudItemData()->m_hand_motions);
	}
}

bool CHudItem::CanStartAction(bool allow_aim_state)
{
	if (GetState() != eIdle || Actor()->GetMovementState(eReal) & ACTOR_DEFS::EMoveCommand::mcSprint || Actor()->GetDetector() && Actor()->GetDetector()->GetState() == CCustomDetector::eShowing)
		return false;

	CWeapon* wpn = smart_cast<CWeapon*>(this);
	if (wpn && (wpn->IsActionProcessing() || !allow_aim_state && (wpn->IsAimStarted || wpn->IsZoomed())))
		return false;

	return true;
}

bool CHudItem::Weapon_SetKeyRepeatFlagIfNeeded(u32 kfACTTYPE)
{
	bool result = CanStartAction();
	if (!result)
	{
		if (Actor()->GetMovementState(eReal) & ACTOR_DEFS::EMoveCommand::mcSprint)
		{
			Actor()->SetMovementState(eWishful, mcSprint, false);
			Actor()->SetActorKeyRepeatFlag((ACTOR_DEFS::EActorKeyflags)kfACTTYPE, true);
		}
	}

	return result;
}

bool CHudItem::IsSuicideAnimPlaying() const
{
	return GetActualCurrentAnim().find("anm_prepare_suicide") == 0 || GetState() == eSuicide || GetActualCurrentAnim().find("anm_suicide") == 0;
}

bool CHudItem::WpnCanShoot() const
{
	return !!(smart_cast<CWeaponMagazined*>(this) != nullptr && smart_cast<CWeaponBinoculars*>(this) == nullptr);
}

CHudItem::jitter_params CHudItem::GetCurJitterParams(shared_str hud_sect)
{
	jitter_params result;

	result.pos_amplitude = READ_IF_EXISTS(pSettings, r_float, "gunslinger_base", "base_jitter_pos_amplitude", 0.001f);
	result.rot_amplitude = READ_IF_EXISTS(pSettings, r_float, "gunslinger_base", "base_jitter_rot_amplitude", 0.1f);

	result.pos_amplitude = READ_IF_EXISTS(pSettings, r_float, hud_sect, "jitter_pos_amplitude", result.pos_amplitude);
	result.rot_amplitude = READ_IF_EXISTS(pSettings, r_float, hud_sect, "jitter_rot_amplitude", result.rot_amplitude);
	return result;
}