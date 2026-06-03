#include "StdAfx.h"
#include "HudPdaAnimator.h"
#include "UIGameCustom.h"
#include "Inventory.h"
#include "InventoryWeaponSlotLayout.h"
#include "Level.h"
#include "ElectronicsProblemsManager.h"
#include "UIPdaWnd.h"
#include "../xrEngine/xr_input.h"

extern bool m_AnimatorForceHideItems;

CHudPdaAnimator::CHudPdaAnimator(CHudAnimatorManager* manager, const shared_str& section) : CHudStateAnimator(manager)
{
	m_section = section;
	m_joystick = BI_NONE;
	Load();
}

void CHudPdaAnimator::Load()
{
	CHudStateAnimator::Load();

	if (pSettings->line_exist(m_section, "snd_blowout"))
	{
		m_sounds.LoadSound(m_section.c_str(), "snd_blowout", "sndBlowout", true);
	}

	m_sounds.LoadSound(m_section.c_str(), "snd_btn_press", "sndButtonPress");
	m_sounds.LoadSound(m_section.c_str(), "snd_btn_release", "sndButtonRelease");

	m_fBlowoutLevel = READ_IF_EXISTS(pSettings, r_float, m_section, "blowout_anim_level", 1000.0f);
	m_fZoomRotateTime = READ_IF_EXISTS(pSettings, r_float, m_section, "zoom_rotate_time", 0.25f);

	//m_base_inertion = m_current_inertion;

	m_zoom_inertion.PitchOffsetR = READ_IF_EXISTS(pSettings, r_float, m_section, "inertion_aim_pitch_offset_r", 0.0f);
	m_zoom_inertion.PitchOffsetD = READ_IF_EXISTS(pSettings, r_float, m_section, "inertion_aim_pitch_offset_d", 0.0f);
	m_zoom_inertion.PitchOffsetN = READ_IF_EXISTS(pSettings, r_float, m_section, "inertion_aim_pitch_offset_n", 0.0f);

	m_zoom_inertion.OriginOffset = READ_IF_EXISTS(pSettings, r_float, m_section, "inertion_aim_origin_offset", ORIGIN_OFFSET * 0.5f);
	m_zoom_inertion.TendtoSpeed = READ_IF_EXISTS(pSettings, r_float, m_section, "inertion_aim_tendto_speed", TENDTO_SPEED);

	m_fHudFovZoomFactor = READ_IF_EXISTS(pSettings, r_float, m_section, "hud_fov_zoom_factor", m_fHudFovFactor);
	m_thumb_rot[0] = READ_IF_EXISTS(pSettings, r_float, m_section, "thumb_rot_x", 0.f);
	m_thumb_rot[1] = READ_IF_EXISTS(pSettings, r_float, m_section, "thumb_rot_y", 0.f);
	m_joystick_bone = READ_IF_EXISTS(pSettings, r_string, m_section, "joystick_bone", nullptr);
}

void CHudPdaAnimator::Update()
{
	if (GetState() != eHidden)
	{
		psHUD_Flags.set(HUD_CROSSHAIR_RT2, false);
		psHUD_Flags.set(HUD_DRAW_RT, !m_bIsZoomed);

		//m_current_inertion.lerp(m_base_inertion, m_zoom_inertion, m_fZoomRotationFactor);
	}

	if (auto ui = CurrentGameUI())
	{
		if (ui->PdaMenu()->IsShown())
		{
			ui->PdaMenu()->Enable(m_bIsEnabled);
			ui->PdaMenu()->Update();
		}
	}

	if (m_bNeedActivated)
	{
		m_manager->SetTargetAnimator(this);
		bool wpn_hide = !g_player_hud->attached_item(0) && !m_manager->Parent()->inventory().ActiveItem() && !m_manager->Parent()->inventory().GetNextActiveSlot() && !m_manager->Parent()->inventory().GetActiveSlot();
		if (wpn_hide && g_player_hud->GetAnimator() == nullptr && !g_player_hud->attached_item(1))
		{
			m_bNeedActivated = false;
			SetState(eShowing);
		}
		else
		{
			CHudAnimatorBase* current_animator = m_manager->Parent()->HudAnimator()->CurrentAnimator();
			if (CHudStateAnimator* state_animator = current_animator != nullptr ? current_animator->cast_hud_state_animator() : nullptr)
			{
				if (state_animator->GetState() != CHudStateAnimator::EAnimatorStates::eHidden && state_animator->GetState() != CHudStateAnimator::EAnimatorStates::eHiding)
				{
					if (m_AnimatorForceHideItems)
					{
						state_animator->StopAnimator();
					}
					else
					{
						state_animator->SetState(CHudStateAnimator::EAnimatorStates::eHiding);
					}
				}
			}

			CHudItem* active_item = m_manager->Parent()->inventory().ActiveItem() ? m_manager->Parent()->inventory().ActiveItem()->cast_hud_item() : nullptr;
			if (active_item != nullptr)
			{
				u16 slot = m_manager->Parent()->inventory().GetActiveSlot();
				m_manager->SlotToRestore() = slot;

				if (m_AnimatorForceHideItems)
				{
					m_manager->Parent()->inventory().SetActiveSlot(NO_ACTIVE_SLOT);
					active_item->SwitchState(CHUDState::EHudStates::eHidden, false);
					active_item->SetState(CHUDState::EHudStates::eHidden);
					g_player_hud->detach_item_idx(0);
				}
				else if (active_item->GetState() != CHUDState::EHudStates::eHiding)
				{
					m_manager->Parent()->inventory().Activate(NO_ACTIVE_SLOT);
				}
			}

			if (CCustomDevice* dev = m_manager->Parent()->GetDevice())
			{
				m_manager->RestoreDevice() = true;

				if (m_AnimatorForceHideItems)
				{
					dev->SwitchState(CHUDState::EHudStates::eHidden, false);
					dev->SetState(CHUDState::EHudStates::eHidden);
					g_player_hud->detach_item_idx(1);
				}
				else if (dev->GetState() != CHUDState::EHudStates::eHiding)
				{
					dev->HideDetector(true, true);
				}
			}
		}
	}
	else
	{
		if (GetState() == eIdle)
		{
			static const bool UseBlowoutAnim = g_player_hud->GetAnimator()->m_hand_motions.has_motion("anm_blowout");
			if (UseBlowoutAnim && !m_bSwitchSprint && m_bNeedBlowoutAnim && m_fBlowoutLevel <= Level().GetElectronicsProblemsManager()->CurrentElectronicsProblemsCnt())
			{
				m_bNeedBlowoutAnim = false;
				SetState(eBlowout);
			}

			if (!m_bDisableBore && Device.dwTimeGlobal - m_dw_curr_substate_time > 20000)
			{
				SetState(eBore);
				ResetSubStateTime();
			}
		}
	}

	UpdateAnimation();
}

void CHudPdaAnimator::OnAnimationEnd(u8 state)
{
	switch (state)
	{
	case eHiding:
	{
		SetState(eHidden);

		if (auto ui = CurrentGameUI())
		{
			ui->PdaMenu()->HideDialog();
		}

		m_manager->Parent()->set_inventory_disabled(false);

		if (m_manager->TargetAnimator() != nullptr)
		{
			break;
		}

		if (!g_player_hud->m_need_reload)
		{
			break;
		}

		u8& slot_to_restore = m_manager->SlotToRestore();
		bool& restore_device = m_manager->RestoreDevice();

		PIItem item_to_restore = m_manager->Parent()->inventory().ItemFromSlot(slot_to_restore);
		if (slot_to_restore > 0 && item_to_restore != nullptr)
		{
			u16 real_slot = item_to_restore->BaseSlot();
			if (!IsSidearmPhysicalSlot(real_slot) && real_slot != KNIFE_SLOT && real_slot != BOLT_SLOT)
			{
				restore_device = false;
			}

			m_manager->Parent()->inventory().Activate(slot_to_restore);
			slot_to_restore = 0;
		}

		if (restore_device && m_manager->Parent()->GetDevice(true))
		{
			m_manager->Parent()->GetDevice(true)->ToggleDetector(true, true);
			restore_device = false;
		}
	}break;
	case eBlowout:
	case eAimStart:
	case eAimEnd:
	{
		SetState(eIdle);
	}break;
	default:
	{
		CHudStateAnimator::OnAnimationEnd(state);
		break;
	}
	}
}

void CHudPdaAnimator::OnStateSwitch(u8 state)
{
	m_current_state = state;

	if (state != eIdle)
	{
		m_dw_curr_state_time = Device.dwTimeGlobal;
		ResetSubStateTime();
	}

	switch (state)
	{
	case eShowing:
	{
		g_player_hud->create_animator_item(this, m_section);
		m_eAnimationsFlags.set(EAnimationsFlags::af_torch, HudAnimationExist("anm_switch_device"));
		m_eAnimationsFlags.set(EAnimationsFlags::af_nvg, m_eAnimationsFlags.test(EAnimationsFlags::af_torch));
		m_eAnimationsFlags.set(EAnimationsFlags::af_clear_mask, HudAnimationExist("anm_gasmask"));
		PlayMotion(SetCurrentStateAnimation("anm_show"), false, eShowing);

		m_bIsPlaying = true;
		m_bNeedBlowoutAnim = true;

		if (m_manager->TargetAnimator() == this)
		{
			m_manager->SetTargetAnimator(nullptr);
		}

		m_manager->SetCurrentAnimator(this);

		if (m_sounds.FindSoundItem("sndDraw", false))
		{
			m_sounds.PlaySound("sndDraw", zero_vel, m_manager->Parent(), true);
		}

		if (auto ui = CurrentGameUI())
		{
			ui->PdaMenu()->ShowDialog(false);
			ui->PdaMenu()->Enable(false);
		}
	}break;
	case eIdle:
	{
		PlayAnimIdle();

		if (m_joystick_bone && m_joystick == BI_NONE)
			m_joystick = g_player_hud->GetAnimator()->m_item->LL_BoneID(m_joystick_bone);

		if (m_joystick != BI_NONE)
		{
			CBoneInstance* bi = &g_player_hud->GetAnimator()->m_item->LL_GetBoneInstance(m_joystick);
			if (bi)
			{
				bi->set_callback(bctCustom, JoystickCallback, this);
			}
		}
	}break;
	//case eHiding:
	//{
	//	g_player_hud->reset_thumb(false);
	//	ResetJoystick(false);
	//
	//	if (m_joystick != BI_NONE)
	//	{
	//		g_player_hud->GetAnimator()->m_item->LL_GetBoneInstance(m_joystick).reset_callback();
	//	}
	//	
	//}break;
	case eHidden:
	{
		g_player_hud->reset_thumb(true);
		ResetJoystick(true);

		StopAnimator();
	}break;
	case eBlowout:
	{
		if (m_sounds.FindSoundItem("sndBlowout", false))
		{
			m_sounds.PlaySound("sndBlowout", zero_vel, m_manager->Parent(), true);
		}
		PlayMotion(SetCurrentStateAnimation("anm_blowout"), true, eBlowout);
	}break;
	case eAimStart:
	{
		PlayMotion("anm_idle_aim_start", true, eAimStart);
		break;
	}
	case eAimEnd:
	{
		PlayMotion("anm_idle_aim_end", true, eAimEnd);
		break;
	}
	default:
	{
		CHudStateAnimator::OnStateSwitch(state);
		break;
	}
	}
}

void CHudPdaAnimator::StopAnimator()
{
	CHudStateAnimator::StopAnimator();

	m_bNeedBlowoutAnim = false;
	m_bIsZoomed = false;
	m_bIsEnabled = false;
}

ENGINE_API extern float psHUD_FOV_def;

float CHudPdaAnimator::GetHudFov() const
{
	bool wpn_hide = !g_player_hud->attached_item(0) && !g_player_hud->attached_item(1);
	if (IsActive() && wpn_hide)
	{
		float get = CHudAnimatorBase::GetHudFov() / m_fHudFovFactor;
		float hud_fov = m_fHudFovFactor;

		if (((IsZoomed() && m_fZoomRotationFactor <= 1.f) || (!IsZoomed() && m_fZoomRotationFactor > 0.f)))
		{
			hud_fov = hud_fov - (hud_fov - m_fHudFovZoomFactor) * m_fZoomRotationFactor;
		}

		return get * hud_fov;
	}

	return psHUD_FOV_def * m_fHudFovFactor;
}

void CHudPdaAnimator::SwitchAnimator()
{
	if (GetState() == eIdle)
	{
		SetState(eHiding);
		m_manager->Parent()->set_inventory_disabled(false);
	}
	else if (!m_bNeedActivated && GetState() == eHidden && g_player_hud->GetAnimator() == nullptr)
	{
		m_bNeedActivated = true;
		m_manager->Parent()->set_inventory_disabled(true);
		if (auto ui = CurrentGameUI())
		{
			ui->HideActorMenu();
		}
	}
}

void CHudPdaAnimator::PlayAnimIdle()
{
	if (TryPlayAnimIdle())
	{
		return;
	}

	if (m_bIsZoomed)
	{
		PlayMotion("anm_idle_aim", true, eIdle);
	}
	else
	{
		PlayMotion("anm_idle", true, eIdle);
	}
}

bool CHudPdaAnimator::TryPlayAnimIdle()
{
	if (m_bIsZoomed)
	{
		return false;
	}

	return CHudStateAnimator::TryPlayAnimIdle();
}

bool CHudPdaAnimator::SwitchZoom()
{
	if (GetState() != eIdle)
	{
		return false;
	}

	if (m_bIsZoomed)
	{
		if (pInput->GetControllerMode())
		{
			return false;
		}

		m_bIsZoomed = false;
		m_bIsEnabled = false;
		SetState(eAimEnd);
	}
	else
	{
		m_bIsZoomed = true;
		m_bIsEnabled = true;
		SetState(eAimStart);
	}

	return true;
}

EHudOffsetType CHudPdaAnimator::GetCurrentHudOffsetIdx() const
{
	bool b_aiming = ((m_bIsZoomed && m_fZoomRotationFactor <= 1.0f) || (!m_bIsZoomed && m_fZoomRotationFactor > 0.0f));

	if (!b_aiming)
	{
		return EHudOffsetType::eDefault;
	}
	else
	{
		return EHudOffsetType::eAim;
	}
}

void CHudPdaAnimator::UpdateHudAdditonal(Fmatrix& trans)
{
	u8 idx = GetCurrentHudOffsetIdx();

	animator_item* animator = g_player_hud->GetAnimator();

	Fvector curr_offs = animator->m_hands_positions.hands_offsets[0][idx];//pos,aim
	Fvector curr_rot = animator->m_hands_positions.hands_offsets[1][idx];//rot,aim
	curr_offs.mul(m_fZoomRotationFactor);
	curr_rot.mul(m_fZoomRotationFactor);

	Fmatrix	hud_rotation;
	hud_rotation.identity();
	hud_rotation.rotateX(curr_rot.x);

	Fmatrix	hud_rotation_y;
	hud_rotation_y.identity();
	hud_rotation_y.rotateY(curr_rot.y);
	hud_rotation.mulA_43(hud_rotation_y);

	hud_rotation_y.identity();
	hud_rotation_y.rotateZ(curr_rot.z);
	hud_rotation.mulA_43(hud_rotation_y);

	hud_rotation.translate_over(curr_offs);
	trans.mulB_43(hud_rotation);

	if (m_bIsZoomed)
	{
		m_fZoomRotationFactor += Device.fTimeDelta / m_fZoomRotateTime;
	}
	else
	{
		m_fZoomRotationFactor -= Device.fTimeDelta / m_fZoomRotateTime;
	}

	clamp(m_fZoomRotationFactor, 0.0f, 1.0f);
}

bool CHudPdaAnimator::InputKeyPress(int cmd)
{
	switch (cmd)
	{
		case kWPN_ZOOM:
		{
			return SwitchZoom();
			break;
		}
		case kWPN_ZOOM_ALTER:
		{
			if (GetState() == eIdle)
			{
				m_bIsEnabled = !m_bIsEnabled;
				return true;
			}
			break;
		}
		case kWPN_1:
		case kWPN_2:
		case kWPN_3:
		case kWPN_4:
		case kWPN_5:
		case kWPN_6:
		{
			if (GetState() == eHiding || GetState() == eHidden)
			{
				return false;
			}

			if (cmd == kWPN_6 && !IsGameTypeSingleCompatible())
			{
				return false;
			}

			u16 slot = u16(cmd - kWPN_1 + 1);
			PIItem item_from_slot = m_manager->Parent()->inventory().ItemFromSlot(slot);
			if (item_from_slot != nullptr && !m_manager->Parent()->inventory().IsSlotBlocked(item_from_slot))
			{
				m_manager->SlotToRestore() = slot;
				SetState(eHiding);
				return true;
			}
		}break;
	};

	return false;
}

shared_str CHudPdaAnimator::SetCurrentStateAnimation(const shared_str& anim_name)
{
	shared_str new_name = anim_name;

	new_name.printf("%s%s", *anim_name, "_aim");
	if (m_bIsZoomed && HudAnimationExist(new_name))
	{
		return new_name;
	}

	return anim_name;
}

void CHudPdaAnimator::JoystickCallback(CBoneInstance* B)
{
	CHudPdaAnimator* pdaAnimator = static_cast<CHudPdaAnimator*>(B->callback_param());
	static float fAvgTimeDelta = Device.fTimeDelta;
	fAvgTimeDelta = _inertion(fAvgTimeDelta, Device.fTimeDelta, 0.8f);

	Fvector& target = pdaAnimator->target_joystickrot;
	Fvector& current = pdaAnimator->joystickrot;
	float& target_press = pdaAnimator->target_buttonpress;
	float& press = pdaAnimator->buttonpress;

	if (!target.similar(current, 0.0001f))
	{
		Fvector diff;
		diff = target;
		diff.sub(current);
		diff.mul(fAvgTimeDelta / 0.1f);
		current.add(diff);
	}
	else
		current.set(target);

	if (!fsimilar(target_press, press, 0.0001f))
	{
		float prev_press = press;

		float diff = target_press;
		diff -= press;
		diff *= (fAvgTimeDelta / .1f);
		press += diff;

		//f (prev_press == 0.f && press < 0.f)
		//
		//	pdaAnimator->m_sounds.PlaySound("sndButtonPress", B->mTransform.c, pdaAnimator->m_manager->Parent(), true);
		//
		//lse if (prev_press < -.001f && press >= -.001f)
		//
		//	pdaAnimator->m_sounds.PlaySound("sndButtonRelease", B->mTransform.c, pdaAnimator->m_manager->Parent(), true);
		//
	}
	else
	{
		press = target_press;
	}

	Fmatrix rotation;
	rotation.identity();
	rotation.rotateX(current.x);

	Fmatrix rotation_y;
	rotation_y.identity();
	rotation_y.rotateY(current.y);
	rotation.mulA_43(rotation_y);

	rotation_y.identity();
	rotation_y.rotateZ(current.z);
	rotation.mulA_43(rotation_y);

	rotation.translate_over(0.f, press, 0.f);

	B->mTransform.mulB_43(rotation);
}

void CHudPdaAnimator::MouseMovement(float x, float y)
{
	x *= 0.1f;
	y *= 0.1f;
	clamp(x, -0.15f, 0.15f);
	clamp(y, -0.15f, 0.15f);

	if (std::abs(x) < 0.05f)
		x = 0.0f;

	if (std::abs(y) < 0.05f)
		y = 0.0f;

	bool buttonpressed = (bButtonL || bButtonR);

	target_buttonpress = (buttonpressed ? -0.0015f : 0.0f);
	target_joystickrot.set(x * -0.75f, 0.0f, y * 0.75f);

	x += y * m_thumb_rot[0];
	y += x * m_thumb_rot[1];

	g_player_hud->m_bone_callback_params[r_finger0]->m_target.set(y * 0.15f, y * -0.05f, (x * -0.15f) + (buttonpressed ? 0.002f : 0.0f));
	g_player_hud->m_bone_callback_params[r_finger01]->m_target.set(0.0f, 0.0f, (x * -0.25f) + (buttonpressed ? 0.01f : 0.0f));
	g_player_hud->m_bone_callback_params[r_finger02]->m_target.set(0.0f, 0.0f, (x * 0.75f) + (buttonpressed ? 0.025f : 0.0f));
}

bool CHudPdaAnimator::OnMouseAction(float x, float y, EUIMessages mouse_action)
{
	switch (mouse_action)
	{
	case WINDOW_LBUTTON_DOWN:
	case WINDOW_RBUTTON_DOWN:
	case WINDOW_LBUTTON_UP:
	case WINDOW_RBUTTON_UP:
	{
		if (GetState() != eIdle)
			return true;

		if (mouse_action == WINDOW_LBUTTON_DOWN)
			bButtonL = true;
		else if (mouse_action == WINDOW_RBUTTON_DOWN)
			bButtonR = true;
		else if (mouse_action == WINDOW_LBUTTON_UP)
			bButtonL = false;
		else if (mouse_action == WINDOW_RBUTTON_UP)
			bButtonR = false;
		
		break;
	}
	}
	
	return true; //always true because StopAnyMove() == false
}