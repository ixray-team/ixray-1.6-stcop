#include "StdAfx.h"
#include "Actor.h"
#include "Torch.h"
#include "trade.h"
#include "../xrEngine/CameraBase.h"

#ifdef DEBUG
#	include "PHDebug.h"
#endif

#include "Hit.h"
#include "PHDestroyable.h"
#include "Car.h"
#include "UIGameSP.h"
#include "Inventory.h"
#include "InventoryWeaponSlotLayout.h"
#include "Level.h"
#include "game_cl_base.h"
#include "../xrEngine/xr_level_controller.h"
#include "UsableScriptObject.h"
#include "ActorCondition.h"
#include "actor_input_handler.h"
#include "../xrEngine/string_table.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "ui/UIActorMenu.h"
#include "ui/UIDragDropReferenceList.h"
#include "CharacterPhysicsSupport.h"
#include "InventoryBox.h"
#include "player_hud.h"
#include "../xrEngine/xr_input.h"
#include "flare.h"
#include "CustomDevice.h"
#include "clsid_game.h"
#include "HUDManager.h"
#include "Weapon.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "ActorHelmet.h"
#include "HudItem.h"
#include "WeaponKnife.h"
#include "../xrEngine/XR_IOConsole.h"
#include "UIMainIngameWnd.h"
#include "../xrEngine/CustomHUD.h"
#include "ui/UIRadialMenuWeapon.h"
#include "ui/UIPdaWnd.h"
#include "ActorEffector.h"
#include "PostprocessAnimator.h"
#include "ControllerAutoaim.h"
#include "CameraFirstEye.h"
#include "Grenade.h"
#include "InteractiveObject.h"
#include "nvg.h"
#include "PickupManager.h"

extern u32 hud_adj_mode;

void CActor::IR_OnKeyboardPress(int dik)
{
	auto bind = get_binded_action(dik);
	if (hud_adj_mode && pInput->iGetAsyncKeyState(SDL_SCANCODE_LSHIFT))
	{
		return;
	}

	if (Remote())
	{
		return;
	}

	if (IsTalking())
	{
		return;
	}

	if (m_input_external_handler && !m_input_external_handler->authorized(bind))
	{
		return;
	}

	if (load_screen_renderer.IsActive())
	{
		return;
	}

	if (HudAnimator() != nullptr)
	{
		if (HudAnimator()->InputKeyPress(bind))
		{
			return;
		}
	}

	switch (bind)
	{
	case kWPN_FIRE:
		{
			if( (mstate_wishful & mcLookout) && !IsGameTypeSingle() ) return;
			//-----------------------------
			if (OnServer())
			{
				NET_Packet P;
				P.w_begin(M_PLAYER_FIRE); 
				P.w_u16(ID());
				u_EventSend(P);
			}
		}break;
	default:
		{
		}break;
	}

#ifndef MASTER_GOLD
	if (psActorFlags.test(AF_NO_CLIP))
	{
		NoClipFly(bind);
		if (m_holder && kUSE != bind)
			m_holder->OnKeyboardPress(dik);
		return;
	}
#endif //DEBUG

	if (!g_Alive()) return;

	auto bindRadial = get_binded_action(dik, agUIRadialWeapon);
	if(m_holder && kUSE != bind)
	{
		m_holder->OnKeyboardPress			(dik);
		if(m_holder->allowWeapon() && inventory().Action((u16)bind, CMD_START))		return;
		return;
	}
	else
	{

		if (inventory().Action(bindRadial == kNOTBINDED ? (u16)bind : (u16)bindRadial, CMD_START))
			return;
	}
	if (IsWaunded)
	{
		return;
	}

	switch(bind)
	{
	case kJUMP:		
		{
			mstate_wishful |= mcJump;
		}break;
	case kSPRINT_TOGGLE:	
		{
			if (Holder() == nullptr)
			mstate_wishful ^= mcSprint;
		}break;
	case kCROUCH:	
		{
		if( psActorFlags.test(AF_CROUCH_TOGGLE) )
			mstate_wishful ^= mcCrouch;
		}break;
	case kCAM_1:	cam_Set			(eacFirstEye);				break;
	case kCAM_2:	
		if (active_cam() != eacLookAt)
			cam_Set(eacLookAt);
		else
			cam_Set(eacFirstEye);
		break;
	case kCAM_3:	cam_Set			(eacFreeLook);				break;
	case kTORCH:
		{
			SwitchTorch();
			break;
		}
	case kCLEARGASMASK:
	{
		ClearMask();
		break;
	}
	case kDETECTOR:
	{
		PIItem det_active = inventory().ItemFromSlot(DEVICE_SLOT);
		if (det_active)
		{
			if (CCustomDevice* dev = det_active->cast_custom_device())
			{
				dev->switch_device();
			}
		}
	}break;
	case kQUICK_KICK:
	{
		PIItem knife_item = inventory().ItemFromSlot(KNIFE_SLOT);
		if (m_sQuickKickAnimator.size() > 0 && knife_item != nullptr)
		{
			if (!HudAnimator()->IsAnyAnimatorActive())
			{
				if (knife_item != inventory().ActiveItem())
				{
					StartAnimator(m_sQuickKickAnimator);
					HudAnimator()->ItemAnimator()->SetLeftCallback({ this, &CActor::MakeKick });
				}
				else
				{
					knife_item->Action(kWPN_FIRE, CMD_START);
					knife_item->Action(kWPN_FIRE, CMD_STOP);
				}
			}
		}
	}break;
	case kQUICK_GRENADE:
	{
		PIItem item_from_slot = inventory().EnsureSlotItemFromRuck(GRENADE_SLOT);
		CGrenade* grenade_item = item_from_slot != nullptr ? item_from_slot->cast_grenade() : nullptr;

		if (grenade_item != nullptr && !inventory().IsSlotBlocked(grenade_item) && grenade_item->HudAnimationExist("anm_throw_quick"))
		{
			if (item_from_slot != inventory().ActiveItem())
			{
				grenade_item->SetQuickThrow();
				inventory().Activate(GRENADE_SLOT);
			}
			else
			{
				grenade_item->Action(kWPN_FIRE, CMD_START);
				grenade_item->Action(kWPN_FIRE, CMD_STOP);
			}
		}
	}break;
	case kUSE:
		ActorUse();
		break;
	case kDROP:
		b_DropActivated			= true;
		f_DropPower				= 0;
		break;
	case kNEXT_SLOT:
		{
			OnNextWeaponSlot();
		}break;
	case kPREV_SLOT:
		{
			OnPrevWeaponSlot();
		}break;
	case kUSE_BANDAGE:
	case kUSE_MEDKIT:
	{
		if (IsGameTypeSingle())
		{
			PIItem itm = inventory().item((bind == kUSE_BANDAGE) ? CLSID_IITEM_BANDAGE : CLSID_IITEM_MEDKIT);
			if (itm)
			{
				inventory().Eat(itm);
				SDrawStaticStruct* _s = CurrentGameUI()->AddCustomStatic("item_used", true, 3.0f);
				string1024					str;
				xr_strconcat(str, g_pStringTable->translate("st_item_used").c_str(), ": ", itm->NameItem());
				_s->wnd()->TextItemControl()->SetText(str);
			}
		}
	}break;
	case kQUICK_USE_1:
	case kQUICK_USE_2:
	case kQUICK_USE_3:
	case kQUICK_USE_4:
	{
		ActorQuickSlotUse(bind);
	}
	break;
	case kSHOW_QUICK_SLOTS:
	{
		// Only process if hide quick slots option is enabled
		if (psHUD_Flags.test(HUD_HIDE_QUICK_SLOTS))
		{
			if (CurrentGameUI() && CurrentGameUI()->UIMainIngameWnd)
			{
				CurrentGameUI()->UIMainIngameWnd->SetQuickSlotsPanelVisible(true);
			}
		}
	}
	break;
	case kWPN_RADIAL_MENU:
	{
		CUIRadialMenuWeapon* RMW = CurrentGameUI()->RadialMenuWeapon();
		if (RMW->isInitialized && !RMW->IsShown())
			RMW->ShowDialog(false);
		break;
	}
	}
	switch (bindRadial)
	{
	case kNIGHT_VISION:
	{
		SwitchNightVision();
		break;
	}
	}
}

void CActor::IR_OnMouseWheel(int direction)
{
	if (hud_adj_mode)
	{
		g_player_hud->tune({ 0.f,0.f,float(direction) });
		return;
	}

	if (inventory().Action(direction > 0 ? (u16)kWPN_ZOOM_DEC : (u16)kWPN_ZOOM_INC, CMD_START))
		return;
	
	if (psActorFlags.test(AF_NO_CLIP))
	{
		if (Level().IR_GetKeyState(SDL_SCANCODE_LSHIFT) || Level().IR_GetKeyState(SDL_SCANCODE_RSHIFT))
		{
			SetNoclipSpeedScale(direction > 0 ? 5.0: -5.0f);
			return;
		}
		SetNoclipSpeedScale(direction > 0 ? 1.0: -1.0f);
	}
	else
	{
		if (direction > 0)
			OnNextWeaponSlot();
		else
			OnPrevWeaponSlot();
	}
}

void CActor::IR_OnKeyboardRelease(int dik)
{
	auto bind = get_binded_action(dik);
	if (hud_adj_mode && pInput->iGetAsyncKeyState(SDL_SCANCODE_LSHIFT))
	{
		return;
	}

	if (Remote())
	{
		return;
	}

	if (m_input_external_handler && !m_input_external_handler->authorized(bind))
	{
		return;
	}

	if (IsWaunded)
	{
		return;
	}

	if (HudAnimator() != nullptr)
	{
		if (HudAnimator()->InputKeyRelease(bind))
		{
			return;
		}
	}

	if (g_Alive())	
	{
		auto bindRadial = get_binded_action(dik, agUIRadialWeapon);
		if(m_holder)
		{
			m_holder->OnKeyboardRelease(dik);
			
			if(m_holder->allowWeapon() && inventory().Action((u16)bind, CMD_STOP))		return;
			return;
		}
		else
		{
			if (inventory().Action(bindRadial == kNOTBINDED ? (u16)bind : (u16)bindRadial, CMD_STOP))
				return;
		}


		switch (bind)
		{
		case kJUMP:		mstate_wishful &= ~mcJump;		break;
		case kSHOW_QUICK_SLOTS:
		{
			// Only process if hide quick slots option is enabled
			if (psHUD_Flags.test(HUD_HIDE_QUICK_SLOTS))
			{
				// Only hide panel if it was shown by key press (not by item use)
				// This prevents hiding when key binding is changed and old key is released
				if (CurrentGameUI() && CurrentGameUI()->UIMainIngameWnd)
				{
					// Check if panel was actually shown by key press before hiding
					// The panel state is managed by SetQuickSlotsPanelVisible which sets m_quick_slots_force_visible_by_key
					// We only hide if the panel was force-visible (meaning it was shown by key)
					CurrentGameUI()->UIMainIngameWnd->SetQuickSlotsPanelVisible(false);
				}
			}
		}
		break;
		case kWPN_RADIAL_MENU:
			CUIRadialMenuWeapon* RMW = CurrentGameUI()->RadialMenuWeapon();
			if (RMW->isInitialized && RMW->IsShown())
			{
				RMW->TryActivateSelectedSector();
				RMW->HideDialog();
			}
			break;
		}
		switch (bindRadial)
		{
		case kDROP:		
			if (GAME_PHASE_INPROGRESS == Game().Phase() && !CImGuiManager::Instance().IsCapturingInputs()) 
				g_PerformDrop();				
			break;
		}
	}
}

void CActor::IR_OnKeyboardHold(int dik)
{
	auto bind = get_binded_action(dik);
	if (hud_adj_mode && pInput->iGetAsyncKeyState(SDL_SCANCODE_LSHIFT))
	{
		if (pInput->iGetAsyncKeyState(SDL_SCANCODE_RIGHT))
		{
			g_player_hud->tune(Fvector().set(1, 0, 0));
		}

		if (pInput->iGetAsyncKeyState(SDL_SCANCODE_LEFT))
		{
			g_player_hud->tune(Fvector().set(-1, 0, 0));
		}

		if (pInput->iGetAsyncKeyState(SDL_SCANCODE_UP))
		{
			g_player_hud->tune(Fvector().set(0, 1, 0));
		}

		if (pInput->iGetAsyncKeyState(SDL_SCANCODE_DOWN))
		{
			g_player_hud->tune(Fvector().set(0, -1, 0));
		}

		if (pInput->iGetAsyncKeyState(SDL_SCANCODE_PAGEUP))
		{
			g_player_hud->tune(Fvector().set(0, 0, 1));
		}

		if (pInput->iGetAsyncKeyState(SDL_SCANCODE_PAGEDOWN))
		{
			g_player_hud->tune(Fvector().set(0, 0, -1));
		}

		return;
	}

	if (Remote())	
	{
		return;
	}
	if (m_input_external_handler && !m_input_external_handler->authorized(bind))
	{
		return;
	}

	if (IsTalking())
	{
		return;
	}

	if (load_screen_renderer.IsActive())
	{
		return;
	}

#ifndef MASTER_GOLD
	if (psActorFlags.test(AF_NO_CLIP) && (bind == kFWD || bind == kBACK || bind == kL_STRAFE || bind == kR_STRAFE
		|| bind == kJUMP || bind == kCROUCH))
	{
		NoClipFly(bind);
		return;
	}
#endif //DEBUG

	if (!g_Alive())
	{
		return;
	}

	if(m_holder)
	{
		m_holder->OnKeyboardHold(dik);
		return;
	}

	if (IsWaunded)
	{
		return;
	}

	if (HudAnimator() != nullptr)
	{
		if (HudAnimator()->InputKeyHold(bind))
		{
			return;
		}
	}

	float LookFactor = GetLookFactor();
	switch(bind)
	{
	case kUP:
	case kDOWN: 
		cam_Active()->Move( (bind==kUP) ? kDOWN : kUP, 0, LookFactor);									break;
	case kCAM_ZOOM_IN: 
	case kCAM_ZOOM_OUT: 
		cam_Active()->Move(bind);												break;
	case kLEFT:
	case kRIGHT:
		if (eacFreeLook!=cam_active) cam_Active()->Move(bind, 0, LookFactor);	break;

	case kACCEL:	mstate_wishful |= mcAccel;									break;
	case kL_STRAFE:	
		leftStickThreshold.x = -1.0f;
		mstate_wishful |= mcLStrafe;
		break;
	case kR_STRAFE:	
		leftStickThreshold.x = 1.0f;
		mstate_wishful |= mcRStrafe;
		break;
	case kFWD:		
		leftStickThreshold.y = 1.0f;
		mstate_wishful |= mcFwd;
		break;
	case kBACK:		
		leftStickThreshold.y = -1.0f;
		mstate_wishful |= mcBack;
		break;
	case kCROUCH:
		{
			if( !psActorFlags.test(AF_CROUCH_TOGGLE) )
					mstate_wishful |= mcCrouch;

		}break;
		case kUSE:
			ActorUse();
			break;
	}

	switch (get_binded_action(dik, agAiming))
	{
	case kL_LOOKOUT:
		if (eacLookAt != cam_active)
			mstate_wishful |= mcLLookout;
		else
			psActorFlags.set(AF_RIGHT_SHOULDER, true);
		break;
	case kR_LOOKOUT:
		if (eacLookAt != cam_active)
			mstate_wishful |= mcRLookout;
		else
			psActorFlags.set(AF_RIGHT_SHOULDER, false);
		break;
	}
}

void CActor::IR_OnMouseMove(int dx, int dy)
{

	if (hud_adj_mode)
	{
		float LookFactor = GetLookFactor();

		CCameraBase* C = cameras[cam_active];
		float scale = (C->f_fov / g_fov) * psMouseSens * psMouseSensScale / 50.0f / LookFactor;
		float dx_ = float(dx) * scale;
		float dy_ = (psMouseInvert ? -1 : 1) * float(dy) * scale * 3.0f / 4.0f;
		g_player_hud->tune({ dx_, dy_, 0.f });
		return;
	}

	PIItem iitem = inventory().ActiveItem();
	if (iitem != nullptr && iitem->cast_hud_item())
	{
		iitem->cast_hud_item()->ResetSubStateTime();
	}

	if (Remote())
	{
		return;
	}

	if (m_holder)
	{
		m_holder->OnMouseMove(dx, dy);
		return;
	}

	float LookFactor = GetLookFactor();

	CCameraBase* C = cameras[cam_active];
	float scale = (C->f_fov / g_fov) * psMouseSens * psMouseSensScale / 50.0f / LookFactor;

	if (CWeapon* wpn = iitem != nullptr ? iitem->cast_weapon() : nullptr)
	{
		if (!wpn->IsGrenadeMode() && wpn->IsLensedScopeInstalled() && !wpn->IsAltZoomed())
		{
			float zoom_scale = scale * (wpn->GetLensFOV() * 0.02f);
			scale = _lerp(scale, zoom_scale, wpn->GetAimFactor());
		}
	}

	if (dx)
	{
		float d = float(dx) * scale;
		cam_Active()->Move((d < 0) ? kLEFT : kRIGHT, std::abs(d));
	}

	if (dy)
	{
		float d = (psMouseInvert ? -1 : 1) * float(dy) * scale * 3.0f / 4.0f;
		cam_Active()->Move((d > 0) ? kUP : kDOWN, std::abs(d));
	}
}

void CActor::IR_GamepadUpdateStick(int id, Fvector2 value)
{
	if (Remote())
	{
		return;
	}

	if (load_screen_renderer.IsActive())
	{
		return;
	}

	if (m_holder)
	{
		m_holder->OnGamepadAxisMove(id, value);
		return;
	}

#ifndef MASTER_GOLD
	if (psActorFlags.test(AF_NO_CLIP) && id == 0)
	{
		NoClipFlyStick(value);
		return;
	}
#endif // DEBUG

	float absValueX = std::abs(value.x);
	if (id != 2 && fis_zero(value.x) && fis_zero(value.y) || 
		(CurrentGameUI() && CurrentGameUI()->RadialMenuWeapon()->isInitialized && CurrentGameUI()->RadialMenuWeapon()->IsShown())
		|| ((HudAnimator() && HudAnimator()->PdaAnimator()) ? HudAnimator()->PdaAnimator()->IsZoomed() : CurrentGameUI()->PdaMenu()->IsShown()))
	{
		if (id == 0)
		{
			if (mstate_wishful & mcAccel)
			{
				mstate_wishful &= ~mcAccel;
			}
			if (mstate_wishful & mcLStrafe)
			{
				mstate_wishful &= ~mcLStrafe;
			}
			if (mstate_wishful & mcRStrafe)
			{
				mstate_wishful &= ~mcRStrafe;
			}
			if (mstate_wishful & mcFwd)
			{
				mstate_wishful &= ~mcFwd;
			}
			if (mstate_wishful & mcBack)
			{
				mstate_wishful &= ~mcBack;
			}
		}
		return;
	}
	// Left stick
	switch (id)
	{
	case 0:
	{
		leftStickThreshold = value;

		if (!g_Alive())
		{
			return;
		}

		if (!fis_zero(value.x))
		{
			mstate_wishful |= (value.x > 0.f) ? mcRStrafe : mcLStrafe;
		}

		if (!fis_zero(value.y))
		{
			mstate_wishful |= (value.y > 0.f) ? mcFwd : mcBack;
		}

		if (std::abs(value.y) < 0.375f
			&& std::abs(value.x) < 0.375f
			&& !(mstate_real & mcCrouch))
		{
			mstate_wishful |= mcAccel;
		}
		else if (!(mstate_real & mcAccel) && !isActorAccelerated(mstate_real, IsZoomAimingMode()))
		{
			mstate_wishful &= ~mcAccel;
		}
	}break;
	// Right stick
	case 1:
	{
		float LookFactor = GetLookFactor();

		CCameraBase* C = cameras[cam_active];
		float scale = (C->f_fov / g_fov) * psGamepadSens * Device.fTimeDelta * psMouseSensScale / LookFactor;

		if (!fis_zero(value.x))
		{
			float d = value.x * scale * 8;
			cam_Active()->Move((d < 0) ? kLEFT : kRIGHT, std::abs(d));
		}

		if (!fis_zero(value.y))
		{
			float d = (psGamepadInvert ? -1 : 1) * value.y * scale * 3.f / 4.f;
			d *= 8;

			cam_Active()->Move((d > 0) ? kUP : kDOWN, std::abs(d));
		}
	}break;
	}
}

u32 gamepad_crouch_time_global = 0;
void CActor::IR_GamepadKeyPress(int id)
{
	auto bind = get_binded_action(id);
	if (hud_adj_mode && pInput->iGetAsyncKeyState(SDL_SCANCODE_LSHIFT))
	{
		return;
	}

	if (Remote())
	{
		return;
	}

	if (IsTalking())
	{
		return;
	}

	if (m_input_external_handler && !m_input_external_handler->authorized(bind))
	{
		return;
	}

	if (load_screen_renderer.IsActive())
	{
		return;
	}

	if (HudAnimator() != nullptr)
	{
		if (HudAnimator()->InputKeyPress(bind))
		{
			return;
		}
	}

#ifndef MASTER_GOLD
	if (psActorFlags.test(AF_NO_CLIP))
	{
		NoClipFlyGamepad(bind);
		if (m_holder && kUSE != bind)
			m_holder->OnGamepadKeyPress(id);
		return;
	}
#endif //DEBUG

	if (!g_Alive()) return;

	if(m_holder && kUSE != bind)
	{
		m_holder->OnGamepadKeyPress		(id);
		if(m_holder->allowWeapon() && inventory().Action((u16)bind, CMD_START))		return;
		return;
	}
	else if(inventory().Action((u16)bind, CMD_START))	
	{
		if (bind == kWPN_ZOOM)
		{
			CWeapon* wpn = inventory().ActiveItem() ? inventory().ActiveItem()->cast_weapon() : nullptr;

			if (wpn)
			{
				CEntityAlive* pAim = nullptr;

				if (pAutoaim->auto_aim_pick_target(this, m_memory, pAim))
				{
					CCameraFirstEye* pCamera = smart_cast<CCameraFirstEye*>(cam_Active());
					if (pCamera)
					{
						Fvector pos;
						pAutoaim->look_at_pos_for_aiming(pos, pAim);
						pCamera->LookAtPoint(pos, PI_DIV_2, PI);

						lastTimeAutoAimStarted = Device.dwTimeContinual;
					}
				}
			}
		}
		return;
	}
	if (IsWaunded)
	{
		return;
	}

	switch (bind)
	{
		case kUSE:
		{
			ActorUse();
			break;
		}
		case kCROUCH:
		{
			if (mstate_real & mcCrouch && mstate_real & mcAccel)
			{
				mstate_wishful &= mcAccel;
			}
			else
			{
				mstate_wishful ^= mcCrouch;
			}
			gamepad_crouch_time_global = Device.dwTimeContinual;
			break;
		}
		case kJUMP:
		{
			mstate_wishful |= mcJump;
			break;
		}
		case kSPRINT_TOGGLE:
		{
			if (Holder() == nullptr)
				mstate_wishful ^= mcSprint;
			break;
		}
		case kTORCH:
		{
			SwitchTorch();
			break;
		}
		case kUSE_BANDAGE:
		case kUSE_MEDKIT:
		{
			if (IsGameTypeSingle())
			{
				PIItem itm = inventory().item((bind == kUSE_BANDAGE) ? CLSID_IITEM_BANDAGE : CLSID_IITEM_MEDKIT);
				if (itm)
				{
					inventory().Eat(itm);
					SDrawStaticStruct* _s = CurrentGameUI()->AddCustomStatic("item_used", true, 3.0f);
					string1024					str;
					xr_strconcat(str, g_pStringTable->translate("st_item_used").c_str(), ": ", itm->NameItem());
					_s->wnd()->TextItemControl()->SetText(str);
				}
			}
		}break;
		case kQUICK_USE_1:
		case kQUICK_USE_2:
		case kQUICK_USE_3:
		case kQUICK_USE_4:
		{
			if (!IsZoomAimingMode())
				ActorQuickSlotUse(bind);
			break;
		}
		case kWPN_RADIAL_MENU:
		{
			if (!IsZoomAimingMode())
			{
				CUIRadialMenuWeapon* RMW = CurrentGameUI()->RadialMenuWeapon();
				if (RMW->isInitialized && !RMW->IsShown())
					RMW->ShowDialog(false);
			}
			break;
		}
	}

	EGameActions bindAim = get_binded_action(id, agAiming);
	switch (bindAim)
	{
		case kWPN_ZOOM_INC:
		case kWPN_ZOOM_DEC:
		{
			if (IsZoomAimingMode())
			{
				if (inventory().Action((u16)bindAim, CMD_START))
					return;
			}
			break;
		}
	}
}

void CActor::IR_GamepadKeyRelease(int id)
{
	auto bind = get_binded_action(id);
	
	if (Remote())
	{
		return;
	}

	if (m_input_external_handler && !m_input_external_handler->authorized(bind))
	{
		return;
	}

	if (IsWaunded)
	{
		return;
	}

	if (HudAnimator() != nullptr)
	{
		if (HudAnimator()->InputKeyRelease(bind))
		{
			return;
		}
	}

	if (g_Alive())	
	{
		if(m_holder)
		{
			m_holder->OnGamepadKeyRelease(id);
			
			if(m_holder->allowWeapon() && inventory().Action((u16)bind, CMD_STOP))		return;
			return;
		}else
			if(inventory().Action((u16)bind, CMD_STOP))		return;



		switch (bind)
		{
		case kJUMP:		
			mstate_wishful &= ~mcJump;		
			break;
		case kWPN_RADIAL_MENU:
			CUIRadialMenuWeapon* RMW = CurrentGameUI()->RadialMenuWeapon();
			if (RMW->isInitialized && RMW->IsShown())
			{
				RMW->TryActivateSelectedSector();
				RMW->HideDialog();
			}
			break;
		}
	}
}

void CActor::IR_GamepadKeyHold(int id)
{
	auto bind = get_binded_action(id);
	
	if (Remote())	
	{
		return;
	}
	if (m_input_external_handler && !m_input_external_handler->authorized(bind))
	{
		return;
	}

	if (IsTalking())
	{
		return;
	}

	if (!g_Alive())
	{
		return;
	}

#ifndef MASTER_GOLD
	if (psActorFlags.test(AF_NO_CLIP) && (bind == kJUMP || bind == kSPRINT_TOGGLE))
	{
		NoClipFlyGamepad(bind);
		return;
	}
#endif // DEBUG

	if(m_holder)
	{
//		m_holder->OnKeyboardHold(dik);
		return;
	}

	if (IsWaunded)
	{
		return;
	}

	if (HudAnimator() != nullptr)
	{
		if (HudAnimator()->InputKeyHold(bind))
		{
			return;
		}
	}

	switch (bind)
	{
		case kCROUCH:
		{
			if (Device.dwTimeContinual > (gamepad_crouch_time_global + 1000))
			{
				mstate_wishful |= mcAccel;
			}
			break;
		}

		case kUSE:
		{
			ActorUse();
			break;
		}
	}

	switch (get_binded_action(id, agAiming))
	{
		case kL_LOOKOUT:
		{
			if (IsZoomAimingMode())
			{
				if (eacLookAt != cam_active)
					mstate_wishful |= mcLLookout;
				else
					psActorFlags.set(AF_RIGHT_SHOULDER, true);
			}
			break;
		}
		case kR_LOOKOUT:
		{
			if (IsZoomAimingMode())
			{
				if (eacLookAt != cam_active)
					mstate_wishful |= mcRLookout;
				else
					psActorFlags.set(AF_RIGHT_SHOULDER, false);
			}
			break;
		}
	}
}

void CActor::StartAnimator(const shared_str& section)
{
	if (IsGameTypeSingle())
	{
		HudAnimator()->ItemAnimator()->StartAnimator(section);
	}
	else
	{
		NET_Packet P;
		CGameObject::u_EventGen(P, GEG_PLAYER_START_HUD_ANIMATOR, ID());
		P.w_stringZ(section);
		CGameObject::u_EventSend(P);
	}
}

static bool IsKeyPressed(int dik)
{
	if (pInput != nullptr)
	{
		return pInput->iGetAsyncKeyState(dik);
	}

	return false;
}

static bool IsActionKeyPressed(const EGameActions& EGameAction)
{
	int key1 = get_action_dik(EGameAction, 0);
	int key2 = get_action_dik(EGameAction, 1);

	return ((key1 != 0) && IsKeyPressed(key1)) || ((key2 != 0) && IsKeyPressed(key2));
}

bool CActor::IsActionKeyPressedInGame(const EGameActions& EGameAction) const
{
	return IsActionKeyPressed(EGameAction) && !Console->bVisible && CurrentGameUI() != nullptr && !CurrentGameUI()->TopInputReceiver() && g_pGameLevel && g_pGameLevel->Cameras().GetCamEffector(cefDemo) == nullptr && !load_screen_renderer.IsActive() && !CImGuiManager::Instance().IsCapturingInputs();
}

void CActor::SetActorKeyRepeatFlag(ACTOR_DEFS::EActorKeyflags mask, bool state, bool ignore_suicide)
{
	//if (!ignore_suicide && IsActorSuicideNow())
	//{
	//	return;
	//}

	if (state)
	{
		m_iKeyFlags |= mask;
	}
	else
	{
		m_iKeyFlags &= ~mask;
	}
}

extern bool b_toggle_weapon_aim;

void CActor::ProcessKeys(CHudItem* itm)
{
	if (itm == nullptr)
	{
		m_iKeyFlags = 0;
		return;
	}

	if ((m_iKeyFlags & kfHEADLAMP) != 0 && itm->CanStartAction(this))
	{
		SwitchTorch();
		SetActorKeyRepeatFlag(kfHEADLAMP, false);
	}

	if ((m_iKeyFlags & kfNIGHTVISION) != 0 && itm->CanStartAction(this))
	{
		SwitchNightVision();
		SetActorKeyRepeatFlag(kfNIGHTVISION, false);
	}

	if ((m_iKeyFlags & kfCLEARMASK) != 0 && itm->CanStartAction(this))
	{
		ClearMask();
		SetActorKeyRepeatFlag(kfCLEARMASK, false);
	}

	CWeapon* wpn = itm->cast_weapon();
	if (wpn == nullptr)
	{
		return;
	}
	
	if (IsActionKeyPressedInGame(kWPN_ZOOM) && (wpn->GetState() == CWeapon::eIdle || wpn->GetState() == CWeapon::eFire))
	{
		if (!b_toggle_weapon_aim && wpn->CanAimNow() && !wpn->IsZoomed())
		{
			wpn->Action(kWPN_ZOOM, CMD_START);
			SetActorKeyRepeatFlag(kfUNZOOM, false);
		}
	}

	if ((m_iKeyFlags & kfUNZOOM) != 0)
	{
		if (wpn->IsZoomed())
		{
			if (wpn->CanLeaveAimNow())
			{
				if (b_toggle_weapon_aim)
				{
					wpn->Action(kWPN_ZOOM, CMD_START);
				}
				else
				{
					wpn->Action(kWPN_ZOOM, CMD_STOP);
				}
			}
		}
		else
		{
			SetActorKeyRepeatFlag(kfUNZOOM, false);
		}
	}

	if ((m_iKeyFlags & kfFIRE) != 0 && itm->CanStartAction(this))
	{
		wpn->Action(kWPN_FIRE, CMD_START);
		SetActorKeyRepeatFlag(kfFIRE, false);
	}

	if ((m_iKeyFlags & kfGLAUNCHSWITCH) != 0 && itm->CanStartAction(this))
	{
		wpn->Action(kWPN_FUNC, CMD_START);
		SetActorKeyRepeatFlag(kfGLAUNCHSWITCH, false);
	}

	if ((m_iKeyFlags & kfNEXTFIREMODE) != 0 && itm->CanStartAction(this))
	{
		wpn->Action(kWPN_FIREMODE_NEXT, CMD_START);
		SetActorKeyRepeatFlag(kfNEXTFIREMODE, false);
	}

	if ((m_iKeyFlags & kfPREVFIREMODE) != 0 && itm->CanStartAction(this))
	{
		wpn->Action(kWPN_FIREMODE_PREV, CMD_START);
		SetActorKeyRepeatFlag(kfPREVFIREMODE, false);
	}

	if ((m_iKeyFlags & kfRELOAD) != 0 && itm->CanStartAction(this))
	{
		wpn->Action(kWPN_RELOAD, CMD_START);
		SetActorKeyRepeatFlag(kfRELOAD, false);
	}

	if ((m_iKeyFlags & kfNEXTAMMO) != 0 && itm->CanStartAction(this))
	{
		wpn->Action(kWPN_NEXT, CMD_START);
		SetActorKeyRepeatFlag(kfNEXTAMMO, false);
	}

	if ((m_iKeyFlags & kfTACTICALTORCH) != 0 && itm->CanStartAction(this))
	{
		wpn->Action(kTACTICALTORCH, CMD_START);
		SetActorKeyRepeatFlag(kfTACTICALTORCH, false);
	}

	if ((m_iKeyFlags & kfLASER) != 0 && itm->CanStartAction(this))
	{
		wpn->Action(kLASER, CMD_START);
		SetActorKeyRepeatFlag(kfLASER, false);
	}

	if ((m_iKeyFlags & kfMAGCHECK) != 0 && itm->CanStartAction(this))
	{
		wpn->Action(kMAG_CHECK, CMD_START);
		SetActorKeyRepeatFlag(kfMAGCHECK, false);
	}

	if ((m_iKeyFlags & kfFIREMODECHECK) != 0 && itm->CanStartAction(this))
	{
		wpn->Action(kFIREMODE_CHECK, CMD_START);
		SetActorKeyRepeatFlag(kfFIREMODECHECK, false);
	}

	if ((m_iKeyFlags & kfCHAMBERLOAD) != 0 && itm->CanStartAction(this))
	{
		wpn->Action(kWPN_CHAMBER_LOAD, CMD_START);
		SetActorKeyRepeatFlag(kfCHAMBERLOAD, false);
	}

	if ((m_iKeyFlags & kfCHAMBERUNLOAD) != 0 && itm->CanStartAction(this))
	{
		wpn->Action(kWPN_CHAMBER_UNLOAD, CMD_START);
		SetActorKeyRepeatFlag(kfCHAMBERUNLOAD, false);
	}

	if ((m_iKeyFlags & kfCHAMBERCHECK) != 0 && itm->CanStartAction(this))
	{
		wpn->Action(kWPN_CHAMBER_CHECK, CMD_START);
		SetActorKeyRepeatFlag(kfCHAMBERCHECK, false);
	}
}

bool CActor::use_Holder(CHolderCustom* holder)
{
	if (CHolderCustom* get_holder = m_holder)
	{
		bool b = false;
		CGameObject* holderGO = get_holder->cast_game_object();
		
		if (holderGO->cast_car())
		{
			b = use_Vehicle(0);
		}
		else if (holderGO->CLS_ID == CLSID_OBJECT_W_STATMGUN || holderGO->CLS_ID == CLSID_OBJECT_HOLDER_ENT)
		{
			b = use_HolderEx(0, false);
		}

		if (inventory().ActiveItem())
		{
			if (CHudItem* hi = inventory().ActiveItem()->cast_hud_item())
			{
				hi->OnAnimationEnd(hi->GetState());
			}
		}

		return b;
	}
	else
	{
		bool b = false;
		CGameObject* holderGO = holder->cast_game_object();
		if (holder->cast_car())
		{
			b = use_Vehicle(holder);
		}

		if (holderGO->CLS_ID == CLSID_OBJECT_W_STATMGUN || holderGO->CLS_ID == CLSID_OBJECT_HOLDER_ENT)
		{
			b = use_HolderEx(holder, false);
		}
		
		if (b)
		{
			//used succesfully
			// switch off torch...
			CAttachableItem *I = CAttachmentOwner::attachedItem(CLSID_DEVICE_TORCH);
			if (I != nullptr)
			{
				if (CTorch* torch = I->cast_torch())
				{
					torch->Switch(false);
				}
			}
		}

		if (inventory().ActiveItem())
		{
			if (CHudItem* hi = inventory().ActiveItem()->cast_hud_item())
			{
				hi->OnAnimationEnd(hi->GetState());
			}
		}

		return b;
	}
}

void CActor::ActorUse()
{
	if (IsActorBurning() /* && !IsActorControlled()*/ && HudAnimator() != nullptr && HudAnimator()->BurnAnimator() != nullptr)
	{
		HudAnimator()->BurnAnimator()->StartAnimator();
		m_need_fire_particle = true;
		return;
	}

	if (HudAnimator() && HudAnimator()->IsAnyAnimatorActive())
	{
		return;
	}

	if (m_holder != nullptr)
	{
		CGameObject* GO = m_holder->cast_game_object();
		NET_Packet P;
		CGameObject::u_EventGen(P, GEG_PLAYER_DETACH_HOLDER, ID());
		P.w_u16(GO->ID());
		CGameObject::u_EventSend(P);
		return;
	}
				
	if(character_physics_support()->movement()->PHCapture())
		character_physics_support()->movement()->PHReleaseObject();

	if (m_pUsableObject && nullptr == m_pObjectWeLookingAt->cast_inventory_item())
	{
		m_pUsableObject->use(this);
	}

	if (m_pObjectWeLookingAt != nullptr)
	{
		if (CInteractiveObject* oCInteractiveObject = smart_cast<CInteractiveObject*>(m_pObjectWeLookingAt))
		{
			oCInteractiveObject->OnUse();
		}
	}
	
	if (m_pInvBoxWeLookingAt && m_pInvBoxWeLookingAt->nonscript_usable() && m_use_disabled == false)
	{
		if (IsGameTypeSingleCompatible())
		{
			if (!m_pInvBoxWeLookingAt->closed())
			{
				CurrentGameUI()->StartCarBody(this, m_pInvBoxWeLookingAt);
			}
		}
		return;
	}

	if (!m_pUsableObject || m_pUsableObject->nonscript_usable())
	{
		bool isKeyHeld = pInput->GetControllerMode() ? pInput->iGetAsyncGamepadKeyState(get_action_dik(kSPRINT_TOGGLE)) : Level().IR_GetKeyState(SDL_SCANCODE_LSHIFT);
		if (m_pPersonWeLookingAt != nullptr)
		{
			CEntityAlive* pEntityAliveWeLookingAt = m_pPersonWeLookingAt->cast_entity_alive();

			VERIFY(pEntityAliveWeLookingAt);

			if (IsGameTypeSingleCompatible())
			{			
				CBaseMonster* pMonster = pEntityAliveWeLookingAt != nullptr ? pEntityAliveWeLookingAt->cast_base_monster() : nullptr;
				const static bool isMonstersInventory = EngineExternal()[EEngineExternalGame::EnableMonstersInventory];
				bool TestMonster = (pMonster == nullptr) || (pMonster != nullptr && isMonstersInventory);

				if (pEntityAliveWeLookingAt->g_Alive())
				{
					TryToTalk();
				}
				else
				{
					//только если находимся в режиме single
					CUIGameCustom* pGameSP = CurrentGameUI();
					if (pGameSP && TestMonster && m_use_disabled == false)
					{
						if (!m_pPersonWeLookingAt->deadbody_closed_status())
						{
							if (pEntityAliveWeLookingAt->AlreadyDie() && pEntityAliveWeLookingAt->GetLevelDeathTime() + 3000 < Device.dwTimeGlobal && !isKeyHeld)
							{
								pGameSP->StartCarBody(this, m_pPersonWeLookingAt);
							}
						}
					}
				}
			}
		}

		collide::rq_result& RQ = HUD().GetCurrentRayQuery();
		CPhysicsShellHolder* object = RQ.O != nullptr ? RQ.O->cast_physics_shell_holder() : nullptr;
		u16 element = BI_NONE;
		if (object) 
		{
			element = (u16)RQ.element;
		}

		if (object && isKeyHeld)
		{
			bool b_allow = !!pSettings->line_exist("ph_capture_visuals", object->cNameVisual());
			if (b_allow && !character_physics_support()->movement()->PHCapture())
			{
				character_physics_support()->movement()->PHCaptureObject(object, element);

			}
		}
		else
		{
			if (object != nullptr && object->cast_holder_custom() != nullptr)
			{
				NET_Packet P;
				CGameObject::u_EventGen(P, GEG_PLAYER_ATTACH_HOLDER, ID());
				P.w_u16(object->ID());
				CGameObject::u_EventSend(P);
				return;
			}

		}
	}

	if (g_Alive())
	{
		pPickup->SetPickupMode(true);
	}
}

bool CActor::HUDview()const
{
	return IsFocused() && (cam_active == eacFirstEye) && ((!m_holder) || (m_holder && m_holder->allowWeapon() && m_holder->HUDView()));
}

namespace
{
void ActorActivateWeaponCycleSlot(CActor& actor, u16 slotId)
{
	const u16 gameAction = InventoryWeaponSlotToGameAction(slotId);
	if (gameAction != kWeaponCycleNoGameAction)
	{
		actor.IR_OnKeyboardPress(get_action_dik(static_cast<EGameActions>(gameAction)));
	}
	else
	{
		actor.inventory().ActiveWeapon(slotId);
	}
}
} // namespace

void	CActor::OnNextWeaponSlot()
{
	u32 activeSlot = inventory().GetActiveSlot();
	if (activeSlot == NO_ACTIVE_SLOT)
	{
		activeSlot = inventory().GetPrevActiveSlot();
	}

	if (activeSlot == NO_ACTIVE_SLOT)
	{
		activeSlot = KNIFE_SLOT;
	}

	xr_span<const u16> const cycle = InventoryWeaponCycleSlots();
	const u32 cycleCount = static_cast<u32>(cycle.size());
	if (cycleCount == 0)
	{
		return;
	}

	s32 curIndex = -1;
	for (u32 i = 0; i < cycleCount; ++i)
	{
		if (cycle[i] == activeSlot)
		{
			curIndex = static_cast<s32>(i);
			break;
		}
	}

	const u32 startIndex = curIndex < 0 ? 0u : static_cast<u32>(curIndex + 1);
	for (u32 i = startIndex; i < cycleCount; ++i)
	{
		if (inventory().ItemFromSlot(cycle[i]))
		{
			ActorActivateWeaponCycleSlot(*this, cycle[i]);
			return;
		}
	}
}

void	CActor::OnPrevWeaponSlot()
{
	u32 activeSlot = inventory().GetActiveSlot();
	if (activeSlot == NO_ACTIVE_SLOT)
	{
		activeSlot = inventory().GetPrevActiveSlot();
	}

	if (activeSlot == NO_ACTIVE_SLOT)
	{
		activeSlot = KNIFE_SLOT;
	}

	xr_span<const u16> const cycle = InventoryWeaponCycleSlots();
	const u32 cycleCount = static_cast<u32>(cycle.size());
	if (cycleCount == 0)
	{
		return;
	}

	s32 curIndex = -1;
	for (u32 i = 0; i < cycleCount; ++i)
	{
		if (cycle[i] == activeSlot)
		{
			curIndex = static_cast<s32>(i);
			break;
		}
	}

	const s32 startIndex = curIndex < 0 ? static_cast<s32>(cycleCount) - 1 : curIndex - 1;
	for (s32 i = startIndex; i >= 0; --i)
	{
		if (inventory().ItemFromSlot(cycle[i]))
		{
			ActorActivateWeaponCycleSlot(*this, cycle[i]);
			return;
		}
	}
}

float CActor::GetLookFactor()
{
	if (m_input_external_handler)
	{
		return m_input_external_handler->mouse_scale_factor();
	}


	float factor = 1.f;

	PIItem pItem = inventory().ActiveItem();
	
	static bool use_weapon_factor = EngineExternal()[EEngineExternalGame::EnableWeaponAffectsOnMouseSensitivity];

	if (pItem && use_weapon_factor)
	{
		factor *= pItem->GetControlInertionFactor();
	}

	VERIFY(!fis_zero(factor));

	return factor;
}

void CActor::set_input_external_handler(CActorInputHandler *handler) 
{
	// clear state
	if (handler) 
		mstate_wishful			= 0;

	// release fire button
	if (handler)
		IR_OnKeyboardRelease	(get_binded_action(kWPN_FIRE));

	// set handler
	m_input_external_handler	= handler;
}

void CActor::StartNVPPE()
{
	if (Cameras().GetPPEffector(EEffectorPPType(10337)) == nullptr)
	{
		CPostprocessAnimator* pp = new CPostprocessAnimator(10337, false);
		pp->Load("night_vision.ppe");
		Cameras().AddPPEffector(pp);
	}
}

void CActor::SwitchNightVision()
{
	PIItem item_from_slot = inventory().ItemFromSlot(NVG_SLOT);
	CNVG* oNVG = item_from_slot != nullptr ? smart_cast<CNVG*>(item_from_slot) : nullptr;

	bool has_nvg = GetOutfit() && GetOutfit()->GetNV_Sect().size() > 0 || GetHelmet() && GetHelmet()->GetNV_Sect().size() > 0 || oNVG;

	if (!has_nvg)
	{
		return;
	}

	if (CHudStateAnimator* state_animator = HudAnimator()->CurrentAnimator() != nullptr ? HudAnimator()->CurrentAnimator()->cast_hud_state_animator() : nullptr)
	{
		if (state_animator->GetState() == CHudStateAnimator::EAnimatorStates::eIdle)
		{
			if (state_animator->m_eAnimationsFlags.test(CHudStateAnimator::EAnimationsFlags::af_nvg))
			{
				state_animator->m_eDevicesFlags.set(CHudItem::EDevicesFlags::df_nvg, true);
				state_animator->SetState(CHudStateAnimator::EAnimatorStates::eDeviceSwitch);
				return;
			}
		}

		return;
	}

	PIItem active_item = inventory().ActiveItem();
	CHudItem* itm = active_item != nullptr ? active_item->cast_hud_item() : nullptr;
	CWeapon* wpn = itm != nullptr ? itm->cast_weapon() : nullptr;
	CCustomDevice* dev = GetDevice();

	if (itm != nullptr && dev != nullptr)
	{
		if (wpn != nullptr && wpn->IsZoomed())
		{
			return;
		}

		if (itm->m_eAnimationsFlags.test(CHudItem::EAnimationsFlags::af_nvg) && dev->m_eAnimationsFlags.test(CCustomDevice::EAnimationsFlags::af_nvg))
		{
			if (!itm->SetKeyRepeatFlag(ACTOR_DEFS::EActorKeyflags::kfNIGHTVISION))
			{
				return;
			}

			if (itm->GetState() != CHUDState::eIdle || dev->GetState() != CCustomDevice::eIdle)
			{
				return;
			}

			itm->m_eDevicesFlags.set(CHudItem::EDevicesFlags::df_nvg, true);
			itm->SwitchState(CHUDState::eDeviceSwitch);
			dev->m_eDevicesFlags.set(CCustomDevice::EDevicesFlags::df_nvg, true);
			dev->SwitchState(CCustomDevice::eDeviceSwitch);
			return;
		}
	}

	if (itm != nullptr)
	{
		if (wpn != nullptr && wpn->IsZoomed())
		{
			return;
		}

		if (itm->m_eAnimationsFlags.test(CHudItem::EAnimationsFlags::af_nvg))
		{
			if (!itm->SetKeyRepeatFlag(ACTOR_DEFS::EActorKeyflags::kfNIGHTVISION))
			{
				return;
			}

			if (itm->GetState() != CHUDState::eIdle)
			{
				return;
			}

			itm->m_eDevicesFlags.set(CHudItem::EDevicesFlags::df_nvg, true);
			itm->SwitchState(CHUDState::eDeviceSwitch);
			return;
		}
	}

	if (dev != nullptr)
	{
		if (dev->m_eAnimationsFlags.test(CCustomDevice::EAnimationsFlags::af_nvg))
		{
			if (dev->GetState() != CCustomDevice::eIdle)
			{
				return;
			}

			dev->m_eDevicesFlags.set(CCustomDevice::EDevicesFlags::df_nvg, true);
			dev->SwitchState(CCustomDevice::eDeviceSwitch);
			return;
		}
	}

	if (GetNightVisionEffector())
	{
		if (m_sNVGAnimator.size() > 0)
		{
			if (HudAnimator() && !HudAnimator()->ItemAnimator()->IsActive())
			{
				StartAnimator(m_sNVGAnimator);
				HudAnimator()->ItemAnimator()->SetLeftCallback({ GetNightVisionEffector(), &CNightVisionEffector::SwitchNightVision });
				HudAnimator()->ItemAnimator()->SetRightCallback({ this, &CActor::StartNVPPE });
			}
		}
		else
		{
			GetNightVisionEffector()->SwitchNightVision();
		}
	}
}

void CActor::SwitchTorch()
{
	static const bool TorchOnlyInOutfit = EngineExternal()[EEngineExternalGame::EnableTorchOnlyInOutfit];
	bool has_torch = TorchOnlyInOutfit && (GetOutfit() && GetOutfit()->IsTorchAvailable() || GetHelmet() && GetHelmet()->IsTorchAvailable()) || !TorchOnlyInOutfit;

	if (!has_torch)
	{
		return;
	}

	PIItem item_from_slot = inventory().ItemFromSlot(TORCH_SLOT);

	if (CTorch* torch = item_from_slot ? item_from_slot->cast_torch() : nullptr)
	{
		if (CHudStateAnimator* state_animator = HudAnimator()->CurrentAnimator() != nullptr ? HudAnimator()->CurrentAnimator()->cast_hud_state_animator() : nullptr)
		{
			if (state_animator->GetState() == CHudStateAnimator::EAnimatorStates::eIdle)
			{
				if (state_animator->m_eAnimationsFlags.test(CHudStateAnimator::EAnimationsFlags::af_torch))
				{
					state_animator->m_eDevicesFlags.set(CHudItem::EDevicesFlags::df_torch, true);
					state_animator->SetState(CHudStateAnimator::EAnimatorStates::eDeviceSwitch);
					return;
				}
			}

			return;
		}

		PIItem active_item = inventory().ActiveItem();
		CHudItem* itm = active_item != nullptr ? active_item->cast_hud_item() : nullptr;
		CWeapon* wpn = itm != nullptr ? itm->cast_weapon() : nullptr;
		CCustomDevice* dev = GetDevice();

		if (itm != nullptr && dev != nullptr)
		{
			if (wpn && wpn->IsZoomed())
			{
				return;
			}

			if (itm->m_eAnimationsFlags.test(CHudItem::EAnimationsFlags::af_torch) && dev->m_eAnimationsFlags.test(CCustomDevice::EAnimationsFlags::af_torch))
			{
				if (!itm->SetKeyRepeatFlag(ACTOR_DEFS::EActorKeyflags::kfHEADLAMP))
				{
					return;
				}

				if (itm->GetState() != CHUDState::eIdle || dev->GetState() != CCustomDevice::eIdle)
				{
					return;
				}

				itm->m_eDevicesFlags.set(CHudItem::EDevicesFlags::df_torch, true);
				itm->SwitchState(CHUDState::eDeviceSwitch);
				dev->m_eDevicesFlags.set(CCustomDevice::EDevicesFlags::df_torch, true);
				dev->SwitchState(CCustomDevice::eDeviceSwitch);
				return;
			}
		}

		if (itm != nullptr)
		{
			if (wpn && wpn->IsZoomed())
			{
				return;
			}

			if (itm->m_eAnimationsFlags.test(CHudItem::EAnimationsFlags::af_torch))
			{
				if (!itm->SetKeyRepeatFlag(ACTOR_DEFS::EActorKeyflags::kfHEADLAMP))
				{
					return;
				}

				if (itm->GetState() != CHUDState::eIdle)
				{
					return;
				}

				itm->m_eDevicesFlags.set(CHudItem::EDevicesFlags::df_torch, true);
				itm->SwitchState(CHUDState::eDeviceSwitch);
				return;
			}
		}

		if (dev != nullptr)
		{
			if (dev->m_eAnimationsFlags.test(CCustomDevice::EAnimationsFlags::af_torch))
			{
				if (dev->GetState() != CCustomDevice::eIdle)
				{
					return;
				}

				dev->m_eDevicesFlags.set(CCustomDevice::EDevicesFlags::df_torch, true);
				dev->SwitchState(CCustomDevice::eDeviceSwitch);
				return;
			}
		}

		if (m_sHeadlampAnimator.size() > 0)
		{
			if (HudAnimator() && !HudAnimator()->ItemAnimator()->IsActive())
			{
				StartAnimator(m_sHeadlampAnimator);
				HudAnimator()->ItemAnimator()->SetLeftCallback({ torch, &CTorch::Switch });
			}
		}
		else
		{
			torch->Switch();
		}
	}
}

void CActor::ClearMask()
{
	bool has_glass = GetOutfit() != nullptr && GetOutfit()->GlassPresent || GetHelmet() != nullptr && GetHelmet()->GlassPresent;
	
	if (!has_glass)
	{
		return;
	}

	if (CHudStateAnimator* state_animator = HudAnimator()->CurrentAnimator() != nullptr ? HudAnimator()->CurrentAnimator()->cast_hud_state_animator() : nullptr)
	{
		if (state_animator->GetState() == CHudStateAnimator::EAnimatorStates::eIdle)
		{
			if (state_animator->m_eAnimationsFlags.test(CHudStateAnimator::EAnimationsFlags::af_clear_mask))
			{
				state_animator->m_eDevicesFlags.set(CHudItem::EDevicesFlags::df_clear_mask, true);
				state_animator->SetState(CHudStateAnimator::EAnimatorStates::eDeviceSwitch);
				return;
			}
		}

		return;
	}

	PIItem active_item = inventory().ActiveItem();
	CHudItem* itm = active_item != nullptr ? active_item->cast_hud_item() : nullptr;
	CWeapon* wpn = itm != nullptr ? itm->cast_weapon() : nullptr;
	CCustomDevice* dev = GetDevice();

	if (itm != nullptr && dev != nullptr)
	{
		if (wpn != nullptr && wpn->IsZoomed())
		{
			return;
		}

		if (itm->m_eAnimationsFlags.test(CHudItem::EAnimationsFlags::af_clear_mask) && dev->m_eAnimationsFlags.test(CCustomDevice::EAnimationsFlags::af_clear_mask))
		{
			if (!itm->SetKeyRepeatFlag(ACTOR_DEFS::EActorKeyflags::kfCLEARMASK))
			{
				return;
			}

			if (itm->GetState() != CHUDState::eIdle || dev->GetState() != CCustomDevice::eIdle)
			{
				return;
			}

			itm->m_eDevicesFlags.set(CHudItem::EDevicesFlags::df_clear_mask, true);
			itm->SwitchState(CHUDState::eDeviceSwitch);
			dev->m_eDevicesFlags.set(CCustomDevice::EDevicesFlags::df_clear_mask, true);
			dev->SwitchState(CCustomDevice::eDeviceSwitch);
			return;
		}
	}

	if (itm != nullptr)
	{
		if (wpn != nullptr && wpn->IsZoomed())
		{
			return;
		}

		if (itm->m_eAnimationsFlags.test(CHudItem::EAnimationsFlags::af_clear_mask))
		{
			if (!itm->SetKeyRepeatFlag(ACTOR_DEFS::EActorKeyflags::kfCLEARMASK))
			{
				return;
			}

			if (itm->GetState() != CHUDState::eIdle)
			{
				return;
			}

			itm->m_eDevicesFlags.set(CHudItem::EDevicesFlags::df_clear_mask, true);
			itm->SwitchState(CHUDState::eDeviceSwitch);
			return;
		}
	}

	if (dev != nullptr)
	{
		if (dev->m_eAnimationsFlags.test(CCustomDevice::EAnimationsFlags::af_clear_mask))
		{
			if (dev->GetState() != CCustomDevice::eIdle)
			{
				return;
			}

			dev->m_eDevicesFlags.set(CCustomDevice::EDevicesFlags::df_clear_mask, true);
			dev->SwitchState(CCustomDevice::eDeviceSwitch);
			return;
		}
	}

	if (m_sClearMaskAnimator.size() > 0)
	{
		if (HudAnimator() && !HudAnimator()->ItemAnimator()->IsActive())
		{
			StartAnimator(m_sClearMaskAnimator);
			HudAnimator()->ItemAnimator()->SetLeftCallback({ this, &CActor::ClearMaskCB });
		}
	}
}

void CActor::ClearMaskCB()
{
	//RAVLIK TO LVUTNER: KOGDA KAPLI NA EBAL'NIKE?
}

void CActor::MakeKick()
{
	PIItem knife_item = inventory().ItemFromSlot(KNIFE_SLOT);
	if (CWeaponKnife* pWeaponKnife = knife_item != nullptr ? knife_item->cast_weapon_knife() : nullptr)
	{
		pWeaponKnife->FastKick();
	}
}

#ifndef MASTER_GOLD
#include "../xrPhysics/IPHWorld.h"
collide::rq_result GetPickResult(Fvector pos, Fvector dir, float range, CObject* ignore);

void CActor::NoClipFly(int cmd)
{
	Fvector cur_pos;
	cur_pos.set(0, 0, 0);
	CCar* pCar = m_holder ? m_holder->cast_car() : nullptr;

	if (pInput->iGetAsyncKeyState(SDL_SCANCODE_DELETE))
	{
		collide::rq_result RQ = GetPickResult(Device.vCameraPosition, Device.vCameraDirection, 1000.0f, this);
		if (RQ.element>=0)
		{
			if (pCar)
			{
				pCar->m_pPhysicsShell->Disable();
				pCar->m_pPhysicsShell->DisableCollision();
				pCar->m_pPhysicsShell->SetGlTransformDynamic(pCar->XFORM().translate(Fvector(Device.vCameraPosition).mad(Fvector(Device.vCameraDirection), RQ.range)));
				pCar->correct_spawn_pos();
				pCar->m_pPhysicsShell->set_LinearVel(zero_vel);
				pCar->m_pPhysicsShell->set_AngularVel(zero_vel);
				pCar->m_pPhysicsShell->GetGlobalTransformDynamic(&XFORM());
				pCar->m_pPhysicsShell->Enable();
			}
			else if (m_pPhysicsShell)
			{
				m_pPhysicsShell->Disable();
				m_pPhysicsShell->DisableCollision();
				m_pPhysicsShell->SetGlTransformDynamic(XFORM().translate(Fvector(Device.vCameraPosition).mad(Fvector(Device.vCameraDirection), RQ.range)));
				correct_spawn_pos();
				m_pPhysicsShell->set_LinearVel(zero_vel);
				m_pPhysicsShell->set_AngularVel(zero_vel);
				m_pPhysicsShell->GetGlobalTransformDynamic(&XFORM());
				m_pPhysicsShell->Enable();
			}
			else
				SetPhPosition(XFORM().translate(Fvector(Device.vCameraPosition).mad(Fvector(Device.vCameraDirection), RQ.range)));
		}
	}

	switch (cmd)
	{
		case kJUMP:
		{
			Fvector top;
			top.set(Device.vCameraTop);
			cur_pos.mad(top, GetNoclipSpeedScale() / 2.0f);
			if (m_pPhysicsShell)
				m_pPhysicsShell->applyImpulseTrace(cur_pos, top, (GetNoclipSpeedScale() * m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
			if (pCar && pCar->m_pPhysicsShell)
				pCar->m_pPhysicsShell->applyImpulse(top, (GetNoclipSpeedScale() * pCar->m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
		}break;
		case kCROUCH:
		{
			Fvector down;
			down.set(Device.vCameraTop).invert();
			cur_pos.mad(down, GetNoclipSpeedScale() / 2.0f);
			if (m_pPhysicsShell)
				m_pPhysicsShell->applyImpulseTrace(cur_pos, down, (GetNoclipSpeedScale() * m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
			if (pCar && pCar->m_pPhysicsShell)
				pCar->m_pPhysicsShell->applyImpulse(down, (GetNoclipSpeedScale() * pCar->m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
		}break;
		case kFWD:
		{
			cur_pos.mad(Device.vCameraDirection, GetNoclipSpeedScale() / 2.0f);
			if (m_pPhysicsShell)
				m_pPhysicsShell->applyImpulseTrace(cur_pos, Device.vCameraDirection, (GetNoclipSpeedScale() * m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
			if (pCar && pCar->m_pPhysicsShell)
				pCar->m_pPhysicsShell->applyImpulse(Device.vCameraDirection, (GetNoclipSpeedScale() * pCar->m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
		}break;
		case kBACK:
		{
			cur_pos.mad(Fvector(Device.vCameraDirection).invert(), GetNoclipSpeedScale() / 2.0f);
			if (m_pPhysicsShell)
				m_pPhysicsShell->applyImpulseTrace(cur_pos, Fvector(Device.vCameraDirection).invert(), (GetNoclipSpeedScale() * m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
			if (pCar && pCar->m_pPhysicsShell)
				pCar->m_pPhysicsShell->applyImpulse(Fvector(Device.vCameraDirection).invert(), (GetNoclipSpeedScale() * pCar->m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
		}break;
		case kR_STRAFE:
		{
			Fvector right;
			right.set(Device.vCameraRight);
			cur_pos.mad(right, GetNoclipSpeedScale() / 2.0f);
			if (m_pPhysicsShell)
				m_pPhysicsShell->applyImpulseTrace(cur_pos, right, (GetNoclipSpeedScale() * m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
			if (pCar && pCar->m_pPhysicsShell)
				pCar->m_pPhysicsShell->applyImpulse(right, (GetNoclipSpeedScale() * pCar->m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
		}break;
		case kL_STRAFE:
		{
			Fvector left;
			left.set(Device.vCameraRight).invert();
			cur_pos.mad(left, GetNoclipSpeedScale() / 2.0f);
			if (m_pPhysicsShell)
				m_pPhysicsShell->applyImpulseTrace(cur_pos, left, (GetNoclipSpeedScale() * m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
			if (pCar && pCar->m_pPhysicsShell)
				pCar->m_pPhysicsShell->applyImpulse(left, (GetNoclipSpeedScale() * pCar->m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
		}break;
		case kCAM_1:
			cam_Set(eacFirstEye);
			break;
		case kCAM_2:
			cam_Set(eacLookAt);
			break;
		case kCAM_3:
			cam_Set(eacFreeLook);
			break;
		case kNIGHT_VISION:
			SwitchNightVision();
			break;
		case kTORCH:
			SwitchTorch();
			break;
		case kUSE:
			ActorUse();
			break;
		case kDETECTOR:
		{
			if ((mstate_real&mcClimb))
			{
				break;
			}

			PIItem dev_active = inventory().ItemFromSlot(DEVICE_SLOT);
			if (dev_active)
			{
				if (CCustomDevice* dev = dev_active->cast_custom_device())
				{
					dev->switch_device();
				}
			}
		}break;
		case kWPN_RADIAL_MENU:
		{
			CUIRadialMenuWeapon* RMW = CurrentGameUI()->RadialMenuWeapon();
			if (RMW->isInitialized && !RMW->IsShown())
			{
				RMW->ShowDialog(false);
			}
			break;
		}
	}
	if(!m_pPhysicsShell && !pCar)
		SetPhPosition(XFORM().translate_add(cur_pos.mul(GetNoclipSpeedScale() * Device.fTimeDelta)));

	if(inventory().Action((u16)cmd, CMD_START))return;
}

void CActor::NoClipFlyStick(Fvector2 value)
{
	Fvector cur_pos;
	cur_pos.set(0, 0, 0);
	CCar* pCar = m_holder ? m_holder->cast_car() : nullptr;

	if (!fis_zero(value.x))
	{
		Fvector camDir = Device.vCameraRight;
		if (value.x < 0.0f)
		{
			camDir.invert();
		}
		cur_pos.mad(camDir, (GetNoclipSpeedScale() / 2.0f) * std::abs(value.x));
		if (m_pPhysicsShell)
		{
			m_pPhysicsShell->applyImpulseTrace(cur_pos, camDir, (GetNoclipSpeedScale() * std::abs(value.x) * m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
		}
		if (pCar && pCar->m_pPhysicsShell)
		{
			pCar->m_pPhysicsShell->applyImpulse(camDir, (GetNoclipSpeedScale() * std::abs(value.x) * pCar->m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
		}
	}

	if (!fis_zero(value.y))
	{
		Fvector camDir = Device.vCameraDirection;
		if (value.y < 0.0f)
		{
			camDir.invert();
		}
		cur_pos.mad(camDir, GetNoclipSpeedScale() / 2.0f * std::abs(value.y));
		if (m_pPhysicsShell)
		{
			m_pPhysicsShell->applyImpulseTrace(cur_pos, camDir, (GetNoclipSpeedScale() * std::abs(value.y) * m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
		}
		if (pCar && pCar->m_pPhysicsShell)
		{
			pCar->m_pPhysicsShell->applyImpulse(camDir, (GetNoclipSpeedScale() * std::abs(value.y) * pCar->m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
		}
	}

	if(!m_pPhysicsShell && !pCar)
		SetPhPosition(XFORM().translate_add(cur_pos.mul(GetNoclipSpeedScale() * Device.fTimeDelta)));
}


void CActor::NoClipFlyGamepad(int cmd)
{
	Fvector cur_pos;
	cur_pos.set(0, 0, 0);
	CCar* pCar = m_holder ? m_holder->cast_car() : nullptr;

	switch (cmd)
	{
		case kJUMP:
		{
			Fvector top;
			top.set(Device.vCameraTop);
			cur_pos.mad(top, GetNoclipSpeedScale() / 2.0f);
			if (m_pPhysicsShell)
			{
				m_pPhysicsShell->applyImpulseTrace(cur_pos, top, (GetNoclipSpeedScale() * m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
			}
			if (pCar && pCar->m_pPhysicsShell)
			{
				pCar->m_pPhysicsShell->applyImpulse(top, (GetNoclipSpeedScale() * pCar->m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
			}
		}
		break;
		case kSPRINT_TOGGLE:
		{
			Fvector down;
			down.set(Device.vCameraTop).invert();
			cur_pos.mad(down, GetNoclipSpeedScale() / 2.0f);
			if (m_pPhysicsShell)
			{
				m_pPhysicsShell->applyImpulseTrace(cur_pos, down, (GetNoclipSpeedScale() * m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
			}
			if (pCar && pCar->m_pPhysicsShell)
			{
				pCar->m_pPhysicsShell->applyImpulse(down, (GetNoclipSpeedScale() * pCar->m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
			}
		}
		break;
		case kTORCH:
			SwitchTorch();
			break;
		case kUSE:
			ActorUse();
			break;
		case kCROUCH:
		{
			collide::rq_result RQ = GetPickResult(Device.vCameraPosition, Device.vCameraDirection, 1000.0f, this);
			if (RQ.element >= 0)
			{
				if (pCar)
				{
					pCar->m_pPhysicsShell->Disable();
					pCar->m_pPhysicsShell->DisableCollision();
					pCar->m_pPhysicsShell->SetGlTransformDynamic(pCar->XFORM().translate(Fvector(Device.vCameraPosition).mad(Fvector(Device.vCameraDirection), RQ.range)));
					pCar->correct_spawn_pos();
					pCar->m_pPhysicsShell->set_LinearVel(zero_vel);
					pCar->m_pPhysicsShell->set_AngularVel(zero_vel);
					pCar->m_pPhysicsShell->GetGlobalTransformDynamic(&XFORM());
					pCar->m_pPhysicsShell->Enable();
				}
				else if (m_pPhysicsShell)
				{
					m_pPhysicsShell->Disable();
					m_pPhysicsShell->DisableCollision();
					m_pPhysicsShell->SetGlTransformDynamic(XFORM().translate(Fvector(Device.vCameraPosition).mad(Fvector(Device.vCameraDirection), RQ.range)));
					correct_spawn_pos();
					m_pPhysicsShell->set_LinearVel(zero_vel);
					m_pPhysicsShell->set_AngularVel(zero_vel);
					m_pPhysicsShell->GetGlobalTransformDynamic(&XFORM());
					m_pPhysicsShell->Enable();
				}
				else
				{
					SetPhPosition(XFORM().translate(Fvector(Device.vCameraPosition).mad(Fvector(Device.vCameraDirection), RQ.range)));
				}
			}
			break;
		}
		case kWPN_RADIAL_MENU:
		{
			CUIRadialMenuWeapon* RMW = CurrentGameUI()->RadialMenuWeapon();
			if (RMW->isInitialized && !RMW->IsShown())
			{
				RMW->ShowDialog(false);
			}
			break;
		}
	}
	if (!m_pPhysicsShell && !pCar)
	{
		SetPhPosition(XFORM().translate_add(cur_pos.mul(GetNoclipSpeedScale() * Device.fTimeDelta)));
	}

	if (inventory().Action((u16)cmd, CMD_START))
	{
		return;
	}
}

#endif //DEBUG

void CActor::ActorQuickSlotUse(int cmd)
{
	// Show quick slots panel on item use
	if (CurrentGameUI() && CurrentGameUI()->UIMainIngameWnd)
	{
		CurrentGameUI()->UIMainIngameWnd->ShowQuickSlotsPanel();
	}

	if (HudAnimator() && HudAnimator()->ItemAnimator()->IsActive())
	{
		return;
	}

	if (!CurrentGameUI()->ActorMenu() || !CurrentGameUI()->ActorMenu()->m_pQuickSlot)
	{
		return;
	}

	const shared_str& item_name = g_quick_use_slots[cmd - kQUICK_USE_1];
	if (item_name.size())
	{
		PIItem best_itm = nullptr;

		for (auto& it : inventory().m_ruck)
		{
			if (it->m_section_id == item_name && (best_itm == nullptr || it->GetCondition() < best_itm->GetCondition()))
			{
				best_itm = it;
			}
		}

		if (best_itm != nullptr)
		{
			IsGameTypeSingle() ? inventory().Eat(best_itm) : inventory().ClientEat(best_itm);

			SDrawStaticStruct* _s = CurrentGameUI()->AddCustomStatic("item_used", true);
			string1024 str = {};

			xr_strconcat(str, *g_pStringTable->translate("st_item_used"), ": ", best_itm->NameItem());
			_s->wnd()->TextItemControl()->SetText(str);

			CurrentGameUI()->ActorMenu()->m_pQuickSlot->ReloadReferences(this);
		}
	}
}