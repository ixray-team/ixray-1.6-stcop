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
#include "clsid_game.h"
#include "HUDManager.h"
#include "Weapon.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "../xrEngine/XR_IOConsole.h"
#include "HUDAnimItem.h"
#include "CustomOutfit.h"
#include "ActorHelmet.h"

extern u32 hud_adj_mode;

void CActor::IR_OnKeyboardPress(int cmd)
{
	if(hud_adj_mode && pInput->iGetAsyncKeyState(SDL_SCANCODE_LSHIFT))	return;

	if (Remote())		return;
	if (IsTalking())	return;
	if (m_input_external_handler && !m_input_external_handler->authorized(cmd))	return;
	if (load_screen_renderer.IsActive()) {
		return;
	}

	switch (cmd)
	{
	case kWPN_FIRE:
		{
			if( (mstate_wishful & mcLookout) && !IsGameTypeSingle() )
				return;

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

	if (!g_Alive()) return;

	if(m_holder && kUSE != cmd)
	{
		m_holder->OnKeyboardPress			(cmd);
		if(m_holder->allowWeapon() && inventory().Action((u16)cmd, CMD_START))		return;
		return;
	}else
		if(inventory().Action((u16)cmd, CMD_START))					return;

#ifndef MASTER_GOLD
	if(psActorFlags.test(AF_NO_CLIP))
	{
		NoClipFly(cmd);
		return;
	}
#endif //DEBUG

	bool is_suicide = IsActorControlled() || IsActorSuicideNow() || IsActorPlanningSuicide();

	switch(cmd)
	{
	case kJUMP:		
		{
			if (is_suicide)
				break;

			mstate_wishful |= mcJump;
		}break;
	case kSPRINT_TOGGLE:	
		{
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
	case kNIGHT_VISION:
		{
			SwitchNightVision();
			break;
		}
	case kTORCH:
		{
			SwitchTorch();
			break;
		}

	case kDETECTOR:
		{
			PIItem det_active					= inventory().ItemFromSlot(DETECTOR_SLOT);
			if(det_active)
			{
				CCustomDetector* det			= smart_cast<CCustomDetector*>(det_active);
				if(det)
					det->switch_detector();
				return;
			}
		}break;
	case kUSE:
		ActorUse();
		break;
	case kDROP:
		b_DropActivated			= TRUE;
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

	case kQUICK_USE_1:
	case kQUICK_USE_2:
	case kQUICK_USE_3:
	case kQUICK_USE_4:
		{
			if (is_suicide)
				break;

			if (eateable_to_delete != nullptr)
				break;

			if (smart_cast<CHUDAnimItem*>(inventory().ActiveItem()) != nullptr)
				break;

			const shared_str& item_name		= g_quick_use_slots[cmd-kQUICK_USE_1];
			if(item_name.size())
			{
				PIItem best_itm = nullptr;

				for (auto& it : inventory().m_ruck)
				{
					if (it->m_section_id == item_name && (best_itm == nullptr || it->GetCondition() < best_itm->GetCondition()))
						best_itm = it;
				}

				if (best_itm != nullptr)
				{
					if (IsGameTypeSingle())
						inventory().Eat(best_itm);
					else
						inventory().ClientEat(best_itm);
					
					SDrawStaticStruct* _s = CurrentGameUI()->AddCustomStatic("item_used", true);
					string1024 str = {};
					xr_strconcat(str,*g_pStringTable->translate("st_item_used"),": ", best_itm->NameItem());
					_s->wnd()->TextItemControl()->SetText(str);
					
					CurrentGameUI()->ActorMenu().m_pQuickSlot->ReloadReferences(this);
				}
			}
		}break;
	}
}

void CActor::IR_OnMouseWheel(int direction)
{
	if(hud_adj_mode)
	{
		g_player_hud->tune	(Ivector().set(0,0,direction));
		return;
	}

	if(inventory().Action( (direction>0)? (u16)kWPN_ZOOM_DEC:(u16)kWPN_ZOOM_INC , CMD_START)) return;


	if (direction>0)
		OnNextWeaponSlot				();
	else
		OnPrevWeaponSlot				();
}

void CActor::IR_OnKeyboardRelease(int cmd)
{
	if(hud_adj_mode && pInput->iGetAsyncKeyState(SDL_SCANCODE_LSHIFT))	return;

	if (Remote())	return;

	if (m_input_external_handler && !m_input_external_handler->authorized(cmd))	return;

	if (g_Alive())	
	{
		if(m_holder)
		{
			m_holder->OnKeyboardRelease(cmd);
			
			if(m_holder->allowWeapon() && inventory().Action((u16)cmd, CMD_STOP))		return;
			return;
		}else
			if(inventory().Action((u16)cmd, CMD_STOP))		return;



		switch(cmd)
		{
		case kJUMP:		mstate_wishful &=~mcJump;		break;
		case kDROP:		if(GAME_PHASE_INPROGRESS == Game().Phase()) g_PerformDrop();				break;
		}
	}
}

void CActor::IR_OnKeyboardHold(int cmd)
{
	if(hud_adj_mode && pInput->iGetAsyncKeyState(SDL_SCANCODE_LSHIFT))	return;

	if (Remote() || !g_Alive())					return;
	if (m_input_external_handler && !m_input_external_handler->authorized(cmd))	return;
	if (IsTalking())							return;

	if(m_holder)
	{
		m_holder->OnKeyboardHold(cmd);
		return;
	}

#ifndef MASTER_GOLD
	if(psActorFlags.test(AF_NO_CLIP) && (cmd==kFWD || cmd==kBACK || cmd==kL_STRAFE || cmd==kR_STRAFE 
		|| cmd==kJUMP || cmd==kCROUCH))
	{
		NoClipFly(cmd);
		return;
	}
#endif //DEBUG
	float LookFactor = GetLookFactor();
	switch(cmd)
	{
	case kUP:
	case kDOWN: 
		cam_Active()->Move( (cmd==kUP) ? kDOWN : kUP, 0, LookFactor);									break;
	case kCAM_ZOOM_IN: 
	case kCAM_ZOOM_OUT: 
		cam_Active()->Move(cmd);												break;
	case kLEFT:
	case kRIGHT:
		if (eacFreeLook!=cam_active) cam_Active()->Move(cmd, 0, LookFactor);	break;

	case kACCEL:	mstate_wishful |= mcAccel;									break;
	case kL_STRAFE:	mstate_wishful |= mcLStrafe;								break;
	case kR_STRAFE:	mstate_wishful |= mcRStrafe;								break;
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
	case kFWD:		mstate_wishful |= mcFwd;									break;
	case kBACK:		mstate_wishful |= mcBack;									break;
	case kCROUCH:
		{
			if( !psActorFlags.test(AF_CROUCH_TOGGLE) )
					mstate_wishful |= mcCrouch;

		}break;
	}
}

void CActor::IR_OnMouseMove_CorrectMouseSense(int& p_dx, int& p_dy, float& sense)
{
	CWeapon* wpn = smart_cast<CWeapon*>(inventory().ActiveItem());
	if (wpn != nullptr && wpn->IsZoomed())
	{
		shared_str sect = wpn->m_section_id;

		if (wpn->IsScopeAttached() && wpn->get_ScopeStatus() == 2/* && !wpn->IsAlterZoom()*/)
			sect = pSettings->r_string(wpn->GetCurrentScopeSection(), "scope_name");

		if (!wpn->IsGrenadeMode())
			sense *= wpn->ModifyFloatUpgradedValue("zoom_mouse_sense_koef", READ_IF_EXISTS(pSettings, r_float, sect, "zoom_mouse_sense_koef", 1.0f));
		else
			sense *= READ_IF_EXISTS(pSettings, r_float, sect, "zoom_gl_mouse_sense_koef", 1.0f);
	}

	controller_input_correction_params controller_correction = GetCurrentControllerInputCorrectionParams();
	if (controller_correction.reverse_axis_y)
		p_dy = -(p_dy);

	float rot_ang = controller_correction.rotate_angle;
	float sense_scale_x = controller_correction.sense_scaler_x;
	float sense_scale_y = controller_correction.sense_scaler_y;

	const float ROTATE_SCALER = 10.0f;

	if (fabs(rot_ang) > EPS || fabs(sense_scale_x - 1) > EPS || fabs(sense_scale_y - 1) > EPS)
	{
		sense /= ROTATE_SCALER;
		float dx = ROTATE_SCALER * p_dx * sense_scale_x;
		float dy = ROTATE_SCALER * p_dy * sense_scale_y;

		if (fabs(rot_ang) > EPS)
		{
			p_dx = static_cast<int>(floor(dx * cos(rot_ang) + dy * sin(rot_ang)));
			p_dy = static_cast<int>(floor(dy * cos(rot_ang) - dx * sin(rot_ang)));
		}
		else
		{
			p_dx = static_cast<int>(floor(dx));
			p_dy = static_cast<int>(floor(dy));
		}

		controller_input_random_offset controller_offset = GetControllerInputRandomOffset();
		p_dx += static_cast<int>(floor(controller_offset.offset_x * ROTATE_SCALER));
		p_dy += static_cast<int>(floor(controller_offset.offset_y * ROTATE_SCALER));
	}
	else
	{
		controller_input_random_offset controller_offset = GetControllerInputRandomOffset();
		p_dx += controller_offset.offset_x;
		p_dy += controller_offset.offset_y;
	}
}

void CActor::IR_OnMouseMove(int dx, int dy)
{

	if(hud_adj_mode)
	{
		g_player_hud->tune	(Ivector().set(dx,dy,0));
		return;
	}

	PIItem iitem = inventory().ActiveItem();
	if(iitem && iitem->cast_hud_item())
		iitem->cast_hud_item()->ResetSubStateTime();

	if (Remote())		return;

	if(m_holder) 
	{
		m_holder->OnMouseMove(dx,dy);
		return;
	}

	float LookFactor = GetLookFactor();

	CCameraBase* C	= cameras	[cam_active];
	float scale		= (C->f_fov/g_fov)*psMouseSens * psMouseSensScale/50.f  / LookFactor;

	IR_OnMouseMove_CorrectMouseSense(dx, dy, scale);

	if (dx)
	{
		float d = float(dx)*scale;
		cam_Active()->Move((d<0)?kLEFT:kRIGHT, _abs(d));
	}

	if (dy)
	{
		float d = ((psMouseInvert.test(1))?-1:1)*float(dy)*scale*3.f/4.f;
		cam_Active()->Move((d>0)?kUP:kDOWN, _abs(d));
	}
}

void CActor::IR_GamepadUpdateStick(int id, Fvector2 value)
{
	// Left stick
	if (id == 0)
	{
		if (!fis_zero(value.x))
		{
			mstate_wishful |= (value.x > 0.f) ? mcRStrafe : mcLStrafe;
		}

		if (!fis_zero(value.y))
		{
			mstate_wishful |= (value.y > 0.f) ? mcFwd : mcBack;

			if (value.y < 0.22f)
			{
				mstate_wishful |= mcAccel;
			}
			else
			{
				mstate_wishful &= ~mcAccel;

				if (value.y > 0.75f)
				{
					mstate_wishful |= mcSprint;
				}
				else
				{
					mstate_wishful &= ~mcSprint;
				}
			}
		}
	}
	// Right stick
	else if (id == 1)
	{
		float LookFactor = GetLookFactor();

		CCameraBase* C = cameras[cam_active];
		float scale = (C->f_fov / g_fov) * psMouseSens * psMouseSensScale / 50.f / LookFactor;

		if (!fis_zero(value.x))
		{
			float d = value.x * scale * 8;
			cam_Active()->Move((d < 0) ? kLEFT : kRIGHT, std::abs(d));
		}

		if (!fis_zero(value.y))
		{
			float d = ((psMouseInvert.test(1)) ? -1 : 1) * value.y * scale * 3.f / 4.f;
			d *= 8;

			cam_Active()->Move((d > 0) ? kUP : kDOWN, std::abs(d));
		}
	}
	else if (id == 2)
	{
		if (!fis_zero(value.x))
		{
			IR_OnKeyboardPress(kWPN_ZOOM);
		}
		else
		{
			IR_OnKeyboardRelease(kWPN_ZOOM);
		}

		if (!fis_zero(value.y))
		{
			IR_OnKeyboardPress(kWPN_FIRE);
		}
		else
		{
			IR_OnKeyboardRelease(kWPN_FIRE);
		}
	}
}

void CActor::IR_GamepadKeyPress(int id)
{
	if (id == SDL_GamepadButton::SDL_GAMEPAD_BUTTON_LEFT_SHOULDER)
	{
		IR_OnKeyboardPress(kPREV_SLOT);
	}
	else if (id == SDL_GamepadButton::SDL_GAMEPAD_BUTTON_RIGHT_SHOULDER)
	{
		IR_OnKeyboardPress(kNEXT_SLOT);
	}
	else if (id == SDL_GamepadButton::SDL_GAMEPAD_BUTTON_SOUTH)
	{
		IR_OnKeyboardPress(kUSE);
	}
	else if (id == SDL_GamepadButton::SDL_GAMEPAD_BUTTON_WEST)
	{
		IR_OnKeyboardPress(kWPN_RELOAD);
	}
	else if (id == SDL_GamepadButton::SDL_GAMEPAD_BUTTON_NORTH)
	{
		inventory().SetActiveSlot(NO_ACTIVE_SLOT);
	}
	else if (id == SDL_GamepadButton::SDL_GAMEPAD_BUTTON_LEFT_STICK)
	{
	}
	else if (id == SDL_GamepadButton::SDL_GAMEPAD_BUTTON_RIGHT_STICK)
	{
	}
}

static bool IsKeyPressed(int dik)
{
	if (pInput)
		return pInput->iGetAsyncKeyState(dik);

	return false;
}

static bool IsActionKeyPressed(const EGameActions& EGameAction)
{
	int key1 = get_action_dik(EGameAction, 0);
	int key2 = get_action_dik(EGameAction, 1);

	return ((key1 != 0) && IsKeyPressed(key1)) || ((key2 != 0) && IsKeyPressed(key2));
}

static bool IsActionKeyPressedInGame(const EGameActions& EGameAction)
{
	if (IsActionKeyPressed(EGameAction) && !Console->bVisible && CurrentGameUI() && !CurrentGameUI()->TopInputReceiver() && g_pGameLevel && g_pGameLevel->Cameras().GetCamEffector(cefDemo) == nullptr)
		return true;

	return false;
}

void CActor::SetActorKeyRepeatFlag(ACTOR_DEFS::EActorKeyflags mask, bool state, bool ignore_suicide)
{
	if (!ignore_suicide && IsActorSuicideNow())
		return;

	if (state)
		_keyflags |= mask;
	else
		_keyflags &= ~mask;
}

extern BOOL b_toggle_weapon_aim;

void CActor::ProcessKeys()
{
	CHudItem* itm = smart_cast<CHudItem*>(inventory().ActiveItem());
	if (itm == nullptr)
		return;

	if ((_keyflags & kfHEADLAMP) != 0 && itm->CanStartAction())
	{
		SwitchTorch();
		SetActorKeyRepeatFlag(kfHEADLAMP, false);
	}

	if ((_keyflags & kfNIGHTVISION) != 0 && itm->CanStartAction())
	{
		SwitchNightVision();
		SetActorKeyRepeatFlag(kfNIGHTVISION, false);
	}

	CWeapon* wpn = smart_cast<CWeapon*>(itm);
	if (wpn == nullptr)
		return;

	if (IsActionKeyPressedInGame(kWPN_ZOOM) && wpn->GetState() == CWeapon::eIdle)
	{
		if (!b_toggle_weapon_aim && wpn->CanAimNow() && !wpn->IsZoomed())
		{
			wpn->Action(kWPN_ZOOM, CMD_START);
			SetActorKeyRepeatFlag(kfUNZOOM, false);
		}
	}

	if ((_keyflags & kfUNZOOM) != 0)
	{
		if (wpn->IsZoomed())
		{
			if (wpn->CanLeaveAimNow())
			{
				if (b_toggle_weapon_aim)
					wpn->Action(kWPN_ZOOM, CMD_START);
				else
					wpn->Action(kWPN_ZOOM, CMD_STOP);
			}
		}
		else
			SetActorKeyRepeatFlag(kfUNZOOM, false);
	}

	if (!wpn->IsActionProcessing() && wpn->GetState() != CWeapon::eSprintEnd && (Actor()->GetMovementState(eReal) & mcSprint) == 0 && (_keyflags & kfFIRE) != 0)
	{
		wpn->Action(kWPN_FIRE, CMD_START);

		if (!IsActionKeyPressed(kWPN_FIRE))
			wpn->Action(kWPN_FIRE, CMD_STOP);

		SetActorKeyRepeatFlag(kfFIRE, false);
	}

	if ((_keyflags & kfGLAUNCHSWITCH) != 0 && itm->CanStartAction())
	{
		wpn->Action(kWPN_FUNC, CMD_START);
		SetActorKeyRepeatFlag(kfGLAUNCHSWITCH, false);
	}

	if ((_keyflags & kfNEXTFIREMODE) != 0 && itm->CanStartAction())
	{
		wpn->Action(kWPN_FIREMODE_NEXT, CMD_START);
		SetActorKeyRepeatFlag(kfNEXTFIREMODE, false);
	}

	if ((_keyflags & kfPREVFIREMODE) != 0 && itm->CanStartAction())
	{
		wpn->Action(kWPN_FIREMODE_PREV, CMD_START);
		SetActorKeyRepeatFlag(kfPREVFIREMODE, false);
	}

	if ((_keyflags & kfRELOAD) != 0 && itm->CanStartAction())
	{
		wpn->Action(kWPN_RELOAD, CMD_START);
		SetActorKeyRepeatFlag(kfRELOAD, false);
	}

	if ((_keyflags & kfNEXTAMMO) != 0 && itm->CanStartAction())
	{
		wpn->Action(kWPN_NEXT, CMD_START);
		SetActorKeyRepeatFlag(kfNEXTAMMO, false);
	}

	if ((_keyflags & kfTACTICALTORCH) != 0 && itm->CanStartAction())
	{
		wpn->Action(kTACTICALTORCH, CMD_START);
		SetActorKeyRepeatFlag(kfTACTICALTORCH, false);
	}
}

#include "HudItem.h"
bool CActor::use_Holder				(CHolderCustom* holder)
{

	if(m_holder){
		bool b = false;
		CGameObject* holderGO			= smart_cast<CGameObject*>(m_holder);
		
		if(smart_cast<CCar*>(holderGO))
			b = use_Vehicle(0);
		else
			if (holderGO->CLS_ID==CLSID_OBJECT_W_STATMGUN)
				b = use_MountedWeapon(0);

		if(inventory().ActiveItem()){
			CHudItem* hi = smart_cast<CHudItem*>(inventory().ActiveItem());
			if(hi) hi->OnAnimationEnd(hi->GetState());
		}

		return b;
	}else{
		bool b = false;
		CGameObject* holderGO			= smart_cast<CGameObject*>(holder);
		if(smart_cast<CCar*>(holder))
			b = use_Vehicle(holder);

		if (holderGO->CLS_ID==CLSID_OBJECT_W_STATMGUN)
			b = use_MountedWeapon(holder);
		
		if(b){//used succesfully
			// switch off torch...
			CAttachableItem *I = CAttachmentOwner::attachedItem(CLSID_DEVICE_TORCH);
			if (I){
				CTorch* torch = smart_cast<CTorch*>(I);
				if (torch) torch->Switch(false);
			}
		}

		if(inventory().ActiveItem()){
			CHudItem* hi = smart_cast<CHudItem*>(inventory().ActiveItem());
			if(hi) hi->OnAnimationEnd(hi->GetState());
		}

		return b;
	}
}

void CActor::ActorUse()
{
	if (m_holder)
	{
		CGameObject*	GO			= smart_cast<CGameObject*>(m_holder);
		NET_Packet		P;
		CGameObject::u_EventGen		(P, GEG_PLAYER_DETACH_HOLDER, ID());
		P.w_u16						(GO->ID());
		CGameObject::u_EventSend	(P);
		return;
	}
				
	if(character_physics_support()->movement()->PHCapture())
		character_physics_support()->movement()->PHReleaseObject();

	if (m_pUsableObject && nullptr == m_pObjectWeLookingAt->cast_inventory_item())
	{
		m_pUsableObject->use(this);
	}
	
	if (eateable_to_delete == nullptr && m_pInvBoxWeLookingAt && m_pInvBoxWeLookingAt->nonscript_usable())
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

	if (eateable_to_delete == nullptr && (!m_pUsableObject||m_pUsableObject->nonscript_usable()))
	{
		if(m_pPersonWeLookingAt)
		{
			CEntityAlive* pEntityAliveWeLookingAt = 
				smart_cast<CEntityAlive*>(m_pPersonWeLookingAt);

			VERIFY(pEntityAliveWeLookingAt);

			if (IsGameTypeSingle())
			{			
				CBaseMonster* pMonster = smart_cast<CBaseMonster*>(pEntityAliveWeLookingAt);
				const static bool isMonstersInventory = EngineExternal()[EEngineExternalGame::EnableMonstersInventory];
				bool TestMonster =	(pMonster == nullptr) ||
									(pMonster != nullptr && isMonstersInventory);

				if(pEntityAliveWeLookingAt->g_Alive())
				{
					TryToTalk();
				}
				else
				{
					//только если находимся в режиме single
					CUIGameSP* pGameSP = smart_cast<CUIGameSP*>(CurrentGameUI());
					if (pGameSP && TestMonster)
					{
						if (!m_pPersonWeLookingAt->deadbody_closed_status())
						{
							if (pEntityAliveWeLookingAt->AlreadyDie() &&
								pEntityAliveWeLookingAt->GetLevelDeathTime() + 3000 < Device.dwTimeGlobal)
								// 99.9% dead
								pGameSP->StartCarBody(this, m_pPersonWeLookingAt);
						}
					}
				}
			}
		}

		collide::rq_result& RQ = HUD().GetCurrentRayQuery();
		CPhysicsShellHolder* object = smart_cast<CPhysicsShellHolder*>(RQ.O);
		u16 element = BI_NONE;
		if(object) 
			element = (u16)RQ.element;

		if(object && Level().IR_GetKeyState(SDL_SCANCODE_LSHIFT))
		{
			bool b_allow = !!pSettings->line_exist("ph_capture_visuals",object->cNameVisual());
			if(b_allow && !character_physics_support()->movement()->PHCapture())
			{
				character_physics_support()->movement()->PHCaptureObject( object, element );

			}

		}
		else
		{
			if (object && smart_cast<CHolderCustom*>(object))
			{
					NET_Packet		P;
					CGameObject::u_EventGen		(P, GEG_PLAYER_ATTACH_HOLDER, ID());
					P.w_u16						(object->ID());
					CGameObject::u_EventSend	(P);
					return;
			}

		}
	}
}

BOOL CActor::HUDview				( )const 
{ 
	return IsFocused() && (cam_active==eacFirstEye)&&
		((!m_holder) || (m_holder && m_holder->allowWeapon() && m_holder->HUDView() ) ); 
}

static	u16 SlotsToCheck [] = {
		KNIFE_SLOT		,		// 0
		INV_SLOT_2		,		// 1
		INV_SLOT_3		,		// 2
		GRENADE_SLOT	,		// 3
		ARTEFACT_SLOT	,		// 10
};

void	CActor::OnNextWeaponSlot()
{
	u32 ActiveSlot = inventory().GetActiveSlot();
	if (ActiveSlot == NO_ACTIVE_SLOT) 
		ActiveSlot = inventory().GetPrevActiveSlot();

	if (ActiveSlot == NO_ACTIVE_SLOT) 
		ActiveSlot = KNIFE_SLOT;
	
	u32 NumSlotsToCheck = sizeof(SlotsToCheck)/sizeof(SlotsToCheck[0]);	
	
	u32 CurSlot			= 0;
	for (; CurSlot<NumSlotsToCheck; CurSlot++)
	{
		if (SlotsToCheck[CurSlot] == ActiveSlot) break;
	};

	if (CurSlot >= NumSlotsToCheck) 
		return;

	for (u32 i=CurSlot+1; i<NumSlotsToCheck; i++)
	{
		if (inventory().ItemFromSlot(SlotsToCheck[i]))
		{
			if (SlotsToCheck[i] == ARTEFACT_SLOT) 
			{
				IR_OnKeyboardPress(kARTEFACT);
			}
			else
				IR_OnKeyboardPress(kWPN_1 + i);
			return;
		}
	}
};

void	CActor::OnPrevWeaponSlot()
{
	u32 ActiveSlot = inventory().GetActiveSlot();
	if (ActiveSlot == NO_ACTIVE_SLOT) 
		ActiveSlot = inventory().GetPrevActiveSlot();

	if (ActiveSlot == NO_ACTIVE_SLOT) 
		ActiveSlot = KNIFE_SLOT;

	u32 NumSlotsToCheck = sizeof(SlotsToCheck)/sizeof(SlotsToCheck[0]);	
	u32 CurSlot		= 0;

	for (; CurSlot<NumSlotsToCheck; CurSlot++)
	{
		if (SlotsToCheck[CurSlot] == ActiveSlot) break;
	};

	if (CurSlot >= NumSlotsToCheck) 
		CurSlot	= NumSlotsToCheck-1; //last in row

	for (s32 i=s32(CurSlot-1); i>=0; i--)
	{
		if (inventory().ItemFromSlot(SlotsToCheck[i]))
		{
			if (SlotsToCheck[i] == ARTEFACT_SLOT) 
			{
				IR_OnKeyboardPress(kARTEFACT);
			}
			else
				IR_OnKeyboardPress(kWPN_1 + i);
			return;
		}
	}
};

float	CActor::GetLookFactor()
{
	if (m_input_external_handler) 
		return m_input_external_handler->mouse_scale_factor();

	
	float factor	= 1.f;

	PIItem pItem	= inventory().ActiveItem();

	if (pItem)
		factor *= pItem->GetControlInertionFactor();

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
		IR_OnKeyboardRelease	(kWPN_FIRE);

	// set handler
	m_input_external_handler	= handler;
}

void CActor::SwitchNightVision()
{
	CHelmet* pHelmet = smart_cast<CHelmet*>(inventory().ItemFromSlot(HELMET_SLOT));
	bool condition = pHelmet != nullptr && pHelmet->m_NightVisionSect.size() > 0 || GetOutfit() != nullptr && GetOutfit()->m_NightVisionSect.size() > 0;

	if (!condition)
		return;

	if (CTorch* torch = smart_cast<CTorch*>(inventory().ItemFromSlot(TORCH_SLOT)))
	{
		bool isGuns = EngineExternal().isModificationGunslinger();
		if (isGuns)
		{
			CHudItemObject* itm = smart_cast<CHudItemObject*>(inventory().ActiveItem());

			if (itm != nullptr && smart_cast<CHUDAnimItem*>(itm) == nullptr && itm->Weapon_SetKeyRepeatFlagIfNeeded(kfNIGHTVISION) && (GetDetector() != nullptr && GetDetector()->GetState() == CCustomDetector::eIdle && !GetDetector()->IsPending() || GetDetector() == nullptr))
			{
				itm->fDeviceFlags.set(CHudItem::DF_NIGHTVISION, TRUE);
				itm->SwitchState(CHUDState::eSwitchDevice);
			}

			if (GetDetector() != nullptr && GetDetector()->GetState() == CCustomDetector::eIdle && !GetDetector()->IsPending() && (itm != nullptr && smart_cast<CHUDAnimItem*>(itm) == nullptr && itm->Weapon_SetKeyRepeatFlagIfNeeded(kfNIGHTVISION) || itm == nullptr))
			{
				GetDetector()->fDeviceFlags.set(CCustomDetector::DF_NIGHTVISION, TRUE);
				GetDetector()->SwitchState(CCustomDetector::eSwitchDevice);
			}

			if (itm == nullptr && GetDetector() == nullptr)
			{
				VERIFY(m_pActorNightVisionDisableAnimatorName && m_pActorNightVisionEnableAnimatorName && "must be initialized strings!");

				VERIFY(m_pActorNightVisionHUDDisableAnimationName && m_pActorNightVisionHUDEnableAnimationName && "must be initialized strings!");

				shared_str sect = torch->GetNightVisionStatus() ? m_pActorNightVisionDisableAnimatorName : m_pActorNightVisionEnableAnimatorName;
				shared_str animation_name = torch->GetNightVisionStatus() ? m_pActorNightVisionHUDDisableAnimationName : m_pActorNightVisionHUDEnableAnimationName;
				CHUDAnimItem::LoadSound(sect.c_str(), "snd_draw", false);
				CHUDAnimItem::PlayHudAnim(animation_name.c_str(), "anm_show", "", {CHudItem::TAnimationEffector(this, &CActor::NVCallback)});
			}
		}
		else
		{
			if (CWeapon* wpn = smart_cast<CWeapon*>(inventory().ActiveItem()))
			{
				if (wpn->IsZoomed())
					return;
			}

			torch->SwitchNightVision();
		}
	}
}

void CActor::HeadlampCallback()
{
	if (CTorch* torch = smart_cast<CTorch*>(Actor()->inventory().ItemFromSlot(TORCH_SLOT)))
		torch->Switch();

	if (CWeapon* wpn = smart_cast<CWeapon*>(inventory().ActiveItem()))
		wpn->MakeLockByConfigParam("lock_time_end_" + wpn->GetActualCurrentAnim());
}

void CActor::NVCallback()
{
	if (CTorch* torch = smart_cast<CTorch*>(Actor()->inventory().ItemFromSlot(TORCH_SLOT)))
		torch->SwitchNightVision();

	if (CWeapon* wpn = smart_cast<CWeapon*>(inventory().ActiveItem()))
		wpn->MakeLockByConfigParam("lock_time_end_" + wpn->GetActualCurrentAnim());
}

void CActor::SwitchTorch()
{
	bool isGuns = EngineExternal().isModificationGunslinger();
	if (isGuns)
	{
		CHelmet* pHelmet = smart_cast<CHelmet*>(inventory().ItemFromSlot(HELMET_SLOT));
		bool condition = pHelmet != nullptr && pHelmet->bIsTorchAvaliable || GetOutfit() != nullptr && GetOutfit()->bIsTorchAvaliable;

		if (!condition)
			return;
	}

	if (CTorch* torch = smart_cast<CTorch*>(inventory().ItemFromSlot(TORCH_SLOT)))
	{
		if (isGuns)
		{
			CHudItemObject* itm = smart_cast<CHudItemObject*>(inventory().ActiveItem());

			if (itm != nullptr && smart_cast<CHUDAnimItem*>(itm) == nullptr && itm->Weapon_SetKeyRepeatFlagIfNeeded(kfHEADLAMP) && (GetDetector() != nullptr && GetDetector()->GetState() == CCustomDetector::eIdle && !GetDetector()->IsPending() || GetDetector() == nullptr))
			{
				itm->fDeviceFlags.set(CHudItem::EDeviceFlags::DF_HEADLAMP, TRUE);
				itm->SwitchState(CHUDState::eSwitchDevice);
			}

			if (GetDetector() != nullptr && GetDetector()->GetState() == CCustomDetector::eIdle && !GetDetector()->IsPending() && (itm != nullptr && smart_cast<CHUDAnimItem*>(itm) == nullptr && itm->Weapon_SetKeyRepeatFlagIfNeeded(kfHEADLAMP) || itm == nullptr))
			{
				GetDetector()->fDeviceFlags.set(CHudItem::EDeviceFlags::DF_HEADLAMP, TRUE);
				GetDetector()->SwitchState(CCustomDetector::eSwitchDevice);
			}

			if (itm == nullptr && GetDetector() == nullptr)
			{
				shared_str sect = torch->IsSwitched() ? m_pActorHeadLampDisableAnimatorSectionName : m_pActorHeadLampEnableAnimatorSectionName;

				shared_str animation_name = torch->IsSwitched() ? m_pActorHeadLampDisableHUDAnimationName : m_pActorHeadLampEnableHUDAnimationName;

				CHUDAnimItem::LoadSound(sect.c_str(), "snd_draw", false);
				CHUDAnimItem::PlayHudAnim(animation_name.c_str(), "anm_show", "", {CHudItem::TAnimationEffector(this, &CActor::HeadlampCallback)});
			}
		}
		else
			torch->Switch();
	}
}

#ifndef MASTER_GOLD
#include "../xrPhysics/IPHWorld.h"
collide::rq_result GetPickResult(Fvector pos, Fvector dir, float range, CObject* ignore);
void CActor::NoClipFly(int cmd)
{
    Fvector cur_pos, right, left;
    cur_pos.set(0, 0, 0);
    float scale = 5.f;
    if (pInput->iGetAsyncKeyState(SDL_SCANCODE_LSHIFT))
        scale = 2.0f;
    else if (pInput->iGetAsyncKeyState(SDL_SCANCODE_X))
        scale = 7.0f;
    else if (pInput->iGetAsyncKeyState(SDL_SCANCODE_LALT))
        scale = 10.0f;
    else if (pInput->iGetAsyncKeyState(SDL_SCANCODE_CAPSLOCK))
        scale = 15.0f;
    else if (pInput->iGetAsyncKeyState(SDL_SCANCODE_DELETE))
	{
        collide::rq_result RQ = GetPickResult(cam_Active()->Position(), cam_Active()->Direction(), 1000.0f, this);
        if (RQ.element>=0)
            SetPhPosition(XFORM().translate(Fvector(cam_Active()->Position()).mad(Fvector(cam_Active()->Direction()), RQ.range)));
    }

	switch (cmd)
	{
		case kJUMP:
			cur_pos.mad({ 0,1,0 }, scale / 2.0f);
			if (m_pPhysicsShell)
				m_pPhysicsShell->applyImpulseTrace(cur_pos, { 0,1,0 }, (scale * m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
			break;
		case kCROUCH:
			cur_pos.mad({ 0,-1,0 }, scale / 2.0f);
			if (m_pPhysicsShell)
				m_pPhysicsShell->applyImpulseTrace(cur_pos, { 0,-1,0 }, (scale * m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
			break;
		case kFWD:
			cur_pos.mad(cam_Active()->vDirection, scale / 2.0f);
			if (m_pPhysicsShell)
				m_pPhysicsShell->applyImpulseTrace(cur_pos, cam_Active()->vDirection, (scale * m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
			break;
		case kBACK:
			cur_pos.mad(Fvector(cam_Active()->vDirection).invert(), scale / 2.0f);
			if (m_pPhysicsShell)
				m_pPhysicsShell->applyImpulseTrace(cur_pos, Fvector(cam_Active()->vDirection).invert(), (scale * m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
			break;
		case kR_STRAFE:
			right.set(cam_Active()->Right());
			cur_pos.mad(right, scale / 2.0f);
			if (m_pPhysicsShell)
				m_pPhysicsShell->applyImpulseTrace(cur_pos, right, (scale * m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
			break;
		case kL_STRAFE:
			left.set(Fvector(cam_Active()->Right()).invert());
			cur_pos.mad(left, scale / 2.0f);
			if (m_pPhysicsShell)
				m_pPhysicsShell->applyImpulseTrace(cur_pos, left, (scale * m_pPhysicsShell->getMass() * physics_world()->Gravity()) * Device.fTimeDelta);
			break;
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
			if ((mstate_real&mcClimb)) break;
			PIItem det_active = inventory().ItemFromSlot(DETECTOR_SLOT);
			if(det_active)
			{
				CCustomDetector* det = smart_cast<CCustomDetector*>(det_active);
				if (det)
					det->switch_detector();
				return;
			}
		}break;
	}
	if(!m_pPhysicsShell)
		SetPhPosition(XFORM().translate_add(cur_pos.mul(scale * Device.fTimeDelta)));

	if(inventory().Action((u16)cmd, CMD_START))return;
}
#endif //DEBUG