#include "stdafx.h"
#include "pch_script.h"
#include "Actor.h"
#include "Torch.h"
#include "customoutfit.h"
#include "trade.h"
#include "../CameraBase.h"
#ifdef DEBUG
#include "PHDebug.h"
#endif
#include "hit.h"
#include "PHDestroyable.h"
#include "Car.h"
#include "HudManager.h"
#include "UIGameSP.h"
#include "inventory.h"
#include "level.h"
#include "game_cl_base.h"
#include "../xrEngine/xr_level_controller.h"
#include "UsableScriptObject.h"
#include "clsid_game.h"
#include "actorcondition.h"
#include "actor_input_handler.h"
#include "../xrEngine/string_table.h"
#include "UI/UIStatic.h"
#include "CharacterPhysicsSupport.h"
#include "InventoryBox.h"
#include "WeaponMagazined.h"
#include "game_object_space.h"
#include "../xrScripts/script_callback_ex.h"
#include "script_game_object.h"
#include "../xr_input.h"
#include "player_hud.h"
#include "ui/UIInventoryWnd.h" //for detector needs

bool g_bAutoClearCrouch = true;
u32  g_bAutoApplySprint = 0;
extern int hud_adj_mode;

void CActor::IR_OnKeyboardPress(int cmd)
{
	if (CAttachableItem::m_dbgItem || hud_adj_mode && pInput->iGetAsyncKeyState(SDL_SCANCODE_LSHIFT))	return;

	if (Remote())		return;

//	if (conditions().IsSleeping())	return;

	callback(GameObject::eOnButtonPress)(lua_game_object(), cmd);

	if (IsTalking())	return;
	if (m_input_external_handler && !m_input_external_handler->authorized(cmd))	return;
	
	switch (cmd)
	{
	case kWPN_FIRE:
		{
			BreakSprint();
		}break;
	default:
		{
		}break;
	}

	if (!g_Alive()) return;

	if(m_holder && kUSE != cmd)
	{
		m_holder->OnKeyboardPress			(cmd);
		if(m_holder->allowWeapon() && inventory().Action(cmd, CMD_START))		return;
		return;
	} else 
	{
		if(inventory().Action(cmd, CMD_START))					return;
	}

	switch(cmd){
	case kJUMP:		
		{
			mstate_wishful |= mcJump;
			{
//				NET_Packet	P;
//				u_EventGen(P, GE_ACTOR_JUMPING, ID());
//				u_EventSend(P);
			}
		}break;
	case kCROUCH_TOGGLE:
		{
			g_bAutoClearCrouch = !g_bAutoClearCrouch;
			if (!g_bAutoClearCrouch)
				mstate_wishful |= mcCrouch;

		}break;
	case kSPRINT_TOGGLE:	
		{
			CWeapon* W = smart_cast<CWeapon*>(inventory().ActiveItem());

			if (IsReloadingWeapon())
			{
				if (trySprintCounter_ == 0) // don't interrupt reloading on first key press and skip sprint request
				{
					trySprintCounter_++;

					return;
				}
				else if (trySprintCounter_ >= 1) // break reloading, if player insist(presses two or more times) and do sprint
				{
					W->StopAllSounds();
					W->SwitchState(CHUDState::EHudStates::eIdle);
				}
			}

			trySprintCounter_ = 0;

			if (mstate_wishful & mcSprint)
			{
				mstate_wishful &= ~mcSprint;
			}
			else
			{
				g_bAutoClearCrouch = true;
				g_bAutoApplySprint = 1;
				mstate_wishful |= mcSprint;
			}
		}break;
	case kCAM_1:{	cam_Set(eacFirstEye);	CCustomDetectorR * detectortoshow = inventory().CurrentDetector();	if (detectortoshow)g_player_hud->attach_item(detectortoshow); }break; //Для востановления детектора в руке
	case kCAM_2:	cam_Set(eacLookAt);				break;
	case kCAM_3:	cam_Set(eacFreeLook);				break;
	case kNIGHT_VISION: SwitchNightVision();					break;
		/*{
			CCustomOutfit* outfit	= GetOutfit();
			if (outfit)
				outfit->SwitchNightVision();
		} break;*/
		
	case kTORCH:{ 
		if (!m_current_torch)
		{
			if (inventory().ItemFromSlot(TORCH_SLOT))
			{
				CTorch *torch = smart_cast<CTorch*>(inventory().ItemFromSlot(TORCH_SLOT));
				if (torch)		
				{
					m_current_torch = torch;
					m_current_torch->Switch();
				}
			}
		} else {
			if (inventory().ItemFromSlot(TORCH_SLOT))
			{
				CTorch *torch = smart_cast<CTorch*>(inventory().ItemFromSlot(TORCH_SLOT));
				if (torch)
				{
					m_current_torch = torch;
					m_current_torch->Switch();
				} else
					m_current_torch = 0;

			} else
				m_current_torch = 0;
		}
		} break;
		
	case kWPN_1:	
	case kWPN_2:	
	case kWPN_3:	
	case kWPN_3b:	
	case kWPN_4:	
	case kWPN_5:	
	case kWPN_6:	
	case kWPN_RELOAD:			
		break;
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

	case kUSE_BANDAGE:
	case kUSE_MEDKIT:
		{
			if (IsGameTypeSingle())
			{
				PIItem itm = inventory().item((cmd == kUSE_BANDAGE) ? CLSID_IITEM_BANDAGE : CLSID_IITEM_MEDKIT);
				if (itm)
				{
					bool used = inventory().Eat(itm);
					if (used)
					{
						SDrawStaticStruct* HudMessage = CurrentGameUI()->AddCustomStatic("inv_hud_message", true);
						HudMessage->m_endTime = Device.fTimeGlobal + 3.0f;// 3sec
						string1024					str;
						xr_sprintf(str, "%s : %s", *CStringTable().translate("st_item_used"), itm->Name());
						HudMessage->wnd()->TextItemControl()->SetText(str);
					}
				}
			}
		}break;
	}
}

void CActor::SwitchNightVision()
{
	CWeapon *wpn1 = nullptr, *wpn2 = nullptr, *wpn3 = nullptr;
	if (inventory().ItemFromSlot(PISTOL_SLOT))
		wpn1 = smart_cast<CWeapon*>(inventory().ItemFromSlot(PISTOL_SLOT));

	if (inventory().ItemFromSlot(RIFLE_SLOT))
		wpn2 = smart_cast<CWeapon*>(inventory().ItemFromSlot(RIFLE_SLOT));

	if (inventory().ItemFromSlot(RIFLE_2_SLOT))
		wpn3 = smart_cast<CWeapon*>(inventory().ItemFromSlot(RIFLE_2_SLOT));

	xr_vector<CAttachableItem*> const& all = CAttachmentOwner::attached_objects();
	xr_vector<CAttachableItem*>::const_iterator it = all.begin();
	xr_vector<CAttachableItem*>::const_iterator it_e = all.end();
	for (; it != it_e; ++it)
	{
		CTorch* torch = smart_cast<CTorch*>(*it);
		if (torch)
		{
			if (wpn1 && wpn1->IsZoomed())
				return;

			if (wpn2 && wpn2->IsZoomed())
				return;

			if (wpn3 && wpn3->IsZoomed())
				return;

			torch->SwitchNightVision();
			return;
		}
	}
}

void CActor::IR_OnMouseWheel(int direction)
{
	if(hud_adj_mode)
	{
		g_player_hud->tune	(Ivector().set(0,0,direction));
		return;
	}

	if (CAttachableItem::m_dbgItem)
	{
		return;
	}

	if(inventory().Action( (direction>0)? kWPN_ZOOM_DEC:kWPN_ZOOM_INC , CMD_START)) return;


	if (direction>0)
	{
		if (eacLookAt==cam_active)
			for (int i=0; i<10; ++i)
				cam_Active()->Move(kCAM_ZOOM_IN);
		else
			OnNextWeaponSlot();
	} else {
		if (eacLookAt==cam_active)
			for (int i=0; i<10; ++i)
				cam_Active()->Move(kCAM_ZOOM_OUT);
		else
			OnPrevWeaponSlot();
	}
}
void CActor::IR_OnKeyboardRelease(int cmd)
{
	if (CAttachableItem::m_dbgItem || hud_adj_mode && pInput->iGetAsyncKeyState(SDL_SCANCODE_LSHIFT))	return;

	if (Remote())		return;

//	if (conditions().IsSleeping())	return;

	callback(GameObject::eOnButtonRelease)(lua_game_object(), cmd);

	if (m_input_external_handler && !m_input_external_handler->authorized(cmd))	return;

	if (g_Alive())	
	{
		if (cmd == kUSE) 
			PickupModeOff();

		if(m_holder)
		{
			m_holder->OnKeyboardRelease(cmd);
			
			if(m_holder->allowWeapon() && inventory().Action(cmd, CMD_STOP))		return;
			return;
		}else{
			if(inventory().Action(cmd, CMD_STOP))		return;
		}



		switch(cmd)
		{
		case kJUMP:		mstate_wishful &=~mcJump;		break;
		case kDROP:		if(GAME_PHASE_INPROGRESS == Game().Phase()) g_PerformDrop();				break;
		case kCROUCH:	g_bAutoClearCrouch = true;
		}
	}
}

void CActor::IR_OnKeyboardHold(int cmd)
{
	if (CAttachableItem::m_dbgItem || hud_adj_mode && pInput->iGetAsyncKeyState(SDL_SCANCODE_LSHIFT))	return;

	if (Remote() || !g_Alive())					return;
//	if (conditions().IsSleeping())				return;

	callback(GameObject::eOnButtonHold)(lua_game_object(), cmd);

	if (m_input_external_handler && !m_input_external_handler->authorized(cmd))	return;
	if (IsTalking())							return;

	if(m_holder)
	{
		m_holder->OnKeyboardHold(cmd);
		return;
	}

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
	case kL_LOOKOUT:mstate_wishful |= mcLLookout;								break;
	case kR_LOOKOUT:mstate_wishful |= mcRLookout;								break;
	case kFWD:		mstate_wishful |= mcFwd;									break;
	case kBACK:		mstate_wishful |= mcBack;									break;
	case kCROUCH:	mstate_wishful |= mcCrouch;									break;


	}
}

void CActor::IR_OnMouseMove(int dx, int dy)
{
	if(hud_adj_mode)
	{
		g_player_hud->tune	(Ivector().set(dx,dy,0));
		return;
	}

	if (CAttachableItem::m_dbgItem)
	{
		return;
	}

	PIItem iitem = inventory().ActiveItem();
	if(iitem && iitem->cast_hud_item())
		iitem->cast_hud_item()->ResetSubStateTime();

	if (Remote())		return;
//	if (conditions().IsSleeping())	return;

	if(m_holder) 
	{
		m_holder->OnMouseMove(dx,dy);
		return;
	}

	float LookFactor = GetLookFactor();

	CCameraBase* C	= cameras	[cam_active];
	float scale		= (C->f_fov/g_fov)*psMouseSens * psMouseSensScale/50.f  / LookFactor;
	if (dx){
		float d = float(dx)*scale;
		cam_Active()->Move((d<0)?kLEFT:kRIGHT, _abs(d));
	}
	if (dy){
		float d = ((psMouseInvert.test(1))?-1:1)*float(dy)*scale*3.f/4.f;
		cam_Active()->Move((d>0)?kUP:kDOWN, _abs(d));
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
			if (holderGO->CLS_ID==CLSID_OBJECT_W_MOUNTED ||
				holderGO->CLS_ID==CLSID_OBJECT_W_STATMGUN ||
				holderGO->CLS_ID==CLSID_OBJECT_W_TURRET)
					b = use_MountedWeapon(0);

		/*if(inventory().ActiveItem()){						//SkyLoader: why we added it? It works incorrent on long distance if use car
			CHudItem* hi = smart_cast<CHudItem*>(inventory().ActiveItem());
			if(hi) hi->OnAnimationEnd(hi->GetState());
		}*/

		return b;
	}else{
		bool b = false;
		CGameObject* holderGO			= smart_cast<CGameObject*>(holder);
		if(smart_cast<CCar*>(holder))
			b = use_Vehicle(holder);

		if (holderGO->CLS_ID==CLSID_OBJECT_W_MOUNTED ||
			holderGO->CLS_ID==CLSID_OBJECT_W_STATMGUN ||
			holderGO->CLS_ID==CLSID_OBJECT_W_TURRET)
				b = use_MountedWeapon(holder);
		
		if(b){//used succesfully
			// switch off torch...
			CAttachableItem *I = CAttachmentOwner::attachedItem(CLSID_DEVICE_TORCH);
			if (I){
				CTorch* torch = smart_cast<CTorch*>(I);
				if (torch) torch->Switch(false);
			}
		}

		/*if(inventory().ActiveItem()){						//SkyLoader: why we added it? It works incorrent on long distance if use car
			CHudItem* hi = smart_cast<CHudItem*>(inventory().ActiveItem());
			if(hi) hi->OnAnimationEnd(hi->GetState());
		}*/

		return b;
	}
}

void CActor::ActorUse()
{
	//mstate_real = 0;
	PickupModeOn();

		
	if (m_holder)
	{
		CGameObject*	GO			= smart_cast<CGameObject*>(m_holder);
		NET_Packet		P;
		CGameObject::u_EventGen		(P, GEG_PLAYER_DETACH_HOLDER, ID());
		P.w_u32						(GO->ID());
		CGameObject::u_EventSend	(P);
		return;
	}
				
	if(character_physics_support()->movement()->PHCapture())
		character_physics_support()->movement()->PHReleaseObject();

	

	if(m_pUsableObject)m_pUsableObject->use(this);
	
	if(m_pInvBoxWeLookingAt && m_pInvBoxWeLookingAt->nonscript_usable())
	{
		CUIGameSP* pGameSP = smart_cast<CUIGameSP*>(CurrentGameUI());
		if (pGameSP) {
			if (m_pInvBoxWeLookingAt->IsSafe){
				luabind::functor<void> lua_func;
				R_ASSERT2(ai().script_engine().functor("ui_lock.start_lock_ui", lua_func), "Can't find ui_lock.start_lock_ui");
				lua_func(m_pInvBoxWeLookingAt->SafeCode, m_pInvBoxWeLookingAt->lua_game_object());
				pGameSP->StoredInvBox = m_pInvBoxWeLookingAt;
			}
			else
			{
				pGameSP->StartStashUI(this, m_pInvBoxWeLookingAt);
			}
		}
		return;
	}

	if(!m_pUsableObject||m_pUsableObject->nonscript_usable())
	{
		if(m_pPersonWeLookingAt)
		{
			CEntityAlive* pEntityAliveWeLookingAt = 
				smart_cast<CEntityAlive*>(m_pPersonWeLookingAt);

			if (pEntityAliveWeLookingAt && GameID()==GAME_SINGLE)
			{			
				if(pEntityAliveWeLookingAt->g_Alive())
				{
					TryToTalk();
				}
				//обыск трупа
				else  if(!Level().IR_GetKeyState(SDL_SCANCODE_LSHIFT))
				{
					CUIGameSP* pGameSP = smart_cast<CUIGameSP*>(CurrentGameUI());
					if (pGameSP)pGameSP->StartStashUI(this, m_pPersonWeLookingAt);
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

			if (m_pPersonWeLookingAt && !m_pPersonWeLookingAt->inventory().CanBeDragged())
				b_allow = false;

			if(b_allow && !character_physics_support()->movement()->PHCapture())
			{
				character_physics_support()->movement()->PHCaptureObject(object,element);

			}
		}
		else
		{
			if (object && smart_cast<CHolderCustom*>(object))
			{
					NET_Packet		P;
					CGameObject::u_EventGen		(P, GEG_PLAYER_ATTACH_HOLDER, ID());
					P.w_u32						(object->ID());
					CGameObject::u_EventSend	(P);
					return;
			}

		}
	}


}
BOOL CActor::HUDview()const
{
	return IsFocused() && (cam_active == eacFirstEye || (cam_active == eacLookAt && m_holder && m_holder->HUDView()));
}

//void CActor::IR_OnMousePress(int btn)
static	u32 SlotsToCheck [] = {
		KNIFE_SLOT		,		// 0
		PISTOL_SLOT		,		// 1
		RIFLE_SLOT		,		// 2
		RIFLE_2_SLOT	,		// 14
		GRENADE_SLOT	,		// 3
		APPARATUS_SLOT	,		// 4
		BOLT_SLOT		,		// 5 - Nova: For mouse scrolling to bolt.
		ARTEFACT_SLOT	,		// 10
};

void	CActor::OnNextWeaponSlot()
{
	u32 ActiveSlot = inventory().GetActiveSlot();
	if (ActiveSlot == NO_ACTIVE_SLOT) 
		ActiveSlot = inventory().GetPrevActiveSlot();

	if (ActiveSlot == NO_ACTIVE_SLOT) 
		ActiveSlot = KNIFE_SLOT;
	
	u32 NumSlotsToCheck = sizeof(SlotsToCheck)/sizeof(u32);	
	u32 CurSlot = 0;
	for (; CurSlot<NumSlotsToCheck; CurSlot++)
	{
		if (SlotsToCheck[CurSlot] == ActiveSlot) break;
	};
	if (CurSlot >= NumSlotsToCheck) return;
	for (u32 i=CurSlot+1; i<NumSlotsToCheck; i++)
	{
		auto slotItm = inventory().ItemFromSlot(SlotsToCheck[i]);
		if (slotItm)
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

	u32 NumSlotsToCheck = sizeof(SlotsToCheck)/sizeof(u32);	
	u32 CurSlot = 0;
	for (; CurSlot<NumSlotsToCheck; CurSlot++)
	{
		if (SlotsToCheck[CurSlot] == ActiveSlot) break;
	};
	if (CurSlot >= NumSlotsToCheck) return;
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
}

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



