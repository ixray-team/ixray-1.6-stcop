#include "stdafx.h"
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
#include "actorcondition.h"
#include "actor_input_handler.h"
#include "../xrEngine/string_table.h"
#include "UI/UIStatic.h"
#include "UI/UIActorMenu.h"
#include "UI/UIDragDropReferenceList.h"
#include "CharacterPhysicsSupport.h"
#include "InventoryBox.h"
#include "player_hud.h"
#include "../xrEngine/xr_input.h"
#include "flare.h"
#include "CustomDetector.h"
#include "clsid_game.h"
#include "HUDManager.h"
#include "Weapon.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "bandage.h"

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
	switch(cmd)
	{
	case kJUMP:		
		{
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
/*
	case kFLARE:{
			PIItem fl_active = inventory().ItemFromSlot(FLARE_SLOT);
			if(fl_active)
			{
				CFlare* fl			= smart_cast<CFlare*>(fl_active);
				fl->DropFlare		();
				return				;
			}

			PIItem fli = inventory().Get(CLSID_DEVICE_FLARE, true);
			if(!fli)			return;

			CFlare* fl			= smart_cast<CFlare*>(fli);
			
			if(inventory().Slot(fl))
				fl->ActivateFlare	();
		}break;
*/
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
			const shared_str& itemName		= g_quick_use_slots[cmd-kQUICK_USE_1];

			if (itemName.size())
			{
				PIItem realItemData = inventory().GetAny(itemName.c_str());
				CEatableItemObject* pRealItem = smart_cast<CEatableItemObject*>(realItemData);

				if (realItemData)
				{
					UseItemsFromFastSlots(realItemData, pRealItem, itemName);
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
	if (dx){
		float d = float(dx)*scale;
		cam_Active()->Move((d<0)?kLEFT:kRIGHT, _abs(d));
	}
	if (dy){
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
	
	if (m_pInvBoxWeLookingAt && m_pInvBoxWeLookingAt->nonscript_usable())
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

	if(!m_pUsableObject||m_pUsableObject->nonscript_usable())
	{
		if(m_pPersonWeLookingAt)
		{
			CEntityAlive* pEntityAliveWeLookingAt = 
				smart_cast<CEntityAlive*>(m_pPersonWeLookingAt);

			VERIFY(pEntityAliveWeLookingAt);

			if (IsGameTypeSingle())
			{			
				CBaseMonster* pMonster = smart_cast<CBaseMonster*>(pEntityAliveWeLookingAt);
				bool TestMonster =	(pMonster == nullptr) ||
									(pMonster != nullptr && EngineExternal()[EEngineExternalGame::EnableMonstersInventory]);

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
	CWeapon* wpn1 = nullptr;
	CWeapon* wpn2 = nullptr;
	if(inventory().ItemFromSlot(INV_SLOT_2))
		wpn1 = smart_cast<CWeapon*>(inventory().ItemFromSlot(INV_SLOT_2));

	if(inventory().ItemFromSlot(INV_SLOT_3))
		wpn2 = smart_cast<CWeapon*>(inventory().ItemFromSlot(INV_SLOT_3));

	xr_vector<CAttachableItem*> const& all = CAttachmentOwner::attached_objects();
	xr_vector<CAttachableItem*>::const_iterator it = all.begin();
	xr_vector<CAttachableItem*>::const_iterator it_e = all.end();
	for ( ; it != it_e; ++it )
	{
		CTorch* torch = smart_cast<CTorch*>(*it);
		if ( torch )
		{	
			if(wpn1 && wpn1->IsZoomed())
				return;

			if(wpn2 && wpn2->IsZoomed())
				return;

			torch->SwitchNightVision();
			return;
		}
	}
}

void CActor::SwitchTorch()
{ 
	xr_vector<CAttachableItem*> const& all = CAttachmentOwner::attached_objects();
	xr_vector<CAttachableItem*>::const_iterator it = all.begin();
	xr_vector<CAttachableItem*>::const_iterator it_e = all.end();
	for ( ; it != it_e; ++it )
	{
		CTorch* torch = smart_cast<CTorch*>(*it);
		if ( torch )
		{		
			torch->Switch();
			return;
		}
	}
}

template<typename T>
void CActor::UseItemsFromFastSlots(const PIItem realItemData, const T* pRealItem, const shared_str& realItemName)
{
	static_assert(std::is_same_v<T, CEatableItemObject>, "Second argument must be of type CEatableItemObject*");

	auto l_CreateTextStatic = [&](shared_str itemStringUse) -> void
	{
		string1024 realString = "Unknown";
		SDrawStaticStruct* DrawStaticItemUsed = CurrentGameUI()->AddCustomStatic("item_used", true);

		xr_strconcat(realString, *g_pStringTable->translate(itemStringUse), ": ", realItemData->NameItem());

		CurrentGameUI()->ActorMenu().m_pQuickSlot->ReloadReferences(this);
		DrawStaticItemUsed->wnd()->TextItemControl()->SetText(realString);
	};

#ifdef DEBUG
	Msg("- [%s]: Trying use item -> %s", __FUNCTION__, realItemData->NameItem());
#endif

	if (IsGameTypeSingle())
	{
		inventory().Eat(realItemData);
	}
	else
	{ 
		inventory().ClientEat(realItemData);
	}

#ifdef DEBUG
	Msg("- [%s]: Eaten item -> %s", __FUNCTION__, realItemData->NameItem());
#endif

	l_CreateTextStatic(pRealItem->GetUseString());
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