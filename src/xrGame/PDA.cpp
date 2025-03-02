#include "StdAfx.h"
#include "pch_script.h"
#include "PDA.h"
#include "../xrPhysics/PhysicsShell.h"
#include "Entity.h"
#include "Actor.h"
#include "Inventory.h"

#include "xrServer.h"
#include "xrServer_Objects_ALife_Items.h"
#include "Level.h"

#include "specific_character.h"
#include "alife_registry_wrappers.h"
#include "../xrScripts/script_engine.h"

#include "player_hud.h"
#include "UIGameCustom.h"
#include "ui/UIPdaWnd.h"
#include "../xrUI/UICursor.h"

CPda::CPda(void)						
{
	m_idOriginalOwner = u16(-1);
	m_SpecificChracterOwner = nullptr;
	TurnOff();
}

CPda::~CPda() 
{
}

BOOL CPda::net_Spawn(CSE_Abstract* DC) 
{
	inherited::net_Spawn(DC);

	CSE_Abstract* abstract = (CSE_Abstract*)(DC);
	CSE_ALifeItemPDA* pda = smart_cast<CSE_ALifeItemPDA*>(abstract);
	R_ASSERT(pda);

	m_idOriginalOwner = pda->m_original_owner;
	m_SpecificChracterOwner = pda->m_specific_character;

	return (TRUE);
}

void CPda::net_Destroy() 
{
	inherited::net_Destroy();
	TurnOff();
	feel_touch.clear();
	UpdateActiveContacts();
}

void ModifAnim(xr_string& anim)
{
	if (Actor() == nullptr)
		return;

	if (Actor()->GetMovementState(eReal) & ACTOR_DEFS::EMoveCommand::mcAnyMove)
	{
		anim += "_moving";

		if (Actor()->GetMovementState(eReal) & ACTOR_DEFS::EMoveCommand::mcCrouch && Actor()->GetMovementState(eReal) & ACTOR_DEFS::EMoveCommand::mcAccel)
			anim += "_crouch_slow";
		else if (Actor()->GetMovementState(eReal) & ACTOR_DEFS::EMoveCommand::mcCrouch)
			anim += "_crouch";
		else if (Actor()->GetMovementState(eReal) & ACTOR_DEFS::EMoveCommand::mcAccel)
			anim += "_slow";
	}
}

void CPda::PlayAnimIdle()
{
	xr_string anm = "anm_idle";

	if (m_bZoomed)
	{
		if (!m_bAimStart)
		{
			m_bAimStart = true;
			PlayHUDMotion("anm_idle_aim_start", TRUE, nullptr, GetState());
			return;
		}
		anm += "_aim";
		ModifAnim(anm);
		PlayHUDMotion(anm, TRUE, nullptr, GetState());
	}
	else
	{
		if (m_bAimStart)
		{
			m_bAimStart = false;
			PlayHUDMotion("anm_idle_aim_end", TRUE, nullptr, GetState());
			return;
		}
		inherited::PlayAnimIdle();
	}
}

void CPda::OnStateSwitch(u32 S)
{
	inherited::OnStateSwitch(S);

	if(H_Parent() != Level().CurrentEntity()) {
		return;
	}

	switch(S) {
		case eShowing:
		{
			m_bZoomed = false;
			m_fZoomfactor = 0.f;

			SetPending(TRUE);
			CurrentGameUI()->ShowPdaMenu();

			g_pGamePersistent->pda_shader_data.pda_display_factor = 0.f;

			m_sounds.PlaySound("sndShow", Position(), H_Root(), !!GetHUDmode(), false);
			PlayHUDMotion("anm_show", TRUE, this, GetState());
		}
		break;
		case eIdle:
		{
			PlayAnimIdle();
		}
		break;
		case eHiding:
		{
			m_sounds.PlaySound("sndHide", Position(), H_Root(), !!GetHUDmode(), false);
			PlayHUDMotion("anm_hide", TRUE, this, GetState());
			m_bZoomed = false;

			SetPending(TRUE);
			CurrentGameUI()->HidePdaMenu();

			g_pGamePersistent->pda_shader_data.pda_display_factor = 0.f;
		}
		break;
		case eHidden:
		{
			m_bZoomed = false;
			m_fZoomfactor = 0.f;

			SetPending(FALSE);
			CurrentGameUI()->HidePdaMenu();

			g_pGamePersistent->pda_shader_data.pda_display_factor = 0.f;
		}
		break;
	}
}

void CPda::OnAnimationEnd(u32 state)
{
	inherited::OnAnimationEnd(state);

	switch(state) {
		case eShowing:
		{
			SetPending(FALSE);
			SwitchState(eIdle);
		}
		break;
		case eHiding:
		{
			SetPending(FALSE);
			SwitchState(eHidden);
		}
		break;
	}
}

void CPda::Load(LPCSTR section) 
{
	m_fRadius = pSettings->r_float(section, "radius");
	m_functor_str = READ_IF_EXISTS(pSettings, r_string, section, "play_function", "");

	if (pSettings->line_exist(section, "hud"))
	{
		// FX: Is HUD mode
		inherited::Load(section);

		m_sounds.LoadSound(section, "snd_draw", "sndShow", true);
		m_sounds.LoadSound(section, "snd_holster", "sndHide", true);
		m_sounds.LoadSound(section, "snd_draw_empty", "sndShowEmpty", true);
		m_sounds.LoadSound(section, "snd_holster_empty", "sndHideEmpty", true);
		m_sounds.LoadSound(section, "snd_btn_press", "sndButtonPress");
		m_sounds.LoadSound(section, "snd_btn_release", "sndButtonRelease");
		m_sounds.LoadSound(section, "snd_empty", "sndEmptyBattery", true);
	}
	else
	{
		CInventoryItemObject::Load(section);

		hud_sect = "";
	}
}

void CPda::shedule_Update(u32 dt) {
	inherited::shedule_Update(dt);

	if(!H_Parent()) return;
	Position().set(H_Parent()->Position());

	if(IsOn() && Level().CurrentEntity() && Level().CurrentEntity()->ID() == H_Parent()->ID()) {
		CEntityAlive* EA = smart_cast<CEntityAlive*>(H_Parent());
		if(!EA || !EA->g_Alive()) {
			TurnOff();
			return;
		}

		feel_touch_update(Position(), m_fRadius);
		UpdateActiveContacts();
	}
}

void CPda::UpdateActiveContacts	()
{
	m_active_contacts.resize(0);
	xr_vector<CObject*>::iterator it= feel_touch.begin();
	for(;it!=feel_touch.end();++it){
		CEntityAlive* pEA = smart_cast<CEntityAlive*>(*it);
		if(!!pEA->g_Alive() && !pEA->cast_base_monster() && !pEA->cast_car())
		{
			m_active_contacts.push_back(*it);
		}
	}
}

void CPda::feel_touch_new(CObject* O) 
{
	CEntityAlive* entity_alive = smart_cast<CEntityAlive*>(O);
	CInventoryOwner* pNewContactInvOwner = smart_cast<CInventoryOwner*>(O);

	if (!entity_alive->cast_base_monster() && !entity_alive->cast_car() && pNewContactInvOwner)
	{
		CInventoryOwner* pOwner = smart_cast<CInventoryOwner*>(H_Parent()); VERIFY(pOwner);
		pOwner->NewPdaContact(pNewContactInvOwner);
	}
}

void CPda::feel_touch_delete(CObject* O) 
{
	if(!H_Parent()) {
		return;
	}

	CEntityAlive* entity_alive = smart_cast<CEntityAlive*>(O);
	CInventoryOwner* pLostContactInvOwner = smart_cast<CInventoryOwner*>(O);
	
	if (!entity_alive->cast_base_monster() && !entity_alive->cast_car() && pLostContactInvOwner)
	{
		CInventoryOwner* pOwner = smart_cast<CInventoryOwner*>(H_Parent()); VERIFY(pOwner);
		pOwner->LostPdaContact(pLostContactInvOwner);
	}
}

BOOL CPda::feel_touch_contact(CObject* O)
{
	CEntityAlive* entity_alive = smart_cast<CEntityAlive*>(O);

	if (entity_alive && (entity_alive->cast_base_monster() || entity_alive->cast_car()))
	{
		return TRUE;
	}
	else if (CInventoryOwner* pInvOwner = smart_cast<CInventoryOwner*>(O))
	{
		if (this != pInvOwner->GetPDA())
		{
			CEntityAlive* pEntityAlive = smart_cast<CEntityAlive*>(O);
			if (pEntityAlive)
				return TRUE;
		}
		else
			return FALSE;
	}

	return FALSE;
}

void CPda::OnH_A_Chield() 
{
	VERIFY(IsOff());

	if(H_Parent()->ID() == m_idOriginalOwner) {
		TurnOn();
		if(m_sFullName.empty()) {
			m_sFullName.assign(NameItem());
			m_sFullName += " ";
			m_sFullName += (smart_cast<CInventoryOwner*>(H_Parent()))->Name();
		}
	};

	inherited::OnH_A_Chield();
}

void CPda::OnH_B_Independent(bool just_before_destroy) {
	inherited::OnH_B_Independent(just_before_destroy);
	TurnOff();
}

CInventoryOwner* CPda::GetOriginalOwner() {
	CObject* pObject = Level().Objects.net_Find(GetOriginalOwnerID());
	CInventoryOwner* pInvOwner = smart_cast<CInventoryOwner*>(pObject);

	return pInvOwner;
}

void CPda::ActivePDAContacts(xr_vector<CPda*>& res)
{
	res.resize(0);

	xr_vector<CObject*>::iterator it = m_active_contacts.begin();
	xr_vector<CObject*>::iterator it_e = m_active_contacts.end();

	for(; it != it_e; ++it) {
		CPda* p = GetPdaFromOwner(*it);
		if(p)
			res.push_back(p);
	}
}

void CPda::save(NET_Packet& output_packet) {
	inherited::save(output_packet);
	save_data(m_sFullName, output_packet);
}

void CPda::load(IReader& input_packet) {
	inherited::load(input_packet);
	load_data(m_sFullName, input_packet);
}

CObject* CPda::GetOwnerObject() {
	return Level().Objects.net_Find(GetOriginalOwnerID());
}

CPda* CPda::GetPdaFromOwner(CObject* owner) {
	return smart_cast<CInventoryOwner*>(owner)->GetPDA();
}

void CPda::PlayScriptFunction()
{
	if(xr_strcmp(m_functor_str, ""))
	{
		luabind::functor<void> m_functor;
		R_ASSERT(ai().script_engine().functor(m_functor_str.c_str(), m_functor));
		m_functor();
	}
}

void CPda::UpdateXForm()
{
	CInventoryItem::UpdateXForm();
}

void CPda::OnMoveToRuck(const SInvItemPlace& prev)
{
	inherited::OnMoveToRuck(prev);

	if(!ParentIsActor()) {
		return;
	}

	if (prev.type == eItemPlaceSlot)
	{
		SwitchState(eHidden);
	}

	CurrentGameUI()->HidePdaMenu();

	StopCurrentAnimWithoutCallback();
	SetPending(FALSE);
}

u8 CPda::GetCurrentHudOffsetIdx()
{
	bool b_aiming = ((m_bZoomed && m_fZoomfactor <= 1.f) || (!m_bZoomed && m_fZoomfactor > 0.f));
	return b_aiming ? 1 : 0;
}

void CPda::UpdateHudAdditonal(Fmatrix& trans) {
	attachable_hud_item* hi = HudItemData();

	if (!hi)
	{
		return;
	}

	u8 idx = GetCurrentHudOffsetIdx();
	Fvector curr_offs = hi->m_measures.m_hands_offset[0][idx];
	Fvector curr_rot = hi->m_measures.m_hands_offset[1][idx];

	curr_offs.mul(m_fZoomfactor);
	curr_rot.mul(m_fZoomfactor);

	Fmatrix hud_rotation;
	hud_rotation.identity();
	hud_rotation.rotateX(curr_rot.x);

	Fmatrix hud_rotation_y;
	hud_rotation_y.identity();
	hud_rotation_y.rotateY(curr_rot.y);
	hud_rotation.mulA_43(hud_rotation_y);

	hud_rotation_y.identity();
	hud_rotation_y.rotateZ(curr_rot.z);
	hud_rotation.mulA_43(hud_rotation_y);

	hud_rotation.translate_over(curr_offs);
	trans.mulB_43(hud_rotation);

	if (m_bZoomed)
		m_fZoomfactor += Device.fTimeDelta / 0.25f;
	else
		m_fZoomfactor -= Device.fTimeDelta / 0.25f;

	clamp(m_fZoomfactor, 0.f, 1.f);

	// inherited::UpdateHudAdditonal(trans);
}

void CPda::UpdateCL()
{
	inherited::UpdateCL();

	if(H_Parent() != Level().CurrentEntity()) {
		return;
	}

	bool b_main_menu_is_active = (g_pGamePersistent->m_pMainMenu && g_pGamePersistent->m_pMainMenu->IsActive());
	// For battery icon
	float condition = GetCondition();
	CUIPdaWnd* pda = &CurrentGameUI()->PdaMenu();

	//if (pda->IsShown())
	//{
	//	// Force update PDA UI if it's disabled (no input) and check for deferred enable or zoom in.
	//	if (!pda->IsEnabled())
	//	{
	//		pda->Update();
	//		pda->Enable(true);
	//		//m_bZoomed = true;
	//	}
	//	// Disable PDA UI input if player is sprinting and no deferred input enable is expected.
	//	else
	//	{
	//		CEntity::SEntityState st;
	//		Actor()->g_State(st);
	//		if (st.bSprint && !st.bCrouch)
	//		{
	//			pda->Enable(false);
	//			m_bZoomed = false;
	//		}
	//	}
	//}
	//else
	if(!pda->IsShown())
	{
		// Show PDA UI if possible
	//	if (!b_main_menu_is_active && state != eHiding && state != eHidden)
		if(GetState() == eIdle && GetNextState() == eIdle)
		{
			SwitchState(eHiding);
			m_pInventory->Activate(NO_ACTIVE_SLOT);
		}

		g_pGamePersistent->pda_shader_data.pda_display_factor = 0.0f;
	}
	else
	{
		u32 state = Actor()->GetMovementState(eReal);
		if (state & ACTOR_DEFS::EMoveCommand::mcSprint)
			m_bZoomed = false;

		g_pGamePersistent->pda_shader_data.pda_display_factor = 1.0f;
	}
}

bool CPda::Action(u16 cmd, u32 flags)
{
	switch(cmd)
	{
		case kWPN_ZOOM:
		{
			if(flags & CMD_START)
			{
				if (GetState() == eIdle)
				{
					m_bZoomed = !m_bZoomed;

					u32 state = Actor()->GetMovementState(eReal);
					if (state & ACTOR_DEFS::EMoveCommand::mcSprint)
						m_bZoomed = false;
					else
						PlayAnimIdle();

					if (m_bZoomed)
					{
						GetUICursor().SetUICursorPosition(GetUICursor().GetCursorPosition());
					}
				}

				return true;
			}
		}
	}
	return inherited::Action(cmd, flags);
}

void CPda::OnActiveItem()
{
	if(H_Parent() != Level().CurrentEntity()) {
		return;
	}

	SwitchState(eShowing);
}

void CPda::OnHiddenItem()
{
	if(H_Parent() != Level().CurrentEntity()) {
		return;
	}

	SwitchState(eHiding);
}