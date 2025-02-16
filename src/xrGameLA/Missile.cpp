#include "stdafx.h"
#include "missile.h"
#include "player_hud.h"
#include "PhysicsShell.h"
#include "actor.h"
#include "../CameraBase.h"
#include "xrserver_objects_alife.h"
#include "ActorEffector.h"
#include "level.h"
#include "xr_level_controller.h"
#include "../../Include/xrRender/Kinematics.h"
#include "ai_object_location.h"
#include "ExtendedGeom.h"
#include "MathUtils.h"
#include "characterphysicssupport.h"
#include "inventory.h"
#include "Weapon.h"
#include "../IGame_Persistent.h"
#ifdef DEBUG
#	include "phdebug.h"
#endif


#define PLAYING_ANIM_TIME 15000


#include "ui/UIProgressShape.h"
#include "ui/UIXmlInit.h"

CUIProgressShape* g_MissileForceShape = NULL;

void create_force_progress()
{
	VERIFY							(!g_MissileForceShape);
	CUIXml						uiXml;
	uiXml.Load					(CONFIG_PATH, UI_PATH, "grenade.xml");

	CUIXmlInit xml_init;
	g_MissileForceShape				= new CUIProgressShape();
	xml_init.InitProgressShape		(uiXml, "progress", 0, g_MissileForceShape);
}

CMissile::CMissile(void) 
{
	m_dwStateTime		= 0;
}

CMissile::~CMissile(void) 
{
}

void CMissile::reinit		()
{
	inherited::reinit	();
	m_throw				= false;
	m_constpower = false;
	m_fThrowForce		= 0;
	m_dwDestroyTime		= 0xffffffff;
	SetPending			(FALSE);
	m_fake_missile		= NULL;
	SetState			( eHidden );
}

void CMissile::Load(LPCSTR section) 
{
	inherited::Load		(section);

	m_fMinForce			= pSettings->r_float(section,"force_min");
	m_fConstForce		= pSettings->r_float(section,"force_const");
	m_fMaxForce			= pSettings->r_float(section,"force_max");
	m_fForceGrowSpeed	= pSettings->r_float(section,"force_grow_speed");

	m_dwDestroyTimeMax	= pSettings->r_u32(section,"destroy_time");
	
	m_vThrowPoint		= pSettings->r_fvector3(section,"throw_point");
	m_vThrowDir			= pSettings->r_fvector3(section,"throw_dir");
	m_vHudThrowPoint	= pSettings->r_fvector3(*hud_sect,"throw_point");
	m_vHudThrowDir		= pSettings->r_fvector3(*hud_sect,"throw_dir");

	m_ef_weapon_type	= READ_IF_EXISTS(pSettings,r_u32,section,"ef_weapon_type",u32(-1));
}

BOOL CMissile::net_Spawn(CSE_Abstract* DC) 
{
	BOOL l_res = inherited::net_Spawn(DC);

	dwXF_Frame					= 0xffffffff;

	m_throw_direction.set(0.0f, 1.0f, 0.0f);
	m_throw_matrix.identity();

	return l_res;
}

void CMissile::net_Destroy() 
{
	inherited::net_Destroy();
	m_fake_missile = 0;
	m_dwStateTime		= 0;
}

void CMissile::OnActiveItem		()
{
	if (m_bDraw_off == true) {
		SwitchState(eShowing);
	}

	inherited::OnActiveItem	();
	SetState				(eIdle);
	SetNextState			(eIdle);		
}

void CMissile::OnHiddenItem()
{
	if (IsGameTypeSingle()) {
		if (m_bHolster_off == true) {
			SwitchState(eHiding);
		}
	} else {
		SwitchState(eHidden);
	}

	inherited::OnHiddenItem	();
	SetState				(eHidden);
	SetNextState			(eHidden);
}


void CMissile::spawn_fake_missile()
{
	if (OnClient()) return;

	if (!getDestroy())
	{
		CSE_Abstract		*object = Level().spawn_item(
			*cNameSect(),
			Position(),
			(g_dedicated_server)?u32(-1):ai_location().level_vertex_id(),
			ID(),
			true
		);

		CSE_ALifeObject				*alife_object = smart_cast<CSE_ALifeObject*>(object);
		VERIFY						(alife_object);
		alife_object->m_flags.set	(CSE_ALifeObject::flCanSave,FALSE);

		NET_Packet			P;
		object->Spawn_Write	(P,TRUE);
		Level().Send		(P,net_flags(TRUE));
		F_entity_Destroy	(object);
	}
}

void CMissile::OnH_A_Chield() 
{
	inherited::OnH_A_Chield();

//	if(!m_fake_missile && !smart_cast<CMissile*>(H_Parent())) 
//		spawn_fake_missile	();
}


void CMissile::OnH_B_Independent(bool just_before_destroy) 
{
	inherited::OnH_B_Independent(just_before_destroy);
	if (!just_before_destroy) 
	{
		VERIFY								(PPhysicsShell());
		PPhysicsShell()->SetAirResistance	(0.f, 0.f);
		PPhysicsShell()->set_DynamicScales	(1.f, 1.f);

		if(GetState() == eThrow)
		{
			Msg("Throw on reject");
			Throw				();
		}
	}

	if(!m_dwDestroyTime && Local()) 
	{
		DestroyObject		();
		return;
	}
}

void CMissile::UpdateCL() 
{
	m_dwStateTime += Device.dwTimeDelta;

	inherited::UpdateCL();
	CActor* pActor	= smart_cast<CActor*>(H_Parent());
	if (pActor && GetState() == eIdle && this==pActor->inventory().ActiveItem() && m_dwStateTime > PLAYING_ANIM_TIME )
	{
		CEntity::SEntityState st;
		pActor->g_State(st);

		if(!st.bSprint)
		{
			SwitchState			(eBore);
			ResetSubStateTime	();
		} else
			m_dwStateTime = 0;
	}
	
	if(GetState() == eReady) 
	{
		if(m_throw)
			SwitchState(eThrow);
		else {
			if (pActor)
			{				
				m_fThrowForce		+= (m_fForceGrowSpeed * Device.dwTimeDelta) * .001f;
				clamp(m_fThrowForce, m_fMinForce, m_fMaxForce);
			}
		}
	}

}
void CMissile::shedule_Update(u32 dt)
{
	inherited::shedule_Update(dt);
	if(!H_Parent() && getVisible() && m_pPhysicsShell) 
	{
		if(m_dwDestroyTime <= Level().timeServer()) 
		{
			m_dwDestroyTime = 0xffffffff;
			VERIFY	(!m_pCurrentInventory);
			Destroy	();
			return;
		}
	} 
}

void CMissile::State(u32 state) 
{
	switch(GetState()) 
	{
	case eShowing:
        {
			SetPending			(TRUE);
			PlayHUDMotion("anim_show", FALSE, this, GetState());
		} break;
	case eIdle:
		{
			SetPending			(FALSE);
			PlayAnimIdle		();
		} break;
	case eHiding:
		{
			if(H_Parent())
			{
				SetPending			(TRUE);
				PlayHUDMotion		("anim_hide", TRUE, this, GetState());
			}
		} break;
	case eHidden:
		{
			StopCurrentAnimWithoutCallback();
			
			if (H_Parent())
			{				
				setVisible(FALSE);
				setEnabled(FALSE);				
			};
			SetPending			(FALSE);			
		} break;
	case eThrowStart:
		{
			SetPending			(TRUE);
			m_fThrowForce		= m_fMinForce;
			PlayHUDMotion		("anim_throw_begin", TRUE, this, GetState());
		} break;
	case eReady:
		{
			PlayHUDMotion		("anim_throw_idle", TRUE, this, GetState());
		} break;
	case eThrow:
		{
			SetPending			(TRUE);
			m_throw				= false;
			PlayHUDMotion		("anim_throw", TRUE, this, GetState());
		} break;
	case eThrowEnd:
		{
			SwitchState			(eShowing); 
		} break;
	/*case MS_PLAYING:
		{
			if (Random.randI(30)==1)
				PlayHUDMotion		("anim_playing_fail", TRUE, this, GetState());
			else
				PlayHUDMotion		("anim_playing", TRUE, this, GetState());
		} break;
	*/
	}
}

void CMissile::OnStateSwitch	(u32 S)
{
	m_dwStateTime				= 0;
	inherited::OnStateSwitch	(S);
	State						(S);
}


void CMissile::OnAnimationEnd(u32 state) 
{
	switch(state) 
	{
	case eHiding:
		{
			setVisible(FALSE);
			SwitchState(eHidden);
		} break;
	case eShowing:
		{
			setVisible(TRUE);
			SwitchState(eIdle);
		} break;
	case eThrowStart:
		{
			if(!m_fake_missile && !smart_cast<CMissile*>(H_Parent())) 
				spawn_fake_missile	();

			if(m_throw) 
				SwitchState(eThrow); 
			else 
				SwitchState(eReady);
		} break;
	case eThrow:
		{
			SwitchState	(eThrowEnd);
		} break;
	case eThrowEnd:
		{
			SwitchState	(eShowing);
		} break;
	default:
		inherited::OnAnimationEnd(state);
	}
}


void CMissile::UpdatePosition(const Fmatrix& trans)
{
	XFORM().mul		(trans,offset());
}

void CMissile::UpdateXForm	()
{
	if (Device.dwFrame!=dwXF_Frame)
	{
		dwXF_Frame			= Device.dwFrame;

		if (0==H_Parent())	return;

		// Get access to entity and its visual
		CEntityAlive*		E		= smart_cast<CEntityAlive*>(H_Parent());
        
		if(!E)				return	;

		const CInventoryOwner	*parent = smart_cast<const CInventoryOwner*>(E);
		if (parent && parent->use_simplified_visual())
			return;

		if (parent->attached(this))
			return;

		VERIFY				(E);
		IKinematics*		V		= smart_cast<IKinematics*>	(E->Visual());
		VERIFY				(V);

		// Get matrices
		int					boneL = -1, boneR = -1, boneR2 = -1;
		E->g_WeaponBones	(boneL,boneR,boneR2);
		if (boneR == -1)	return;

		boneL = boneR2;


		V->CalculateBones	();
		Fmatrix& mL			= V->LL_GetTransform(u16(boneL));
		Fmatrix& mR			= V->LL_GetTransform(u16(boneR));

		// Calculate
		Fmatrix				mRes;
		Fvector				R,D,N;
		D.sub				(mL.c,mR.c);	D.normalize_safe();
		R.crossproduct		(mR.j,D);		R.normalize_safe();
		N.crossproduct		(D,R);			N.normalize_safe();
		mRes.set			(R,N,D,mR.c);
		mRes.mulA_43		(E->XFORM());
		UpdatePosition		(mRes);
	}
}

void CMissile::setup_throw_params()
{
	CEntity					*entity = smart_cast<CEntity*>(H_Parent());
	VERIFY					(entity);
	CInventoryOwner			*inventory_owner = smart_cast<CInventoryOwner*>(H_Parent());
	VERIFY					(inventory_owner);
	Fmatrix					trans;
	trans.identity			();
	Fvector					FirePos, FireDir;
	if (this == inventory_owner->inventory().ActiveItem())
	{
		CInventoryOwner* io		= smart_cast<CInventoryOwner*>(H_Parent());
		if(NULL == io->inventory().ActiveItem())
		{
				Msg("current_state %d", GetState() );
				Msg("next_state %d", GetNextState());
				Msg("state_time %d", m_dwStateTime);
				Msg("item_sect %s", cNameSect().c_str());
				Msg("H_Parent %s", H_Parent()->cNameSect().c_str());
		}

		entity->g_fireParams(this, FirePos, FireDir);
	}else{
		FirePos				= XFORM().c;
		FireDir				= XFORM().k;
	}
	trans.k.set				(FireDir);
	Fvector::generate_orthonormal_basis(trans.k, trans.j,trans.i);
	trans.c.set				(FirePos);
	m_throw_matrix.set		(trans);
	m_throw_direction.set	(trans.k);
}

void CMissile::OnMotionMark(u32 state, const motion_marks& M)
{
	inherited::OnMotionMark(state, M);
	if(state==eThrow && !m_throw)
	{
		if (H_Parent())
			Throw	();
	}
}

void CMissile::Throw() 
{
	/*Msg("Throw is called for %d", ID());*/
	VERIFY								(smart_cast<CEntity*>(H_Parent()));
	setup_throw_params					();
	
	m_fake_missile->m_throw_direction	= m_throw_direction;
	m_fake_missile->m_throw_matrix		= m_throw_matrix;
		
	CInventoryOwner						*inventory_owner = smart_cast<CInventoryOwner*>(H_Parent());
	VERIFY								(inventory_owner);
	if (inventory_owner->use_default_throw_force())
		m_fake_missile->m_fThrowForce	= m_constpower ? m_fConstForce : m_fThrowForce; 
	else
		m_fake_missile->m_fThrowForce	= inventory_owner->missile_throw_force(); 
	
	m_fThrowForce						= m_fMinForce;

	if (Local() && H_Parent()) 
	{
		NET_Packet						P;
		u_EventGen						(P,GE_OWNERSHIP_REJECT,ID());
		P.w_u16							(u16(m_fake_missile->ID()));
		u_EventSend						(P);
	}
}

void CMissile::OnEvent(NET_Packet& P, u16 type) 
{
	inherited::OnEvent		(P,type);
	u16						id;
	switch (type) {
		case GE_OWNERSHIP_TAKE : {
			P.r_u16(id);
			CMissile		*missile = smart_cast<CMissile*>(Level().Objects.net_Find(id));			
			m_fake_missile	= missile;
			missile->H_SetParent(this);
			missile->Position().set(Position());
			break;
		} 
		case GE_OWNERSHIP_REJECT : {
			P.r_u16			(id);
			bool IsFakeMissile = false;
			if (m_fake_missile && (id == m_fake_missile->ID()))
			{
				m_fake_missile	= NULL;
				IsFakeMissile = true;
			}

			CMissile		*missile = smart_cast<CMissile*>(Level().Objects.net_Find(id));
			if (!missile)
			{
				break;
			}
			missile->H_SetParent(0,!P.r_eof() && P.r_u8());
			if (IsFakeMissile && OnClient()) 
				missile->set_destroy_time(m_dwDestroyTimeMax);
			break;
		}
	}
}

void CMissile::Destroy() 
{
	if (Local())		DestroyObject();
}

bool CMissile::Action(u16 cmd, u32 flags) 
{
	if(inherited::Action(cmd, flags)) return true;
	/*Msg("CMissile::Action %d %d %u", ID(), cmd, flags);*/
	switch(cmd) 
	{
	case kWPN_FIRE:
		{
			m_constpower = true;			
			if(flags&CMD_START) 
			{
				if (GetState() == eIdle || GetState() == eBore)
				{
					m_throw = true;
					SwitchState(eThrowStart);
				}
			} 
			return true;
		}break;

	case kWPN_ZOOM:
		{
			m_constpower = false;
        	if(flags&CMD_START) 
			{
				m_throw = false;
				if (GetState() == eIdle || GetState() == eBore)
					SwitchState(eThrowStart);
				else 
				if(GetState()==eReady)
				{
					m_throw = true; 
				}

			} 
			else 
			if(GetState()==eReady || GetState()==eThrowStart || GetState()==eIdle) 
			{
				m_throw = true; 
				if(GetState()==eReady) 
					SwitchState(eThrow);
			}
			return true;
		}break;
	}
	return false;
}

void  CMissile::UpdateFireDependencies_internal	()
{
	if (0==H_Parent())		return;

    if (Device.dwFrame!=dwFP_Frame){
		dwFP_Frame = Device.dwFrame;

		UpdateXForm			();
		
		if (GetHUDmode() && !IsHidden()){
/*
			// 1st person view - skeletoned
			IKinematics* V			= smart_cast<IKinematics*>(GetHUD()->Visual());
			VERIFY					(V);
			V->CalculateBones		();

			// fire point&direction
			Fmatrix& parent			= GetHUD()->Transform	();
			m_throw_direction.set	(parent.k);
*/
		}else{
			// 3rd person
			Fmatrix& parent			= H_Parent()->XFORM();

			m_throw_direction.set	(m_vThrowDir);
			parent.transform_dir	(m_throw_direction);
		}
	}
}

void CMissile::activate_physic_shell()
{
	if (!smart_cast<CMissile*>(H_Parent())) {
		inherited::activate_physic_shell();
		if(m_pPhysicsShell&&m_pPhysicsShell->isActive()&&!IsGameTypeSingle())
		{
				m_pPhysicsShell->add_ObjectContactCallback		(ExitContactCallback);
				m_pPhysicsShell->set_CallbackData	(smart_cast<CPhysicsShellHolder*>(H_Root()));
		}
		return;
	}

	Fvector				l_vel;
	l_vel.set			(m_throw_direction);
	l_vel.normalize_safe();
	l_vel.mul			(m_fThrowForce);

	Fvector				a_vel;
	CInventoryOwner		*inventory_owner = smart_cast<CInventoryOwner*>(H_Root());
	if (inventory_owner && inventory_owner->use_throw_randomness()) {
		float			fi,teta,r;
		fi				= ::Random.randF(0.f,2.f*M_PI);
		teta			= ::Random.randF(0.f,M_PI);
		r				= ::Random.randF(2.f*M_PI,3.f*M_PI);
		float			rxy = r*_sin(teta);
		a_vel.set		(rxy*_cos(fi),rxy*_sin(fi),r*_cos(teta));
	}
	else
		a_vel.set		(0.f,0.f,0.f);

	XFORM().set			(m_throw_matrix);

	CEntityAlive		*entity_alive = smart_cast<CEntityAlive*>(H_Root());
	if (entity_alive && entity_alive->character_physics_support()){
		Fvector			parent_vel;
		entity_alive->character_physics_support()->movement()->GetCharacterVelocity(parent_vel);
		l_vel.add		(parent_vel);
	}

	VERIFY								(!m_pPhysicsShell);
	create_physic_shell					();
	m_pPhysicsShell->Activate			(m_throw_matrix, l_vel, a_vel);
//	m_pPhysicsShell->AddTracedGeom		();
	m_pPhysicsShell->SetAllGeomTraced	();
	m_pPhysicsShell->add_ObjectContactCallback		(ExitContactCallback);
	m_pPhysicsShell->set_CallbackData	(smart_cast<CPhysicsShellHolder*>(entity_alive));
//	m_pPhysicsShell->remove_ObjectContactCallback	(ExitContactCallback);
	m_pPhysicsShell->SetAirResistance	(0.f,0.f);
	m_pPhysicsShell->set_DynamicScales	(1.f,1.f);

	IKinematics							*kinematics = smart_cast<IKinematics*>(Visual());
	VERIFY								(kinematics);
	kinematics->CalculateBones_Invalidate();
	kinematics->CalculateBones			();
}
void	CMissile::net_Relcase(CObject* O)
{
	inherited::net_Relcase(O);
	if(PPhysicsShell()&&PPhysicsShell()->isActive())
	{
		if(O==smart_cast<CObject*>((CPhysicsShellHolder*)PPhysicsShell()->get_CallbackData()))
		{
			PPhysicsShell()->remove_ObjectContactCallback(ExitContactCallback);
			PPhysicsShell()->set_CallbackData(NULL);
		}
	}

}
void CMissile::create_physic_shell	()
{
	//create_box2sphere_physic_shell();
	CInventoryItemObject::CreatePhysicsShell();
}

void CMissile::setup_physic_shell	()
{
	VERIFY(!m_pPhysicsShell);
	create_physic_shell();
	m_pPhysicsShell->Activate	(XFORM(),0,XFORM());//,true 
	IKinematics					*kinematics = smart_cast<IKinematics*>(Visual());
	VERIFY						(kinematics);
	kinematics->CalculateBones_Invalidate();
	kinematics->CalculateBones			();
}

u32	CMissile::ef_weapon_type		() const
{
	VERIFY	(m_ef_weapon_type != u32(-1));
	return	(m_ef_weapon_type);
}


bool CMissile::render_item_ui_query()
{
	bool b_is_active_item = m_pCurrentInventory->ActiveItem()==this;
	return b_is_active_item && (GetState()==eReady) && !m_throw && smart_cast<CActor*>(H_Parent());
}

void CMissile::render_item_ui()
{
	CActor	*actor = smart_cast<CActor*>(H_Parent());
	R_ASSERT(actor);

	if(!g_MissileForceShape) 
		create_force_progress();
	float k = (m_fThrowForce-m_fMinForce)/(m_fMaxForce-m_fMinForce);
	g_MissileForceShape->SetPos	(k);
	g_MissileForceShape->Draw	();
}

void	 CMissile::ExitContactCallback(bool& do_colide,bool bo1,dContact& c,SGameMtl * /*material_1*/,SGameMtl * /*material_2*/)
{
	dxGeomUserData	*gd1=NULL,	*gd2=NULL;
	if(bo1)
	{
		gd1 =retrieveGeomUserData(c.geom.g1);
		gd2 =retrieveGeomUserData(c.geom.g2);
	}
	else
	{
		gd2 =retrieveGeomUserData(c.geom.g1);
		gd1 =retrieveGeomUserData(c.geom.g2);
	}
	if(gd1&&gd2&&(CPhysicsShellHolder*)gd1->callback_data==gd2->ph_ref_object)	
																				do_colide=false;
}

void CMissile::GetBriefInfo(xr_string& str_name, xr_string& icon_sect_name, xr_string& str_count)
{
	str_name		= NameShort();
	str_count		= "";
	icon_sect_name	= "";
}

u16 CMissile::bone_count_to_synchronize	() const
{
	return CInventoryItem::object().PHGetSyncItemsNumber();
}