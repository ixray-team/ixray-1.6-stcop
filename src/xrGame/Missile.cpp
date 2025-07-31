#include "StdAfx.h"
#include "Missile.h"
//.#include "WeaponHUD.h"
#include "../xrPhysics/PhysicsShell.h"
#include "Actor.h"
#include "../xrEngine/CameraBase.h"
#include "xrServer_Objects_ALife.h"
#include "ActorEffector.h"
#include "Level.h"
#include "../Include/xrRender/Kinematics.h"
#include "ai_object_location.h"
#include "../xrPhysics/MathUtils.h"
#include "CharacterPhysicsSupport.h"
#include "Inventory.h"
#include "../xrEngine/IGame_Persistent.h"
#include "../xrSound/ai_sounds.h"
#ifdef DEBUG
#	include "phdebug.h"
#endif

#define PLAYING_ANIM_TIME 10000

#include "../../xrUI/Widgets/UIProgressShape.h"
#include "../../xrUI/UIXmlInit.h"
#include "PhysicsShellHolder.h"

CUIProgressShape* g_MissileForceShape = nullptr;

void create_force_progress()
{
	VERIFY							(!g_MissileForceShape);
	CUIXml uiXml;
	uiXml.Load						(CONFIG_PATH, UI_PATH, "grenade.xml");


	CUIXmlInit						xml_init;
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
	m_fake_missile		= nullptr;
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

	m_ef_weapon_type	= READ_IF_EXISTS(pSettings,r_u32,section,"ef_weapon_type",u32(-1));

	if (pSettings->line_exist(section, "checkout_bones"))
	{
		m_sCheckoutBones.clear();
		LPCSTR lineStr = pSettings->r_string(section, "checkout_bones");
		for (int j = 0, cnt = _GetItemCount(lineStr); j < cnt; ++j)
		{
			string128 bone_name;
			_GetItem(lineStr, j, bone_name);
			m_sCheckoutBones.push_back(bone_name);
		}
	}
}

void CMissile::LoadSounds(LPCSTR section)
{
	inherited::LoadSounds(section);

	if (pSettings->line_exist(section, "snd_draw"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_draw, TRUE);
		m_sounds.LoadSound(section, "snd_draw", "SndShow", false, ESoundTypes(SOUND_TYPE_ITEM_TAKING));
	}

	if (pSettings->line_exist(section, "snd_holster"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_holster, TRUE);
		m_sounds.LoadSound(section, "snd_holster", "SndHide", false, ESoundTypes(SOUND_TYPE_ITEM_HIDING));
	}

	if (pSettings->line_exist(section, "snd_throw_begin"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_throw_begin, TRUE);
		m_sounds.LoadSound(section, "snd_throw_begin", "sndThrowBegin", false, ESoundTypes(SOUND_TYPE_ITEM_TAKING));
	}

	if (pSettings->line_exist(section, "snd_throw"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_throw, TRUE);
		m_sounds.LoadSound(section, "snd_throw", "sndThrow", false, ESoundTypes(SOUND_TYPE_ITEM_HIDING));
	}
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

void CMissile::PH_A_CrPr		()
{
	if (m_just_after_spawn)
	{
		CPhysicsShellHolder& obj = CInventoryItem::object();
		VERIFY(obj.Visual());
		IKinematics *K = obj.Visual()->dcast_PKinematics();
		VERIFY( K );
		if (!obj.PPhysicsShell())
		{
			Msg("! ERROR: PhysicsShell is nullptr, object [%s][%d]", obj.cName().c_str(), obj.ID());
			return;
		}
		if(!obj.PPhysicsShell()->isFullActive())
		{
			K->CalculateBones_Invalidate();
			K->CalculateBones(TRUE);
		}
		obj.PPhysicsShell()->GetGlobalTransformDynamic(&obj.XFORM());
		K->CalculateBones_Invalidate();
		K->CalculateBones(TRUE);
		obj.spatial_move();
		m_just_after_spawn = false;
	}
}

void CMissile::OnActiveItem		()
{
	SwitchState				(eShowing);
	inherited::OnActiveItem	();
	SetState				(eIdle);
	SetNextState			(eIdle);	
}

void CMissile::OnHiddenItem()
{

//. -Hide
	if(IsGameTypeSingle())
		SwitchState			(eHiding);
	else
		SwitchState			(eHidden);
//-

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
			Throw				();
		}
	}

	if(!m_dwDestroyTime && Local()) 
	{
		DestroyObject		();
		return;
	}
}

extern u32 hud_adj_mode;

void CMissile::UpdateCL() 
{
	m_dwStateTime += Device.dwTimeDelta;

	inherited::UpdateCL();

	if (AllowBore())
	{
		CActor* pActor = H_Parent() ? H_Parent()->cast_actor() : NULL;
		if (pActor && !pActor->AnyMove() && this == pActor->inventory().ActiveItem())
		{
			if (hud_adj_mode == 0 && GetState() == eIdle && (Device.dwTimeGlobal - m_dw_curr_substate_time > 20000))
			{
				SwitchState(eBore);
				ResetSubStateTime();
			}
		}
	}


	if(GetState() == eReady) 
	{
		if(m_throw)
		{ 
			SwitchState(eThrow);
		}else 
		{
			CActor	*actor = H_Parent() ? H_Parent()->cast_actor() : NULL;
			if (actor) 
			{				
				m_fThrowForce		+= (m_fForceGrowSpeed * Device.dwTimeDelta) * .001f;
				clamp(m_fThrowForce, m_fMinForce, m_fMaxForce);
			}
		}
	}

	if (Device.dwFrame == dwUpdateSounds_Frame)
		return;

	dwUpdateSounds_Frame = Device.dwFrame;

	Fvector P;
	Center(P);

	if (m_sounds.FindSoundItem("SndShow", false))
		m_sounds.SetPosition("SndShow", P);

	if (m_sounds.FindSoundItem("SndHide", false))
		m_sounds.SetPosition("SndHide", P);

	if (m_sounds.FindSoundItem("sndThrow", false))
		m_sounds.SetPosition("sndThrow", P);

	if (m_sounds.FindSoundItem("sndThrowBegin", false))
		m_sounds.SetPosition("sndThrowBegin", P);
}

void CMissile::shedule_Update(u32 dt)
{
	inherited::shedule_Update(dt);
	if(!H_Parent() && getVisible() && m_pPhysicsShell) 
	{
		if(m_dwDestroyTime <= Level().timeServer()) 
		{
			m_dwDestroyTime = 0xffffffff;
			VERIFY	(!m_pInventory);
			Destroy	();
			return;
		}
	}
}
#include "player_hud.h"
void CMissile::State(u32 state) 
{
	switch(GetState()) 
	{
	case eShowing:
        {
			SetPending			(TRUE);
			PlayHUDMotion("anm_show", FALSE, GetState());

			if (m_eSoundsFlags.test(ESoundsFlags::sf_draw))
			{
				PlaySound("SndShow", Position());
			}
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
				PlayHUDMotion		("anm_hide", TRUE, GetState());
				if (m_eSoundsFlags.test(ESoundsFlags::sf_holster))
				{
					PlaySound("SndHide", Position());
				}
			}
		} break;
	case eHidden:
		{
			
			if (1 /*GetHUD()*/) 
			{
				StopCurrentAnimWithoutCallback	();
			};
			
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
			if (m_eSoundsFlags.test(ESoundsFlags::sf_throw_begin))
			{
				PlaySound("sndThrowBegin", Position());
			}
			PlayHUDMotion		("anm_throw_begin", TRUE, GetState());

			if (CActor* actor = H_Parent() != nullptr ? H_Parent()->cast_actor() : nullptr)
			{
				if (CCustomDetector* det = actor->GetDetector())
				{
					if (det->CanThrowHand())
					{
						det->SwitchState(CCustomDetector::EDetectorStates::eHandThrowStart);
					}
				}
			}
		} break;
	case eReady:
		{
			PlayHUDMotion		("anm_throw_idle", TRUE, GetState());
			if (CActor* actor = H_Parent() != nullptr ? H_Parent()->cast_actor() : nullptr)
			{
				if (CCustomDetector* det = actor->GetDetector())
				{
					if (det->CanThrowHand())
					{
						det->SwitchState(CCustomDetector::EDetectorStates::eHandThrowIdle);
					}
				}
			}
		} break;
	case eThrow:
		{
			SetPending			(TRUE);
			m_throw				= false;
			if (m_eSoundsFlags.test(ESoundsFlags::sf_throw))
			{
				PlaySound("sndThrow", Position());
			}
			PlayHUDMotion		("anm_throw", TRUE, GetState());

			if (CActor* actor = H_Parent() != nullptr ? H_Parent()->cast_actor() : nullptr)
			{
				if (CCustomDetector* det = actor->GetDetector())
				{
					if (det->CanThrowHand())
					{
						det->SwitchState(CCustomDetector::EDetectorStates::eHandThrowEnd);
					}
				}
			}

		} break;
	case eThrowEnd:
		{
			SwitchState			(eShowing); 
		} break;
/*	case eBore:
		{
			PlaySound			(sndPlaying,Position());
			PlayHUDMotion		("anm_bore", TRUE, GetState());
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
			if(H_Parent() && !m_fake_missile && !H_Parent()->cast_missile())
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

		if (!H_Parent())	return;

		// Get access to entity and its visual
		CGameObject* GO = H_Parent()->cast_game_object(); if (!GO || GO->cast_trader()) return;
		CEntityAlive* E = GO->cast_entity_alive(); if(!E) return;
		CInventoryOwner	*IO = GO->cast_inventory_owner(); if (IO && IO->use_simplified_visual()) return;

		if (IO->attached(this))
			return;

		IKinematics* V = PKinematics(E->Visual());

		// Get matrices
		int					boneL = -1, boneR = -1, boneR2 = -1;
		E->g_WeaponBones	(boneL,boneR,boneR2);
		if (boneR == -1)	return;


		boneL = boneR2;

		Fmatrix mL, mR;
		if (GO->cast_actor())
		{
			V->Bone_GetAnimPos(mL, boneL, u8(-1), false);
			V->Bone_GetAnimPos(mR, boneR, u8(-1), false);
		}
		else
		{
			// V->CalculateBones();
			mL = V->LL_GetTransform(boneL);
			mR = V->LL_GetTransform(boneR);
		}

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
	if (!H_Parent()) return;
	CGameObject* GO = H_Parent()->cast_game_object();
	if (!GO || GO->getDestroy()) return;
	CEntity					*entity = GO->cast_entity();
	if (!entity) return;
	CInventoryOwner			*inventory_owner = entity->cast_inventory_owner();
	if (!inventory_owner || !inventory_owner->m_inventory) return;
	Fmatrix					trans;
	trans.identity			();
	Fvector					FirePos, FireDir;
	if (this == inventory_owner->inventory().ActiveItem())
	{
		entity->g_fireParams(this, FirePos, FireDir);
	}
	else
	{
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
	if (!H_Parent()) 
		return;

	CActor* pActor = smart_cast<CActor*>(H_Parent());

	if (pActor && pActor == Level().CurrentControlEntity() || Local())
	{
		VERIFY(smart_cast<CEntity*>(H_Parent()));
		setup_throw_params();

		m_fake_missile->m_throw_direction = m_throw_direction;
		m_fake_missile->m_throw_matrix = m_throw_matrix;

		CInventoryOwner* inventory_owner = smart_cast<CInventoryOwner*>(H_Parent());
		VERIFY(inventory_owner);
		if (inventory_owner->use_default_throw_force())
			m_fake_missile->m_fThrowForce = m_constpower ? m_fConstForce : m_fThrowForce;
		else
			m_fake_missile->m_fThrowForce = inventory_owner->missile_throw_force();

		m_fThrowForce = m_fMinForce;
	}

	if (Local() && H_Parent()) {
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
			CObject* O = Level().Objects.net_Find(id); if(!O || O->getDestroy()) break;
			CMissile *missile = O->cast_missile(); if(!missile) break;
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
				m_fake_missile	= nullptr;
				IsFakeMissile = true;
			}

			CObject* O = Level().Objects.net_Find(id); if (!O || O->getDestroy()) break;
			CMissile* missile = O->cast_missile(); if (!missile) break;
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
		
		if (GetHUDmode() && !IsHidden())
		{
			R_ASSERT(0);  //implement this!!!
/*
			// 1st person view - skeletoned
			CKinematics* V			= smart_cast<CKinematics*>(GetHUD()->Visual());
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
	if (H_Parent() && !H_Parent()->cast_missile())
	{
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
	CInventoryOwner		*inventory_owner = H_Root() ? H_Root()->cast_inventory_owner() : NULL;
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

	CEntityAlive *entity_alive = H_Root()->cast_entity_alive();
	if (entity_alive && entity_alive->character_physics_support())
	{
		Fvector			parent_vel_;
		entity_alive->character_physics_support()->movement()->GetCharacterVelocity(parent_vel_);
		l_vel.add		(parent_vel_);
	}

	R_ASSERT							(!m_pPhysicsShell);
	create_physic_shell					();
	m_pPhysicsShell->Activate			(m_throw_matrix, l_vel, a_vel);
//	m_pPhysicsShell->AddTracedGeom		();
	m_pPhysicsShell->SetAllGeomTraced	();
	m_pPhysicsShell->add_ObjectContactCallback		(ExitContactCallback);
	m_pPhysicsShell->set_CallbackData	(H_Root()->cast_game_object());
//	m_pPhysicsShell->remove_ObjectContactCallback	(ExitContactCallback);
	m_pPhysicsShell->SetAirResistance	(0.f,0.f);
	m_pPhysicsShell->set_DynamicScales	(1.f,1.f);

	IKinematics							*kinematics = PKinematics(Visual());
	VERIFY								(kinematics);
	kinematics->CalculateBones_Invalidate();
	if (m_fThrowForce != 0.f&&!m_sCheckoutBones.empty())
	{
		u16 bone_id;
		for (const auto& boneName : m_sCheckoutBones)
		{
			bone_id = kinematics->LL_BoneID(boneName);
			if (bone_id != BI_NONE && kinematics->LL_GetBoneVisible(bone_id))
				kinematics->LL_SetBoneVisible(bone_id, FALSE, TRUE);
		}
	}
	kinematics->CalculateBones			(TRUE);
}
void	CMissile::net_Relcase(CObject* O)
{
	inherited::net_Relcase(O);
	if(PPhysicsShell()&&PPhysicsShell()->isActive())
	{
		if(O==smart_cast<CObject*>((CPhysicsShellHolder*)PPhysicsShell()->get_CallbackData()))
		{
			PPhysicsShell()->remove_ObjectContactCallback(ExitContactCallback);
			PPhysicsShell()->set_CallbackData(nullptr);
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
	R_ASSERT(!m_pPhysicsShell);
	create_physic_shell();
	m_pPhysicsShell->Activate	(XFORM(),0,XFORM());//,true 
	IKinematics					*kinematics = PKinematics(Visual());
	R_ASSERT					(kinematics);
	kinematics->CalculateBones_Invalidate();
	kinematics->CalculateBones			(TRUE);
}

u32	CMissile::ef_weapon_type		() const
{
	VERIFY	(m_ef_weapon_type != u32(-1));
	return	(m_ef_weapon_type);
}


bool CMissile::render_item_ui_query()
{
	bool b_is_active_item = m_pInventory->ActiveItem()==this;
	return b_is_active_item && (GetState()==eReady) && !m_throw && smart_cast<CActor*>(H_Parent());
}

void CMissile::render_item_ui()
{
	if (!H_Parent() || !H_Parent()->cast_actor()) return;

	if(!g_MissileForceShape) 
		create_force_progress();
	float k = (m_fThrowForce-m_fMinForce)/(m_fMaxForce-m_fMinForce);
	g_MissileForceShape->SetPos	(k);
	g_MissileForceShape->Draw	();
}

void CMissile::ExitContactCallback(bool& do_colide,bool bo1,dContact& c,SGameMtl * /*material_1*/,SGameMtl * /*material_2*/)
{
	dxGeomUserData	*gd1=nullptr,	*gd2=nullptr;
	if(bo1)
	{
		gd1 =PHRetrieveGeomUserData(c.geom.g1);
		gd2 =PHRetrieveGeomUserData(c.geom.g2);
	}
	else
	{
		gd2 =PHRetrieveGeomUserData(c.geom.g1);
		gd1 =PHRetrieveGeomUserData(c.geom.g2);
	}
	if (gd1 && gd2 && (CPhysicsShellHolder*)gd1->callback_data == gd2->ph_ref_object)	
	{
		do_colide = false;
	}

	if (do_colide)
	{
		ExitContactCallback_Patch(c.geom.g2);
		ExitContactCallback_Patch(c.geom.g1);
	}
}

#include "Grenade.h"

void CMissile::ExitContactCallback_Patch(dGeomID dxGeom)
{
	dxGeomUserData* geom_data = PHRetrieveGeomUserData(dxGeom);

	if (geom_data == nullptr)
	{
		return;
	}

	CGrenade* grenade = smart_cast<CGrenade*>(geom_data->ph_ref_object);

	if (grenade == nullptr)
	{
		return;
	}

	auto& contact_params = grenade->ContactParams();

	u32 destroy_time = grenade->destroy_time();
	u32 now = Device.dwTimeGlobal;

	if (destroy_time == 0xffffffff || destroy_time <= now)
	{
		return;
	}

	u32 time_from_throw = grenade->destroy_time_max() - (destroy_time - now);

	u32 safe_time = contact_params.SafeTime;
	u32 delay_time = contact_params.DelayTime;

	if (safe_time != 0 && safe_time > time_from_throw)
	{
		grenade->set_destroy_time_now(0xffffffff);
	}
	else if (delay_time != 0 && delay_time > time_from_throw)
	{
		// Просто пропускаем контакт
	}
	else if (contact_params.ExplosionOnKick)
	{
		u32 new_destroy_time = now;
		float min_speed = contact_params.MinExplosionSpeed;
		if (min_speed > 0.0f)
		{
			CPhysicsShellHolder* cpsh = smart_cast<CPhysicsShellHolder*>(grenade);
			if (cpsh)
			{
				Fvector linear_vel = zero_vel;
				cpsh->m_pPhysicsShell->get_LinearVel(linear_vel);
				float speed = linear_vel.magnitude();
				if (speed < min_speed)
				{
					if (contact_params.DeactivateOnLowSpeedContact)
					{
						new_destroy_time = 0xffffffff;
					}
					else
					{
						new_destroy_time = destroy_time;
					}
				}
			}
		}
		grenade->set_destroy_time_now(new_destroy_time);
	}
}

bool CMissile::GetBriefInfo( II_BriefInfo& info )
{
	info.clear();
	info.name._set( m_nameShort );
	return true;
}