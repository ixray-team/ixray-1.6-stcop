#include "stdafx.h"

#include "WeaponKnife.h"
#include "Entity.h"
#include "Actor.h"
#include "level.h"
#include "xr_level_controller.h"
#include "game_cl_base.h"
#include "../Include/xrRender/Kinematics.h"
#include "../xrEngine/gamemtllib.h"
#include "level_bullet_manager.h"
#include "ai_sounds.h"
#include "game_cl_single.h"
#include "../xrEngine/SkeletonMotions.h"
#include "player_hud.h"
#include "ActorEffector.h"

#define KNIFE_MATERIAL_NAME "objects\\knife"

#ifdef DEBUG
#	include "debug_renderer.h"
	extern BOOL g_bDrawBulletHit;
#endif //#ifdef DEBUG

CWeaponKnife::CWeaponKnife()
{
	SetState				( eHidden );
	SetNextState			( eHidden );
	knife_material_idx		= (u16)-1;
	fHitImpulse_cur			= 0.0f;

	m_Hit1Distance			= 1.f;
	m_Hit2Distance			= 1.f;

	m_Hit1SplashRadius		= 1.0f;
	m_Hit2SplashRadius		= 1.0f;

	m_Hit1SpashDir.set	(0.f,0.f,1.f);
	m_Hit2SpashDir.set	(0.f,0.f,1.f);
}

CWeaponKnife::~CWeaponKnife()
{
}

void CWeaponKnife::Load	(LPCSTR section)
{
	// verify class
	inherited::Load		(section);

	fWallmarkSize = pSettings->r_float(section,"wm_size");
	m_sounds.LoadSound(section,"snd_shoot"		, "sndShot"		, false, SOUND_TYPE_WEAPON_SHOOTING		);
	
	m_Hit1SpashDir		=	pSettings->r_fvector3(section, "splash1_direction");
	m_Hit2SpashDir		=	pSettings->r_fvector3(section, "splash2_direction");

	m_Hit1Distance		=	pSettings->r_float(section, "spash1_dist");
	m_Hit2Distance		=	pSettings->r_float(section, "spash2_dist");

	m_Hit1SplashRadius	=	pSettings->r_float(section, "spash1_radius");
	m_Hit2SplashRadius	=	pSettings->r_float(section, "spash2_radius");

	m_SplashHitBone		=	pSettings->r_string(section, "splash_bone");

	knife_material_idx =  GMLib.GetMaterialIdx(KNIFE_MATERIAL_NAME);
}

void CWeaponKnife::OnStateSwitch	(u32 S)
{
	inherited::OnStateSwitch(S);
	switch (S)
	{
	case eIdle:
		switch2_Idle	();
		break;
	case eShowing:
		switch2_Showing	();
		break;
	case eHiding:
		switch2_Hiding	();
		break;
	case eHidden:
		switch2_Hidden	();
		break;
	case eFire:
		{
			//-------------------------------------------
			m_eHitType		= m_eHitType_1;
			//fHitPower		= fHitPower_1;
			if (ParentIsActor())
			{
				if (GameID() == eGameIDSingle)
				{
					fCurrentHit			= fvHitPower_1[g_SingleGameDifficulty];
				}
				else
				{
					fCurrentHit			= fvHitPower_1[egdMaster];
				}
			}
			else
			{
				fCurrentHit			= fvHitPower_1[egdMaster];
			}
			fHitImpulse_cur	= fHitImpulse_1;
			//-------------------------------------------
			switch2_Attacking	(S);
		}break;
	case eFire2:
		{
			//-------------------------------------------
			m_eHitType		= m_eHitType_2;
			//fHitPower		= fHitPower_2;
			if (ParentIsActor())
			{
				if (GameID() == eGameIDSingle)
				{
					fCurrentHit			= fvHitPower_2[g_SingleGameDifficulty];
				}
				else
				{
					fCurrentHit			= fvHitPower_2[egdMaster];
				}
			}
			else
			{
				fCurrentHit			= fvHitPower_2[egdMaster];
			}
			fHitImpulse_cur	= fHitImpulse_2;
			//-------------------------------------------
			switch2_Attacking	(S);
		}break;
	}
}


void CWeaponKnife::KnifeStrike(const Fvector& pos, const Fvector& dir)
{
	VERIFY(H_Parent());
	m_except_id = H_Parent()->ID();
	CObject* real_victim = TryPick(pos, dir, m_hit_dist);
	if (real_victim)
	{
		MakeShot(pos, dir);
		return;
	}
	
	Fvector	new_dir;
	CObject* tmp_victim = SelectBestHitVictim(pos, new_dir);
	if (tmp_victim)
	{
#ifdef DEBUG
		m_dbg_data.m_pick_vector = new_dir;
#endif
		MakeShot(pos, new_dir);
		return;
	}
	MakeShot(pos, dir);
}

void CWeaponKnife::MakeShot(Fvector const & pos, Fvector const & dir)
{
	CCartridge						cartridge; 
	cartridge.param_s.buckShot		= 1;				
	cartridge.param_s.impair		= 1.0f;
	cartridge.param_s.kDisp			= 1.0f;
	cartridge.param_s.kHit			= 1.0f;
//.	cartridge.param_s.kCritical		= 1.0f;
	cartridge.param_s.kImpulse		= 1.0f;
	cartridge.param_s.kAP			= 1.0f;
	cartridge.m_flags.set			(CCartridge::cfTracer, FALSE);
	cartridge.m_flags.set			(CCartridge::cfRicochet, FALSE);
	cartridge.param_s.fWallmarkSize	= fWallmarkSize;
	cartridge.bullet_material_idx	= knife_material_idx;

	while(m_magazine.size() < 2)	m_magazine.push_back(cartridge);
	iAmmoElapsed					= m_magazine.size();
	bool SendHit					= SendHitAllowed(H_Parent());

	PlaySound						("sndShot",pos);

	Level().BulletManager().AddBullet(	pos, 
										dir, 
										m_fStartBulletSpeed, 
										fCurrentHit, 
										fHitImpulse_cur, 
										H_Parent()->ID(), 
										ID(), 
										m_eHitType, 
										fireDistance, 
										cartridge, 
										1.f, 
										SendHit);
}

void CWeaponKnife::OnMotionMark(u32 state, const motion_marks& M)
{
	inherited::OnMotionMark(state, M);
	if (state == eFire)
	{
		m_hit_dist		=	m_Hit1Distance;
		m_splash_dir	=	m_Hit1SpashDir;
		m_splash_radius	=	m_Hit1SplashRadius;
	} else if (state == eFire2)
	{
		m_hit_dist		=	m_Hit2Distance;
		m_splash_dir	=	m_Hit2SpashDir;
		m_splash_radius	=	m_Hit2SplashRadius;
	} else
	{
		return;
	}

	Fvector	p1, d; 
	p1.set	(get_LastFP()); 
	d.set	(get_LastFD());

	if(H_Parent())
	{
		smart_cast<CEntity*>(H_Parent())->g_fireParams(this, p1,d);
		KnifeStrike(p1,d);
	}
}

void CWeaponKnife::OnAnimationEnd(u32 state)
{
	switch (state)
	{
	case eHiding:	SwitchState(eHidden);	break;
	
	case eFire: 
	case eFire2: 	SwitchState(eIdle);		break;

	case eShowing:
	case eIdle:		SwitchState(eIdle);		break;	

	default:		inherited::OnAnimationEnd(state);
	}
}

void CWeaponKnife::state_Attacking	(float)
{
}

void CWeaponKnife::switch2_Attacking	(u32 state)
{
	if(IsPending())	return;

	if(state==eFire)
		PlayHUDMotion("anm_attack",		FALSE, this, state);
	else //eFire2
		PlayHUDMotion("anm_attack2",	FALSE, this, state);

	SetPending			(TRUE);
}

void CWeaponKnife::switch2_Idle	()
{
	VERIFY(GetState()==eIdle);

	PlayAnimIdle		();
	SetPending			(FALSE);
}

void CWeaponKnife::switch2_Hiding	()
{
	FireEnd					();
	VERIFY(GetState()==eHiding);
	PlayHUDMotion("anm_hide", TRUE, this, GetState());
}

void CWeaponKnife::switch2_Hidden()
{
	signal_HideComplete		();
	SetPending				(FALSE);
}

void CWeaponKnife::switch2_Showing	()
{
	VERIFY(GetState()==eShowing);
	PlayHUDMotion("anm_show", FALSE, this, GetState());
}


void CWeaponKnife::FireStart()
{	
	inherited::FireStart();
	SwitchState			(eFire);
}

void CWeaponKnife::Fire2Start () 
{
	SwitchState(eFire2);
}


bool CWeaponKnife::Action(u16 cmd, u32 flags) 
{
	if(inherited::Action(cmd, flags)) return true;
	switch(cmd) 
	{

		case kWPN_ZOOM : 
			if(flags&CMD_START) 
				Fire2Start			();

			return true;
	}
	return false;
}

void CWeaponKnife::LoadFireParams(LPCSTR section)
{
	inherited::LoadFireParams(section);

	string32			buffer;
	shared_str			s_sHitPower_2;
	shared_str			s_sHitPowerCritical_2;

	fvHitPower_1		= fvHitPower;
	fvHitPowerCritical_1= fvHitPowerCritical;
	fHitImpulse_1		= fHitImpulse;
	m_eHitType_1		= ALife::g_tfString2HitType(pSettings->r_string(section, "hit_type"));

	//fHitPower_2			= pSettings->r_float	(section,strconcat(full_name, prefix, "hit_power_2"));
	s_sHitPower_2			= pSettings->r_string_wb	(section, "hit_power_2" );
	s_sHitPowerCritical_2	= pSettings->r_string_wb	(section, "hit_power_critical_2" );
	
	fvHitPower_2[egdMaster]			= (float)atof(_GetItem(*s_sHitPower_2,0,buffer));//первый параметр - это хит для уровня игры мастер
	fvHitPowerCritical_2[egdMaster]	= (float)atof(_GetItem(*s_sHitPowerCritical_2,0,buffer));//первый параметр - это хит для уровня игры мастер

	fvHitPower_2[egdNovice] = fvHitPower_2[egdStalker] = fvHitPower_2[egdVeteran] = fvHitPower_2[egdMaster];//изначально параметры для других уровней сложности такие же
	fvHitPowerCritical_2[egdNovice] = fvHitPowerCritical_2[egdStalker] = fvHitPowerCritical_2[egdVeteran] = fvHitPowerCritical_2[egdMaster];//изначально параметры для других уровней сложности такие же

	int num_game_diff_param=_GetItemCount(*s_sHitPower_2);//узнаём колличество параметров для хитов
	if (num_game_diff_param>1)//если задан второй параметр хита
	{
		fvHitPower_2[egdVeteran] = (float)atof(_GetItem(*s_sHitPower_2,1,buffer));//то вычитываем его для уровня ветерана
	}
	if (num_game_diff_param>2)//если задан третий параметр хита
	{
		fvHitPower_2[egdStalker] = (float)atof(_GetItem(*s_sHitPower_2,2,buffer));//то вычитываем его для уровня сталкера
	}
	if (num_game_diff_param>3)//если задан четвёртый параметр хита
	{
		fvHitPower_2[egdNovice]  = (float)atof(_GetItem(*s_sHitPower_2,3,buffer));//то вычитываем его для уровня новичка
	}

	num_game_diff_param=_GetItemCount(*s_sHitPowerCritical_2);//узнаём колличество параметров
	if (num_game_diff_param>1)//если задан второй параметр хита
	{
		fvHitPowerCritical_2[egdVeteran] = (float)atof(_GetItem(*s_sHitPowerCritical_2,1,buffer));//то вычитываем его для уровня ветерана
	}
	if (num_game_diff_param>2)//если задан третий параметр хита
	{
		fvHitPowerCritical_2[egdStalker] = (float)atof(_GetItem(*s_sHitPowerCritical_2,2,buffer));//то вычитываем его для уровня сталкера
	}
	if (num_game_diff_param>3)//если задан четвёртый параметр хита
	{
		fvHitPowerCritical_2[egdNovice]  = (float)atof(_GetItem(*s_sHitPowerCritical_2,3,buffer));//то вычитываем его для уровня новичка
	}

	fHitImpulse_2		= pSettings->r_float	(section, "hit_impulse_2" );
	m_eHitType_2		= ALife::g_tfString2HitType(pSettings->r_string(section, "hit_type_2"));
}

bool CWeaponKnife::GetBriefInfo( II_BriefInfo& info )
{
	info.clear();
	info.name._set( m_nameShort );
	info.icon._set( cNameSect() );
	return true;
}

#ifdef DEBUG
void CWeaponKnife::OnRender()
{
	if (g_bDrawBulletHit && !fis_zero(m_dbg_data.m_pos.square_magnitude()))
	{
		CDebugRenderer& renderer	= Level().debug_renderer();
		Fmatrix	sphere				= Fmatrix().scale(.05f, .05f, .05f);
		sphere.c					= m_dbg_data.m_pos;
		renderer.draw_ellipse		(sphere, D3DCOLOR_XRGB(255, 0, 0));
		renderer.draw_line			(Fidentity, m_dbg_data.m_pos, m_dbg_data.m_endpos, D3DCOLOR_XRGB(255, 255, 0));
		float	sc_r				= m_dbg_data.m_splash_radius;
		sphere						= Fmatrix().scale(sc_r, sc_r, sc_r);
		sphere.c					= m_dbg_data.m_endpos;
		renderer.draw_ellipse		(sphere, D3DCOLOR_XRGB(100, 255, 0));
		Fvector victim_end			(m_dbg_data.m_pos);
		victim_end.add				(m_dbg_data.m_pick_vector);
		renderer.draw_line			(Fidentity, m_dbg_data.m_pos, victim_end, D3DCOLOR_XRGB(0, 255, 255));
	}
}
#endif

static bool intersect	( Fsphere const& bone, Fsphere const& query )
{
	return			bone.P.distance_to_sqr(query.P) > _sqr(bone.R + query.R);
}

static bool intersect	( Fobb bone, Fsphere const& query )
{
	Fmatrix	transform;
	bone.m_halfsize.add	( Fvector().set( query.R, query.R, query.R) );
	bone.xform_full		( transform );
	transform.invert	( );

	Fvector new_position;
	transform.transform_tiny( new_position, query.P );

	return				
		( new_position.x >= -1.f ) &&
		( new_position.y >= -1.f ) &&
		( new_position.z >= -1.f ) &&
		( new_position.x <=  1.f ) &&
		( new_position.y <=  1.f ) &&
		( new_position.z <=  1.f );
}

static bool intersect	( Fcylinder const& bone, Fsphere const& query )
{
	Fvector const bone2query	= Fvector().sub( query.P, bone.m_center );
	float const axe_projection	= bone2query.dotproduct(bone.m_direction);
	float const half_height		= bone.m_height/2.f;
	if ( _abs(axe_projection) > half_height + query.R )
		return					false;

	VERIFY						( bone2query.square_magnitude() >= _sqr(axe_projection) );
	float const axe_projection2_sqr	= bone2query.square_magnitude() - _sqr(axe_projection);
	if ( axe_projection2_sqr > _sqr(bone.m_radius + query.R) )
		return					false;

	if ( _abs(axe_projection) <= half_height )
		return					true;

	if ( axe_projection2_sqr <= _sqr(bone.m_radius) )
		return					true;

	Fvector const center_direction		= Fvector(bone.m_direction).mul( axe_projection >= 0.f ? 1.f : -1.f);
	Fvector const circle_center			= Fvector(bone.m_center).mad( center_direction, half_height );
	Fvector const circle2sphere			= Fvector().sub( query.P, circle_center );
	float const distance2plane			= circle2sphere.dotproduct( center_direction );
	VERIFY								( distance2plane > 0.f );
	VERIFY								( _sqr(query.R) >= _sqr(distance2plane) );
	float const circle_radius			= _sqrt( _sqr(query.R) - _sqr(distance2plane) );
	Fvector const sphere_circle_center	= Fvector(query.P).mad(center_direction,-distance2plane);
	float const distance2center_sqr		= circle_center.distance_to_sqr(sphere_circle_center);
	return								distance2center_sqr <= _sqr( bone.m_radius + circle_radius );
}

CWeaponKnife::best_victim_selector::best_victim_selector(
		u16 except_id,
		CWeaponKnife* owner,
		Fvector const & pos,
		Fvector & best_dir_dest,
		CObject* & best_obj_dest,
		float const query_distance) :
	m_best_object_dest(best_obj_dest),
	m_best_object_dir(best_dir_dest),
	m_except_id(except_id),
	m_owner(owner),
	m_start_pos(pos),
	m_query_distance(query_distance)
{
}

CWeaponKnife::best_victim_selector::best_victim_selector(
		best_victim_selector const & copy) :
	m_best_object_dest(copy.m_best_object_dest),
	m_best_object_dir(copy.m_best_object_dir),
	m_min_dist		(copy.m_min_dist),
	m_except_id		(copy.m_except_id),
	m_owner			(copy.m_owner),
	m_start_pos		(copy.m_start_pos),
	m_query_distance(copy.m_query_distance)
{
}

void CWeaponKnife::best_victim_selector::operator()(
	spartial_base_t::value_type const & left)
{
	CObject* const tmp_obj = left->dcast_CObject();
	VERIFY			(tmp_obj);
	if (tmp_obj->ID() == m_except_id)
		return;
	
	CEntityAlive*	const tmp_actor = smart_cast<CEntityAlive*>(tmp_obj);
	if (!tmp_actor)
		return;

	Fvector			obj_pos;
	m_owner->GetVictimPos(tmp_actor, obj_pos);

	Fvector	tmp_dir			= Fvector(obj_pos).sub(m_start_pos);
	float const tmp_dist	= tmp_dir.magnitude();
	tmp_dir.normalize		( );

	if ( tmp_dist > m_query_distance )
		return;

	/*if (tmp_obj != m_owner->TryPick(m_start_pos, tmp_dir, m_owner->fireDistance))
		return;*/
		
	if (!m_best_object_dest || (m_min_dist > tmp_dist))
	{
		m_best_object_dir	=	tmp_dir;
		m_best_object_dest	=	tmp_obj;
		m_min_dist			=	tmp_dist;
		return;
	}
}


void CWeaponKnife::GetVictimPos(CEntityAlive* victim, Fvector & pos_dest)
{
	VERIFY(victim);
	IKinematics*	tmp_kinem	= smart_cast<IKinematics*>(victim->Visual());
	u16 hit_bone_id				= tmp_kinem->LL_BoneID(m_SplashHitBone);
	if (hit_bone_id != BI_NONE)
	{
		Fmatrix			tmp_matrix;
		tmp_kinem->Bone_GetAnimPos	(tmp_matrix, hit_bone_id, u8(-1), true);
		pos_dest.set(tmp_matrix.c);
		Fmatrix	& tmp_xform			= victim->XFORM();
		tmp_xform.transform_tiny	(pos_dest);
	} else
	{
		Fbox const & tmp_box = tmp_kinem->GetBox();
		Fvector tmp_fake_vec;
		tmp_box.get_CD(pos_dest, tmp_fake_vec);
		pos_dest.add(victim->Position());
	}
	
	/*CBoneData& tmp_bone_data	= tmp_kinem->LL_GetData(hit_bone_id);
	Fmatrix	& tmp_xform			= victim->XFORM();
	CBoneInstance &bi			= tmp_kinem->LL_GetBoneInstance();

	switch (tmp_bone_data.shape.type)
	{
	case SBoneShape::stBox:
		{
			pos_dest = tmp_bone_data.shape.box.m_translate;
			break;
		};
	case SBoneShape::stSphere:
		{
			pos_dest = tmp_bone_data.shape.sphere.P;
		}break;
	case SBoneShape::stCylinder:
		{
			pos_dest = tmp_bone_data.shape.cylinder.m_center;
		}break;
	};//switch (tmp_bone_data.shape.type)
	tmp_xform.transform_tiny(pos_dest);
	bi.mTransform.transform_tiny(pos_dest);*/
}

CObject* CWeaponKnife::SelectBestHitVictim(Fvector const & f_pos,
										   Fvector & new_dir)
{
	Fvector		end_fpos;
	Fmatrix		parent_xform;

	CActor* tmp_parent = smart_cast<CActor*>(H_Parent());
	VERIFY(tmp_parent);

	if (GetHUDmode())
		tmp_parent->Cameras().hud_camera_Matrix(parent_xform);
	else
		return NULL;

	parent_xform.transform_dir	(m_splash_dir);
	end_fpos.set(f_pos).mad(m_splash_dir, m_hit_dist);

#ifdef DEBUG
	m_dbg_data.m_pos			= f_pos;
	m_dbg_data.m_endpos			= end_fpos;
	m_dbg_data.m_splash_radius	= m_splash_radius;
#endif

	m_spartial_query_res.clear	();
	g_SpatialSpace->q_sphere(
		m_spartial_query_res,
		0,
		STYPE_COLLIDEABLE,
		end_fpos,
		m_splash_radius
	);

	CObject* ret_obj = NULL;
	best_victim_selector	tmp_selector(tmp_parent->ID(), this, f_pos, new_dir, ret_obj, m_splash_radius);

	std::for_each(
		m_spartial_query_res.begin(),
		m_spartial_query_res.end(),
		tmp_selector
	);
	return ret_obj;
}

BOOL CWeaponKnife::RayQueryCallback(collide::rq_result& result, LPVOID this_ptr)
{
	CWeaponKnife*	me = static_cast<CWeaponKnife*>(this_ptr);
	if (result.O && (result.O->ID() != me->m_except_id))
	{
		me->m_last_picked_obj = result.O;
		return FALSE;	//first hit
	}
	return TRUE;
}

CObject* CWeaponKnife::TryPick(Fvector const & start_pos, Fvector const & dir, float const dist)
{
	collide::ray_defs		tmp_rdefs(start_pos, dir, dist, CDB::OPT_FULL_TEST, collide::rqtObject);
	m_ray_query_results.r_clear();
	m_last_picked_obj	= NULL;
	Level().ObjectSpace.RayQuery(
		m_ray_query_results,
		tmp_rdefs,
		&CWeaponKnife::RayQueryCallback,
		static_cast<LPVOID>(this),
		NULL,
		NULL
	);
	return m_last_picked_obj;
}