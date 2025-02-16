#include "pch_script.h"
#pragma hdrstop
#include "mounted_turret.h"
#include "xrServer_Objects_ALife.h"
#include "camerafirsteye.h"
#include "actor.h"
#include "weaponammo.h"
#include "actoreffector.h"
#include "effectorshot.h"
#include "../xrSound/ai_sounds.h"
#include "level.h"
#include "../xrEngine/xr_level_controller.h"
#include "../../Include/xrRender/Kinematics.h"
#include "game_object_space.h"
#include "PhysicsShell.h"
#include "Physics.h"


#define MOUNTED_TURRET_DEF_SECT			"mounted_weapon_definition"
#define MOUNTED_TURRET_CAM_SECT			"mounted_weapon_cam"

#define MOUNTED_TURRET_MASS				250.f

static const float dir_eps			= deg2rad								(5.f);

CMountedTurret::CMountedTurret()
{
	m_allow_fire					= false;
	m_camera						= new CCameraFirstEye(this, CCameraBase::flRelativeLink | CCameraBase::flPositionRigid | CCameraBase::flDirectionRigid);
	m_camera->Load													(MOUNTED_TURRET_CAM_SECT);
	m_current_ammo					= new CCartridge();
	m_temperature = m_temp_incr		= 0;
}

CMountedTurret::~CMountedTurret()
{
	xr_delete														(m_camera);
	xr_delete														(m_current_ammo);
	HUD_SOUND_ITEM::DestroySound											(m_snd_shot);
}


void CMountedTurret::Load(LPCSTR section)
{
	CShootingObject::Load											(section);
	CEntity::Load													(section);
//	ISpatial *self					= smart_cast<ISpatial*>			(this);
//	if (self)
//		self->spatial.type			|=	STYPE_VISIBLEFORAI;	

	HUD_SOUND_ITEM::LoadSound											(section, "snd_shoot", m_snd_shot, SOUND_TYPE_WEAPON_SHOOTING);

	m_ammo_type						= pSettings->r_string			(section, "ammo_class");
	m_current_ammo->Load											(*m_ammo_type, 0);

	m_cam_max_angle					= deg2rad						(pSettings->r_float(section, "cam_max_angle")); 
	m_cam_relax_speed				= deg2rad						(pSettings->r_float(section, "cam_relax_speed")); 
}

BOOL CMountedTurret::net_Spawn(CSE_Abstract* DC)
{
	CSE_Abstract			*e		= (CSE_Abstract*)						(DC);
	CSE_ALifeMountedTurret	*mt		= smart_cast<CSE_ALifeMountedTurret*>	(e);
	R_ASSERT																(mt);

	if (!CEntity::net_Spawn(DC))
		return FALSE;

	R_ASSERT																(Visual() && smart_cast<IKinematics*>(Visual()));

	CPHSkeleton::Spawn														((CSE_Abstract*)(DC));
	setVisible																(TRUE);
	setEnabled																(TRUE);
	IKinematics				*K		= smart_cast<IKinematics*>				(Visual());
	K->CalculateBones														();
	CInifile				*data	= K->LL_UserData						();
	m_rotate_x_bone					= K->LL_BoneID							(data->r_string(MOUNTED_TURRET_DEF_SECT, "wpn_rotate_x_bone"));
	m_rotate_y_bone					= K->LL_BoneID							(data->r_string(MOUNTED_TURRET_DEF_SECT, "wpn_rotate_y_bone"));
	m_fire_bone						= K->LL_BoneID							(data->r_string(MOUNTED_TURRET_DEF_SECT, "wpn_fire_bone"));
	m_actor_bone					= K->LL_BoneID							(data->r_string(MOUNTED_TURRET_DEF_SECT, "actor_bone"));
	m_camera_bone					= K->LL_BoneID							(data->r_string(MOUNTED_TURRET_DEF_SECT, "camera_bone"));
	
	CBoneData& bdX					= K->LL_GetData							(m_rotate_x_bone); 
	m_lim_x_rot.set															(bdX.IK_data.limits[0].limit.x, bdX.IK_data.limits[0].limit.y);
	m_camera->lim_pitch.set													(bdX.IK_data.limits[0].limit.x, bdX.IK_data.limits[0].limit.y);
	CBoneData& bdY					= K->LL_GetData							(m_rotate_y_bone); 
	m_lim_y_rot.set															(bdY.IK_data.limits[1].limit.x, bdY.IK_data.limits[1].limit.y);
	m_camera->lim_yaw.set													(bdY.IK_data.limits[1].limit.x, bdY.IK_data.limits[1].limit.y);

	xr_vector<Fmatrix>		matrices;
	K->LL_GetBindTransform													(matrices);
	m_i_bind_x_xform.invert													(matrices[m_rotate_x_bone]);
	m_i_bind_y_xform.invert													(matrices[m_rotate_y_bone]);
	m_bind_x_rot					= matrices[m_rotate_x_bone].k.getP		();
	m_bind_y_rot					= matrices[m_rotate_y_bone].k.getH		();
	m_bind_x.set															(matrices[m_rotate_x_bone].c);
	m_bind_y.set															(matrices[m_rotate_y_bone].c);

	m_cur_x_rot						= m_bind_x_rot;
	m_cur_y_rot						= m_bind_y_rot;
	m_selected_fire_dir.setHP												(m_bind_y_rot, m_bind_x_rot);
	XFORM().transform_dir													(m_selected_fire_dir);

	SetBoneCallbacks														();

	IKinematicsAnimated		*A		= smart_cast<IKinematicsAnimated*>		(Visual());
	if (A) 
	{
		A->PlayCycle														("idle");
		K->CalculateBones													();
	}

	CShootingObject::Light_Create											();
	
	m_fire_norm.set															(0, 1, 0);
	m_fire_dir.set															(0, 0, 1);
	m_fire_pos.set															(0, 0, 0);

	processing_activate														();

//	if (!CScriptEntity::net_Spawn(DC))
//		return FALSE;
	
	return TRUE;
}

void CMountedTurret::net_Destroy()
{
	processing_deactivate													();
	ResetBoneCallbacks														();
	if (OwnerActor())
		detach_Actor														();
//	CScriptEntity::net_Destroy												();
	CEntity::net_Destroy													();
	CShootingObject::Light_Destroy											();
	CShootingObject::StopFlameParticles										();
	CPHSkeleton::RespawnInit												();
}

void CMountedTurret::net_Save(NET_Packet &P)
{
	CEntity::net_Save														(P);	
	CPHSkeleton::SaveNetState												(P);
}

void CMountedTurret::RestoreNetState(CSE_PHSkeleton *ph)
{
	if (!ph->_flags.test(CSE_PHSkeleton::flSavedData))
		return;
	CPHSkeleton::RestoreNetState											(ph);
	//PPhysicsShell()->DisableCollision										();
}

void CollisionCbAlife(bool &do_colide, bool bo1, dContact &c, SGameMtl *material_1, SGameMtl *material_2)
{
	do_colide = false; 
}

void ContactCbAlife(CDB::TRI *T, dContactGeom *c)
{
}

void CMountedTurret::SpawnInitPhysics(CSE_Abstract *D)	
{
	IKinematics				*K		= smart_cast<IKinematics*>				(Visual());	
	PPhysicsShell()					= P_build_Shell							(this, false);
	if (g_Alive())
	{
		PPhysicsShell()->set_PhysicsRefObject(this);
		PPhysicsShell()->mXFORM.set											(XFORM());
		PPhysicsShell()->SetAirResistance									(0.001f, 0.02f);
		PPhysicsShell()->setMass1											(MOUNTED_TURRET_MASS);
		PPhysicsShell()->SetPrefereExactIntegration							();
		PPhysicsShell()->Activate(true);
	}
	//#+# skyloader: fix bones
	CInifile	*data	= K->LL_UserData ();
	if (data->line_exist("physics_common", "fixed_bones"))
	{
		LPCSTR	bones = data->r_string("physics_common", "fixed_bones");
		ApplySpawnIniToPhysicShell(K->LL_UserData(),m_pPhysicsShell,bones[0]!='\0');
	}
	//###

	K->CalculateBones														(TRUE);
}

void CMountedTurret::UpdateCL()
{
	IKinematics				*K		= smart_cast<IKinematics*>				(Visual());
	CEntity::UpdateCL														();
	UpdateBarrelDir															();
//	K->CalculateBones_Invalidate											();
	K->CalculateBones														(TRUE);

	m_temperature					+=	m_temp_incr;

	if (m_temperature >= MAX_FIRE_TEMP)
	{
		m_allow_fire				= false;
		m_temperature				= MAX_FIRE_TEMP;
	}
	UpdateFire																();
	if(OwnerActor() && OwnerActor()->IsMyCamera()) 
	{
		cam_Update															(Device.fTimeDelta, g_fov);
		OwnerActor()->Cameras().UpdateFromCamera										(Camera());
		OwnerActor()->Cameras().ApplyDevice									(VIEWPORT_NEAR);
	}
	m_pPhysicsShell->InterpolateGlobalTransform								(&XFORM());
	if (m_temperature <= 0)
	{	
		m_temperature				= 0;
		m_temp_incr					= 0;
	}
}

void CMountedTurret::shedule_Update(u32 dt)
{
	CEntity::shedule_Update													(dt);
	CPHSkeleton::Update														(dt);
}

void CMountedTurret::renderable_Render()
{
	CEntity::renderable_Render												();
	RenderLight																();
}

void CMountedTurret::OnMouseMove(int dx, int dy)
{
	if (Remote())
		return;

	float					scale	= psMouseSens * psMouseSensScale / 50.f;
	float					h, p;
	m_selected_fire_dir.getHP												(h, p);
	if (dx)
	{
		float				d	= float(dx) * scale;
		h						-= d;
		SetDesiredDir														(h, p);
	}
	if (dy)
	{
		float				d	= ((psMouseInvert.test(1)) ? -1 : 1) * float(dy) * scale * 3.f / 4.f;
		p						-= d;
		SetDesiredDir														(h, p);
	}
}

void CMountedTurret::OnKeyboardPress(int dik)
{
	if (Remote())				
		return;

	switch (dik)	
	{
		case kWPN_FIRE:					
		{
			FireStart													();
			m_temp_incr							= 1;
			break;
		}
		default: break;
	};

}
void CMountedTurret::OnKeyboardRelease(int dik)
{
	if (Remote())	
		return;
	switch (dik)	
	{
		case kWPN_FIRE:
		{
			FireEnd															();
			m_temp_incr							= -1;
			break;
		}
		default: break;
	};
}

void CMountedTurret::OnKeyboardHold(int dik)
{
	if (Remote())	
		return;
}

void CMountedTurret::cam_Update(float dt, float fov)
{
	Fvector			P, Da;
	Da.set																	(0, 0, 0);

	IKinematics* K					= smart_cast<IKinematics*>				(Visual());
	K->CalculateBones_Invalidate											();
	K->CalculateBones														(TRUE);
	Fmatrix		C				= K->LL_GetTransform					(m_camera_bone);
	C.c.y						+= 0.2f;
	XFORM().transform_tiny													(P, C.c);

	Fvector			d				= C.k;
	XFORM().transform_dir													(d);
	Fvector2		des_cam_dir;

	d.getHP																	(des_cam_dir.x, des_cam_dir.y);
	des_cam_dir.mul															(-1.0f);


	Camera()->yaw		= angle_inertion_var								(Camera()->yaw,	des_cam_dir.x, 0.5f, 7.5f, PI_DIV_6, Device.fTimeDelta);
	Camera()->pitch		= angle_inertion_var								(Camera()->pitch, des_cam_dir.y, 0.5f, 7.5f, PI_DIV_6, Device.fTimeDelta);

	if(OwnerActor())
	{
		// rotate head
		OwnerActor()->Orientation().yaw										= -Camera()->yaw;
		OwnerActor()->Orientation().pitch									= -Camera()->pitch;
	}
	

	Camera()->Update														(P, Da);
	Level().Cameras().UpdateFromCamera												(Camera());
}

bool CMountedTurret::Use(const Fvector& pos, const Fvector& dir, const Fvector& foot_pos)
{
	return (!Owner());
}

void CMountedTurret::SetBoneCallbacks()
{
//	PPhysicsShell()->EnabledCallbacks(FALSE);
	IKinematics				*K		= smart_cast<IKinematics*>				(Visual());
	CBoneInstance			&biX	= K->LL_GetBoneInstance					(m_rotate_x_bone);		
	biX.set_callback														(bctCustom, BoneCallbackX, this);
	CBoneInstance			&biY	= K->LL_GetBoneInstance					(m_rotate_y_bone);		
	biY.set_callback														(bctCustom, BoneCallbackY, this);
}

void CMountedTurret::ResetBoneCallbacks()
{
	IKinematics				*K		= smart_cast<IKinematics*>				(Visual());
	CBoneInstance			&biX	= K->LL_GetBoneInstance					(m_rotate_x_bone);	
	biX.reset_callback														();
	CBoneInstance			&biY	= K->LL_GetBoneInstance					(m_rotate_y_bone);	
	biY.reset_callback														();

//	PPhysicsShell()->EnabledCallbacks(TRUE);
}

bool CMountedTurret::attach_Actor(CGameObject *actor)
{
	CActor *pA = smart_cast<CActor*>(actor);
	if (pA)
	{
		pA->cam_Set(eacFirstEye);
		pA->setVisible(FALSE);
	}
	m_initial_pos.set														(actor->Position());
	CHolderCustom::attach_Actor												(actor);
	processing_activate														();
	FireEnd																	();
	return true;
}

void CMountedTurret::detach_Actor()
{
	if (OwnerActor())
		OwnerActor()->setVisible(TRUE);
	StopLight														();
	FireEnd																	();
	CShootingObject::StopLight												();
	CHolderCustom::detach_Actor												();
	m_temp_incr										= -1;
	processing_deactivate													();
}

Fvector	CMountedTurret::ExitPosition()
{
	/*
	Fvector					pos, dir_from_car, add, add1;;
	Fmatrix					pf;
	Fobb					bb;
	IKinematics				*K		= smart_cast<IKinematics*>				(Visual());
	CBoneData				&bd		= K->LL_GetData							(m_actor_bone);
	xr_vector<Fmatrix>		bones_bind_forms;
	K->LL_GetBindTransform													(bones_bind_forms);
	pf.mul																	(XFORM(), bones_bind_forms[m_actor_bone]);
	bb.transform															(bd.obb, pf);
	bb.xform_get															(pf);
	pos.set																	(pf.c);
	MAX_OF																	(abs(pf.i.y), add.set(pf.i); add.mul(bb.m_halfsize.x * fsignum(pf.i.y)),
																			 abs(pf.j.y), add.set(pf.j); add.mul(bb.m_halfsize.y * fsignum(pf.j.y)),
																			 abs(pf.k.y), add.set(pf.k); add.mul(bb.m_halfsize.z * fsignum(pf.k.y)));
	pos.sub																	(add);
	MIN_OF																	(bb.m_halfsize.x, add1.set(pf.i); add1.mul(bb.m_halfsize.x),
																		     bb.m_halfsize.y, add1.set(pf.j); add1.mul(bb.m_halfsize.y),
																			 bb.m_halfsize.z, add1.set(pf.k); add1.mul(bb.m_halfsize.z));
	dir_from_car.sub														(pf.c, Position());
	dir_from_car.y					= 0.f;
	if (add1.dotproduct(dir_from_car) < 0.f)
		add1.invert															();	
	add1.mul																(-50.f);
	pos.add																	(add1);
	return pos;
	*/
	return m_initial_pos;
}

Fvector CMountedTurret::ExitVelocity()
{
//	CPhysicsShell			*P		= PPhysicsShell							();
//	if (!P || !P->isActive())
		return Fvector().set(0, 0, 0);
//	CPhysicsElement			*E		= P->get_ElementByStoreOrder			(0);
//	Fvector					v		=	ExitPosition						();
//	dBodyGetPointVel														(E->get_body(), v.x, v.y, v.z, cast_fp(v));
//	return v;
}

CCameraBase *CMountedTurret::Camera()
{
	return m_camera;
}

void CMountedTurret::FireStart()
{
	if (!m_allow_fire)
	{
		FireEnd();
		return;
	}
	CShootingObject::FireStart												();
}

DLL_Pure *CMountedTurret::_construct()
{
	CEntity::_construct														();
//	CScriptEntity::_construct												();
	return this;
}

void CMountedTurret::reinit()
{
	CEntity::reinit															();
//	CScriptEntity::reinit													();
}
/*
void CMountedTurret::ResetScriptData(void *P)
{
	CScriptEntity::ResetScriptData(P);
}
*/
void CMountedTurret::FireEnd()
{
	CShootingObject::FireEnd												();
	StopFlameParticles														();
	RemoveShotEffector														();
}

void CMountedTurret::OnShot()
{

	FireBullet																(get_CurrentFirePoint(), m_fire_dir, fireDispersionBase,
																				*m_current_ammo, Owner()->ID(), ID(), SendHitAllowed(Owner()));

	StartShotParticles														();

	if (m_bLightShotEnabled) 
		Light_Start															();

	StartFlameParticles														();
	StartSmokeParticles														(m_fire_pos, zero_vel);
	OnShellDrop																(m_fire_pos, zero_vel);

	bool hud_mode						= (Level().CurrentEntity() == smart_cast<CObject*>(Owner()));
	HUD_SOUND_ITEM::PlaySound													(m_snd_shot, m_fire_pos, Owner(), hud_mode);

	AddShotEffector															();

	
}

void CMountedTurret::UpdateFire()
{
	fTime								-= Device.fTimeDelta;
	if (!m_allow_fire)
	{
		FireEnd();
		return;
	}
	CShootingObject::UpdateFlameParticles									();
	CShootingObject::UpdateLight											();
	if (!IsWorking())
	{
		if (fTime < 0)
			fTime = 0.f;
		return;
	}

	if(fTime<=0)
	{
		OnShot();
		fTime							+= fTimeToFire;
	}
}

const Fmatrix &CMountedTurret::get_ParticlesXFORM()
{
	return m_fire_bone_xform;
}

void CMountedTurret::AddShotEffector()
{
	if (OwnerActor())
	{
		CCameraShotEffector		*S	= smart_cast<CCameraShotEffector*>		(OwnerActor()->Cameras().GetCamEffector(eCEShot)); 
		if (!S)
			S						= (CCameraShotEffector*)OwnerActor()->Cameras().AddCamEffector(new CCameraShotEffector(m_cam_max_angle,
																								m_cam_relax_speed, 0.25f, 0.01f, 0.7f));
		R_ASSERT															(S);
		S->Shot																(0.01f);
	}
}

void CMountedTurret::RemoveShotEffector	()
{
	if (OwnerActor())
		OwnerActor()->Cameras().RemoveCamEffector							(eCEShot);
}

void CMountedTurret::BoneCallbackX(CBoneInstance *B)
{
	CMountedTurret				*P	= static_cast<CMountedTurret*>			(B->callback_param());
	Fmatrix rX;	
	rX.rotateX																(P->m_cur_x_rot);
	B->mTransform.mulB_43													(rX);
}	

void CMountedTurret::BoneCallbackY(CBoneInstance *B)
{
	CMountedTurret				*P	= static_cast<CMountedTurret*>			(B->callback_param());
	Fmatrix rY;
	rY.rotateY																(P->m_cur_y_rot);
	B->mTransform.mulB_43													(rY);
}

void CMountedTurret::SetDesiredDir(float h, float p)
{
	m_selected_fire_dir.setHP												(h, p);
}

void CMountedTurret::SetDesiredDir(Fvector3 new_dir)
{
	m_selected_fire_dir.set													(new_dir);
}

void CMountedTurret::SetDesiredEnemyPos(Fvector3 pos)
{
	m_selected_fire_dir.sub(pos, m_fire_pos).normalize_safe();
}

void CMountedTurret::UpdateBarrelDir()
{
	IKinematics					*K	= smart_cast<IKinematics*>				(Visual());
	m_fire_bone_xform				= K->LL_GetTransform					(m_fire_bone);

	m_fire_bone_xform.mulA_43												(XFORM());
	m_fire_pos.set															(0, 0, 0); 
	m_fire_bone_xform.transform_tiny										(m_fire_pos);
	m_fire_dir.set															(0, 0, 1);
	m_fire_bone_xform.transform_dir											(m_fire_dir);
	m_fire_norm.set															(0, 1, 0);
	m_fire_bone_xform.transform_dir											(m_fire_norm);


	m_allow_fire					= true;
	Fmatrix						XFi;
	XFi.invert																(XFORM());
	Fvector						dep;
	XFi.transform_dir														(dep, m_selected_fire_dir);
	{// x angle
		m_i_bind_x_xform.transform_dir										(dep);
		dep.normalize														();
		m_tgt_x_rot					= angle_normalize_signed				(m_bind_x_rot - dep.getP());
		clamp																(m_tgt_x_rot, -m_lim_x_rot.y, -m_lim_x_rot.x);
	}
	{// y angle
		m_i_bind_y_xform.transform_dir										(dep); 
		dep.normalize														();
		m_tgt_y_rot					= angle_normalize_signed				(m_bind_y_rot - dep.getH());
		clamp																(m_tgt_y_rot, - m_lim_y_rot.y, - m_lim_y_rot.x);
	}

	m_cur_x_rot						= angle_inertion_var					(m_cur_x_rot, m_tgt_x_rot, 0.5f, 3.5f, PI, Device.fTimeDelta);
	m_cur_y_rot						= angle_inertion_var					(m_cur_y_rot, m_tgt_y_rot, 0.5f, 3.5f, PI, Device.fTimeDelta);
}


void CMountedTurret::SetParam(int id, Fvector2 val)
{
	CHolderCustom::SetParam													(id, val);
	switch (id)
	{
		case eDesiredDir:
		{
			SetDesiredDir													(val.x, val.y);
			break;
		}
	}
}

void CMountedTurret::SetParam(int id, Fvector3 val)
{
	CHolderCustom::SetParam													(id, val);
	switch (id)
	{
		case eDesiredEnemyDir:
		{
			SetDesiredDir													(val);
			break;
		}
		case eDesiredEnemyPos:
		{
			SetDesiredEnemyPos												(val);
			break;
		}
		default: NODEFAULT;
	}
}

void CMountedTurret::Action(int id, u32 flags)
{
	CHolderCustom::Action													(id, flags);
	switch (id)
	{
		case eActivate:
		{
			processing_activate														();
			break;
		}
		case eDeactivate:
		{
			processing_deactivate													();
			if (OwnerActor())
				OwnerActor()->use_MountedWeapon											(0);
			CHolderCustom::SetNpcOwner												(nullptr);
			break;
		}
		case eFireStart:
		{
			FireStart();
			break;
		}
		case eFireStop:
		{
			FireEnd();
			break;
		}
		default: NODEFAULT;
	}
}

void CMountedTurret::SetNpcOwner(CGameObject *obj)
{
	if (OwnerActor())
		OwnerActor()->use_MountedWeapon (0);
	CHolderCustom::SetNpcOwner												(obj);
}


void CMountedTurret::save(NET_Packet &output_packet)
{	
	output_packet.w_vec3													(XFORM().c);
	output_packet.w_vec3													(m_selected_fire_dir);
	output_packet.w_vec3													(m_initial_pos);
	save_data																(m_temperature, output_packet);
	save_data																(m_temp_incr, output_packet);
}

void CMountedTurret::load(IReader &input_packet)
{
	input_packet.r_fvector3													(XFORM().c);
	input_packet.r_fvector3													(m_selected_fire_dir);
	input_packet.r_fvector3													(m_initial_pos);
	load_data																(m_temperature, input_packet);
	load_data																(m_temp_incr, input_packet);
}