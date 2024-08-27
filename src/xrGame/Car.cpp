#include "stdafx.h"
#include "car.h"

#ifdef DEBUG
#	include "../xrEngine/StatGraph.h"
#	include "PHDebug.h"
#endif // DEBUG
#include "Inventory.h"
//#include "Hit.h"
#include "PHDestroyable.h"

#include "cameralook.h"
#include "camerafirsteye.h"
#include "Actor.h"
#include "ActorEffector.h"
#include "math.h"
#include "script_entity_action.h"
#include "Inventory.h"
#include "xrServer_Objects_ALife_Items.h"
#include "../Include/xrRender/Kinematics.h"
#include "Level.h"
#include "ui/UIMainIngameWnd.h"
#include "CarWeapon.h"
#include "game_object_space.h"
#include "../xrEngine/GameMtlLib.h"

#include "CharacterPhysicsSupport.h"
#include "car_memory.h"
#include "../xrPhysics/IPHWorld.h"
#include "../xrPhysics/IActivationShape.h"
#include "CharacterPhysicsSupport.h"
#include "UIGameSP.h"

CCar::CCar()
{
	m_bone_trunk = BI_NONE;
	m_bone_steer = BI_NONE;

	m_rpm_b = BI_NONE;
	m_speed_b = BI_NONE;
	m_rpm_offsets.set(0, 0, 0);
	m_speed_offsets.set(0, 0, 0);

	m_memory = nullptr;
	m_driver_anim_type = 0;
	active_camera = 0;
	camera[ectFirst] = new CCameraFirstEye(this, CCameraBase::flRelativeLink | CCameraBase::flPositionRigid);
	camera[ectFirst]->tag = ectFirst;
	camera[ectFirst]->Load("car_firsteye_cam");

	camera[ectChase] = new CCameraLook(this, CCameraBase::flRelativeLink);
	camera[ectChase]->tag = ectChase;
	camera[ectChase]->Load("car_look_cam");

	camera[ectFree] = new CCameraLook(this);
	camera[ectFree]->tag = ectFree;
	camera[ectFree]->Load("car_free_cam");
	OnCameraChange(ectFirst);

	///////////////////////////////
	b_wheels_limited = false;
	b_engine_on = false;
	e_state_steer = idle;
	e_state_drive = neutral;
	m_current_gear_ratio = phInfinity;
	rsp = false; lsp = false; fwp = false; bkp = false; brp = false;
	///////////////////////////////

	m_exhaust_particles = "vehiclefx\\exhaust_1";
	m_car_sound = new SCarSound(this);

	//у машины слотов в инвентаре нет
	m_doors_torque_factor = 2.f;
	m_power_increment_factor = 0.5f;
	m_rpm_increment_factor = 0.5f;
	m_power_decrement_factor = 0.5f;
	m_rpm_decrement_factor = 0.5f;
	b_breaks = false;
	m_break_start = 0.f;
	m_break_time = 1.;
	m_breaks_to_back_rate = 1.f;

	b_exploded = false;
	m_car_weapon = nullptr;
	m_power_neutral_factor = 0.25f;
	m_steer_angle = 0.f;
#ifdef DEBUG
	InitDebug();
#endif
}

CCar::~CCar(void)
{
	xr_delete(camera[0]);
	xr_delete(camera[1]);
	xr_delete(camera[2]);
	xr_delete(m_car_sound);
	ClearExhausts();
	xr_delete(m_car_weapon);
	xr_delete(m_memory);
}

void CCar::reinit()
{
	CEntity::reinit();
	CScriptEntity::reinit();
	if (m_memory)
		m_memory->reinit();
}

void CCar::reload(LPCSTR section)
{
	CEntity::reload(section);
	if (m_memory)
		m_memory->reload(section);
}

void CCar::cb_Steer(CBoneInstance* B)
{
	VERIFY2(fsimilar(DET(B->mTransform), 1.f, DET_CHECK_EPS), "Bones receive returns 0 matrix");
	CCar* C = static_cast<CCar*>(B->callback_param());
	Fmatrix m;


	m.rotateZ(C->m_steer_angle);

	B->mTransform.mulB_43(m);
#ifdef DEBUG
	if (!fsimilar(DET(B->mTransform), 1.f, DET_CHECK_EPS)) {

		Msg("RotatingZ angle=%f", C->m_steer_angle);
		VERIFY2(0, "Bones callback returns BAD!!! matrix");
	}
#endif
}

// Core events
void	CCar::Load(LPCSTR section)
{
	inherited::Load(section);
	ISpatial* self = smart_cast<ISpatial*> (this);
	if (self)		self->spatial.type |= STYPE_VISIBLEFORAI;

	CInventoryOwner::Load(section);
	inventory().m_pOwner = this;
}

BOOL CCar::net_Spawn(CSE_Abstract* DC)
{
#ifdef DEBUG
	InitDebug();
#endif

	CSE_Abstract* e = (CSE_Abstract*)(DC);
	CSE_ALifeCar* co = smart_cast<CSE_ALifeCar*>(e);
	BOOL							R = inherited::net_Spawn(DC);

	PKinematics(Visual())->CalculateBones_Invalidate();
	PKinematics(Visual())->CalculateBones(TRUE);

	CPHSkeleton::Spawn(e);
	setEnabled(TRUE);
	setVisible(TRUE);
	PKinematics(Visual())->CalculateBones_Invalidate();
	PKinematics(Visual())->CalculateBones(TRUE);

	SetfHealth(co->health);

	if (!g_Alive())					b_exploded = true;
	else							b_exploded = false;

	CDamagableItem::RestoreEffect();


	CInifile* pUserData = PKinematics(Visual())->LL_UserData();
	if (pUserData != nullptr)
	{
		if (pUserData->section_exist("destroyed"))
			CPHDestroyable::Load(pUserData, "destroyed");

		if (pUserData->section_exist("mounted_weapon_definition"))
			m_car_weapon = new CCarWeapon(this);

		if (pUserData->line_exist("car_definition", "trunk_bone"))
			m_bone_trunk = PKinematics(Visual())->LL_BoneID(pUserData->r_string("car_definition", "trunk_bone"));

		if (pUserData->section_exist("visual_memory_definition"))
		{
			m_memory = new car_memory(this);
			m_memory->reload(pUserData->r_string("visual_memory_definition", "section"));
		}
	}

	return (CScriptEntity::net_Spawn(DC) && R);
}

void CCar::ActorObstacleCallback(bool& do_colide, bool bo1, dContact& c, SGameMtl* material_1, SGameMtl* material_2)
{
	if (!do_colide)
	{
		if (material_1 && material_1->Flags.test(SGameMtl::flActorObstacle))do_colide = true;
		if (material_2 && material_2->Flags.test(SGameMtl::flActorObstacle))do_colide = true;
	}
}

void CCar::SpawnInitPhysics(CSE_Abstract* D)
{
	CSE_PHSkeleton* so = smart_cast<CSE_PHSkeleton*>(D);
	R_ASSERT(so);
	ParseDefinitions();//parse ini filling in m_driving_wheels,m_steering_wheels,m_breaking_wheels
	CreateSkeleton(D);//creates m_pPhysicsShell & fill in bone_map
	IKinematics* K = smart_cast<IKinematics*>(Visual());
	K->CalculateBones_Invalidate();//this need to call callbacks
	K->CalculateBones(TRUE);
	Init();

	SetDefaultNetState(so);
	CPHUpdateObject::Activate();
}

void CCar::net_Destroy()
{
#ifdef DEBUG
	DBgClearPlots();
#endif

	IKinematics* pKinematics = smart_cast<IKinematics*>(Visual());
	if (m_bone_steer != BI_NONE)
	{
		pKinematics->LL_GetBoneInstance(m_bone_steer).reset_callback();
	}
	if (m_rpm_b != BI_NONE) 
	{
		pKinematics->LL_GetBoneInstance(m_rpm_b).reset_callback();
	}
	if (m_speed_b != BI_NONE) 
	{
		pKinematics->LL_GetBoneInstance(m_speed_b).reset_callback();
	}

	CScriptEntity::net_Destroy();
	inherited::net_Destroy();
	CExplosive::net_Destroy();
	if (m_pPhysicsShell)
	{
		m_pPhysicsShell->Deactivate();
		m_pPhysicsShell->ZeroCallbacks();
		xr_delete(m_pPhysicsShell);
	}
	CHolderCustom::detach_Actor();
	ClearExhausts();
	m_wheels_map.clear();
	m_steering_wheels.clear();
	m_driving_wheels.clear();
	m_exhausts.clear();
	m_breaking_wheels.clear();
	m_doors.clear();
	m_gear_ratious.clear();
	m_car_sound->Destroy();
	CPHUpdateObject::Deactivate();
	CPHSkeleton::RespawnInit();
	m_damage_particles.Clear();
	CPHDestroyable::RespawnInit();
	CPHCollisionDamageReceiver::Clear();
	b_breaks = false;
}

void CCar::net_Save(NET_Packet& P)
{
	inherited::net_Save(P);
	SaveNetState(P);
}

BOOL CCar::net_SaveRelevant()
{
	return TRUE;
}

void CCar::SaveNetState(NET_Packet& P)
{
	CPHSkeleton::SaveNetState(P);
	P.w_vec3(Position());

	Fvector Angle;
	XFORM().getXYZ(Angle);
	P.w_vec3(Angle);

	P.w_u16(u16(m_doors.size()));
	for (auto& [ID, CarObj] : m_doors)
		CarObj.SaveNetState(P);

	P.w_u16(u16(m_wheels_map.size()));
	for (auto& [ID, WhellData] : m_wheels_map)
		WhellData.SaveNetState(P);

	P.w_float(GetfHealth());
}

void CCar::RestoreNetState(CSE_PHSkeleton* po)
{
	if (!po->_flags.test(CSE_PHSkeleton::flSavedData))
		return;

	CPHSkeleton::RestoreNetState(po);

	CSE_ALifeCar* co = smart_cast<CSE_ALifeCar*>(po);

	size_t StackIterator = 0;
	for (auto& [ID, CarObj] : m_doors)
	{
		CarObj.RestoreNetState(*(co->door_states.begin() + StackIterator));
		StackIterator++;
	}

	StackIterator = 0;
	for (auto& [ID, WhellData] : m_wheels_map)
	{
		WhellData.RestoreNetState(*(co->wheel_states.begin() + StackIterator));
		StackIterator++;
	}

	//as later may kill diable/enable state save it;
	bool enable = PPhysicsShell()->isEnabled();

	Fmatrix restored_form;
	m_pPhysicsShell->GetGlobalTransformDynamic(&restored_form);

	Fmatrix inv, replace, sof;
	sof.setXYZ(co->o_Angle.x, co->o_Angle.y, co->o_Angle.z);
	sof.c.set(co->o_Position);
	inv.set(restored_form);
	inv.invert();
	replace.mul(sof, inv);

	PKinematics(Visual())->CalculateBones_Invalidate();
	PKinematics(Visual())->CalculateBones(TRUE);
	m_pPhysicsShell->DisableCollision();

	Fvector center; Center(center);
	Fvector obj_size; BoundingBox().getsize(obj_size);

	get_box(m_pPhysicsShell, restored_form, obj_size, center);

	Fvector ap = Fvector().set(0, 0, 0);

	ActivateShapePhysShellHolder(this, XFORM(), obj_size, center, ap);

	m_pPhysicsShell->EnableCollision();

	replace.mul(sof, inv);
	m_pPhysicsShell->TransformPosition(replace, mh_clear);

	if (enable)
		m_pPhysicsShell->Enable();
	else
		m_pPhysicsShell->Disable();

	m_pPhysicsShell->GetGlobalTransformDynamic(&XFORM());
}

void CCar::SetDefaultNetState(CSE_PHSkeleton* po)
{
	if (po->_flags.test(CSE_PHSkeleton::flSavedData))
		return;

	for (auto& [ID, CarObj] : m_doors)
	{
		CarObj.SetDefaultNetState();
	}
}

void CCar::shedule_Update(u32 dt)
{
	inherited::shedule_Update(dt);
	if (CPHDestroyable::Destroyed())CPHDestroyable::SheduleUpdate(dt);
	else	CPHSkeleton::Update(dt);

	if (CDelayedActionFuse::isActive() && CDelayedActionFuse::Update(GetfHealth()))
	{
		//CarExplode();
	}
	if (b_exploded && !m_explosion_flags.test(flExploding) && !getEnabled())//!m_bExploding
		setEnabled(TRUE);
#ifdef DEBUG
	DbgSheduleUpdate();
#endif
}

void CCar::UpdateEx(float fov)
{
#ifdef DEBUG
	DbgUbdateCl();
#endif

	VisualUpdate(fov);
	if (OwnerActor() && OwnerActor()->IsMyCamera())
	{
		cam_Update(Device.fTimeDelta, fov);
		OwnerActor()->Cameras().UpdateFromCamera(Camera());
		OwnerActor()->Cameras().ApplyDevice(VIEWPORT_NEAR);
	}
}

BOOL CCar::AlwaysTheCrow()
{
	return (m_car_weapon && m_car_weapon->IsActive());
}

void CCar::UpdateCL()
{
	inherited::UpdateCL();
	CExplosive::UpdateCL();
	if (m_car_weapon)
	{
		m_car_weapon->UpdateCL();
		if (m_memory)
			m_memory->set_camera(m_car_weapon->ViewCameraPos(), m_car_weapon->ViewCameraDir(), m_car_weapon->ViewCameraNorm());
	}
	ASCUpdate();
	if (Owner()) return;
	//	UpdateEx			(g_fov);
	VisualUpdate(90);
	if (GetScriptControl())
		ProcessScripts();

}

void CCar::VisualUpdate(float fov)
{
	Fvector lin_vel = zero_vel;
	if(m_pPhysicsShell)
	{
		m_pPhysicsShell->InterpolateGlobalTransform(&XFORM());
		m_pPhysicsShell->get_LinearVel(lin_vel);
	}

	m_car_sound->Update();
	if (Owner())
	{
		if (m_pPhysicsShell->isEnabled())
		{
			Owner()->XFORM().mul_43(XFORM(), m_sits_transforms[0]);
		}

		if (CurrentGameUI() != nullptr && CurrentGameUI()->UIMainIngameWnd != nullptr)
		{
			CUICarPanel& Panel = CurrentGameUI()->UIMainIngameWnd->CarPanel();
			Panel.SetEngineLamp(b_engine_on);
			Panel.SetLightLamp(m_lights.IsOn());

			Panel.SetCarHealth(GetfHealth() /* /100.f*/);
			Panel.SetCarFuel(m_fuel/* /100.f*/);
			Panel.SetSpeed(lin_vel.magnitude() / 1000.f * 3600.f / 100.f);
			Panel.SetRPM(m_current_rpm / m_max_rpm / 2.f);
			Panel.SetSpeedMode(xr_string::ToString((int)m_current_transmission_num));
		}
	}

	UpdateExhausts();
	m_lights.Update();
}

void	CCar::renderable_Render()
{
	inherited::renderable_Render();
	if (m_car_weapon)
		m_car_weapon->Render_internal();
}

void	CCar::net_Export(NET_Packet& P)
{
	inherited::net_Export(P);
}

void	CCar::net_Import(NET_Packet& P)
{
	inherited::net_Import(P);
}

void	CCar::OnHUDDraw(CCustomHUD* /**hud*/)
{
#ifdef DEBUG
	Fvector velocity;
	m_pPhysicsShell->get_LinearVel(velocity);
	UI().Font().pFontStat->SetColor(0xffffffff);
	UI().Font().pFontStat->OutSet(120, 530);
	UI().Font().pFontStat->OutNext("Position:      [%3.2f, %3.2f, %3.2f]", VPUSH(Position()));
	UI().Font().pFontStat->OutNext("Velocity:      [%3.2f]", velocity.mul(3.6f).magnitude());
#endif
}

void	CCar::Hit(SHit* pHDS)
{

	SHit	HDS = *pHDS;
	WheelHit(HDS.damage(), HDS.bone(), HDS.hit_type);
	DoorHit(HDS.damage(), HDS.bone(), HDS.hit_type);
	float hitScale = 1.f, woundScale = 1.f;
	if (HDS.hit_type != ALife::eHitTypeStrike) CDamageManager::HitScale(HDS.bone(), hitScale, woundScale);
	HDS.power *= GetHitImmunity(HDS.hit_type) * hitScale;

	inherited::Hit(&HDS);
	if (!CDelayedActionFuse::isActive())
	{
		CDelayedActionFuse::CheckCondition(GetfHealth());
	}
	CDamagableItem::HitEffect();
}

void CCar::ChangeCondition(float fDeltaCondition)
{

	CEntity::CalcCondition(-fDeltaCondition);
	CDamagableItem::HitEffect();
	if (Local() && !g_Alive() && !AlreadyDie())
		KillEntity(Initiator());
}

bool CCar::TryTrunk()
{
	if (m_bone_trunk == BI_NONE)
		return false;

	RQR.r_clear();
	collide::ray_defs Q(Device.vCameraPosition, Device.vCameraDirection, 3.f, CDB::OPT_CULL, collide::rqtObject);

	if (g_pGameLevel->ObjectSpace.RayQuery(RQR, collidable.model, Q))
	{

		collide::rq_results& R = RQR;
		int y = R.r_count();

		for (int k = 0; k < y; ++k)
		{
			collide::rq_result* I = R.r_begin() + k;
			if (IsGameTypeSingle() && m_bone_trunk == (u16)I->element)
			{
				return true;
			}
		}
	}

	return false;
}

bool CCar::TryUsableBones()
{
	if (UsableBones.empty())
		return false;

	UsableBonesActive = BI_NONE;

	RQR.r_clear();
	collide::ray_defs Q(Device.vCameraPosition, Device.vCameraDirection, 3.f, CDB::OPT_CULL, collide::rqtObject);

	if (g_pGameLevel->ObjectSpace.RayQuery(RQR, collidable.model, Q))
	{
		collide::rq_results& R = RQR;
		int y = R.r_count();

		for (int k = 0; k < y; ++k)
		{
			collide::rq_result* I = R.r_begin() + k;
			if (IsGameTypeSingle() && UsableBones.contains(I->element))
			{
				UsableBonesActive = I->element;
				return true;
			}
		}
	}


	return false;
}

void CCar::PHHit(SHit& H)
{
	if (!m_pPhysicsShell)	return;
	if (m_bone_steer == H.bone()) return;
	if (CPHUpdateObject::IsActive())
	{
		Fvector vimpulse; vimpulse.set(H.direction());
		vimpulse.mul(H.phys_impulse());
		vimpulse.y *= GravityFactorImpulse();
		float mag = vimpulse.magnitude();
		if (!fis_zero(mag))
		{
			vimpulse.mul(1.f / mag);
			m_pPhysicsShell->applyHit(H.bone_space_position(), vimpulse, mag, H.bone(), H.type());
		}

	}
	else
	{
		m_pPhysicsShell->applyHit(H.bone_space_position(), H.direction(), H.phys_impulse(), H.bone(), H.type());
	}
}


void CCar::ApplyDamage(u16 level)
{
	CDamagableItem::ApplyDamage(level);
	switch (level)
	{
	case 1: m_damage_particles.Play1(this); break;
	case 2:
	{
		if (!CDelayedActionFuse::isActive())
		{
			CDelayedActionFuse::CheckCondition(GetfHealth());
		}
		m_damage_particles.Play2(this);
	}
	break;
	case 3: m_fuel = 0.f;

	}

}
void CCar::detach_Actor()
{
	if (!Owner()) return;
	Owner()->setVisible(1);
	CHolderCustom::detach_Actor();
	PPhysicsShell()->remove_ObjectContactCallback(ActorObstacleCallback);
	NeutralDrive();
	Unclutch();
	ResetKeys();
	m_current_rpm = m_min_rpm;

	HandBreak();
	processing_deactivate();
#ifdef DEBUG
	DBgClearPlots();
#endif
	CUICarPanel& Panel = CurrentGameUI()->UIMainIngameWnd->CarPanel();
	Panel.Show(false);
}

bool CCar::attach_Actor(CGameObject* actor)
{
	if (Owner() || CPHDestroyable::Destroyed()) return false;
	CHolderCustom::attach_Actor(actor);

	IKinematics* K = smart_cast<IKinematics*>(Visual());
	CInifile* ini = K->LL_UserData();
	int id;

	Fmatrix	driver_xform;
	if (ini->line_exist("car_definition", "driver_position") && ini->line_exist("car_definition", "driver_direction"))
	{
		driver_xform.c.set(ini->r_fvector3("car_definition", "driver_position"));
		driver_xform.k.set(ini->r_fvector3("car_definition", "driver_direction"));
	}
	else if (ini->line_exist("car_definition", "driver_place")) {

		id = K->LL_BoneID(ini->r_string("car_definition", "driver_place"));
		CBoneInstance& instance = K->LL_GetBoneInstance(u16(id));
		driver_xform.set(instance.mTransform);
	}
	else {
		Owner()->setVisible(0);
		id = K->LL_GetBoneRoot();
		CBoneInstance& instance = K->LL_GetBoneInstance(u16(id));
		driver_xform.set(instance.mTransform);
	}

	m_sits_transforms.push_back(driver_xform);
	OnCameraChange(ectFirst);
	PPhysicsShell()->Enable();
	PPhysicsShell()->add_ObjectContactCallback(ActorObstacleCallback);

	processing_activate();
	ReleaseHandBreak();

	CUICarPanel& Panel = CurrentGameUI()->UIMainIngameWnd->CarPanel();
	Panel.Show(true);
	return true;
}

bool CCar::is_Door(u16 id, xr_map<u16, CCarDoor>::iterator& i)
{
	if (m_doors.contains(id))
	{
		i = m_doors.find(id);
		if (i->second.Joint)
			return true;
	}
	return false;
}

bool CCar::is_Door(u16 id)
{
	return m_doors.contains(id);
}

bool CCar::Enter(const Fvector& pos, const Fvector& dir, const Fvector& foot_pos)
{
	Fvector enter_pos;
	enter_pos.add(pos, foot_pos);
	enter_pos.mul(0.5f);

	for (auto& [ID, CarObj] : m_doors)
	{
		if (CarObj.CanEnter(pos, dir, enter_pos))
		{
			//m_doors[ID].Open();
			return true;
		}
	}
	return false;
}

bool CCar::Exit(const Fvector& pos, const Fvector& dir)
{
	for (auto& [ID, CarObj] : m_doors)
	{
		if (CarObj.CanExit(pos, dir))
		{
			CarObj.GetExitPosition(m_exit_position);
			return true;
		}
	}
	return false;

}

void CCar::cb_Speed(CBoneInstance* B)
{
	CCar* car = static_cast<CCar*>(B->callback_param()); Fmatrix m;	Fvector Vel;
	car->m_pPhysicsShell->get_LinearVel(Vel); float m_speed = Vel.magnitude() * 0.036f;

	float current_rotation = car->m_speed_offsets.x + (car->m_speed_offsets.y - car->m_speed_offsets.x) * m_speed;
	m.rotateZ(deg2rad(clampr(current_rotation, car->m_speed_offsets.z, car->m_speed_offsets.w)));

	B->mTransform.mulB_43(m);
}

void CCar::cb_Rpm(CBoneInstance* B)
{
	CCar* car = static_cast<CCar*>(B->callback_param()); Fmatrix m;

	auto local_rpm = car->m_current_rpm / car->m_max_rpm;
	float current_rotation = car->m_rpm_offsets.x + (car->m_rpm_offsets.y - car->m_rpm_offsets.x) * local_rpm;
	m.rotateZ(deg2rad(clampr(current_rotation, car->m_rpm_offsets.z, car->m_rpm_offsets.w)));

	B->mTransform.mulB_43(m);
}

void CCar::cb_Fuel(CBoneInstance* B)
{
	CCar* car = static_cast<CCar*>(B->callback_param()); Fmatrix m;

	auto local_fuel = car->m_fuel / car->m_fuel_tank;
	float current_rotation = car->m_fuel_offsets.x + (car->m_fuel_offsets.y - car->m_fuel_offsets.x) * local_fuel;
	m.rotateZ(deg2rad(clampr(current_rotation, car->m_fuel_offsets.z, car->m_fuel_offsets.w)));

	B->mTransform.mulB_43(m);
}

void CCar::ParseDefinitions()
{
	bone_map.clear();

	IKinematics* pKinematics = smart_cast<IKinematics*>(Visual());
	bone_map.insert(std::make_pair(pKinematics->LL_GetBoneRoot(), physicsBone()));
	CInifile* ini = pKinematics->LL_UserData();

	if (ini == nullptr && Device.IsEditorMode())
		return;

	R_ASSERT2(ini, "Car has no description !!! See ActorEditor Object - UserData");
	CExplosive::Load(ini, "explosion");
	//CExplosive::SetInitiator(ID());
	m_camera_position = ini->r_fvector3("car_definition", "camera_pos");
	///////////////////////////car definition///////////////////////////////////////////////////
	fill_wheel_vector(ini->r_string("car_definition", "driving_wheels"), m_driving_wheels);
	fill_wheel_vector(ini->r_string("car_definition", "steering_wheels"), m_steering_wheels);
	fill_wheel_vector(ini->r_string("car_definition", "breaking_wheels"), m_breaking_wheels);
	fill_exhaust_vector(ini->r_string("car_definition", "exhausts"), m_exhausts);
	fill_doors_map(ini->r_string("car_definition", "doors"), m_doors);

	if (ini->line_exist("car_definition", "usable_bones"))
	{
		const char* Bones = ini->r_string("car_definition", "usable_bones");
		const char* Callbacks = ini->r_string("car_definition", "usable_bones_callback");

		int BonesCount = _GetItemCount(Bones);
		R_ASSERT2(BonesCount == _GetItemCount(Callbacks), "Bones list should be equal to callback list!");

		string32 BoneName = {};
		string64 CallbackName = {};
		
		for (int Iter = 0; Iter < BonesCount; Iter++)
		{
			_GetItem(Bones, Iter, BoneName);
			_GetItem(Callbacks, Iter, CallbackName);

			u16 BoneID = pKinematics->LL_BoneID(BoneName);

			UsableBones[BoneID] = Callbacks;
		}
	}

	if (ini->line_exist("car_definition", "trunk_bone"))
	{
		u16 bone_id = pKinematics->LL_BoneID(ini->r_string("car_definition", "trunk_bone"));
		CCarDoor door(this);
		door.bone_id = bone_id;
		m_doors.insert(std::make_pair(bone_id, door));

		if (!bone_map.contains(bone_id))
		{
			bone_map.insert(std::make_pair(bone_id, physicsBone()));
		}
	}

	if (ini->section_exist("dashboard")) 
	{
		if (ini->line_exist("dashboard", "rpm_bone")) 
		{
			auto bonename = ini->r_string("dashboard", "rpm_bone");
			m_rpm_b = pKinematics->LL_BoneID(bonename);

			R_ASSERT4(m_rpm_b != BI_NONE, "Car model has no rpm bone", cNameSect().c_str(), bonename);

			auto temp = m_rpm_offsets = ini->r_fvector4("dashboard", "rpm_angle");
			m_rpm_offsets.z = std::min(temp.z, temp.w);
			m_rpm_offsets.w = std::max(temp.z, temp.w);

			pKinematics->LL_GetBoneInstance(m_rpm_b).set_callback(bctPhysics, cb_Rpm, this);
		}
		if (ini->line_exist("dashboard", "fuel_bone")) 
		{
			auto bonename = ini->r_string("dashboard", "fuel_bone");
			m_fuel_b = pKinematics->LL_BoneID(bonename);

			R_ASSERT4(m_fuel_b != BI_NONE, "Car model has no fuel bone", cNameSect().c_str(), bonename);

			auto temp = m_fuel_offsets = ini->r_fvector4("dashboard", "fuel_angle");
			m_fuel_offsets.z = std::min(temp.z, temp.w);
			m_fuel_offsets.w = std::max(temp.z, temp.w);

			pKinematics->LL_GetBoneInstance(m_fuel_b).set_callback(bctPhysics, cb_Fuel, this);
		}
		if (ini->line_exist("dashboard", "speed_bone")) 
		{
			auto bonename = ini->r_string("dashboard", "speed_bone");
			m_speed_b = pKinematics->LL_BoneID(bonename);

			R_ASSERT4(m_speed_b != BI_NONE, "Car model has no speed bone", cNameSect().c_str(), bonename);

			auto temp = m_speed_offsets = ini->r_fvector4("dashboard", "speed_angle");
			m_speed_offsets.z = std::min(temp.z, temp.w);
			m_speed_offsets.w = std::max(temp.z, temp.w);

			pKinematics->LL_GetBoneInstance(m_speed_b).set_callback(bctPhysics, cb_Speed, this);
		}
	}

	///////////////////////////car properties///////////////////////////////
	m_max_power = ini->r_float("car_definition", "engine_power");
	m_max_power *= (0.8f * 1000.f);

	m_max_rpm = ini->r_float("car_definition", "max_engine_rpm");
	m_max_rpm *= (1.f / 60.f * 2.f * M_PI);


	m_min_rpm = ini->r_float("car_definition", "idling_engine_rpm");
	m_min_rpm *= (1.f / 60.f * 2.f * M_PI);

	m_power_rpm = ini->r_float("car_definition", "max_power_rpm");
	m_power_rpm *= (1.f / 60.f * 2.f * M_PI);//

	m_torque_rpm = ini->r_float("car_definition", "max_torque_rpm");
	m_torque_rpm *= (1.f / 60.f * 2.f * M_PI);//

	m_power_increment_factor = READ_IF_EXISTS(ini, r_float, "car_definition", "power_increment_factor", m_power_increment_factor);
	m_rpm_increment_factor = READ_IF_EXISTS(ini, r_float, "car_definition", "rpm_increment_factor", m_rpm_increment_factor);
	m_power_decrement_factor = READ_IF_EXISTS(ini, r_float, "car_definition", "power_decrement_factor", m_power_increment_factor);
	m_rpm_decrement_factor = READ_IF_EXISTS(ini, r_float, "car_definition", "rpm_decrement_factor", m_rpm_increment_factor);
	m_power_neutral_factor = READ_IF_EXISTS(ini, r_float, "car_definition", "power_neutral_factor", m_power_neutral_factor);
	R_ASSERT2(m_power_neutral_factor > 0.1f && m_power_neutral_factor < 1.f, "power_neutral_factor must be 0 - 1 !!");
	if (ini->line_exist("car_definition", "exhaust_particles"))
	{
		m_exhaust_particles = ini->r_string("car_definition", "exhaust_particles");
	}

	b_auto_switch_transmission = !!ini->r_bool("car_definition", "auto_transmission");

	InitParabola();

	m_axle_friction = ini->r_float("car_definition", "axle_friction");
	m_steering_speed = ini->r_float("car_definition", "steering_speed");

	if (ini->line_exist("car_definition", "break_time"))
	{
		m_break_time = ini->r_float("car_definition", "break_time");
	}
	// transmission
	float main_gear_ratio = ini->r_float("car_definition", "main_gear_ratio");

	R_ASSERT2(ini->section_exist("transmission_gear_ratio"), "no section transmission_gear_ratio");
	m_gear_ratious.push_back(ini->r_fvector3("transmission_gear_ratio", "R"));
	m_gear_ratious[0][0] = -m_gear_ratious[0][0] * main_gear_ratio;
	string32 rat_num;
	for (int i = 1; true; ++i)
	{
		xr_sprintf(rat_num, "N%d", i);
		if (!ini->line_exist("transmission_gear_ratio", rat_num)) break;
		Fvector gear_rat = ini->r_fvector3("transmission_gear_ratio", rat_num);
		gear_rat[0] *= main_gear_ratio;
		gear_rat[1] *= (1.f / 60.f * 2.f * M_PI);
		gear_rat[2] *= (1.f / 60.f * 2.f * M_PI);
		m_gear_ratious.push_back(gear_rat);
	}

	// sound
	m_car_sound->Init();

	// fuel
	m_fuel_tank = ini->r_float("car_definition", "fuel_tank");
	m_fuel_tank = ini->r_float("car_definition", "fuel_tank");
	m_fuel = m_fuel_tank;

	m_fuel_consumption = ini->r_float("car_definition", "fuel_consumption");
	m_fuel_consumption /= 100000.f;
	if (ini->line_exist("car_definition", "exhaust_particles"))
		m_exhaust_particles = ini->r_string("car_definition", "exhaust_particles");

	// lights
	m_lights.Init(this);
	m_lights.ParseDefinitions();

	if (ini->section_exist("animations"))
	{
		m_driver_anim_type = ini->r_u16("animations", "driver_animation_type");
	}


	if (ini->section_exist("doors"))
	{
		m_doors_torque_factor = ini->r_u16("doors", "open_torque_factor");
	}

	m_damage_particles.Init(this);
}

void CCar::CreateSkeleton(CSE_Abstract* po)
{
	if (!Visual()) return;
	IRenderVisual *pVis = Visual();
	IKinematics* pK = smart_cast<IKinematics*>(pVis);
	IKinematicsAnimated* pKA = smart_cast<IKinematicsAnimated*>(pVis);
	if(pKA)
	{
		pKA->PlayCycle		("idle");
		pK->CalculateBones	(TRUE);
	}

	m_pPhysicsShell		= P_build_Shell(this, false, &bone_map);
	m_pPhysicsShell->SetPrefereExactIntegration();
	m_pPhysicsShell->Activate(true);
	m_pPhysicsShell->applyForce({ 0.f, 0.f, 0.f }, 0.00001f);
	ApplySpawnIniToPhysicShell(&po->spawn_ini(),m_pPhysicsShell,false);
	ApplySpawnIniToPhysicShell(pK->LL_UserData(),m_pPhysicsShell,false);
	pK->CalculateBones(TRUE);
}

void CCar::Init()
{
	CPHCollisionDamageReceiver::Init();

	//get reference wheel radius
	IKinematics* pKinematics = smart_cast<IKinematics*>(Visual());
	CInifile* ini = pKinematics->LL_UserData();

	if (ini == nullptr && Device.IsEditorMode())
		return;

	R_ASSERT2(ini, "Car has no description !!! See ActorEditor Object - UserData");

	if (ini->section_exist("air_resistance"))
	{
		PPhysicsShell()->SetAirResistance(default_k_l * ini->r_float("air_resistance", "linear_factor"), default_k_w * ini->r_float("air_resistance", "angular_factor"));
	}

	if (ini->line_exist("car_definition", "steer"))
	{
		m_bone_steer = pKinematics->LL_BoneID(ini->r_string("car_definition", "steer"));
		VERIFY2(fsimilar(DET(pKinematics->LL_GetTransform(m_bone_steer)), 1.f, EPS_L), "BBADD MTX");
		pKinematics->LL_GetBoneInstance(m_bone_steer).set_callback(bctPhysics, cb_Steer, this);
	}

	m_steer_angle = 0.f;
	m_ref_radius = ini->r_float("car_definition", "reference_radius");//ref_wheel.radius;
	b_exploded = false;
	b_engine_on = false;
	b_clutch = false;
	b_starting = false;
	b_transmission_switching = false;
	m_root_transform.set(bone_map.find(pKinematics->LL_GetBoneRoot())->second.element->mXFORM);
	m_current_transmission_num = 0;
	m_pPhysicsShell->set_DynamicScales(1.f, 1.f);

	CDamagableItem::Init(GetfHealth(), 3);
	float l_time_to_explosion = READ_IF_EXISTS(ini, r_float, "car_definition", "time_to_explosion", 120.f);
	CDelayedActionFuse::Initialize(l_time_to_explosion, CDamagableItem::DamageLevelToHealth(2));

	for (auto& [ID, WhellData] : m_wheels_map)
	{
		WhellData.Init();
		WhellData.CDamagableHealthItem::Init(100.f, 2);
	}

	for (SWheelDrive& Wheel : m_driving_wheels)
		Wheel.Init();

	for (SWheelBreak& Wheel : m_breaking_wheels)
		Wheel.Init();

	for (SWheelSteer& WheelSteer : m_steering_wheels)
		WheelSteer.Init();

	for (SExhaust& Exhaust : m_exhausts)
		Exhaust.Init();

	for (auto& [ID, CarObj] : m_doors)
	{
		CarObj.Init();
		CarObj.CDamagableHealthItem::Init(100, 1);
	}

	if (ini->section_exist("damage_items"))
	{
		CInifile::Sect& data = ini->r_section("damage_items");
		for (CInifile::SectCIt I = data.Data.begin(); I != data.Data.end(); I++)
		{
			const CInifile::Item& item = *I;
			u16 index = pKinematics->LL_BoneID(*item.first);
			R_ASSERT3(index != BI_NONE, "Wrong bone name", *item.first);
			xr_map <u16, SWheel>::iterator i = m_wheels_map.find(index);

			if (i != m_wheels_map.end())
			{
				i->second.CDamagableHealthItem::Init(float(atof(*item.second)), 2);
			}
			else
			{
				xr_map   <u16, CCarDoor>::iterator i_ = m_doors.find(index);
				R_ASSERT3(i_ != m_doors.end(), "only wheel and doors bones allowed for damage defs", *item.first);
				i_->second.CDamagableHealthItem::Init(float(atof(*item.second)), 1);
			}
		}
	}

	if (ini->section_exist("immunities"))
	{
		LoadImmunities("immunities", ini);
	}

	CDamageManager::reload("car_definition", "damage", ini);

	HandBreak();
	Transmission(1);
}

void CCar::NeutralDrive()
{
	for (SWheelDrive& Wheel : m_driving_wheels)
		Wheel.Neutral();

	e_state_drive = neutral;
}

void CCar::ReleaseHandBreak()
{
	for (SWheelBreak& Wheel : m_breaking_wheels)
		Wheel.Neutral();

	if (e_state_drive == drive)
		Drive();
}

void CCar::Drive()
{
	if (!b_clutch || !b_engine_on) 
		return;

	m_pPhysicsShell->Enable();
	m_current_rpm = EngineDriveSpeed();
	m_current_engine_power = EnginePower();

	for (SWheelDrive& Wheel : m_driving_wheels)
		Wheel.Drive();

	e_state_drive = drive;

}

void CCar::StartEngine()
{
	if (m_fuel < EPS || b_engine_on)
		return;

	PlayExhausts();
	m_car_sound->Start();
	b_engine_on = true;
	m_current_rpm = 0.f;
	b_starting = true;
}

void CCar::StopEngine()
{
	if (!b_engine_on) 
		return;

	AscCall(ascSndStall);
	AscCall(ascExhoustStop);

	NeutralDrive();//set zero speed
	b_engine_on = false;

	UpdatePower();//set engine friction;
	m_current_rpm = 0.f;
}

void CCar::Stall()
{
	AscCall(ascSndStall);
	AscCall(ascExhoustStop);

	NeutralDrive();//set zero speed
	b_engine_on = false;

	UpdatePower();//set engine friction;
	m_current_rpm = 0.f;

}
void CCar::ReleasePedals()
{
	Clutch();
	NeutralDrive();//set zero speed
	UpdatePower();//set engine friction;
}

void CCar::SwitchEngine()
{
	if (b_engine_on) StopEngine();
	else			StartEngine();
}

void CCar::Clutch()
{
	b_clutch = true;
}

void CCar::Unclutch()
{
	b_clutch = false;
}

void CCar::Starter()
{
	b_starting = true;
	m_dwStartTime = Device.dwTimeGlobal;
}

void CCar::UpdatePower()
{
	m_current_rpm = EngineDriveSpeed();
	m_current_engine_power = EnginePower();
	if (b_auto_switch_transmission && !b_transmission_switching)
	{
		VERIFY2(CurrentTransmission() < m_gear_ratious.size(), "wrong transmission");
		if (m_current_rpm < m_gear_ratious[CurrentTransmission()][1]) TransmissionDown();
		if (m_current_rpm > m_gear_ratious[CurrentTransmission()][2]) TransmissionUp();
	}

	for (SWheelDrive& Wheel : m_driving_wheels)
		Wheel.UpdatePower();
}

void CCar::SteerRight()
{
	b_wheels_limited = true;  //no need to limit wheels when stiring
	m_pPhysicsShell->Enable();

	for (SWheelSteer& Wheel : m_steering_wheels)
		Wheel.SteerRight();

	e_state_steer = right;

}
void CCar::SteerLeft()
{
	b_wheels_limited = true; //no need to limit wheels when stiring
	m_pPhysicsShell->Enable();

	for (SWheelSteer& Wheel : m_steering_wheels)
		Wheel.SteerLeft();

	e_state_steer = left;
}

void CCar::SteerIdle()
{
	b_wheels_limited = false;
	m_pPhysicsShell->Enable();

	for (SWheelSteer& Wheel : m_steering_wheels)
		Wheel.SteerIdle();

	e_state_steer = idle;
}

void CCar::LimitWheels()
{
	if (b_wheels_limited)
		return;

	b_wheels_limited = true;

	for (SWheelSteer& Wheel : m_steering_wheels)
		Wheel.Limit();
}

void CCar::HandBreak()
{
	for (SWheelBreak& Wheel : m_breaking_wheels)
		Wheel.HandBreak();
}

void CCar::StartBreaking()
{
	if (!b_breaks)
	{
		b_breaks = true;
		m_break_start = Device.fTimeGlobal;
	}
}

void CCar::StopBreaking()
{
	for (SWheelBreak& Wheel : m_breaking_wheels)
		Wheel.Neutral();

	if (e_state_drive == drive)
		Drive();

	b_breaks = false;
}

void CCar::PressRight()
{
	if (lsp)
	{
		if (!fwp)SteerIdle();
	}
	else
		SteerRight();

	rsp = true;
}

void CCar::PressLeft()
{
	if (rsp)
	{
		if (!fwp)SteerIdle();
	}
	else
		SteerLeft();

	lsp = true;
}

void CCar::PressForward()
{
	if (bkp)
	{
		Unclutch();
		NeutralDrive();
	}
	else
	{
		DriveForward();
	}
	fwp = true;
}

void CCar::PressBack()
{
	if (fwp)
	{
		Unclutch();
		NeutralDrive();
	}
	else
	{
		Unclutch();
		NeutralDrive();
		StartBreaking();
	}
	bkp = true;
}

void CCar::PressBreaks()
{
	HandBreak();
	brp = true;
}

void CCar::DriveBack()
{
	Clutch();
	Transmission(0);

	if (0 == CurrentTransmission())
		Starter();

	Drive();
}

void CCar::DriveForward()
{
	Clutch();
	if (0 == CurrentTransmission()) 
		Transmission(1);

	if (1 == CurrentTransmission())
		Starter();

	Drive();
}

void CCar::ReleaseRight()
{
	if (lsp)
		SteerLeft();
	else
		SteerIdle();

	rsp = false;
}

void CCar::ReleaseLeft()
{
	if (rsp)
		SteerRight();
	else
		SteerIdle();

	lsp = false;
}

void CCar::ReleaseForward()
{
	if (bkp)
	{
		Clutch();
		Transmission(0);
		Starter();

		Drive();
	}
	else
	{
		Unclutch();
		NeutralDrive();
	}

	fwp = false;
}

void CCar::ReleaseBack()
{
	if (b_breaks)
	{
		StopBreaking();
	}
	if (fwp)
	{
		Clutch();

		if (0 == CurrentTransmission())
			Transmission(1);

		if (1 == CurrentTransmission()) 
			Starter();

		Drive();
	}
	else
	{
		Unclutch();
		NeutralDrive();
	}
	bkp = false;
}

void CCar::ReleaseBreaks()
{
	ReleaseHandBreak();
	brp = false;
}

void CCar::Transmission(size_t num)
{
	if (num < m_gear_ratious.size())
	{
		if (CurrentTransmission() != num)
		{
			AscCall(ascSndTransmission);
			m_current_transmission_num = num;
			m_current_gear_ratio = m_gear_ratious[num][0];
			b_transmission_switching = true;
			Drive();
		}
	}
}

void CCar::TransmissionUp()
{
	if (0 == CurrentTransmission())
		return;

	size_t transmission = 1 + CurrentTransmission();
	size_t max_transmition_num = m_gear_ratious.size() - 1;
	transmission > max_transmition_num ? transmission = max_transmition_num : transmission;
	Transmission(transmission);

}

void CCar::TransmissionDown()
{
	if (0 == CurrentTransmission())
		return;

	size_t transmission = CurrentTransmission() - 1;
	transmission < 1 ? transmission = 1 : transmission;
	Transmission(transmission);
}

void CCar::PhTune(float step)
{
	for (u16 i = PPhysicsShell()->get_ElementsNumber(); i != 0; i--)
	{
		CPhysicsElement* e = PPhysicsShell()->get_ElementByStoreOrder(i - 1);
		if (e->isActive() && e->isEnabled())
			e->applyForce(0, e->getMass() * AntiGravityAccel(), 0);
	}
}

float CCar::EffectiveGravity()
{
	float g = physics_world()->Gravity();
	if (CPHUpdateObject::IsActive())g *= 0.5f;
	return g;
}

float CCar::AntiGravityAccel()
{
	return physics_world()->Gravity() - EffectiveGravity();
}

float CCar::GravityFactorImpulse()
{
	return _sqrt(EffectiveGravity() / physics_world()->Gravity());
}

void CCar::UpdateBack()
{
	if (b_breaks)
	{
		float k = 1.f;
		float time = (Device.fTimeGlobal - m_break_start);
		if (time < m_break_time)
		{
			k *= (time / m_break_time);
		}

		for (SWheelBreak& Wheel : m_breaking_wheels)
			Wheel.Break(k);

		Fvector v;
		m_pPhysicsShell->get_LinearVel(v);

		if (v.dotproduct(XFORM().k) < EPS)
		{
			StopBreaking();
			DriveBack();
		}
	}
}

void CCar::PlayExhausts()
{
	for (SExhaust& Exhaust : m_exhausts)
		Exhaust.Play();
}

void CCar::StopExhausts()
{
	for (SExhaust& Exhaust : m_exhausts)
		Exhaust.Stop();
}

void CCar::UpdateExhausts()
{
	if (!b_engine_on)
		return;

	for (SExhaust& Exhaust : m_exhausts)
		Exhaust.Update();
}

void CCar::ClearExhausts()
{
	for (SExhaust& Exhaust : m_exhausts)
		Exhaust.Clear();
}

void CCar::DoExit()
{
	if (CActor* A = OwnerActor())
	{
		if (!m_doors.empty())m_doors.begin()->second.GetExitPosition(m_exit_position);
		else m_exit_position.set(Position());

		A->detach_Vehicle();

		if (A->g_Alive() <= 0.f)
		{
			A->character_physics_support()->movement()->DestroyCharacter();
		}
	}
}

bool CCar::Use(const Fvector& pos, const Fvector& dir, const Fvector& foot_pos)
{
	if (UsableBonesActive != BI_NONE)
	{
		luabind::functor<bool> Callback;
		ai().script_engine().functor(UsableBones[UsableBonesActive].c_str(), Callback);
		bool CheckComplete = Callback(this);

		if (CheckComplete)
		{
			// FX: Если скрипт вернул true, то будем считать, 
			// что Use был выполнен и посадка в машину не требуется 

			return false;
		}
	}

	xr_map<u16, CCarDoor>::iterator i;

	RQR.r_clear();
	collide::ray_defs	Q(pos, dir, 2.0f, CDB::OPT_CULL, collide::rqtObject);

	VERIFY(!fis_zero(Q.dir.square_magnitude()));
	if (g_pGameLevel->ObjectSpace.RayQuery(RQR, collidable.model, Q))
	{
		collide::rq_results& R = RQR;
		for (int k = 0; k < R.r_count(); ++k)
		{
			collide::rq_result* I = R.r_begin() + k;

			if (IsGameTypeSingle() && m_bone_trunk == (u16)I->element)
			{
				bool IsDoorBone = is_Door((u16)I->element, i);
				if (I->range < 1.f)
				{
					if (IsDoorBone)
					{
						CCarDoor& TrunkDoor = m_doors.at(m_bone_trunk);
						if (TrunkDoor.state != CCarDoor::eState::opened)
							continue;
					}

					CUIGameSP* pGameSP = smart_cast<CUIGameSP*>(CurrentGameUI());
					pGameSP->StartCarBody(Actor(), this);
				}
				else if (IsDoorBone)
				{
					i->second.Use();
				}

				return false;
			}

			if (is_Door((u16)I->element, i))
			{
				bool front = i->second.IsFront(pos, dir);
				if ((Owner()) || (!Owner() && front))i->second.Use();
				if (i->second.state == CCarDoor::broken) break;
				return false;
			}
		}
	}

	if (Owner())
	{
		return Exit(pos, dir);
	}

	return Enter(pos, dir, foot_pos);
}

bool CCar::DoorUse(u16 id)
{
	xr_map<u16, CCarDoor>::iterator i;
	if (is_Door(id, i))
	{
		i->second.Use();
		return true;
	}

	return false;
}

bool CCar::DoorSwitch(u16 id)
{
	xr_map<u16, CCarDoor>::iterator i;
	if (is_Door(id, i))
	{
		i->second.Switch();
		return true;
	}

	return false;
}

bool CCar::DoorClose(u16 id)
{
	xr_map<u16, CCarDoor>::iterator i;
	if (is_Door(id, i))
	{
		i->second.Close();
		return true;
	}

	return false;
}

bool CCar::DoorOpen(u16 id)
{
	xr_map<u16, CCarDoor>::iterator i;
	if (is_Door(id, i))
	{
		i->second.Open();
		return true;
	}

	return false;
}

void CCar::InitParabola()
{
	m_a = expf((m_power_rpm - m_torque_rpm) / (2.f * m_power_rpm)) * m_max_power / m_power_rpm;
	m_b = m_torque_rpm;
	m_c = _sqrt(2.f * m_power_rpm * (m_power_rpm - m_torque_rpm));
}

float CCar::Parabola(float rpm)
{
	float ex = (rpm - m_b) / m_c;
	float value = m_a * expf(-ex * ex) * rpm;
	if (value < 0.f) return 0.f;
	if (e_state_drive == neutral) value *= m_power_neutral_factor;
	return value;
}

float CCar::EnginePower()
{
	float value; value = Parabola(m_current_rpm);
	if (b_starting)
	{
		if (m_current_rpm < m_min_rpm)
		{
			value = Parabola(m_min_rpm);
		}
		else if (Device.dwTimeGlobal - m_dwStartTime > 1000) b_starting = false;
	}

	if (value > m_current_engine_power)
		return value * m_power_increment_factor + m_current_engine_power * (1.f - m_power_increment_factor);
	else
		return value * m_power_decrement_factor + m_current_engine_power * (1.f - m_power_decrement_factor);
}

float CCar::DriveWheelsMeanAngleRate() const
{
	float drive_speed = 0.f;

	for (const SWheelDrive& Wheel : m_driving_wheels)
	{
		drive_speed += Wheel.ASpeed();
	}

	return drive_speed / m_driving_wheels.size();
}

float CCar::EngineDriveSpeed()
{
	float calc_rpm = 0.f;
	if (b_transmission_switching)
	{
		calc_rpm = m_max_rpm;
		if (m_current_rpm > m_power_rpm)
		{
			b_transmission_switching = false;
		}
	}
	else
	{
		calc_rpm = EngineRpmFromWheels();

		if (!b_clutch && calc_rpm < m_min_rpm)
		{
			calc_rpm = m_min_rpm;
		}
		limit_above(calc_rpm, m_max_rpm);
	}

	if (calc_rpm > m_current_rpm)
		return		(1.f - m_rpm_increment_factor) * m_current_rpm + m_rpm_increment_factor * calc_rpm;
	else
		return		(1.f - m_rpm_decrement_factor) * m_current_rpm + m_rpm_decrement_factor * calc_rpm;
}

void CCar::UpdateFuel(float time_delta)
{
	if (!b_engine_on) 
		return;

	if (m_current_rpm > m_min_rpm)
		m_fuel -= time_delta * (m_current_rpm - m_min_rpm) * m_fuel_consumption;
	else
		m_fuel -= time_delta * m_min_rpm * m_fuel_consumption;

	if (m_fuel < EPS) 
		StopEngine();
}

float CCar::AddFuel(float ammount)
{
	float free_space = m_fuel_tank - m_fuel;
	if (ammount < free_space)
	{
		m_fuel += ammount;
		return ammount;
	}
	else
	{
		m_fuel = m_fuel_tank;
		return free_space;
	}
}

void CCar::ResetKeys()
{
	bkp = false;
	fwp = false;
	lsp = false;
	rsp = false;
}

#undef   _USE_MATH_DEFINES

void CCar::OnEvent(NET_Packet& P, u16 type)
{
	inherited::OnEvent(P, type);
	CExplosive::OnEvent(P, type);

	//обработка сообщений, нужных для работы с багажником машины
	u16 id;
	switch (type)
	{
		case GE_TRADE_BUY:
		case GE_OWNERSHIP_TAKE:
		{
			P.r_u16(id);
			CObject* O = Level().Objects.net_Find(id);
			VERIFY(O);

			CGameObject* GO = smart_cast<CGameObject*>(O);
			CInventoryItem* pIItem = smart_cast<CInventoryItem*>(GO);
			VERIFY(inventory().CanTakeItem(pIItem));
			pIItem->m_ItemCurrPlace.type = eItemPlaceRuck;

			O->H_SetParent(this);
			inventory().Take(GO, true, true);
			break;
		}
		case GE_TRADE_SELL:
		case GE_OWNERSHIP_REJECT:
		{
			P.r_u16(id);
			CObject* O = Level().Objects.net_Find(id);
			VERIFY(O);

			bool just_before_destroy = !P.r_eof() && P.r_u8();
			bool dont_create_shell = (type == GE_TRADE_SELL) || just_before_destroy;

			O->SetTmpPreDestroy(just_before_destroy);
			inventory().DropItem(smart_cast<CGameObject*>(O), just_before_destroy, dont_create_shell);
			break;
		}
	}
}

void CCar::ResetScriptData(void* P)
{
	CScriptEntity::ResetScriptData(P);
}

void CCar::PhDataUpdate(float step)
{
	LimitWheels();
	UpdateFuel(step);

	UpdatePower();
	if (b_engine_on && !b_starting && m_current_rpm < m_min_rpm)
		Stall();

	if (bkp)
	{
		UpdateBack();
	}

	if (brp)
		HandBreak();

	for (int k = 0; k < (int)m_doors_update.size(); ++k)
	{
		CCarDoor* D = m_doors_update[k];
		if (!D->update)
		{
			m_doors_update.erase(m_doors_update.begin() + k);
			--k;
		}
		else
		{
			D->Update();
		}
	}

	if (m_steering_wheels.empty())
	{
		m_steer_angle = 0;
		return;
	}

	m_steer_angle = m_steering_wheels.begin()->GetSteerAngle() * 0.1f + m_steer_angle * 0.9f;
	VERIFY(_valid(m_steer_angle));
}

BOOL CCar::UsedAI_Locations()
{
	return (FALSE);
}

u16 CCar::DriverAnimationType()
{
	return m_driver_anim_type;
}

void CCar::OnAfterExplosion()
{

}

void CCar::OnBeforeExplosion()
{
	setEnabled(FALSE);
}

void CCar::CarExplode()
{
	if (b_exploded) 
		return;

	CPHSkeleton::SetNotNeedSave();
	if (m_car_weapon)
		m_car_weapon->Action(CCarWeapon::eWpnActivate, 0);

	m_lights.TurnOffHeadLights();
	m_damage_particles.Stop1(this);
	m_damage_particles.Stop2(this);
	b_exploded = true;

	CExplosive::GenExplodeEvent(Position(), Fvector().set(0.f, 1.f, 0.f));

	if (CActor* A = OwnerActor())
	{
		if (!m_doors.empty())
			m_doors.begin()->second.GetExitPosition(m_exit_position);
		else 
			m_exit_position.set(Position());

		A->detach_Vehicle();
		if (A->g_Alive() <= 0.f)A->character_physics_support()->movement()->DestroyCharacter();
	}

	if (CPHDestroyable::CanDestroy())
		CPHDestroyable::Destroy(ID(), "physic_destroyable_object");
}

template <class T> IC void CCar::fill_wheel_vector(LPCSTR S, xr_vector<T>& type_wheels)
{
	IKinematics* pKinematics = smart_cast<IKinematics*>(Visual());
	string64 S1;
	int count = _GetItemCount(S);
	for (int i = 0; i < count; ++i)
	{
		_GetItem(S, i, S1);

		u16 bone_id = pKinematics->LL_BoneID(S1);

		type_wheels.push_back(T());
		T& twheel = type_wheels.back();

		if (!bone_map.contains(bone_id))
		{
			bone_map.insert(std::make_pair(bone_id, physicsBone()));

			SWheel& wheel = (m_wheels_map.insert(std::make_pair(bone_id, SWheel(this)))).first->second;
			wheel.bone_id = bone_id;
			twheel.pwheel = &wheel;
			wheel.Load(S1);
			twheel.Load(S1);
		}
		else
		{
			twheel.pwheel = &(m_wheels_map.find(bone_id))->second;
			twheel.Load(S1);
		}
	}
}

IC void CCar::fill_exhaust_vector(LPCSTR S, xr_vector<SExhaust>& exhausts)
{
	IKinematics* pKinematics = smart_cast<IKinematics*>(Visual());
	string64					S1;
	int count = _GetItemCount(S);
	for (int i = 0; i < count; ++i)
	{
		_GetItem(S, i, S1);

		u16 bone_id = pKinematics->LL_BoneID(S1);
		exhausts.push_back(SExhaust(this));

		SExhaust& exhaust = exhausts.back();
		exhaust.bone_id = bone_id;

		if (!bone_map.contains(bone_id))
		{
			bone_map.insert(std::make_pair(bone_id, physicsBone()));
		}
	}
}

IC void CCar::fill_doors_map(LPCSTR S, xr_map<u16, CCarDoor>& doors)
{
	IKinematics* pKinematics = smart_cast<IKinematics*>(Visual());
	string64 S1;
	int count = _GetItemCount(S);

	for (int i = 0; i < count; ++i)
	{
		_GetItem(S, i, S1);

		u16 bone_id = pKinematics->LL_BoneID(S1);
		CCarDoor door(this);
		door.bone_id = bone_id;
		doors.insert(std::make_pair(bone_id, door));

		if (!bone_map.contains(bone_id))
		{
			bone_map.insert(std::make_pair(bone_id, physicsBone()));
		}
	}
}

DLL_Pure* CCar::_construct()
{
	inherited::_construct();
	CScriptEntity::_construct();
	return						(this);
}

u16 CCar::Initiator()
{
	if (g_Alive() && Owner())
	{
		return Owner()->ID();
	}

	return ID();
}

float CCar::RefWheelMaxSpeed()
{
	return m_max_rpm / m_current_gear_ratio;
}

float CCar::EngineCurTorque()
{
	return m_current_engine_power / m_current_rpm;
}
float CCar::RefWheelCurTorque()
{
	if (b_transmission_switching) 
		return 0.f;

	return EngineCurTorque() * ((m_current_gear_ratio < 0.f) ? -m_current_gear_ratio : m_current_gear_ratio);
}

void CCar::GetRayExplosionSourcePos(Fvector& pos)
{
	random_point_in_object_box(pos, this);
}

void CCar::net_Relcase(CObject* O)
{
	CExplosive::net_Relcase(O);
	inherited::net_Relcase(O);

	if (m_memory)
		m_memory->remove_links(O);
}

void CCar::ASCUpdate()
{
	for (u16 i = 0; i < cAsCallsnum; ++i)
	{
		EAsyncCalls c = EAsyncCalls(1 << i);

		if (async_calls.test(u16(c)))
			ASCUpdate(c);
	}
}

void CCar::ASCUpdate(EAsyncCalls c)
{
	async_calls.set(u16(c), FALSE);
	switch (c) 
	{
	case ascSndTransmission:m_car_sound->TransmissionSwitch(); break;
	case ascSndStall:m_car_sound->Stop(); break;
	case ascExhoustStop:StopExhausts(); break;
	default: NODEFAULT;
	}
}

void CCar::AscCall(EAsyncCalls c)
{
	async_calls.set(u16(c), TRUE);
}

bool CCar::CanRemoveObject()
{
	return CExplosive::IsExploded() && !CExplosive::IsSoundPlaying();
}

void CCar::SetExplodeTime(u32 et)
{
	CDelayedActionFuse::Initialize(float(et) / 1000.f, CDamagableItem::DamageLevelToHealth(2));
}

u32 CCar::ExplodeTime()
{
	if (CDelayedActionFuse::isInitialized())
		return u32(CDelayedActionFuse::Time()) * 1000;
	else return 0;
}

void CCar::Die(CObject* who)
{
	inherited::Die(who);
	CarExplode();
}

Fvector	CCar::ExitVelocity()
{
	CPhysicsShell* P = PPhysicsShell();
	if (!P || !P->isActive())return Fvector().set(0, 0, 0);
	CPhysicsElement* E = P->get_ElementByStoreOrder(0);
	Fvector v = ExitPosition();

	E->GetPointVel(v, v);
	return v;
}

bool CCar::DoorHit(float P, s16 element, ALife::EHitType hit_type)
{
	if (hit_type == ALife::eHitTypeStrike && P > 20.f)
	{
		for (auto& [ID, DoorObj] : m_doors)
		{
			DoorObj.Open();
		}
	}

	auto i = m_doors.find(element);
	if (i != m_doors.end())
	{
		i->second.Hit(P);
		return true;
	}

	return false;
}