/////////////////////////////////////////////////////////////////////////////////////////////////
#include "StdAfx.h"
#include "PHCharacter.h"
#include "Physics.h"
#include "ExtendedGeom.h"
#include "PHCapture.h"
#include "PHElement.h"
#include "../Include/xrRender/Kinematics.h"
#include "IPhysicsShellHolder.h"
#include "../xrEngine/bone.h"
#include "../xrEngine/device.h"

extern class CPHWorld *ph_world;

XRPHYSICS_API float g_CameraCaptureMotorForce = 50000.f;
XRPHYSICS_API bool g_CameraCaptureNoCollision = false;
XRPHYSICS_API bool g_CameraCaptureSelfCollision = false;
XRPHYSICS_API bool g_CameraCaptureExactIntegration = false;
XRPHYSICS_API SCameraCaptureShellInfo g_CameraCaptureShellInfo = {0, 0, false};

///////////////////////////////////////////////////////////////////////////////////
bool can_capture( CPHCharacter   *a_character, IPhysicsShellHolder	*a_taget_object  )
{
	if(!a_taget_object									||
	   !a_taget_object->ObjectPPhysicsShell()					||
	   !a_taget_object->ObjectPPhysicsShell()->isActive()		||
	   a_taget_object->IsInventoryItem()				||
	   !a_character										||
	   !a_character->b_exist							||
	   !a_character->PhysicsRefObject()					||
	   !a_character->PhysicsRefObject()->ObjectKinematics( )
	   ) return false;

	IKinematics* p_kinematics = a_character->PhysicsRefObject()->ObjectKinematics( ) ;
	VERIFY( p_kinematics );
	CInifile* ini = p_kinematics->LL_UserData();
	if(!ini								||
	   !ini->section_exist( "capture" )
	   ) return false;
	return true;
}

bool can_capture(CPHCharacter* a_character, IPhysicsShellHolder* a_taget_object, u16 a_taget_element)
{
	if (!can_capture(a_character, a_taget_object) || a_taget_element == BI_NONE || !a_taget_object->ObjectKinematics())
		return false;

	IKinematics* K = a_taget_object->ObjectKinematics();

	if (!K || !K->LL_GetBoneInstance(a_taget_element).callback_param())
		return false;

	return true;
}

static CBoneInstance* get_capture_bone(CPHCharacter* a_character)
{
	VERIFY(a_character);
	VERIFY(a_character->PhysicsRefObject());

	IKinematics* p_kinematics = a_character->PhysicsRefObject()->ObjectKinematics();
	VERIFY(p_kinematics);

	CInifile* ini = p_kinematics->LL_UserData();
	VERIFY(ini);
	VERIFY(ini->section_exist("capture"));

	u16 capture_bone_id = p_kinematics->LL_BoneID(ini->r_string("capture", "bone"));
	R_ASSERT2(capture_bone_id != BI_NONE, "wrong capture bone");

	return &p_kinematics->LL_GetBoneInstance(capture_bone_id);
}

CPHCapture::CPHCapture(CPHCharacter* a_character, IPhysicsShellHolder* a_taget_object, NearestToPointCallback* cb /*=0*/) :
	m_joint(nullptr),
	m_ajoint(nullptr),
	m_body(nullptr),
	m_free_chain(false),
	m_global_pos(Fvector().set(0, 0, 0)),
	m_taget_object(a_taget_object),
	m_character(a_character),
	b_disabled(false),
	b_character_feedback(false),
	e_state(cstFree),
	///////////////////////////////////////////////////////////////
	m_taget_element(0),
	m_capture_pos(Fvector().set(0, 0, 0)),
	m_back_force(0),
	m_pull_force(0),
	m_capture_force(0),
	m_capture_distance(0),
	m_capture_time(0),
	m_time_start(0),
	m_capture_bone(0),
	b_collide(false),
	m_rel_rotation_valid(false),
	m_camera_velocity_valid(false),
	m_gravity_disabled(false),
	m_saved_gravity_mode(1),
	m_shell_collision_taken(false),
	m_shell_collision_was_disabled(false),
	m_self_collision_taken(false),
	m_self_collision_was(false),
	m_exact_integration_taken(false),
	m_exact_integration_was(false),
	m_camera_angular_velocity(Fvector().set(0, 0, 0))
{
	if (!can_capture(a_character, a_taget_object))
		return;

	VERIFY(m_taget_object);
	VERIFY(m_taget_object->ObjectPPhysicsShell());
	VERIFY(a_character);
	m_capture_bone = get_capture_bone(a_character);
	VERIFY(m_capture_bone);
	m_taget_element = m_taget_object->ObjectPPhysicsShell()->NearestToPoint(m_capture_bone->mTransform.c, cb);
	if (!m_taget_element)
		return;
	Init();
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////
CPHCapture::CPHCapture(CPHCharacter* a_character, IPhysicsShellHolder* a_taget_object, u16 a_taget_element) :
	m_joint(nullptr),
	m_ajoint(nullptr),
	m_body(nullptr),
	b_disabled(false),
	b_character_feedback(false),
	m_taget_object(a_taget_object),
	m_character(a_character),
	e_state(cstFree),

	///////////////////////////////////////////////////////////////
	m_taget_element(0),
	m_capture_pos(Fvector().set(0, 0, 0)),
	m_back_force(0),
	m_pull_force(0),
	m_capture_force(0),
	m_capture_distance(0),
	m_capture_time(0),
	m_time_start(0),
	m_capture_bone(0),
	b_collide(false),
	m_rel_rotation_valid(false),
	m_camera_velocity_valid(false),
	m_gravity_disabled(false),
	m_saved_gravity_mode(1),
	m_shell_collision_taken(false),
	m_shell_collision_was_disabled(false),
	m_self_collision_taken(false),
	m_self_collision_was(false),
	m_exact_integration_taken(false),
	m_exact_integration_was(false),
	m_camera_angular_velocity(Fvector().set(0, 0, 0))
{

	if (!can_capture(a_character, a_taget_object, a_taget_element))
		return;
	VERIFY(m_taget_object);
	VERIFY(a_character);
	m_capture_bone = get_capture_bone(a_character);

	IKinematics* K = m_taget_object->ObjectKinematics();
	VERIFY(K);

	CBoneInstance& tag_bone = K->LL_GetBoneInstance(a_taget_element);
	VERIFY(tag_bone.callback_param());

	m_taget_element = (CPhysicsElement*)tag_bone.callback_param();
	VERIFY(m_taget_element);

	Init();
}

CPHCapture::CPHCapture(CPHCharacter* a_character, IPhysicsShellHolder* a_taget_object, u16 a_taget_element, const Fvector& global_position, bool free_chain) :
	m_joint(nullptr),
	m_ajoint(nullptr),
	m_body(nullptr),
	m_free_chain(free_chain),
	m_global_pos(global_position),
	m_taget_object(a_taget_object),
	m_character(a_character),
	b_disabled(false),
	b_character_feedback(false),
	e_state(cstFree),
	m_taget_element(0),
	m_capture_pos(Fvector().set(0, 0, 0)),
	m_back_force(0),
	m_pull_force(0),
	m_capture_force(0),
	m_capture_distance(0),
	m_capture_time(0),
	m_time_start(0),
	m_capture_bone(0),
	b_collide(false),
	m_rel_rotation_valid(false),
	m_camera_velocity_valid(false),
	m_gravity_disabled(false),
	m_saved_gravity_mode(1),
	m_shell_collision_taken(false),
	m_shell_collision_was_disabled(false),
	m_self_collision_taken(false),
	m_self_collision_was(false),
	m_exact_integration_taken(false),
	m_exact_integration_was(false),
	m_camera_angular_velocity(Fvector().set(0, 0, 0))
{
	if (!a_taget_object)
		return;

	CPhysicsShell* shell = a_taget_object->ObjectPPhysicsShell();

	if (!shell || !shell->isActive())
		return;

	if (a_character)
	{
		if (!can_capture(a_character, a_taget_object, a_taget_element))
			return;

		m_capture_bone = get_capture_bone(a_character);
	}

	IKinematics* K = a_taget_object->ObjectKinematics();

	if (K && a_taget_element != BI_NONE && a_taget_element < K->LL_BoneCount())
	{
		CBoneInstance& tag_bone = K->LL_GetBoneInstance(a_taget_element);

		if (tag_bone.callback_type() == bctPhysics && tag_bone.callback_param())
			m_taget_element = static_cast<CPhysicsElement*>(tag_bone.callback_param());
	}

	if (!m_taget_element)
		m_taget_element = shell->NearestToPoint(m_global_pos);

	if (!m_taget_element)
		return;

	Init();
}

void CPHCapture::Init()
{
	VERIFY(m_taget_element);

	Fvector dir;

	if (m_character)
	{
		VERIFY(m_character->PhysicsRefObject());
		VERIFY(m_character->PhysicsRefObject()->ObjectKinematics());
		VERIFY(m_capture_bone);

		IKinematics* p_kinematics = m_character->PhysicsRefObject()->ObjectKinematics();
		VERIFY(p_kinematics);
		CInifile* ini = p_kinematics->LL_UserData();
		VERIFY(ini);

		Fvector capture_bone_position;
		capture_bone_position.set(m_capture_bone->mTransform.c);
		b_character_feedback = true;
		(m_character->PhysicsRefObject())->ObjectXFORM().transform_tiny(capture_bone_position);

		m_taget_element->GetGlobalPositionDynamic(&dir);
		dir.sub(capture_bone_position, dir);

		m_pull_distance = ini->r_float("capture", "pull_distance");
		if (dir.magnitude() > m_pull_distance)
			return;

		float 					pool_force_factor = 4.f;
		m_capture_distance = ini->r_float("capture", "distance");				//distance
		m_capture_force = ini->r_float("capture", "capture_force");				//capture force
		m_capture_time = ini->r_u32("capture", "time_limit") * 1000;			//time;		
		m_time_start = Device.dwTimeGlobal;

		float max_pull_force = ini->r_float("capture", "pull_force");			//pull force
		m_pull_force = pool_force_factor * ph_world->Gravity() * m_taget_element->PhysicsShell()->getMass();

		if (m_pull_force > max_pull_force)
			m_pull_force = max_pull_force;

		float pulling_vel_scale = ini->r_float("capture", "velocity_scale");

		m_taget_element->set_DynamicLimits(default_l_limit * pulling_vel_scale, default_w_limit * pulling_vel_scale);
		m_character->SetObjectContactCallback(object_contactCallbackFun);
	}
	else
	{
		b_character_feedback = false;

		if (!m_taget_element || !m_taget_element->isFullActive())
			return;

		m_taget_element->GetGlobalPositionDynamic(&dir);
		dir.sub(m_global_pos, dir);

		m_pull_distance = 20.f;
		if (dir.magnitude() > m_pull_distance)
			return;

		float 					pool_force_factor = 4.f;
		m_time_start = Device.dwTimeGlobal;
		m_capture_distance = 10.f;											//distance
		m_capture_force = 20500.f;											//capture force
		m_capture_time = 3 * 1000;											//time;		
		float max_pull_force = 8000.f;										//pull force
		m_pull_force = pool_force_factor * ph_world->Gravity() * m_taget_element->PhysicsShell()->getMass();

		if (m_pull_force > max_pull_force)
			m_pull_force = max_pull_force;

		float pulling_vel_scale = 0.022f;

		m_taget_element->set_DynamicLimits(default_l_limit * pulling_vel_scale, default_w_limit * pulling_vel_scale);
	}

	m_island.Init();

	if (m_character)
	{
		IPhysicsShellHolder* A = (m_character->PhysicsRefObject());
		if (A->IsActor())
		{
			A->HideAllWeapons(true);
		}
	}

	CPHUpdateObject::Activate();
	e_state = cstPulling;
}

void CPHCapture::Release()
{
	if (e_state == cstReleased || e_state == cstFree)
		return;

	RestoreGravity();
	RestoreShellCollisions();
	RestoreShellSelfCollision();
	RestoreShellExactIntegration();

	VERIFY(m_island.DActiveIsland() == &m_island);

	if (m_joint)
	{
		m_island.RemoveJoint(m_joint);
		dJointDestroy(m_joint);
	}

	m_joint = nullptr;

	if (m_ajoint)
	{
		m_island.RemoveJoint(m_ajoint);
		dJointDestroy(m_ajoint);
	}

	m_ajoint = nullptr;

	if (m_body)
	{
		m_island.RemoveBody(m_body);
		dBodyDestroy(m_body);
	}

	m_body = nullptr;

	CPHElement* elem = static_cast<CPHElement*>(m_taget_element);

	if (elem && elem->get_body() && m_taget_object && !m_taget_object->ObjectGetDestroy() &&
		m_taget_object->ObjectPPhysicsShell() && m_taget_object->ObjectPPhysicsShell()->isActive())
	{
		elem->set_DynamicLimits();
	}

	b_collide = true;

	if (m_character)
	{
		if (IPhysicsShellHolder* A = (m_character->PhysicsRefObject()))
			A->HideAllWeapons(false);
	}

	e_state = cstReleased;
}

#include "PHElement.h"
void CPHCapture::Deactivate()
{
	Release();
	if (m_taget_element)
	{
		VERIFY(dynamic_cast<CPHElement*>(m_taget_element));

		m_taget_element->Enable();
	}

	if (m_character)
		m_character->SetObjectContactCallback(0);

	CPHUpdateObject::Deactivate();
	e_state = cstFree;
	m_character = nullptr;
	m_taget_object = nullptr;
	m_taget_element = nullptr;
}