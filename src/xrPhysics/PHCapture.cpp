/////////////////////////////////////////////////////////////////////////////////////////////////
#include "StdAfx.h"

#include "PHCapture.h"
#include "PHCharacter.h"
#include "Physics.h"
#include "ExtendedGeom.h"

//#include "entity_alive.h"
//#include "phmovementcontrol.h"
#include "../Include/xrRender/Kinematics.h"
#include "IPhysicsShellHolder.h"
#include "../xrEngine/bone.h"
#include "../xrEngine/device.h"
#include "MathUtilsOde.h"
#include "PHElement.h"
#include "PHDynamicData.h"

//#include "characterphysicssupport.h"
///////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////
IPHCapture* phcapture_create(CPHCharacter* ch, IPhysicsShellHolder* object, NearestToPointCallback* cb /*=0*/)
{
	VERIFY(ch);
	return new CPHCapture(ch, object, cb);
}

IPHCapture *phcapture_create(CPHCharacter *ch, IPhysicsShellHolder* object,u16 element)
{
	VERIFY( ch );
	return new CPHCapture(ch, object, element);
}

IPHCapture *phcapture_create(CPHCharacter *ch, IPhysicsShellHolder* object,u16 element,const Fvector& global_pos,bool free_chain)
{
	return new CPHCapture(ch, object, element, global_pos, free_chain);
}

void phcapture_destroy(IPHCapture*& c)
{
	CPHCapture* capture = smart_cast<CPHCapture*>(c);
	xr_delete(capture);
	c = 0;
}

void CPHCapture::CreateBody()
{
	m_body= dBodyCreate(0);
	m_island.AddBody(m_body);
	dMass m;
	dMassSetSphere(&m,1.f,1000000.f);
	dMassAdjust(&m,100000.f);
	dBodySetMass(m_body,&m);
	dBodySetGravityMode(m_body,0);
}

CPHCapture::~CPHCapture()
{
	Deactivate();
}

bool CPHCapture::Invalid()
{
	return  !m_taget_object->ObjectPPhysicsShell()||
			!m_taget_object->ObjectPPhysicsShell()->isActive()||
			(m_character && !m_character->b_exist);
}

void CPHCapture::PhDataUpdate(dReal step)
{
	switch(e_state) 
	{
	case cstFree:  
		break;
	case cstPulling:  PullingUpdate();
		break;
	case cstCaptured: CapturedUpdate(step);
		break;
	case cstReleased: ReleasedUpdate();
		break;
	default: NODEFAULT;
	}
}

void CPHCapture::PhTune(dReal step)
{
	if(e_state == cstFree)
		return;

	CPHElement* elem = static_cast<CPHElement*>(m_taget_element);

	if(!m_taget_object || !elem || !elem->get_body())
		return;

	bool act_capturer = !!(m_character && m_character->CPHObject::is_active());
	bool act_taget = elem->isEnabled();

	b_disabled = !act_taget;

	if(!act_taget)
		elem->Enable();

	if(act_taget && act_capturer)
		m_character->Enable();

	switch(e_state) 
	{
	case cstPulling:  ;
		break;
	case cstCaptured: 
		{
				if(!m_character && !m_free_chain && m_rel_rotation_valid)
				{
					if(m_body)
					{
						dBodySetPosition(m_body, m_global_pos.x, m_global_pos.y, m_global_pos.z);
						dBodySetLinearVel(m_body, 0.f, 0.f, 0.f);
					}

					ApplyAnchorRotation(elem, step);

					DisableGravity();
				}

				if(!m_character)
				{
					if(g_CameraCaptureNoCollision)
					{
						DisableShellCollisions();
					}
					else
					{
						RestoreShellCollisions();
					}

					if(g_CameraCaptureSelfCollision)
					{
						EnableShellSelfCollision();
					}
					else
					{
						RestoreShellSelfCollision();
					}

					if(g_CameraCaptureExactIntegration)
					{
						EnableShellExactIntegration();
					}
					else
					{
						RestoreShellExactIntegration();
					}

					CPhysicsShell* Shell=elem->PhysicsShell();
					if(Shell)
					{
						g_CameraCaptureShellInfo.Elements=int(Shell->get_ElementsNumber());
						g_CameraCaptureShellInfo.SelfCollisionContacts=int(Shell->LastSelfCollisionContacts());
						g_CameraCaptureShellInfo.Animator=Shell->PPhysicsShellAnimator()!=nullptr;
					}
				}

				if(b_disabled)
					dBodyDisable(m_body);
				else 
				{
					if(m_character)
						m_character->Island().Merge(&m_island);
					elem->PhysicsShell()->PIsland()->Merge(&m_island);
					VERIFY(!m_island.IsActive());
				}
		}
		break;
	case cstReleased: ;
		break;
	default: NODEFAULT;
	}
	
}

void CPHCapture::PullingUpdate()
{
	CPHElement* elem = static_cast<CPHElement*>(m_taget_element);
	if(!elem || !elem->get_body())
		return;

	if(m_character && (!m_taget_element->isActive() || Device.dwTimeGlobal - m_time_start > m_capture_time))
	{
		Release();
		return;
	}

	Fvector dir;
	Fvector capture_position = m_global_pos;

	if(m_capture_bone)
		capture_position.set(m_capture_bone->mTransform.c);

	if(m_character)
		m_character->PhysicsRefObject()->ObjectXFORM().transform_tiny(capture_position);

	m_taget_element->GetGlobalPositionDynamic(&dir);

	if(!_valid(capture_position))
	{
		Release();
		return;
	}

	dir.sub(capture_position, dir);
	float dist = dir.magnitude();

	if(m_character && dist > m_pull_distance)
	{
		Release();
		return;
	}

	if(dist < EPS_L)
	{
		if(m_character)
		{
			Release();
			return;
		}

		dir.set(Device.vCameraDirection);
	}
	else
	{
		dir.mul(1.f / dist);
	}

	if(dist < m_capture_distance)
	{
		m_back_force = 0.f;
		m_joint = dJointCreateBall(0, 0);
		m_island.AddJoint(m_joint);
		m_ajoint = dJointCreateAMotor(0, 0);
		m_island.AddJoint(m_ajoint);

		dJointSetAMotorMode(m_ajoint, dAMotorEuler);
		dJointSetAMotorNumAxes(m_ajoint, 3);

		CreateBody();
		dBodySetPosition(m_body, capture_position.x, capture_position.y, capture_position.z);
		VERIFY(smart_cast<CPHElement*>(m_taget_element));
		CPHElement* e = static_cast<CPHElement*>(m_taget_element);

		dJointAttach(m_joint, m_body, e->get_body());
		dJointAttach(m_ajoint, m_body, e->get_body());
		dJointSetFeedback(m_joint, &m_joint_feedback);
		dJointSetFeedback(m_ajoint, &m_joint_feedback);
		dJointSetBallAnchor(m_joint, capture_position.x, capture_position.y, capture_position.z);

		if (!m_character)
		{
			Fvector elem_pos;
			m_taget_element->GetGlobalPositionDynamic(&elem_pos);

			Fvector dir2 = Fvector().sub(Device.vCameraPosition, elem_pos);
			float mag = dir2.magnitude();
			if (mag > EPS_L)
			{
				dir2.mul(1.f / mag);
				dir = dir2;
			}
		}

		dJointSetAMotorAxis(m_ajoint, 0, 1, dir.x, dir.y, dir.z);

		if(dir.x > EPS)
		{
			if(dir.y > EPS)
			{
				float mag = dir.x * dir.x + dir.y * dir.y;
				dJointSetAMotorAxis(m_ajoint, 2, 2, -dir.y / mag, dir.x / mag, 0.f);
			}
			else if(dir.z > EPS)
			{
				float mag = dir.x * dir.x + dir.z * dir.z;
				dJointSetAMotorAxis(m_ajoint, 2, 2, -dir.z / mag, 0.f, dir.x / mag);
			}
			else
			{
				dJointSetAMotorAxis(m_ajoint, 2, 2, 1.f, 0.f, 0.f);
			}
		}
		else
		{
			if(dir.y > EPS)
			{
				if(dir.z > EPS)
				{
					float mag = dir.y * dir.y + dir.z * dir.z;
					dJointSetAMotorAxis(m_ajoint, 2, 2, 0.f, -dir.z / mag, dir.y / mag);
				}
				else
				{
					dJointSetAMotorAxis(m_ajoint, 2, 2, 0.f, 1.f, 0.f);
				}
			}
			else
			{
				dJointSetAMotorAxis(m_ajoint, 2, 2, 0.f, 0.f, 1.f);
			}
		}

		if(!m_free_chain)
		{
			const float motor_force = m_character ? m_capture_force * 0.2f : g_CameraCaptureMotorForce;

			dJointSetAMotorParam(m_ajoint, dParamFMax, motor_force);
			dJointSetAMotorParam(m_ajoint, dParamVel, 0.f);

			dJointSetAMotorParam(m_ajoint, dParamFMax2, motor_force);
			dJointSetAMotorParam(m_ajoint, dParamVel2, 0.f);

			dJointSetAMotorParam(m_ajoint, dParamFMax3, motor_force);
			dJointSetAMotorParam(m_ajoint, dParamVel3, 0.f);
		}

		///////////////////////////////////
		if(m_character)
		{
			float sf = 0.1f, df = 10.f;

			float erp = Erp(world_spring * sf, world_damping * df);
			float cfm = Cfm(world_spring * sf, world_damping * df);

			dJointSetAMotorParam(m_ajoint, dParamStopERP, erp);
			dJointSetAMotorParam(m_ajoint, dParamStopCFM, cfm);

			dJointSetAMotorParam(m_ajoint, dParamStopERP2, erp);
			dJointSetAMotorParam(m_ajoint, dParamStopCFM2, cfm);

			dJointSetAMotorParam(m_ajoint, dParamStopERP3, erp);
			dJointSetAMotorParam(m_ajoint, dParamStopCFM3, cfm);

			sf = 0.1f, df = 10.f;
			erp = Erp(world_spring * sf, world_damping * df);
			cfm = Cfm(world_spring * sf, world_damping * df);

			dJointSetAMotorParam(m_ajoint, dParamCFM, cfm);
			dJointSetAMotorParam(m_ajoint, dParamCFM2, cfm);
			dJointSetAMotorParam(m_ajoint, dParamCFM3, cfm);
		}
		else
		{
			dJointSetAMotorParam(m_ajoint, dParamStopERP, 0);
			dJointSetAMotorParam(m_ajoint, dParamStopCFM, 0);

			dJointSetAMotorParam(m_ajoint, dParamStopERP2, 0);
			dJointSetAMotorParam(m_ajoint, dParamStopCFM2, 0);

			dJointSetAMotorParam(m_ajoint, dParamStopERP3, 0);
			dJointSetAMotorParam(m_ajoint, dParamStopCFM3, 0);

			dJointSetAMotorParam(m_ajoint, dParamFudgeFactor, 0);
			dJointSetAMotorParam(m_ajoint, dParamFudgeFactor2, 0);
			dJointSetAMotorParam(m_ajoint, dParamFudgeFactor3, 0);

			dJointSetAMotorParam(m_ajoint, dParamCFM, 0);
			dJointSetAMotorParam(m_ajoint, dParamCFM2, 0);
			dJointSetAMotorParam(m_ajoint, dParamCFM3, 0);

			dJointSetAMotorParam(m_ajoint, dParamBounce, 0);
			dJointSetAMotorParam(m_ajoint, dParamBounce2, 0);
			dJointSetAMotorParam(m_ajoint, dParamBounce3, 0);
		}

		if(m_free_chain)
		{
			Fvector angular = *(Fvector*)dBodyGetAngularVel(e->get_body());
			dBodySetAngularVel(m_body, VPUSH(angular));

			Fvector torque = *(Fvector*)dBodyGetTorque(e->get_body());
			dBodySetTorque(m_body, VPUSH(torque));
		}
		else
		{
			m_taget_element->set_LinearVel(Fvector().set(0, 0, 0));
			m_taget_element->set_AngularVel(Fvector().set(0, 0, 0));
		}

		m_taget_element->set_DynamicLimits();
		e_state = cstCaptured;
		return;
	}

	m_taget_element->applyForce(dir, m_pull_force);
}

void CPHCapture::CapturedUpdate(dReal step)
{
	CPHElement* elem = static_cast<CPHElement*>(m_taget_element);
	if(!elem || !elem->get_body())
		return;

	m_island.Unmerge();

	if((m_character && m_character->CPHObject::is_active()) || !m_character)
		m_taget_element->Enable();

	if(!m_character && !m_free_chain && m_rel_rotation_valid)
	{
		ApplyAnchorRotation(elem, step);

		DisableGravity();
	}

	if(!m_character)
	{
		if(g_CameraCaptureNoCollision)
		{
			DisableShellCollisions();
		}
		else
		{
			RestoreShellCollisions();
		}

		if(g_CameraCaptureSelfCollision)
		{
			EnableShellSelfCollision();
		}
		else
		{
			RestoreShellSelfCollision();
		}

		if(g_CameraCaptureExactIntegration)
		{
			EnableShellExactIntegration();
		}
		else
		{
			RestoreShellExactIntegration();
		}
	}

	if(m_character && (!m_taget_element->isActive() || dDOT(m_joint_feedback.f2, m_joint_feedback.f2) > m_capture_force * m_capture_force))
	{
		Release();
		return;
	}

	float mag = dSqrt(dDOT(m_joint_feedback.f1, m_joint_feedback.f1));

	if(b_character_feedback && mag > m_capture_force / 2.2f && m_character)
	{
		float f = mag / (m_capture_force / 15.f);
		m_character->ApplyForce(m_joint_feedback.f1[0] / f, m_joint_feedback.f1[1] / f, m_joint_feedback.f1[2] / f);
	}

	Fvector capture_position = m_global_pos;

	if(m_capture_bone)
		capture_position.set(m_capture_bone->mTransform.c);

	if(m_character)
		m_character->PhysicsRefObject()->ObjectXFORM().transform_tiny(capture_position);

	if(!_valid(capture_position))
	{
		Release();
		return;
	}

	dBodySetPosition(m_body, capture_position.x, capture_position.y, capture_position.z);
}

void CPHCapture::EnableAMotor(bool val)
{
	if(e_state != cstCaptured || !m_body || !m_ajoint)
		return;

	dJointSetAMotorParam(m_ajoint, dParamFMax, val ? dInfinity : 0);
	dJointSetAMotorParam(m_ajoint, dParamFMax2, val ? dInfinity : 0);
	dJointSetAMotorParam(m_ajoint, dParamFMax3, val ? dInfinity : 0);
}

void CPHCapture::SetFreeChain(bool val)
{
	if(m_free_chain == val)
		return;

	if(val)
	{
		RestoreGravity();
		RestoreShellCollisions();
		RestoreShellSelfCollision();
		RestoreShellExactIntegration();
	}

	m_free_chain = val;

	m_rel_rotation_valid = false;

	if(!m_character && m_ajoint)
	{
		const float motor_force = val ? 0.f : g_CameraCaptureMotorForce;
		dJointSetAMotorParam(m_ajoint, dParamFMax, motor_force);
		dJointSetAMotorParam(m_ajoint, dParamFMax2, motor_force);
		dJointSetAMotorParam(m_ajoint, dParamFMax3, motor_force);
	}
}

void CPHCapture::SetTargetRotation(const Fmatrix& camera_rotation)
{
	CPHElement* elem = static_cast<CPHElement*>(m_taget_element);
	if(!elem || !elem->get_body())
		return;

	m_camera_rotation.set(camera_rotation);

	if(m_rel_rotation_valid)
		return;

	Fmatrix element_rotation;
	element_rotation.identity();
	PHDynamicData::DMXtoFMX(dBodyGetRotation(elem->get_body()), element_rotation);

	Fmatrix camera_basis;
	camera_basis.set(camera_rotation.i, camera_rotation.j, camera_rotation.k, Fvector().set(0.f, 0.f, 0.f));

	Fmatrix inverse_camera;
	inverse_camera.transpose(camera_basis);

	m_rel_rotation.mul(inverse_camera, element_rotation);
	m_rel_rotation_valid = true;
}

void CPHCapture::RotateTarget(const Fmatrix& delta)
{
	if(!m_rel_rotation_valid)
		return;

	Fmatrix rotated;
	rotated.mul(delta, m_rel_rotation);
	m_rel_rotation.set(rotated);
}

void CPHCapture::SetTargetAngularVelocity(const Fvector& velocity)
{
	m_camera_angular_velocity.set(velocity);
	m_camera_velocity_valid = true;
}

void CPHCapture::ApplyAnchorRotation(CPHElement* elem, dReal step)
{
	if(!m_body || !m_ajoint || !elem || !elem->get_body())
		return;

	dJointSetAMotorParam(m_ajoint, dParamFMax, g_CameraCaptureMotorForce);
	dJointSetAMotorParam(m_ajoint, dParamFMax2, g_CameraCaptureMotorForce);
	dJointSetAMotorParam(m_ajoint, dParamFMax3, g_CameraCaptureMotorForce);

	Fmatrix desired_rotation;
	desired_rotation.mul(m_camera_rotation, m_rel_rotation);

	Fmatrix current_rotation;
	current_rotation.identity();
	PHDynamicData::DMXtoFMX(dBodyGetRotation(elem->get_body()), current_rotation);

	Fmatrix inverse_current;
	inverse_current.transpose(current_rotation);

	Fmatrix error_rotation;
	error_rotation.mul(desired_rotation, inverse_current);

	Fvector angular_error;
	angular_error.set(
		(error_rotation._23 - error_rotation._32) * 0.5f,
		(error_rotation._31 - error_rotation._13) * 0.5f,
		(error_rotation._12 - error_rotation._21) * 0.5f);

	float gain = (step > EPS_S) ? 0.25f / step : 0.f;
	clamp(gain, 0.f, 60.f);
	angular_error.mul(gain);

	Fvector target_velocity;

	if(m_camera_velocity_valid)
	{
		target_velocity.set(m_camera_angular_velocity);
	}
	else
	{
		target_velocity.set(0.f, 0.f, 0.f);
	}

	target_velocity.add(angular_error);
	target_velocity.clamp(Fvector().set(-20.f, -20.f, -20.f), Fvector().set(20.f, 20.f, 20.f));

	dMatrix3 body_rotation;
	PHDynamicData::FMXtoDMX(desired_rotation, body_rotation);
	dBodySetRotation(m_body, body_rotation);
	dBodySetAngularVel(m_body, VPUSH(target_velocity));
}

void CPHCapture::DisableShellCollisions()
{
	if(m_shell_collision_taken)
		return;

	CPHElement* elem = static_cast<CPHElement*>(m_taget_element);
	if(!elem || !elem->get_body())
		return;

	CPhysicsShell* shell = elem->PhysicsShell();
	if(!shell)
		return;

	m_shell_collision_was_disabled = shell->IsCollisionDisabled();

	if(!m_shell_collision_was_disabled)
	{
		shell->DisableCollision();
	}

	m_shell_collision_taken = true;
}

void CPHCapture::RestoreShellCollisions()
{
	if(!m_shell_collision_taken)
		return;

	if(!m_shell_collision_was_disabled)
	{
		CPHElement* elem = static_cast<CPHElement*>(m_taget_element);

		if(elem && m_taget_object && !m_taget_object->ObjectGetDestroy() && m_taget_object->ObjectPPhysicsShell())
		{
			CPhysicsShell* shell = elem->PhysicsShell();
			if(shell)
			{
				shell->EnableCollision();
			}
		}
	}

	m_shell_collision_taken = false;
	m_shell_collision_was_disabled = false;
}

void CPHCapture::EnableShellSelfCollision()
{
	CPHElement* elem = static_cast<CPHElement*>(m_taget_element);
	if(!elem || !elem->get_body())
		return;

	CPhysicsShell* shell = elem->PhysicsShell();
	if(!shell)
		return;

	if(!m_self_collision_taken)
	{
		m_self_collision_was = shell->IsSelfCollision();

		if(!m_self_collision_was)
		{
			shell->SetSelfCollision(true);
		}

		m_self_collision_taken = true;

		Msg("~ [grab] self-collision on: shell=%p elements=%u animator=%d splitted=%d was=%d",
			shell, shell->get_ElementsNumber(), shell->PPhysicsShellAnimator() != nullptr,
			shell->SplitterHolder() != nullptr, m_self_collision_was ? 1 : 0);
	}

	g_CameraCaptureShellInfo.Elements = int(shell->get_ElementsNumber());
	g_CameraCaptureShellInfo.SelfCollisionContacts = int(shell->LastSelfCollisionContacts());
	g_CameraCaptureShellInfo.Animator = shell->PPhysicsShellAnimator() != nullptr;
}

void CPHCapture::RestoreShellSelfCollision()
{
	if(!m_self_collision_taken)
		return;

	if(!m_self_collision_was)
	{
		CPHElement* elem = static_cast<CPHElement*>(m_taget_element);

		if(elem && m_taget_object && !m_taget_object->ObjectGetDestroy() && m_taget_object->ObjectPPhysicsShell())
		{
			CPhysicsShell* shell = elem->PhysicsShell();
			if(shell)
			{
				shell->SetSelfCollision(false);
			}
		}
	}

	m_self_collision_taken = false;
	m_self_collision_was = false;

	g_CameraCaptureShellInfo.Elements = 0;
	g_CameraCaptureShellInfo.SelfCollisionContacts = 0;
	g_CameraCaptureShellInfo.Animator = false;
}

void CPHCapture::EnableShellExactIntegration()
{
	if(m_exact_integration_taken)
		return;

	CPHElement* elem = static_cast<CPHElement*>(m_taget_element);
	if(!elem || !elem->get_body())
		return;

	CPhysicsShell* shell = elem->PhysicsShell();
	if(!shell)
		return;

	m_exact_integration_was = shell->IsPrefereExactIntegration();

	if(!m_exact_integration_was)
	{
		shell->SetPrefereExactIntegration();
	}

	m_exact_integration_taken = true;
}

void CPHCapture::RestoreShellExactIntegration()
{
	if(!m_exact_integration_taken)
		return;

	if(!m_exact_integration_was)
	{
		CPHElement* elem = static_cast<CPHElement*>(m_taget_element);

		if(elem && m_taget_object && !m_taget_object->ObjectGetDestroy() && m_taget_object->ObjectPPhysicsShell())
		{
			CPhysicsShell* shell = elem->PhysicsShell();
			if(shell)
			{
				shell->UnSetPrefereExactIntegration();
			}
		}
	}

	m_exact_integration_taken = false;
	m_exact_integration_was = false;
}

void CPHCapture::DisableGravity()
{
	if(m_gravity_disabled)
		return;

	CPHElement* elem = static_cast<CPHElement*>(m_taget_element);
	if(!elem || !elem->get_body())
		return;

	m_saved_gravity_mode = dBodyGetGravityMode(elem->get_body());
	dBodySetGravityMode(elem->get_body(), 0);
	m_gravity_disabled = true;
}

void CPHCapture::RestoreGravity()
{
	if(!m_gravity_disabled)
		return;

	if(m_taget_object && !m_taget_object->ObjectGetDestroy() &&
		m_taget_object->ObjectPPhysicsShell() && m_taget_object->ObjectPPhysicsShell()->isActive())
	{
		CPHElement* elem = static_cast<CPHElement*>(m_taget_element);
		if(elem && elem->get_body())
			dBodySetGravityMode(elem->get_body(), m_saved_gravity_mode);
	}

	m_gravity_disabled = false;
}

void CPHCapture::SetRotation(Fmatrix form)
{
	if(e_state != cstCaptured || !m_body)
		return;

	Fmatrix bm;
	PHDynamicData::DMXPStoFMX(dBodyGetRotation(m_body), dBodyGetPosition(m_body), bm);

	Fmatrix new_bm;
	new_bm.mul(form, bm);

	dMatrix3 dBM;
	PHDynamicData::FMXtoDMX(new_bm, dBM);
	dBodySetRotation(m_body, dBM);
}

void CPHCapture::SetAMotorVel(Fvector dir)
{
	if(e_state != cstCaptured || !m_body)
		return;

	dir.clamp(Fvector().set(-10.f, -10.f, -10.f), Fvector().set(10.f, 10.f, 10.f));
	dBodySetAngularVel(m_body, VPUSH(dir));
}

void CPHCapture::ApplyForce(Fvector dir, float force)
{
	CPHElement* elem = static_cast<CPHElement*>(m_taget_element);
	if(!elem || !elem->get_body())
		return;

	if(m_taget_element)
		m_taget_element->applyForce(dir, force * ph_world->Gravity() * m_taget_element->PhysicsShell()->getMass());
}

void CPHCapture::ReleasedUpdate()
{
	if(b_disabled) return;
	if(!b_collide) 
	{
		e_state=cstFree;
		m_taget_element->Enable();
	}

	b_collide=false;
}

void CPHCapture::ReleaseInCallBack()
{
	b_collide = true;
}

void CPHCapture::object_contactCallbackFun(bool& do_colide, bool bo1, dContact& c, SGameMtl* /*material_1*/, SGameMtl* /*material_2*/)
{
	dxGeomUserData* l_pUD1 = nullptr;
	dxGeomUserData* l_pUD2 = nullptr;
	l_pUD1 = retrieveGeomUserData(c.geom.g1);
	l_pUD2 = retrieveGeomUserData(c.geom.g2);

	if (!l_pUD1 || !l_pUD2) return;

	IPhysicsShellHolder* capturer = (l_pUD1->ph_ref_object);
	if (capturer)
	{
		IPHCapture* icapture = capturer->PHCapture();
		CPHCapture* capture = static_cast<CPHCapture*>(icapture);
		if (capture)
		{
			if (capture->m_taget_element->PhysicsRefObject() == l_pUD2->ph_ref_object)
			{
				do_colide = false;
				capture->m_taget_element->Enable();
				if (capture->e_state == CPHCapture::cstReleased)
					capture->ReleaseInCallBack();
			}

		}
	}

	capturer = l_pUD2->ph_ref_object;
	if (capturer)
	{
		CPHCapture* capture = static_cast<CPHCapture*>(capturer->PHCapture());
		if (capture)
		{
			if (capture->m_taget_element->PhysicsRefObject() == l_pUD1->ph_ref_object)
			{
				do_colide = false;
				capture->m_taget_element->Enable();
				if (capture->e_state == CPHCapture::cstReleased)
					capture->ReleaseInCallBack();
			}

		}
	}
}

void CPHCapture::RemoveConnection(IPhysicsShellHolder* O)
{
	if (m_taget_object == O)
	{
		Deactivate();
	}
}

void CPHCapture::NetRelcase(CPhysicsShell* s)
{
	VERIFY(s);
	VERIFY(s->get_ElementByStoreOrder(0));
	VERIFY(s->get_ElementByStoreOrder(0)->PhysicsRefObject());
	RemoveConnection(s->get_ElementByStoreOrder(0)->PhysicsRefObject());
}