#include "stdafx.h"
#ifdef DEBUG
#include "ode_include.h"
#include "../xrEngine/StatGraph.h"
#include "PHDebug.h"
#endif
#include "alife_space.h"
#include "hit.h"
#include "PHDestroyable.h"
#include "car.h"
#include "Level.h"
#include "../xrCore/net_utils.h"
#include "../Include/xrRender/Kinematics.h"
#include "MathUtils.h"
#include "game_object_space.h"
#include "actor.h"
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Below this speed the car is treated as parked: an open door is held by the
// hinge motor instead of being driven by the moving-car door logic.
constexpr float CarDoorHoldSpeed = 0.5f;

bool CCar::DoorHit(float P,s16 element,ALife::EHitType hit_type)
{
	if(hit_type==ALife::eHitTypeStrike && P > 20.f)
	{
		xr_map<u16,SDoor>::iterator	   i=m_doors.begin(),e=m_doors.end();
		//for(;e!=i;++i)i->second.Open();
	}
	xr_map   <u16,SDoor>::iterator i=m_doors.find(element);
	if(i!=m_doors.end())
	{
		// A door without a physics joint cannot open or visually break.
		// Damaging it only flips the logical state and can reach a null
		// joint in Break(); keep such doors intact regardless of
		// [damage_items].
		if (i->second.joint != nullptr)
		{
			i->second.Hit(P);
		}
		return true;
	}
	else return false;
}
void CCar::SDoor::Init()
{
	update=false;
	float OpenTorqueFactor = pcar->m_doors_torque_factor;
	float HoldTorqueFactor = pcar->m_doors_hold_factor;
	float OpenVelFactor = pcar->m_doors_vel_factor;
	joint = pcar->bone_map.find(bone_id)->second.joint;
	if (joint == nullptr || joint->PSecond_element() == nullptr)
	{
		// A door bone without a usable hinge can still be damaged and broken;
		// keep it as a non-physical door instead of dereferencing null later.
		joint = nullptr;
		return;
	}
	R_ASSERT2(dJointGetType(joint->GetDJoint())==dJointTypeHinge,"Wrong door joint!!! Only simple joint valid for a door and only one axis can be active, check other axes are zerro limited !!!");
	joint->SetBackRef(&joint);
	dJointGetHingeAxis(joint->GetDJoint(), (float*)&default_hinge_axis);
	joint->GetJointSDfactors(default_joint_sf, default_joint_df);
	joint->GetAxisSDfactors(default_axis_sf, default_axis_df, 0);
	Fvector door_position,door_axis;
	dJointGetHingeAnchor (joint->GetDJoint(),(float*) &door_position);
	dJointGetHingeAxis (joint->GetDJoint(), (float*) &door_axis);
	door_position.sub(pcar->XFORM().c);
	IKinematics* pKinematics = smart_cast<IKinematics*>(pcar->Visual());
	if (pKinematics)
	{
		CInifile* ini = pKinematics->LL_UserData();
		if (ini)
		{
			LPCSTR bone_name = pKinematics->LL_BoneName_dbg(bone_id);
			if (ini->section_exist("car_sound") && ini->line_exist("car_sound", "snd_volume"))
			{
				string128 door_line_open, door_line_close;
				xr_sprintf(door_line_open, "%s_open", bone_name);
				xr_sprintf(door_line_close, "%s_close", bone_name);
				if (ini->line_exist("car_sound", door_line_open))
					snd_door_open.create(ini->r_string("car_sound", door_line_open), st_Effect, sg_SourceType);
				if (ini->line_exist("car_sound", door_line_close))
					snd_door_close.create(ini->r_string("car_sound", door_line_close), st_Effect, sg_SourceType);
			}
			if (ini->section_exist(bone_name))
			{
				if (ini->line_exist(bone_name, "use_distance"))
					dist_to_door_avlbl = ini->r_float(bone_name, "use_distance");
				OpenTorqueFactor = READ_IF_EXISTS(ini, r_float, bone_name, "open_torque_factor", OpenTorqueFactor);
				HoldTorqueFactor = READ_IF_EXISTS(ini, r_float, bone_name, "hold_torque_factor", HoldTorqueFactor);
				OpenVelFactor = READ_IF_EXISTS(ini, r_float, bone_name, "open_vel_factor", OpenVelFactor);

				if (ini->line_exist(bone_name, "non_enterable"))
					non_enterable = !!ini->r_bool(bone_name, "non_enterable");
				if (ini->line_exist(bone_name, "non_exitable"))
					non_exitable = !!ini->r_bool(bone_name, "non_exitable");
				if (ini->line_exist(bone_name, "spring_loaded"))
					spring_loaded_door = !!ini->r_bool(bone_name, "spring_loaded");
				if (ini->line_exist(bone_name, "broken_door_handle"))
					broken_door_handle = !!ini->r_bool(bone_name, "broken_door_handle");
				if (ini->line_exist(bone_name, "lamba_door"))
					lamba_door = !!ini->r_bool(bone_name, "lamba_door");

				if (ini->line_exist(bone_name, "no_deactivate"))
					no_deactivate = !!ini->r_bool(bone_name, "no_deactivate");
			}
		}
	}
	Fmatrix door_transform;
	joint->PSecond_element()->InterpolateGlobalTransform(&door_transform);
	// The element transform is world-space here; the door math expects a car-local form.
	Fmatrix inv_car_form = pcar->XFORM();
	inv_car_form.invert();
	closed_door_form_in_object.mul(inv_car_form, door_transform);
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
	Fvector jaxis,janchor;
	float lo_ext,hi_ext,ext;
	joint->GetAxisDirDynamic(0,jaxis);
	joint->GetAnchorDynamic(janchor);
	joint->PSecond_element()->get_Extensions(jaxis,janchor.dotproduct(jaxis),lo_ext,hi_ext);
	door_plane_ext.x=hi_ext-lo_ext;
	Fvector jaxis_in_door;
	Fmatrix inv_door_transform;
	inv_door_transform.set(door_transform);
	inv_door_transform.invert();
	inv_door_transform.transform_dir(jaxis_in_door,jaxis);

	float	door_dir_sign;
	if(jaxis_in_door.x>jaxis_in_door.y)
	{
		if(jaxis_in_door.x>jaxis_in_door.z)
		{
			joint->PSecond_element()->get_Extensions(door_transform.j,janchor.dotproduct(door_transform.j),lo_ext,hi_ext);
			door_plane_ext.y=hi_ext-lo_ext;
			door_dir_sign=hi_ext>-lo_ext ? 1.f : -1.f;
			door_plane_axes.x=0;
			door_plane_axes.y=1;
			joint->PSecond_element()->get_Extensions(door_transform.k,janchor.dotproduct(door_transform.k),lo_ext,hi_ext);
			ext=hi_ext-lo_ext;
			if(ext>door_plane_ext.y)
			{
				door_dir_sign=hi_ext>-lo_ext ? 1.f : -1.f;
				door_plane_ext.y=ext;
				door_plane_axes.y=2;
			}
		}
		else
		{
			joint->PSecond_element()->get_Extensions(door_transform.j,janchor.dotproduct(door_transform.j),lo_ext,hi_ext);
			door_plane_ext.y=hi_ext-lo_ext;
			door_dir_sign=hi_ext>-lo_ext ? 1.f : -1.f;
			door_plane_axes.x=2;
			door_plane_axes.y=1;
			joint->PSecond_element()->get_Extensions(door_transform.i,janchor.dotproduct(door_transform.i),lo_ext,hi_ext);
			ext=hi_ext-lo_ext;
			if(ext>door_plane_ext.y)
			{
				door_dir_sign=hi_ext>-lo_ext ? 1.f : -1.f;
				door_plane_ext.y=ext;
				door_plane_axes.y=0;
			}
		}
	}
	else
	{
		if(jaxis_in_door.y>jaxis_in_door.z)	
		{
			joint->PSecond_element()->get_Extensions(door_transform.i,janchor.dotproduct(door_transform.i),lo_ext,hi_ext);
			door_plane_ext.y=hi_ext-lo_ext;
			door_dir_sign=hi_ext>-lo_ext ? 1.f : -1.f;
			door_plane_axes.x=1;
			door_plane_axes.y=0;
			joint->PSecond_element()->get_Extensions(door_transform.k,janchor.dotproduct(door_transform.k),lo_ext,hi_ext);
			ext=hi_ext-lo_ext;
			if(ext>door_plane_ext.y)
			{
				door_dir_sign=hi_ext>-lo_ext ? 1.f : -1.f;
				door_plane_ext.y=ext;
				door_plane_axes.y=2;
			}
		}
		else
		{
			joint->PSecond_element()->get_Extensions(door_transform.j,janchor.dotproduct(door_transform.j),lo_ext,hi_ext);
			door_plane_ext.y=hi_ext-lo_ext;
			door_dir_sign=hi_ext>-lo_ext ? 1.f : -1.f;
			door_plane_axes.x=2;
			door_plane_axes.y=1;
			joint->PSecond_element()->get_Extensions(door_transform.i,janchor.dotproduct(door_transform.i),lo_ext,hi_ext);
			ext=hi_ext-lo_ext;
			if(ext>door_plane_ext.y)
			{
				door_dir_sign=hi_ext>-lo_ext ? 1.f : -1.f;
				door_plane_ext.y=ext;
				door_plane_axes.y=0;
			}
		}
	}

	switch(door_plane_axes.y) 
	{
		case 0:
			door_dir_in_door.set(door_dir_sign,0.f,0.f);
			break;
		case 1:
			door_dir_in_door.set(0.f,door_dir_sign,0.f);
			break;
		case 2:
			door_dir_in_door.set(0.f,0.f,door_dir_sign);
			break;
		default: NODEFAULT;
	}
///////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
	///////////////////////////define positive open///////////////////////////////////
   	Fvector door_dir,door_test;
	door_transform.transform_dir(door_dir,door_dir_in_door);
	//cr_dr_pos.crossproduct(door_dir,door_position);
	door_test.crossproduct(door_dir,door_axis);
	safe_normalize(door_test);
	joint->PFirst_element()->get_Extensions(door_test,door_transform.c.dotproduct(door_test),lo_ext,hi_ext);
	if(hi_ext>-lo_ext)	pos_open=-1.f;
	 else				pos_open=1.f;
	//pos_open=-cr_dr_pos.dotproduct(door_axis);
	//pos_open=door_position.dotproduct(pcar->m_root_transform.i)*door_axis.dotproduct(pcar->m_root_transform.j);
	if (pKinematics)
	{
		CInifile* ini = pKinematics->LL_UserData();
		if (ini)
		{
			LPCSTR bone_name = pKinematics->LL_BoneName_dbg(bone_id);
			if (ini->section_exist(bone_name))
			{
				if (ini->line_exist(bone_name, "pos_open_invert"))
				{
					bool pos_invert = !!ini->r_bool(bone_name, "pos_open_invert");
					if(pos_invert)
						pos_open = -pos_open;
				}

			}
		}
	}

	if(pos_open>0.f)
	{
		pos_open=1.f;
		joint->GetLimits(closed_angle,opened_angle,0);
		//	closed_angle+=2.f*M_PI/180.f;
		// Keep the same small margin as the opposite hinge direction; the old
		// 25% reduction made positive-side doors report "opened" long before
		// the actual limit and they visibly stopped short.
		opened_angle-=2.f*M_PI/180.f;
	}
	else
	{
		pos_open=-1.f;
		joint->GetLimits(opened_angle,closed_angle,0);
		opened_angle+=2.f*M_PI/180.f;
		closed_angle-=2.f*M_PI/180.f;
	}
	joint->GetLimits(default_opened_angle,default_closed_angle,0);
	joint->PSecond_element()->SetAirResistance(0, 0);
	CInifile* ini = pKinematics->LL_UserData();
	LPCSTR b_name = pKinematics->LL_BoneName_dbg(bone_id);
	if (ini->section_exist(b_name))
	{
		if (ini->line_exist(b_name, "linear_factor") && ini->line_exist(b_name, "angular_factor"))
		{
			default_lin_resist = default_k_l * ini->r_float(b_name, "linear_factor");
			default_ang_resist = default_k_w * ini->r_float(b_name, "angular_factor");
			joint->PSecond_element()->SetAirResistance(default_lin_resist, default_ang_resist);
		}
	}
	saved_mass = joint->PSecond_element()->getMass();
	Fvector shoulder;

	shoulder.sub(door_transform.c,joint->PSecond_element()->mass_Center());
	torque=shoulder.magnitude()*joint->PSecond_element()->getMass()*OpenTorqueFactor*10.f;
	// A zero-velocity hinge motor with this force limit acts as static
	// friction for an open door. Air resistance (linear_factor/angular_factor)
	// only damps movement and cannot hold a door on a slope.
	clamp(HoldTorqueFactor, 0.f, 10.f);
	hold_torque=torque*HoldTorqueFactor;
	a_vel=M_PI*OpenVelFactor;
	state=opened;
//	Close();
}
void CCar::SDoor::Open()
{

	if(!joint)
	{
		state=opened;

		return;
	}

	switch(state) 
	{
	case closed:
			ClosedToOpening();
			PlaceInUpdate();
	case closing:
			state=opening;
			ApplyTorque(torque, a_vel);
	case opened:
	case opening: break;
	case broken : break;
	default: NODEFAULT;
	}

}

bool CCar::SDoor::IsFullyOpen()
{
	if (state != opened)
		return false;

	if (!joint || !joint->bActive)
		return false;

	const float Current = pos_open * GetAngle();
	const float Limit = pos_open * opened_angle;
	return Current >= Limit - 0.01f;
}

void CCar::SDoor::Reopen()
{
	if (!joint)
	{
		state = opened;
		return;
	}

	if (state == broken)
		return;

	// A door can report `opened` while it has physically settled to a slightly
	// open position. Restart the opening from that position instead of waiting
	// for the old state machine.
	if (state == closed)
	{
		ClosedToOpening();
	}
	else
	{
		real_open_time = Device.dwTimeGlobal;
		real_close_time = 0;
	}

	if (!joint->bActive)
	{
		pcar->m_pPhysicsShell->Enable();

		Fmatrix door_form = joint->PSecond_element()->XFORM();
		joint->PSecond_element()->mXFORM.set(door_form);

		Fmatrix root_form;
		pcar->m_pPhysicsShell->GetGlobalTransformDynamic(&root_form);
		joint->PSecond_element()->Activate(root_form, true);
		joint->Activate();
	}

	state = opening;
	PlaceInUpdate();
	ApplyTorque(torque, a_vel);
}

void CCar::SDoor::Close()
{
	if(!joint)
	{
		state=closed;
		return;
	}

	switch(state)
	{
	case opened:
		PlaceInUpdate();
	case opening:
		state=closing;
		ApplyTorque(torque, -a_vel);
		PlaceInUpdate();
	case closed:
	case closing:
		break;
	default: NODEFAULT;
	}

}

void CCar::SDoor::PlaceInUpdate()
{
	if(update) return;
	pcar->m_doors_update.push_back(this);
	update=true;
}

void CCar::SDoor::RemoveFromUpdate()
{
	update=false;
}
void CCar::SDoor::Update()
{
	if (joint && joint->bActive)
	{
		float door_angle = GetAngle();
		switch (state)
		{
			case closing:
			{
				if (pos_open * closed_angle > pos_open * door_angle)
					ClosingToClosed();
				break;
			}
			case opening:
			{
				if (pos_open * opened_angle < pos_open * door_angle)
				{
					if (!spring_loaded_door)
						NeutralTorque(hold_torque);
					open_time = Device.dwTimeGlobal;
					state = opened;
				}
				break;
			}
			case opened:
			{
				// non_enterable/non_exitable only restrict entry and exit; they
				// must not affect the door physics or prevent it from closing.
				if (spring_loaded_door)
				{
					if (Device.dwTimeGlobal - open_time > 1000)
					{
						// The spring keeps pushing the door towards its limit.
						// Once it is there, replace the drive with a zero-velocity
						// hold. Keeping the door in the update list lets it recover
						// if a slope or a collision pushes it off the limit.
						if (IsFullyOpen())
							NeutralTorque(hold_torque);
						else
							ApplyTorque(torque / 5.f, a_vel);
					}
					break;
				}

				// A parked car must not lose the door hold. The moving-car
				// branch used to replace it with a near-zero motor force while
				// the car slowly rolled on a slope, and the door then closed
				// under gravity.
				if (!pcar->m_pPhysicsShell->isEnabled() || pcar->GetVelocityM() <= CarDoorHoldSpeed)
				{
					NeutralTorque(hold_torque);
					break;
				}

				Fvector car_ang_vel;
				pcar->m_pPhysicsShell->get_AngularVel(car_ang_vel);
				const float odoor_trq = car_ang_vel.magnitude();
				const float odoor_vel = odoor_trq - pcar->GetVelocityM() * 0.8f;
				ApplyTorque(odoor_trq, odoor_vel);
				break;
			}
		}
		if (Device.dwTimeGlobal - real_open_time > 1000 && _abs(GetAngleRate()) > 2.0f)
		{
			if (pos_open * closed_angle > pos_open * door_angle)
				ClosingToClosed();
		}
	}

	if (real_close_time && Device.dwTimeGlobal - real_close_time > 1000)
		Deactivate();

	if (snd_door_open._p)
	{
		if (snd_door_open.is_playing())
			snd_door_open.set_position(GetWorldPosition());
	}
	if (snd_door_close._p)
	{
		if (snd_door_close.is_playing())
			snd_door_close.set_position(GetWorldPosition());
	}
}

void CCar::SDoor::Use()
{
	switch(state) 
	{
		case opened:
		case opening: 
			Close();
			break;
		case closed: 
		case closing:
			if(broken_door_handle && !pcar->Owner())break;
			Open();
			break;
		default:	return;
	}
}

void CCar::SDoor::Switch()
{
	switch(state) {
case opened:
	Close();
	break;
case closed: 
	Open();
	break;
default:	return;
	}
}

void CCar::SDoor::ApplyTorque(float atorque,float aa_vel)
{
	if(!joint||!joint->bActive)return;
	joint->PSecond_element()->Enable();
	dJointSetHingeParam(joint->GetDJoint(),dParamFMax,atorque);
	dJointSetHingeParam(joint->GetDJoint(),dParamVel,aa_vel*pos_open);
}
void CCar::SDoor::NeutralTorque(float atorque)
{
	if (!joint || !joint->bActive)return;
	joint->PSecond_element()->Enable();
	dJointSetHingeParam(joint->GetDJoint(),dParamFMax,atorque);
	dJointSetHingeParam(joint->GetDJoint(),dParamVel,0);
}

void CCar::SDoor::ClosedToOpening(bool obuser)
{
	if (state != closed) return;

	// Some doors have no physics joint (for example, a door bone without a
	// hinge in the model). They can still be damaged and broken; only the
	// physics part of the opening must be skipped.
	if (joint == nullptr || joint->PSecond_element() == nullptr)
		return;

	IKinematics* pKinematics = PKinematics(pcar->Visual());
	Fmatrix door_form; pKinematics->Bone_GetAnimPos(door_form, u16(bone_id), u8(-1), false);
	door_form.mulA_43(pcar->XFORM());
	if (snd_door_open._p && obuser)
	{
		snd_door_open.play_at_pos(pcar, GetWorldPosition(), sm_NoCarFactor | sm_NoOCC);
		snd_door_open.set_frequency(Random.randF(0.9f, 1.1f));
	}
	real_open_time = Device.dwTimeGlobal;
	real_close_time = 0;
	if (joint && !joint->bActive)
	{
		pcar->m_pPhysicsShell->Enable();
		pKinematics->Bone_GetAnimPos(door_form, u16(bone_id), u8(-1), false);
		joint->PSecond_element()->mXFORM.set(door_form);
		Fmatrix root_form;
		pcar->m_pPhysicsShell->GetGlobalTransformDynamic(&root_form);
		joint->PSecond_element()->Activate(root_form, true);
		joint->Activate();
	}
	//joint->SetLimits(default_opened_angle,default_closed_angle,0);
	joint->PSecond_element()->SetAirResistance(default_lin_resist, default_ang_resist);
	//joint->PSecond_element()->set_ApplyByGravity(TRUE);
}

void CCar::SDoor::ClosingToClosed(bool obuser)
{
	if (state==closed) return;
	state =closed;
	if(!joint) return;
	if (!joint->bActive)return;
	real_close_time = Device.dwTimeGlobal;
	ApplyTorque(torque, -a_vel);

	if (snd_door_close._p && obuser)
	{
		snd_door_close.play_at_pos(pcar, GetWorldPosition(), sm_NoCarFactor | sm_NoOCC);
		snd_door_close.set_frequency(Random.randF(0.9f, 1.1f));
	}
	//joint->SetLimits(0,0,0);
	joint->PSecond_element()->SetAirResistance(0.0f, 0.0f);
	//joint->PSecond_element()->set_ApplyByGravity(FALSE);
	//RemoveFromUpdate();
}

void CCar::SDoor::Deactivate()
{
	if (state != closed) return;
	if (!joint) return;
	if (!joint->bActive)return;
	if(no_deactivate) return;
	joint->PSecond_element()->Deactivate();
	joint->Deactivate();
	RemoveFromUpdate();
	real_close_time = 0;
}


float CCar::SDoor::GetAngleRate()
{
	if (!joint || !joint->bActive) return 0.f;
	return dJointGetHingeAngleRate(joint->GetDJoint());
}
float CCar::SDoor::GetAngle()
{
	if(!joint||!joint->bActive) return 0.f;
	return dJointGetHingeAngle(joint->GetDJoint());
}
Fvector CCar::SDoor::GetWorldPosition()
{
	Fmatrix BoneMatrix;
	Fmatrix BoneMatrixRes;
	Fobb BoneOBB = PKinematics(pcar->Visual())->LL_GetBox(bone_id);
	BoneOBB.xform_get(BoneMatrix);
	BoneMatrixRes.mul(PKinematics(pcar->Visual())->LL_GetTransform(bone_id), BoneMatrix);
	BoneMatrix.mul(pcar->XFORM(), BoneMatrixRes);
	return BoneMatrix.c;
}

bool CCar::SDoor::IsDistOfSight(const Fvector& pos)
{
	if (GetWorldPosition().distance_to(pos) <= dist_to_door_avlbl)
		return true;

	return false;
}

bool CCar::SDoor::IsLineOfSight()
{
	Fvector diff;
	diff.sub(Fvector(GetWorldPosition()), Fvector(Device.vCameraPosition));
	if (!safe_normalize(diff)) return false;
	float ang_diff_H				= angle_difference(Device.vCameraDirection.getH(), diff.getH());
	float ang_diff_P				= angle_difference(Device.vCameraDirection.getP(), diff.getP());
	if(ang_diff_H <= 1.2f && ang_diff_P <= 1.2f)
		return true;

	return false;
}

bool CCar::SDoor::IsFront(const Fvector& pos,const Fvector& dir)
{
	if(pcar->bones_bind_forms.empty())
		return false;

	Fvector tdir;tdir.set(pcar->XFORM().i);if(tdir.dotproduct(dir)<0.f)tdir.invert();
	Fmatrix pf;
	pf.mul(pcar->XFORM(),pcar->bones_bind_forms[bone_id]);
	Fvector dif,dif1;
	dif.sub(pf.c,pos);
	pcar->Center(dif1);
	Fvector c_to_d;c_to_d.sub(pf.c,dif1);

	dif1.sub(pos);
	dif.normalize_safe();
	return (dif1.dotproduct(tdir)>dif.dotproduct(tdir) && abs(c_to_d.dotproduct(tdir)) < dif1.dotproduct(tdir) );
}
bool CCar::SDoor::IsInArea(const Fvector& pos,const Fvector& dir)
{
	if(pcar->bones_bind_forms.empty())
		return false;
	if(!joint)
	{
		if(!IsFront(pos,dir))return false;

		IKinematics* K=PKinematics(pcar->Visual());
		//CBoneInstance bi=K->LL_GetBoneInstance(bone_id);
		//CBoneData& bd=K->LL_GetData(bone_id);
		//		Fobb bb=bd.obb;
		Fvector tdir;tdir.set(pcar->XFORM().i);if(tdir.dotproduct(dir)<0.f)tdir.invert();
		Fmatrix pf;
		pf.mul(pcar->XFORM(),pcar->bones_bind_forms[bone_id]);
		Fvector dif,dif1;
		dif.sub(pf.c,pos);
		pcar->Center(dif1);
		Fvector c_to_d;c_to_d.sub(pf.c,dif1);
		dif1.sub(pos);
		return 2.f*abs(c_to_d.dotproduct(pcar->XFORM().i))>abs(dif1.dotproduct(pcar->XFORM().i));
	}
	Fmatrix closed_door_form,door_form;
	Fvector closed_door_dir,door_dir,anchor_to_pos,door_axis;
	if (joint && joint->bActive)
	{
		joint->GetAxisDirDynamic(0, door_axis);
		joint->PSecond_element()->InterpolateGlobalTransform(&door_form);
	}

	closed_door_form.mul(pcar->XFORM(),closed_door_form_in_object);
	closed_door_form.transform_dir(closed_door_dir,door_dir_in_door);
	//closed_door_form_in_object.transform_dir(closed_door_dir,door_dir_in_door);
	//pcar->XFORM().transform_dir(closed_door_dir);


	door_form.transform_dir(door_dir,door_dir_in_door);
	safe_normalize(door_dir);
	safe_normalize(closed_door_dir);
	float cprg=door_dir.dotproduct(door_form.c);
	float loe,hie;
	if (joint && joint->bActive)
		joint->PSecond_element()->get_Extensions(door_dir,cprg,loe,hie);
	float signum=(hie>-loe) ? 1.f : -1.f;

	Fvector closed_door_norm,door_norm;
	closed_door_norm.crossproduct(door_axis,closed_door_dir);
	door_norm.crossproduct(door_axis,door_dir);
	anchor_to_pos.sub(pos,closed_door_form.c);
	float a,b,c;
	a=anchor_to_pos.dotproduct(closed_door_dir)*signum;
	b=anchor_to_pos.dotproduct(door_dir)*signum;
	c= anchor_to_pos.dotproduct(closed_door_norm)*anchor_to_pos.dotproduct(door_norm);
	if(
		a	<	(signum>0.f ? hie : -loe)	&&	a	>	0.f	&&
		b	<	(signum>0.f ? hie : -loe)	&&	b	>	0.f	&&
		anchor_to_pos.dotproduct(closed_door_norm)*anchor_to_pos.dotproduct(door_norm)<0.f
		)return true;
	else return false;
}

bool CCar::SDoor::CanExit(const Fvector& pos,const Fvector& dir)
{
	if(state==closed&&joint||(pcar->GetTrunkBone()==bone_id)||non_exitable)return false;
	// An open door still limits the exit to the doorway area. TestPass() can
	// miss a transparent upper insert because it lies outside the door element
	// extents, so also test the cached visual bounds of the closed door.
	if(state == opened || state == opening || state == broken)
		return TestPass(pos,dir) || IsExitThroughDoor(pos,dir);
	return TestPass(pos,dir);
}

void CCar::SDoor::BuildExitArea()
{
	ExitArea.invalidate();
	ExitAreaValid = false;

	IKinematics* K = PKinematics(pcar->Visual());
	if (!K || bone_id == BI_NONE || bone_id >= K->LL_BoneCount())
		return;

	// Cache the visual bounds in the door's bind space, not in the current
	// model pose. A car loaded from a save may already have this door open, and
	// the doorway must still be where the closed door was.
	Fmatrix DoorCurrent = K->LL_GetTransform(bone_id);
	Fmatrix DoorCurrentInv;
	DoorCurrentInv.invert(DoorCurrent);

	u32 VertexCount = 0;
	for (auto& Bone : *K->LL_Bones())
	{
		const u16 TestBone = Bone.second;
		if (pcar->GetDoorBone(TestBone) != bone_id)
			continue;

		xr_vector<Fvector> Vertices;
		K->EnumBoneVertices(Vertices, TestBone);
		VertexCount += u32(Vertices.size());
		for (const Fvector& Vertex : Vertices)
		{
			Fvector Local;
			DoorCurrentInv.transform_tiny(Local, Vertex);
			ExitArea.modify(Local);
		}
	}

	ExitAreaValid = VertexCount > 0;
}

bool CCar::SDoor::IsExitThroughDoor(const Fvector& pos, const Fvector& dir)
{
	if (pcar->bones_bind_forms.empty() || bone_id == BI_NONE || bone_id >= pcar->bones_bind_forms.size())
		return false;

	IKinematics* K = PKinematics(pcar->Visual());
	if (!K || bone_id >= K->LL_BoneCount())
		return false;

	Fvector RayDir = dir;
	if (!safe_normalize(RayDir))
		return false;

	// The visual door bounds include the transparent upper insert, which may
	// not be part of the physics bone OBB. BuildExitArea() caches the bounds in
	// the door's bind space, so the test stays inside the place where the door
	// was even when the door itself is already open.
	if (ExitAreaValid)
	{
		Fvector Center, Half;
		ExitArea.get_CD(Center, Half);

		Fobb LocalOBB;
		LocalOBB.m_rotate.identity();
		LocalOBB.m_translate = Center;
		LocalOBB.m_halfsize = Half;

		Fmatrix DoorForm;
		DoorForm.mul(pcar->XFORM(), pcar->bones_bind_forms[bone_id]);

		Fobb WorldOBB;
		WorldOBB.transform(LocalOBB, DoorForm);

		float Distance = dist_to_door_avlbl;
		if (WorldOBB.intersect(pos, RayDir, Distance))
			return true;
	}

	// Fallback for models/renderers without bone vertices: test the closed bind
	// OBB of the door and its child bones.
	for (auto& Bone : *K->LL_Bones())
	{
		const u16 TestBone = Bone.second;
		if (TestBone >= pcar->bones_bind_forms.size())
			continue;
		if (pcar->GetDoorBone(TestBone) != bone_id)
			continue;

		Fmatrix BoneForm;
		BoneForm.mul(pcar->XFORM(), pcar->bones_bind_forms[TestBone]);

		Fobb BoneOBB;
		BoneOBB.transform(K->LL_GetData(TestBone).obb, BoneForm);

		float Distance = dist_to_door_avlbl;
		if (BoneOBB.intersect(pos, RayDir, Distance))
			return true;
	}

	return false;
}

void CCar::SDoor::GetExitPosition(Fvector& pos)
{
	if(pcar->bones_bind_forms.empty())
		return;
//	if(!joint)
	{
		IKinematics* K=PKinematics(pcar->Visual());
		//CBoneInstance bi=K->LL_GetBoneInstance(bone_id);
		CBoneData& bd=K->LL_GetData(bone_id);
		Fobb bb;//=bd.obb;
		
		Fmatrix pf;
		pf.mul(pcar->XFORM(),pcar->bones_bind_forms[bone_id]);
		bb.transform(bd.obb,pf);
		bb.xform_get(pf);
		pos.set(pf.c);
		Fvector add,add1;
		MAX_OF(abs(pf.i.y),add.set(pf.i);add.mul(bb.m_halfsize.x*fsignum(pf.i.y)),abs(pf.j.y),add.set(pf.j);add.mul(bb.m_halfsize.y*fsignum(pf.j.y)),abs(pf.k.y),add.set(pf.k);add.mul(bb.m_halfsize.z*fsignum(pf.k.y)));
		pos.sub(add);

		MIN_OF(bb.m_halfsize.x,add1.set(pf.i);add1.mul(bb.m_halfsize.x),
			   bb.m_halfsize.y,add1.set(pf.j);add1.mul(bb.m_halfsize.y),
			  bb.m_halfsize.z,add1.set(pf.k);add1.mul(bb.m_halfsize.z))
		Fvector dir_from_car;dir_from_car.sub(pf.c,pcar->Position());
		dir_from_car.y=0.f;
		if(add1.dotproduct(dir_from_car)<0.f)add1.invert();	
		add1.mul(3.f);

		// A smaller clearance than before: enough for the actor capsule, but not
		// so far that the player is moved away from the door unnecessarily.
		Fvector exit_dir = add1;
		const bool has_exit_dir = safe_normalize(exit_dir);
		if (has_exit_dir)
			add1.mad(exit_dir, 0.30f);

		pos.add(add1);

		bool exit_blocked = false;
		if (has_exit_dir)
		{
			Fvector side;
			side.crossproduct(exit_dir, Fvector().set(0.f, 1.f, 0.f));
			safe_normalize(side);

			const float check_distance =
				Fvector(pos.x - pf.c.x, 0.f, pos.z - pf.c.z).magnitude() + 0.45f;
			const float side_offsets[] = { -0.30f, 0.f, 0.30f };
			const float height_offsets[] = { 0.60f, 1.25f };
			for (float side_offset : side_offsets)
			{
				for (float height_offset : height_offsets)
				{
					Fvector from = pf.c;
					from.y = pos.y + height_offset;
					from.mad(side, side_offset);
					collide::rq_result result;
					if (Level().ObjectSpace.RayPick(from, exit_dir, check_distance, collide::rqtStatic, result, pcar))
					{
						exit_blocked = true;
						break;
					}
				}
				if (exit_blocked)
					break;
			}
		}

		// A car lying on its side or upside down cannot use its doors even if
		// the horizontal exit ray happens to be clear. Force the world-space
		// point above the car instead of transforming the seat through the
		// rolled car transform.
		const bool CarRolled = pcar->XFORM().j.y < 0.5f;
		if (exit_blocked || CarRolled)
		{
			Fbox LocalBox = pcar->BoundingBox();
			if (LocalBox.is_valid())
			{
				Fbox WorldBox;
				WorldBox.invalidate();

				const Fmatrix& CarXForm = pcar->XFORM();
				for (int ix = 0; ix < 2; ++ix)
				{
					for (int iy = 0; iy < 2; ++iy)
					{
						for (int iz = 0; iz < 2; ++iz)
						{
							Fvector Corner;
							Corner.set(ix ? LocalBox.max.x : LocalBox.min.x,
								iy ? LocalBox.max.y : LocalBox.min.y,
								iz ? LocalBox.max.z : LocalBox.min.z);

							Fvector WorldCorner;
							CarXForm.transform_tiny(WorldCorner, Corner);
							WorldBox.modify(WorldCorner);
						}
					}
				}

				// Place the actor above the blocked door, not above the car
				// center: use the door's world X/Z and the current car pose top.
				pos.set(
					pf.c.x,
					WorldBox.max.y + 1.0f,
					pf.c.z);
			}
			else
			{
				pos.set(pf.c.x, pcar->Position().y + pcar->Radius() + 1.0f, pf.c.z);
			}
		}
		return;
	}
	//float lo_ext,hi_ext;
	//Fvector door_axis,door_pos,door_dir,closed_door_dir,add;
	//joint->GetAxisDirDynamic(0,door_axis);
	//joint->GetAnchorDynamic(door_pos);

	//Fmatrix door_form,root_form;
	//root_form.mul(pcar->m_root_transform,pcar->XFORM());
	//joint->PSecond_element()->InterpolateGlobalTransform(&door_form);
	//door_form.transform_dir(door_dir,door_dir_in_door);

	//
	//closed_door_form_in_object.transform_dir(closed_door_dir,door_dir_in_door);
	//pcar->XFORM().transform_dir(closed_door_dir);


	//pos.set(door_pos);
	//door_axis.normalize();
	//float center_prg=door_axis.dotproduct(door_pos);
	//joint->PSecond_element()->get_Extensions(door_axis,center_prg,lo_ext,hi_ext);
	//add.set(door_axis);
	//if(door_axis.dotproduct(root_form.j)>0.f) add.mul(lo_ext);
	//else									  add.mul(hi_ext);
	//pos.add(add);

	//door_dir.normalize();
	//center_prg=door_pos.dotproduct(door_dir);
	//joint->PSecond_element()->get_Extensions(door_dir,center_prg,lo_ext,hi_ext);
	//closed_door_dir.normalize();
	//add.add(closed_door_dir,door_dir);
	//add.normalize();
	//if(hi_ext>-lo_ext)add.mul(hi_ext);
	//else			  add.mul(lo_ext);
	//pos.add(add);
}



bool CCar::SDoor::TestPass(const Fvector& pos,const Fvector& dir)
{
	if(pcar->bones_bind_forms.empty())
		return false;
	if(!joint)
	{
		if (IKinematics* pKinematics = smart_cast<IKinematics*>(pcar->Visual()))
		{
			if(pcar->bones_bind_forms.size() > pKinematics->LL_BoneCount())
				return false;

			if(bone_id == BI_NONE || bone_id >= pKinematics->LL_BoneCount())
				return false;
		}
		Fmatrix pf, global, local;
		global = pcar->XFORM();
		local = pcar->bones_bind_forms[bone_id];
		pf.mul(global,local);
		Fvector dif;
		dif.sub(pf.c,pos);
		return (dif.dotproduct(dir)>0.f);
	}
	float lo_ext,hi_ext;
	Fvector door_axis,door_pos,door_dir,closed_door_dir;

	if (joint && joint->bActive)
	{
		joint->GetAxisDirDynamic(0, door_axis);
		joint->GetAnchorDynamic(door_pos);
	}

	Fmatrix door_form,root_form;
	root_form.mul(pcar->m_root_transform,pcar->XFORM());
	if (joint && joint->bActive)
		joint->PSecond_element()->InterpolateGlobalTransform(&door_form);
	door_form.transform_dir(door_dir,door_dir_in_door);
//	closed_door_form.mul(closed_door_form_in_object,pcar->XFORM());
	closed_door_form_in_object.transform_dir(closed_door_dir,door_dir_in_door);
	pcar->XFORM().transform_dir(closed_door_dir);
	safe_normalize(door_axis);

	safe_normalize(door_dir);
	safe_normalize(closed_door_dir);

	Fvector closed_door_norm;

	closed_door_norm.crossproduct(door_axis,closed_door_dir);

	Fvector point_on_door,add,sub;
	add.set(dir);
	sub.sub(pos,door_pos);
	add.mul(-sub.dotproduct(closed_door_norm)/(dir.dotproduct(closed_door_norm)));
	
	if(add.dotproduct(dir)<0.f) return false;

	point_on_door.add(pos,add);

	float center_prg=door_pos.dotproduct(door_dir);
	if (joint && joint->bActive)
		joint->PSecond_element()->get_Extensions(door_dir,center_prg,lo_ext,hi_ext);

	float point_prg=point_on_door.dotproduct(closed_door_dir);
	center_prg=door_pos.dotproduct(closed_door_dir);
	if(!(center_prg+hi_ext>point_prg)||!(center_prg+lo_ext<point_prg)) return false;

	center_prg=door_axis.dotproduct(door_pos);
	if (joint && joint->bActive)
		joint->PSecond_element()->get_Extensions(door_axis,center_prg,lo_ext,hi_ext);

	if(!lamba_door)
	{
		point_prg=point_on_door.dotproduct(door_axis);
		if(!(center_prg+hi_ext>point_prg)||!(center_prg+lo_ext<point_prg)) return false;
	}

	return true;
}

bool CCar::SDoor::CanEnter(const Fvector&,const Fvector& dir,const Fvector& foot_pos)
{
	// Deviation from Cars-Ext: IsInArea() rejects the camera positions produced by the
	// IXRAY player controller, while TestPass() already validates the door approach.
	return (state == opening || state == closing || state==opened || state == broken || !joint) && TestPass(foot_pos,dir) && (pcar->GetTrunkBone()!=bone_id) && !non_enterable;//
}

void CCar::SDoor::SaveNetState(NET_Packet& P)
{
	CSE_ALifeCar::SDoorState ds;
	ds.health=Health();
	ds.open_state=u8(state);
	ds.write(P);
}

void CCar::SDoor::RestoreNetState(const CSE_ALifeCar::SDoorState& a_state)
{
	// Doors without a physics joint are invulnerable and always closed;
	// ignore a broken/opened state that may have been written by an older
	// build.
	if (joint == nullptr)
	{
		state = closed;
		return;
	}

	eState lstate=eState(a_state.open_state);
	if (lstate == closed)
	{
		ClosingToClosed(false);
		Deactivate();
	}
	if (lstate == opening || lstate == opened || lstate == closing)
	{
		// These runtime timers are not saved. Restart them after load so a
		// spring-loaded door receives the full opening torque first and does
		// not immediately switch to the weak hold with a stale timestamp.
		open_time = Device.dwTimeGlobal;
		real_open_time = Device.dwTimeGlobal;
		real_close_time = 0;
		PlaceInUpdate();
		//joint->SetLimits(default_opened_angle,default_closed_angle,0);
		if (joint && joint->bActive)
		{
			joint->PSecond_element()->SetAirResistance(default_lin_resist, default_ang_resist);
			joint->PSecond_element()->set_ApplyByGravity(TRUE);
		}
		if(lstate == closing)
			ApplyTorque(torque, -a_vel);
		if(lstate == opening || lstate == opened)
			ApplyTorque(torque, a_vel);
	}
	state=lstate;
	SetHealth(a_state.health);
	RestoreEffect();
}

void CCar::SDoor::SetDefaultNetState()
{
	ClosingToClosed(false);
	Deactivate();
}

void CCar::SDoor::Break()
{
	switch(state) {
	case closed:
		ClosedToOpening(false);
		break;
	case opened:
	case closing:
		RemoveFromUpdate();
	case opening:
		ApplyTorque(torque/10.f,0.f);
	}
	if (joint && joint->bActive)
	{
		dVector3 v;float sf,df;
		dJointID dj=joint->GetDJoint();
		dJointGetHingeAxis(dj,v);
		v[0]+=0.1f;v[1]+=0.1f;v[2]+=0.1f;
		dNormalize3(v);
		dJointSetHingeAxis(dj,v[0],v[1],v[2]);
		joint->GetJointSDfactors(sf,df);
		sf/=30.f;df*=8.f;
		joint->SetJointSDfactors(sf,df);
		joint->GetAxisSDfactors(sf,df,0);
		sf/=20.f;df*=8.f;
		joint->SetAxisSDfactors(sf,df,0);
		float lo,hi;
		joint->GetLimits(lo,hi,0);
		if(pos_open>0.f)
			joint->SetLimits(lo+M_PI/4.f,hi,0);
		else
			joint->SetLimits(lo,hi-M_PI/4.f,0);
	}
	state=broken;
}

void CCar::SDoor::ApplyDamage(u16 level)
{
	// Non-physical doors are not damageable; this also covers a broken state
	// restored from an old save.
	if (joint == nullptr)
	{
		return;
	}

	inherited::ApplyDamage(level);
	switch(level)
	{
		case 1: Break();
	}
}

void CCar::SDoor::Repair()
{
	const bool was_broken = (state == broken);
	inherited::Repair();

	if (!joint)
	{
		state = closed;
		return;
	}

	if (was_broken)
	{
		dJointID dj = joint->GetDJoint();
		dJointSetHingeAxis(dj, default_hinge_axis.x, default_hinge_axis.y, default_hinge_axis.z);
		joint->SetJointSDfactors(default_joint_sf, default_joint_df);
		joint->SetAxisSDfactors(default_axis_sf, default_axis_df, 0);
		joint->SetLimits(default_opened_angle, default_closed_angle, 0);
		ClosingToClosed(false);
		Deactivate();
	}
}
// UNUSED: legacy doorway helper
#if 0
CCar::SDoor::SDoorway::SDoorway()
{
	door=NULL;
	door_plane_ext.set(0.f,0.f);
	door_plane_axes.set(0,0);
}
void CCar::SDoor::SDoorway::Init(SDoor* adoor)
{
	door=adoor;	
	Fmatrix door_transform;
	if (!door->joint || !door->joint->bActive)return;
	door->joint->PSecond_element()->InterpolateGlobalTransform(&door_transform);
	Fmatrix inv_car_form = door->pcar->XFORM();
	inv_car_form.invert();
	door->closed_door_form_in_object.mul(inv_car_form, door_transform);
	Fvector jaxis,janchor;
	door->joint->GetAxisDirDynamic(0,jaxis);
	door->joint->GetAnchorDynamic(janchor);
	Fmatrix inv_door_transform;
	inv_door_transform.set(door_transform);
	inv_door_transform.invert();
	Fvector door_axis_in_door;
	inv_door_transform.transform_dir(door_axis_in_door,jaxis);
	float lo_ext,hi_ext,ext;

	if(_abs(door_axis_in_door.x)>_abs(door_axis_in_door.y))
	{
		if(_abs(door_axis_in_door.x)>_abs(door_axis_in_door.z))
		{
			//door axis aligned along x
			door_plane_axes.y=0;		   //door axis is x (door_plane_axes.y stores door axis direction (i,j,k)=(0,1,2)
			door->joint->PSecond_element()->get_Extensions(door_transform.i,janchor.dotproduct(door_transform.i),lo_ext,hi_ext);
			door->door_plane_ext.y=hi_ext-lo_ext; //door extension along door axis
	
			door->joint->PSecond_element()->get_Extensions(door_transform.j,janchor.dotproduct(door_transform.j),lo_ext,hi_ext);
			door_plane_ext.x=hi_ext-lo_ext;//door extensions
			door_plane_axes.x=1;		   //door_plane_axes.x stores door direction it may be j or k in this point
	
			door->joint->PSecond_element()->get_Extensions(door_transform.k,janchor.dotproduct(door_transform.k),lo_ext,hi_ext);
			ext=hi_ext-lo_ext;
			if(_abs(ext)>_abs(door_axis_in_door.x))
			{
				door->door_plane_ext.x=ext;
				door->door_plane_axes.x=2;
			}
	 
		}
		else
		{
			door->joint->PSecond_element()->get_Extensions(door_transform.j,janchor.dotproduct(door_transform.j),lo_ext,hi_ext);
			//door->door_plane_ext.y=hi_ext-lo_ext;
			//door_dir_sign=hi_ext>-lo_ext ? 1.f : -1.f;
			//door->door_plane_axes.x=2;
			//door->door_plane_axes.y=1;
	
			door->joint->PSecond_element()->get_Extensions(door_transform.i,janchor.dotproduct(door_transform.i),lo_ext,hi_ext);
			ext=hi_ext-lo_ext;
			if(_abs(ext)>door->door_plane_ext.y)
			{
				//door_dir_sign=hi_ext>-lo_ext ? 1.f : -1.f;
				//door->door_plane_ext.y=ext;
				//door->door_plane_axes.y=0;
	
			}
		}
	}
	else
	{
		if(door_axis_in_door.y>door_axis_in_door.z)	
		{
			door->joint->PSecond_element()->get_Extensions(door_transform.i,janchor.dotproduct(door_transform.i),lo_ext,hi_ext);
			door->door_plane_ext.y=hi_ext-lo_ext;
			//door_dir_sign=hi_ext>-lo_ext ? 1.f : -1.f;
			door->door_plane_axes.x=1;
			door->door_plane_axes.y=0;
			door->joint->PSecond_element()->get_Extensions(door_transform.k,janchor.dotproduct(door_transform.k),lo_ext,hi_ext);
			ext=hi_ext-lo_ext;
			if(ext>door->door_plane_ext.y)
			{
				//door_dir_sign=hi_ext>-lo_ext ? 1.f : -1.f;
				door->door_plane_ext.y=ext;
				door->door_plane_axes.y=2;
			}
		}
		else
		{
			door->joint->PSecond_element()->get_Extensions(door_transform.j,janchor.dotproduct(door_transform.j),lo_ext,hi_ext);
			door->door_plane_ext.y=hi_ext-lo_ext;
			//door_dir_sign=hi_ext>-lo_ext ? 1.f : -1.f;
			door->door_plane_axes.x=2;
			door->door_plane_axes.y=1;
			door->joint->PSecond_element()->get_Extensions(door_transform.i,janchor.dotproduct(door_transform.i),lo_ext,hi_ext);
			ext=hi_ext-lo_ext;
			if(ext>door->door_plane_ext.y)
			{
				//door_dir_sign=hi_ext>-lo_ext ? 1.f : -1.f;
				door->door_plane_ext.y=ext;
				door->door_plane_axes.y=0;
			}
		}
	}
/*
switch(door->door_plane_axes.y) 
{
case 0:
	door_dir.set(door_dir_sign,0.f,0.f);
	break;
case 1:
	door_dir.set(0.f,door_dir_sign,0.f);
	break;
case 2:
	door_dir.set(0.f,0.f,door_dir_sign);
	break;
default: NODEFAULT;
}
*/
}
void CCar::SDoor::SDoorway::Trace(const Fvector &point,const Fvector &dir)
{

}
#endif // 0
