#include "stdafx.h"
#ifdef DEBUG
#include "PHDebug.h"
#endif

#include "alife_space.h"
#include "Hit.h"
#include "PHDestroyable.h"
#include "Car.h"
#include "../Include/xrRender/Kinematics.h"
#include "../xrPhysics/MathUtils.h"
#include "game_object_space.h"

CCarDoor::CCarDoor(CCar* acar)
{
	bone_id = BI_NONE;
	pCar = acar;
	Joint = nullptr;
	state = closed;
	torque = 500.f;
	a_vel = M_PI;
}

void CCarDoor::Init()
{
	update = false;

	if (!pCar->bone_map.contains(bone_id))
		return;

	Joint = pCar->bone_map.find(bone_id)->second.joint;
	if (!Joint)
		return;

	R_ASSERT2(Joint->IsHingeJoint(), "Wrong door Joint!!! Only simple Joint valid for a door and only one axis can be active, check other axes are zerro limited !!!");
	Joint->SetBackRef(&Joint);
	Fvector door_position, door_axis;
	Joint->GetAnchorDynamic(door_position);
	Joint->GetAxisDirDynamic(0, door_axis);
	door_position.sub(pCar->XFORM().c);

	Fmatrix door_transform;
	Joint->PSecond_element()->InterpolateGlobalTransform(&door_transform);
	closed_door_form_in_object.set(Joint->PSecond_element()->mXFORM);

	Fvector jaxis, janchor;
	float lo_ext, hi_ext, ext;
	Joint->GetAxisDirDynamic(0, jaxis);
	Joint->GetAnchorDynamic(janchor);
	Joint->PSecond_element()->get_Extensions(jaxis, janchor.dotproduct(jaxis), lo_ext, hi_ext);
	door_plane_ext.x = hi_ext - lo_ext;
	Fvector jaxis_in_door;
	Fmatrix inv_door_transform;
	inv_door_transform.set(door_transform);
	inv_door_transform.invert();
	inv_door_transform.transform_dir(jaxis_in_door, jaxis);

	float	door_dir_sign;
	if (jaxis_in_door.x > jaxis_in_door.y)
	{
		if (jaxis_in_door.x > jaxis_in_door.z)
		{
			Joint->PSecond_element()->get_Extensions(door_transform.j, janchor.dotproduct(door_transform.j), lo_ext, hi_ext);
			door_plane_ext.y = hi_ext - lo_ext;
			door_dir_sign = hi_ext > -lo_ext ? 1.f : -1.f;
			door_plane_axes.x = 0;
			door_plane_axes.y = 1;
			Joint->PSecond_element()->get_Extensions(door_transform.k, janchor.dotproduct(door_transform.k), lo_ext, hi_ext);
			ext = hi_ext - lo_ext;
			if (ext > door_plane_ext.y)
			{
				door_dir_sign = hi_ext > -lo_ext ? 1.f : -1.f;
				door_plane_ext.y = ext;
				door_plane_axes.y = 2;
			}
		}
		else
		{
			Joint->PSecond_element()->get_Extensions(door_transform.j, janchor.dotproduct(door_transform.j), lo_ext, hi_ext);
			door_plane_ext.y = hi_ext - lo_ext;
			door_dir_sign = hi_ext > -lo_ext ? 1.f : -1.f;
			door_plane_axes.x = 2;
			door_plane_axes.y = 1;
			Joint->PSecond_element()->get_Extensions(door_transform.i, janchor.dotproduct(door_transform.i), lo_ext, hi_ext);
			ext = hi_ext - lo_ext;
			if (ext > door_plane_ext.y)
			{
				door_dir_sign = hi_ext > -lo_ext ? 1.f : -1.f;
				door_plane_ext.y = ext;
				door_plane_axes.y = 0;
			}
		}
	}
	else
	{
		if (jaxis_in_door.y > jaxis_in_door.z)
		{
			Joint->PSecond_element()->get_Extensions(door_transform.i, janchor.dotproduct(door_transform.i), lo_ext, hi_ext);
			door_plane_ext.y = hi_ext - lo_ext;
			door_dir_sign = hi_ext > -lo_ext ? 1.f : -1.f;
			door_plane_axes.x = 1;
			door_plane_axes.y = 0;
			Joint->PSecond_element()->get_Extensions(door_transform.k, janchor.dotproduct(door_transform.k), lo_ext, hi_ext);
			ext = hi_ext - lo_ext;
			if (ext > door_plane_ext.y)
			{
				door_dir_sign = hi_ext > -lo_ext ? 1.f : -1.f;
				door_plane_ext.y = ext;
				door_plane_axes.y = 2;
			}
		}
		else
		{
			Joint->PSecond_element()->get_Extensions(door_transform.j, janchor.dotproduct(door_transform.j), lo_ext, hi_ext);
			door_plane_ext.y = hi_ext - lo_ext;
			door_dir_sign = hi_ext > -lo_ext ? 1.f : -1.f;
			door_plane_axes.x = 2;
			door_plane_axes.y = 1;
			Joint->PSecond_element()->get_Extensions(door_transform.i, janchor.dotproduct(door_transform.i), lo_ext, hi_ext);
			ext = hi_ext - lo_ext;
			if (ext > door_plane_ext.y)
			{
				door_dir_sign = hi_ext > -lo_ext ? 1.f : -1.f;
				door_plane_ext.y = ext;
				door_plane_axes.y = 0;
			}
		}
	}

	switch (door_plane_axes.y)
	{
	case 0:
		door_dir_in_door.set(door_dir_sign, 0.f, 0.f);
		break;
	case 1:
		door_dir_in_door.set(0.f, door_dir_sign, 0.f);
		break;
	case 2:
		door_dir_in_door.set(0.f, 0.f, door_dir_sign);
		break;
	default: NODEFAULT;
	}

	///////////////////////////define positive open///////////////////////////////////
	Fvector door_dir, door_test;
	door_transform.transform_dir(door_dir, door_dir_in_door);
	door_test.crossproduct(door_dir, door_axis);
	door_test.normalize();

	Joint->PFirst_element()->get_Extensions(door_test, door_transform.c.dotproduct(door_test), lo_ext, hi_ext);
	if (hi_ext > -lo_ext)	pos_open = -1.f;
	else				pos_open = 1.f;

	if (pos_open > 0.f)
	{
		pos_open = 1.f;
		Joint->GetLimits(closed_angle, opened_angle, 0);
		opened_angle -= opened_angle / 4.f;
	}
	else
	{
		pos_open = -1.f;
		Joint->GetLimits(opened_angle, closed_angle, 0);
		opened_angle += 2.f * M_PI / 180.f;
		closed_angle -= 2.f * M_PI / 180.f;
	}
	Joint->GetLimits(saved_lo, saved_hi, 0);
	Fvector shoulder;

	shoulder.sub(door_transform.c, Joint->PSecond_element()->mass_Center());
	torque = shoulder.magnitude() * Joint->PSecond_element()->getMass() * pCar->m_doors_torque_factor * 10.f;
	state = opened;
}

void CCarDoor::Open()
{
	if (!Joint)
	{
		state = opened;

		return;
	}
	Joint->SetLimits(saved_lo, saved_hi, 0);
	switch (state)
	{
	case closed:
		ClosedToOpening();
		PlaceInUpdate();
	case closing:
		state = opening;

		ApplyOpenTorque();
	case opened:
	case opening: break;
	case broken: break;
	default: NODEFAULT;
	}

}

void CCarDoor::Close()
{
	if (!Joint)
	{
		state = closed;
		return;
	}

	switch (state)
	{
	case opened:
		PlaceInUpdate();
	case opening:
		state = closing;
		ApplyCloseTorque();
	case closed:
	case closing:
		break;
	default: NODEFAULT;
	}

}

void CCarDoor::PlaceInUpdate()
{
	if (update) 
		return;

	pCar->m_doors_update.push_back(this);
	update = true;
}

void CCarDoor::RemoveFromUpdate()
{
	update = false;
}

void CCarDoor::Update()
{
	switch (state)
	{
	case closing:
	{
		if (pos_open * closed_angle > pos_open * GetAngle()) ClosingToClosed();

		break;
	}
	case opening:
	{
		if (pos_open * opened_angle < pos_open * GetAngle())
		{
			NeutralTorque(torque);
			open_time = Device.dwTimeGlobal;
			state = opened;
		}
		break;

	}
	case opened:
	{
		if (Device.dwTimeGlobal - open_time > 1000)
		{
			ApplyTorque(torque / 5.f, a_vel);
			RemoveFromUpdate();
		}
	}
	}
}

void CCarDoor::Use()
{
	switch (state) 
	{
	case opened:
	case opening: Close();	break;
	case closed:
	case closing: Open();	break;
	default:				return;
	}
}

void CCarDoor::Switch()
{
	switch (state)
	{
	case opened: Close(); break;
	case closed: Open();  break;
	default:			  return;
	}
}

void CCarDoor::ApplyTorque(float atorque, float aa_vel)
{
	if (!Joint || !Joint->bActive)return;
	Joint->PSecond_element()->Enable();
	Joint->SetForce(atorque, 0);
	Joint->SetVelocity(aa_vel * pos_open, 0);

}
void CCarDoor::ApplyOpenTorque()
{
	if (!Joint->bActive)return;
	Joint->PSecond_element()->Enable();
	Joint->SetForce(torque, 0);
	Joint->SetVelocity(a_vel * pos_open, 0);
}

void CCarDoor::ApplyCloseTorque()
{
	if (!Joint->bActive)return;
	Joint->PSecond_element()->Enable();
	Joint->SetForce(torque, 0);
	Joint->SetVelocity(-a_vel * pos_open, 0);
}

void CCarDoor::NeutralTorque(float atorque)
{
	if (!Joint->bActive)
		return;

	Joint->SetForce(atorque, 0);
	Joint->SetVelocity(0, 0);
}

void CCarDoor::ClosedToOpening()
{
	if (!Joint)return;
	if (Joint->bActive)return;
	Fmatrix door_form, root_form;
	IKinematics* pKinematics = smart_cast<IKinematics*>(pCar->Visual());
	CBoneInstance& bone_instance = pKinematics->LL_GetBoneInstance(u16(bone_id));
	bone_instance.set_callback(bctPhysics, pCar->PPhysicsShell()->GetBonesCallback(), Joint->PSecond_element());

	door_form.set(bone_instance.mTransform);
	Joint->PSecond_element()->mXFORM.set(door_form);
	pCar->m_pPhysicsShell->GetGlobalTransformDynamic(&root_form);
	Joint->PSecond_element()->Activate(root_form, false);
	pCar->m_pPhysicsShell->Enable();
	Joint->Activate();
	pKinematics->CalculateBones();
}

void CCarDoor::ClosingToClosed()
{
	state = closed;
	if (!Joint) return;
	smart_cast<IKinematics*>(pCar->Visual())->CalculateBones();

	//	Fmatrix door_form;
	//IKinematics* pKinematics = smart_cast<IKinematics*>(pCar->Visual());
	//	CBoneData& bone_data= pKinematics->LL_GetData(u16(bone_id));
	//CBoneInstance& bone_instance = pKinematics->LL_GetBoneInstance(u16(bone_id));
	//bone_instance.set_callback(bctPhysics, 0, Joint->PFirst_element(), FALSE);
	//bone_instance.set_callback( bone_instance.callback_type(),bone_instance.callback(),bone_instance.callback_param(),FALSE);
	//bone_instance.Callback_overwrite=FALSE;
	//Joint->PSecond_element()->Deactivate();
	//Joint->Deactivate();
	Joint->SetLimits(0, 0, 0);
	RemoveFromUpdate();

	//door_form.set(bone_data.bind_transform);
	//bone_instance.mTransform.set(door_form);
}



float CCarDoor::GetAngle()
{
	if (!Joint || !Joint->bActive) return 0.f;
	//return dJointGetHingeAngle(Joint->GetDJoint());
	return Joint->GetAxisAngle(0);
}


static xr_vector<Fmatrix> bones_bind_forms;
bool CCarDoor::IsFront(const Fvector& pos,const Fvector& dir)
{
	IKinematics* K=PKinematics(pCar->Visual());
	K->LL_GetBindTransform(bones_bind_forms);

	Fvector tdir;tdir.set(pCar->XFORM().i);if(tdir.dotproduct(dir)<0.f)tdir.invert();
	Fmatrix pf;
	pf.mul(pCar->XFORM(),bones_bind_forms[bone_id]);
	Fvector dif,dif1;
	dif.sub(pf.c,pos);
	pCar->Center(dif1);
	Fvector c_to_d;c_to_d.sub(pf.c,dif1);

	dif1.sub(pos);
	dif.normalize_safe();
	return (dif1.dotproduct(tdir)>dif.dotproduct(tdir) && abs(c_to_d.dotproduct(tdir)) < dif1.dotproduct(tdir) );
}
bool CCarDoor::IsInArea(const Fvector& pos,const Fvector& dir)
{
	return true;
}

bool CCarDoor::CanExit(const Fvector& pos, const Fvector& dir)
{
	if (state == closed && Joint)return false;
	return TestPass(pos, dir);
}

void CCarDoor::GetExitPosition(Fvector& pos)
{
//	if(!joint) 
	{
		IKinematics* K=PKinematics(pCar->Visual());
		//CBoneInstance bi=K->LL_GetBoneInstance(bone_id);
		CBoneData& bd=K->LL_GetData(bone_id);
		K->LL_GetBindTransform(bones_bind_forms);
		Fobb bb;//=bd.obb;
		
		Fmatrix pf;
		pf.mul(pCar->XFORM(),bones_bind_forms[bone_id]);
		bb.transform(bd.obb,pf);
		bb.xform_get(pf);
		pos.set(pf.c);
		Fvector add,add1;
		MAX_OF(abs(pf.i.y),add.set(pf.i);add.mul(bb.m_halfsize.x*fsignum(pf.i.y)),abs(pf.j.y),add.set(pf.j);add.mul(bb.m_halfsize.y*fsignum(pf.j.y)),abs(pf.k.y),add.set(pf.k);add.mul(bb.m_halfsize.z*fsignum(pf.k.y)));
		pos.sub(add);

		MIN_OF(bb.m_halfsize.x,add1.set(pf.i);add1.mul(bb.m_halfsize.x),
			   bb.m_halfsize.y,add1.set(pf.j);add1.mul(bb.m_halfsize.y),
			  bb.m_halfsize.z,add1.set(pf.k);add1.mul(bb.m_halfsize.z))
		Fvector dir_from_car;dir_from_car.sub(pf.c,pCar->Position());
		dir_from_car.y=0.f;
		if(add1.dotproduct(dir_from_car)<0.f)add1.invert();	
		add1.mul(3.f);
		pos.add(add1);
		return;
	}
}



bool CCarDoor::TestPass(const Fvector& pos,const Fvector& dir)
{
	if(!Joint)
	{
		IKinematics* K=PKinematics(pCar->Visual());
		//CBoneInstance bi=K->LL_GetBoneInstance(bone_id);
		//CBoneData& bd=K->LL_GetData(bone_id);
		K->LL_GetBindTransform(bones_bind_forms);
		//		Fobb bb=bd.obb;
		Fmatrix pf;
		pf.mul(pCar->XFORM(),bones_bind_forms[bone_id]);
		Fvector dif;
		dif.sub(pf.c,pos);
		//dif.normalize_safe();
		return (dif.dotproduct(dir)>0.f);
	}
	float lo_ext,hi_ext;
	Fvector door_axis,door_pos,door_dir,closed_door_dir;

	if (Joint && Joint->bActive)
	{
		Joint->GetAxisDirDynamic(0, door_axis);
		Joint->GetAnchorDynamic(door_pos);
	}

	Fmatrix door_form,root_form;
	root_form.mul(pCar->m_root_transform,pCar->XFORM());
	if (Joint && Joint->bActive)
		Joint->PSecond_element()->InterpolateGlobalTransform(&door_form);
	door_form.transform_dir(door_dir,door_dir_in_door);
//	closed_door_form.mul(closed_door_form_in_object,pcar->XFORM());
	closed_door_form_in_object.transform_dir(closed_door_dir,door_dir_in_door);
	pCar->XFORM().transform_dir(closed_door_dir);
	door_axis.normalize();

	door_dir.normalize();
	closed_door_dir.normalize();

	Fvector closed_door_norm;

	closed_door_norm.crossproduct(door_axis,closed_door_dir);

	Fvector point_on_door,add,sub;
	add.set(dir);
	sub.sub(pos,door_pos);
	add.mul(-sub.dotproduct(closed_door_norm)/(dir.dotproduct(closed_door_norm)));
	
	if(add.dotproduct(dir)<0.f) return false;

	point_on_door.add(pos,add);

	float center_prg=door_pos.dotproduct(door_dir);
	if (Joint && Joint->bActive)
		Joint->PSecond_element()->get_Extensions(door_dir,center_prg,lo_ext,hi_ext);

	float point_prg=point_on_door.dotproduct(closed_door_dir);
	center_prg=door_pos.dotproduct(closed_door_dir);
	if(!(center_prg+hi_ext>point_prg)||!(center_prg+lo_ext<point_prg)) return false;

	center_prg=door_axis.dotproduct(door_pos);
	if (Joint && Joint->bActive)
		Joint->PSecond_element()->get_Extensions(door_axis,center_prg,lo_ext,hi_ext);

	point_prg=point_on_door.dotproduct(door_axis);
	if(!(center_prg+hi_ext>point_prg)||!(center_prg+lo_ext<point_prg)) return false;

	return true;
}

bool CCarDoor::CanEnter(const Fvector& pos, const Fvector& dir, const Fvector& foot_pos)
{
	return (state == opened || state == broken || !Joint) && TestPass(foot_pos, dir) && IsInArea(pos, dir) && (pCar->GetTrunkBone() != bone_id);
}

void CCarDoor::SaveNetState(NET_Packet& P)
{
	CSE_ALifeCar::SDoorState ds;
	ds.health = Health();
	ds.open_state = u8(state);
	ds.write(P);
}

void CCarDoor::RestoreNetState(const CSE_ALifeCar::SDoorState& a_state)
{
	eState lstate = eState(a_state.open_state);
	if (lstate == closed)	ClosingToClosed();
	state = lstate;
	SetHealth(a_state.health);
	RestoreEffect();
}

void CCarDoor::SetDefaultNetState()
{
	ClosingToClosed();
}

void CCarDoor::Break()
{
	switch (state) 
	{
	case closed:
		ClosedToOpening();
		break;
	case opened:
	case closing:
		RemoveFromUpdate();
	case opening:
		ApplyTorque(torque / 10.f, 0.f);
	}

	if (Joint)
	{
		Fvector v;
		float sf, df;
		Joint->GetAxisDirDynamic(0, v);

		v[0] += 0.1f; v[1] += 0.1f; v[2] += 0.1f;
		VERIFY(v.magnitude() > EPS_S);
		v.normalize();

		Joint->SetAxisDir(v, 0);

		Joint->GetJointSDfactors(sf, df);
		sf /= 30.f; df *= 8.f;
		Joint->SetJointSDfactors(sf, df);
		Joint->GetAxisSDfactors(sf, df, 0);
		sf /= 20.f; df *= 8.f;
		Joint->SetAxisSDfactors(sf, df, 0);
		float lo, hi;
		Joint->GetLimits(lo, hi, 0);
		if (pos_open > 0.f)
			Joint->SetLimits(lo + M_PI / 4.f, hi, 0);
		else
			Joint->SetLimits(lo, hi - M_PI / 4.f, 0);
	}

	state = broken;
}

void CCarDoor::ApplyDamage(u16 level)
{
	inherited::ApplyDamage(level);
	switch (level)
	{
	case 1: Break();
	}
}

CCarDoor::SDoorway::SDoorway()
{
	door = nullptr;
	door_plane_ext.set(0.f, 0.f);
	door_plane_axes.set(0, 0);
}

void CCarDoor::SDoorway::Init(CCarDoor* adoor)
{
	door = adoor;
	Fmatrix door_transform;
	door->Joint->PSecond_element()->InterpolateGlobalTransform(&door_transform);
	door->closed_door_form_in_object.set(door->Joint->PSecond_element()->mXFORM);
	Fvector jaxis, janchor;
	door->Joint->GetAxisDirDynamic(0, jaxis);
	door->Joint->GetAnchorDynamic(janchor);
	Fmatrix inv_door_transform;
	inv_door_transform.set(door_transform);
	inv_door_transform.invert();
	Fvector door_axis_in_door;
	inv_door_transform.transform_dir(door_axis_in_door, jaxis);
	float lo_ext, hi_ext, ext;

	if (_abs(door_axis_in_door.x) > _abs(door_axis_in_door.y))
	{
		if (_abs(door_axis_in_door.x) > _abs(door_axis_in_door.z))
		{
			//door axis aligned along x
			door_plane_axes.y = 0;		   //door axis is x (door_plane_axes.y stores door axis direction (i,j,k)=(0,1,2)
			door->Joint->PSecond_element()->get_Extensions(door_transform.i, janchor.dotproduct(door_transform.i), lo_ext, hi_ext);
			door->door_plane_ext.y = hi_ext - lo_ext; //door extension along door axis

			door->Joint->PSecond_element()->get_Extensions(door_transform.j, janchor.dotproduct(door_transform.j), lo_ext, hi_ext);
			door_plane_ext.x = hi_ext - lo_ext;//door extensions
			door_plane_axes.x = 1;		   //door_plane_axes.x stores door direction it may be j or k in this point

			door->Joint->PSecond_element()->get_Extensions(door_transform.k, janchor.dotproduct(door_transform.k), lo_ext, hi_ext);
			ext = hi_ext - lo_ext;
			if (_abs(ext) > _abs(door_axis_in_door.x))
			{
				door->door_plane_ext.x = ext;
				door->door_plane_axes.x = 2;
			}

		}
		else
		{
			door->Joint->PSecond_element()->get_Extensions(door_transform.j, janchor.dotproduct(door_transform.j), lo_ext, hi_ext);
			door->Joint->PSecond_element()->get_Extensions(door_transform.i, janchor.dotproduct(door_transform.i), lo_ext, hi_ext);
			ext = hi_ext - lo_ext;
		}
	}
	else
	{
		if (door_axis_in_door.y > door_axis_in_door.z)
		{
			door->Joint->PSecond_element()->get_Extensions(door_transform.i, janchor.dotproduct(door_transform.i), lo_ext, hi_ext);
			door->door_plane_ext.y = hi_ext - lo_ext;

			door->door_plane_axes.x = 1;
			door->door_plane_axes.y = 0;
			door->Joint->PSecond_element()->get_Extensions(door_transform.k, janchor.dotproduct(door_transform.k), lo_ext, hi_ext);
			ext = hi_ext - lo_ext;

			if (ext > door->door_plane_ext.y)
			{
				door->door_plane_ext.y = ext;
				door->door_plane_axes.y = 2;
			}
		}
		else
		{
			door->Joint->PSecond_element()->get_Extensions(door_transform.j, janchor.dotproduct(door_transform.j), lo_ext, hi_ext);
			door->door_plane_ext.y = hi_ext - lo_ext;
			door->door_plane_axes.x = 2;
			door->door_plane_axes.y = 1;
			door->Joint->PSecond_element()->get_Extensions(door_transform.i, janchor.dotproduct(door_transform.i), lo_ext, hi_ext);
			ext = hi_ext - lo_ext;
			if (ext > door->door_plane_ext.y)
			{
				door->door_plane_ext.y = ext;
				door->door_plane_axes.y = 0;
			}
		}
	}
}