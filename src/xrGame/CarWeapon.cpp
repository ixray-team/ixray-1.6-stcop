#include "stdafx.h"
collide::rq_result GetPickResult(Fvector pos, Fvector dir, float range, CObject* ignore);
#include "CarWeapon.h"
#include "Inventory.h"
#include "../xrPhysics/PhysicsShell.h"
#include "PhysicsShellHolder.h"
#include "../Include/xrRender/Kinematics.h"
#include "../xrEngine/CameraManager.h"
#include "../xrEngine/CameraBase.h"
#include "object_broker.h"
#include "../xrSound/ai_sounds.h"
#include "../xrEngine/xr_level_controller.h"
#include "game_object_space.h"
#include "holder_custom.h"
#include "ExplosiveRocket.h"
#include "CameraFirstEye.h"
void CCarTurret::BoneCallbackX		(CBoneInstance *B)
{
	CCarTurret	*P = static_cast<CCarTurret*>(B->callback_param());
	Fmatrix rX;		rX.rotateX		(P->m_cur_x_rot);
	B->mTransform.mulB_43			(rX);
}

void CCarTurret::BoneCallbackY		(CBoneInstance *B)
{
	CCarTurret	*P = static_cast<CCarTurret*>(B->callback_param());
	Fmatrix rY;		rY.rotateY		(P->m_cur_y_rot);
	B->mTransform.mulB_43			(rY);
}

void CCarTurret::SetBoneCallbacks()
{
//	m_object->PPhysicsShell()->EnabledCallbacks(FALSE);
	if(m_rotate_x_bone != BI_NONE)
	{
		CBoneInstance& biX		= PKinematics(m_pcar->Visual())->LL_GetBoneInstance(m_rotate_x_bone);	
		biX.set_callback		(bctCustom,BoneCallbackX,this);
	}
	if(m_rotate_y_bone != BI_NONE)
	{
		CBoneInstance& biY		= PKinematics(m_pcar->Visual())->LL_GetBoneInstance(m_rotate_y_bone);	
		biY.set_callback		(bctCustom,BoneCallbackY,this);
	}
}

void CCarTurret::ResetBoneCallbacks()
{
	if(m_rotate_x_bone != BI_NONE)
	{
		CBoneInstance& biX		= PKinematics(m_pcar->Visual())->LL_GetBoneInstance(m_rotate_x_bone);	
		biX.reset_callback		();
	}
	if(m_rotate_y_bone != BI_NONE)
	{
		CBoneInstance& biY		= PKinematics(m_pcar->Visual())->LL_GetBoneInstance(m_rotate_y_bone);	
		biY.reset_callback		();
	}
//	m_object->PPhysicsShell()->EnabledCallbacks(TRUE);
}

CCarTurret::CCarTurret(CCar* pcar, LPCSTR definition)
{
	inited					= false;
	m_bActive				= false;
	m_bMouseMoving_x		= false;
	m_bMouseMoving_y		= false;
	m_AvlblMgun				= false;
	m_AvlblRockets			= false;
	m_icur_gun_idx			= 0;
	m_pcar					= pcar;
	m_cur_gun				= NULL;
	IKinematics* K			= smart_cast<IKinematics*>(m_pcar->Visual());
	CInifile* pUserData		= K->LL_UserData(); 

	camera					= new CCameraFirstEye(m_pcar, CCameraBase::flPositionRigid|CCameraBase::flDirectionRigid);
	camera->Load("car_firsteye_cam");
	const shared_str CarSection = m_pcar->cNameSect();
	m_cam_bone = BI_NONE;
	if (pUserData->line_exist(definition, "cam_bone"))
	{
		m_cam_bone = K->LL_BoneID(pUserData->r_string(definition, "cam_bone"));
	}
	else if (pSettings->line_exist(CarSection, "cam_bone"))
	{
		m_cam_bone = K->LL_BoneID(pSettings->r_string(CarSection, "cam_bone"));
	}

	m_cam_pos_rel.set(0.0f, 0.0f, 0.0f);
	if (pUserData->line_exist(definition, "cam_pos_rel"))
	{
		m_cam_pos_rel = pUserData->r_fvector3(definition, "cam_pos_rel");
	}
	else if (pSettings->line_exist(CarSection, "cam_pos_rel"))
	{
		m_cam_pos_rel = pSettings->r_fvector3(CarSection, "cam_pos_rel");
	}

	m_cam_dir_rel.set(0.0f, 0.0f, 0.0f);
	if (pUserData->line_exist(definition, "cam_dir_rel"))
	{
		m_cam_dir_rel = pUserData->r_fvector3(definition, "cam_dir_rel");
	}
	else if (pSettings->line_exist(CarSection, "cam_dir_rel"))
	{
		m_cam_dir_rel = pSettings->r_fvector3(CarSection, "cam_dir_rel");
	}

	m_guns_size				= READ_IF_EXISTS(pUserData, r_u16, definition, "guns_size", 100);
	for (u16 i = 0; i < m_guns_size; ++i)
	{
		m_turret_guns.push_back(new CCarWeapon(m_pcar, definition, i));
		if(!m_turret_guns[i]->HasValid())
		{
			CCarWeapon*pwp = m_turret_guns[i];
			m_turret_guns.erase(m_turret_guns.begin()+i);
			--i;
			xr_delete(pwp);
			--m_guns_size;
		}
	}
	SetGun(m_icur_gun_idx);
	m_combine_input			= READ_IF_EXISTS(pUserData, r_bool, definition, "combine_input", TRUE);
	m_rotate_x_bone			= pUserData->line_exist(definition,"rotate_x_bone") ? K->LL_BoneID(pUserData->r_string(definition,"rotate_x_bone")) : BI_NONE;
	m_rotate_y_bone			= pUserData->line_exist(definition,"rotate_y_bone") ? K->LL_BoneID(pUserData->r_string(definition,"rotate_y_bone")) : BI_NONE;

	// Use the turret pivot as a camera fallback when cam_bone is absent or invalid.
	// cam_pos_rel/cam_dir_rel still apply on top of this bone.
	if (m_cam_bone == BI_NONE)
	{
		m_cam_bone = m_rotate_y_bone != BI_NONE ? m_rotate_y_bone : m_rotate_x_bone;
	}

	m_bRelative_link_x		= READ_IF_EXISTS(pUserData, r_bool, definition, "rotate_x_relative", FALSE);
	m_bRelative_link_y		= READ_IF_EXISTS(pUserData, r_bool, definition, "rotate_y_relative", FALSE);

	m_min_gun_speed			= READ_IF_EXISTS(pUserData, r_float, definition, "min_gun_speed", 0.0f);
	m_max_gun_speed			= READ_IF_EXISTS(pUserData, r_float, definition, "max_gun_speed", 0.0f);

	m_fGunVelMult_x			= READ_IF_EXISTS(pUserData, r_float, definition, "gun_vel_mult_x", 1.0f);
	m_fGunVelMult_y			= READ_IF_EXISTS(pUserData, r_float, definition, "gun_vel_mult_y", 1.0f);

	m_fGunForcMult_x		= READ_IF_EXISTS(pUserData, r_float, definition, "gun_forc_mult_x", 1.0f);
	m_fGunForcMult_y		= READ_IF_EXISTS(pUserData, r_float, definition, "gun_forc_mult_y", 1.0f);

	if(m_rotate_x_bone != BI_NONE)
	{
		CBoneData& bdX			= K->LL_GetData(m_rotate_x_bone);
		m_lim_x_rot.set			(bdX.IK_data.limits[0].limit.x,bdX.IK_data.limits[0].limit.y);
		m_i_bind_x_xform.invert	(bdX.bind_transform);
		m_saved_x_xform			= bdX.bind_transform;
		m_bind_x_rot			= bdX.bind_transform.k.getP();
		JointGun				= pcar->m_pPhysicsShell->get_Joint(m_rotate_x_bone);
		if(JointGun && JointGun->PSecond_element())
			JointGun->PSecond_element()->SetAirResistance(0.0f, 0.0f);
	}

	if(m_rotate_y_bone != BI_NONE)
	{
		CBoneData& bdY			= K->LL_GetData(m_rotate_y_bone);
		m_lim_y_rot.set			(bdY.IK_data.limits[1].limit.x,bdY.IK_data.limits[1].limit.y);
		m_i_bind_y_xform.invert	(bdY.bind_transform);
		m_saved_y_xform			= bdY.bind_transform;
		m_bind_y_rot			= bdY.bind_transform.k.getH();
		m_weapon_h				= bdY.bind_transform.c.y;
		JointTurret				= pcar->m_pPhysicsShell->get_Joint(m_rotate_y_bone);
		if(JointTurret && JointTurret->PSecond_element())
			JointTurret->PSecond_element()->SetAirResistance(0.0f, 0.0f);
	}

	m_cur_x_rot = m_tgt_x_rot = m_bind_x_rot;
	m_cur_y_rot = m_tgt_y_rot = m_bind_y_rot;
	m_destEnemyDir.setHP			(m_bind_y_rot,m_bind_x_rot);
	m_pcar->XFORM().transform_dir	(m_destEnemyDir);

	if(!JointGun && !JointTurret)
		SetBoneCallbacks();

	m_pcar->processing_activate		();
	mouse_moving_time = 0;
	inited = true;
}

CCarTurret::~CCarTurret()
{
	delete_data(camera);
	auto TGIt = m_turret_guns.begin();
	for(;TGIt!=m_turret_guns.end();++TGIt)
		delete_data((*TGIt));
	m_turret_guns.clear();
}

void CCarTurret::UpdateCL()
{
	if(!inited)return;

	UpdateBarrelDir();

	bool has_pick = false;
	collide::rq_result RQ;

	if(m_pcar->Owner())
	{
		for(auto TGIt = m_turret_guns.begin();TGIt!=m_turret_guns.end();++TGIt)
			if((*TGIt)->IsDirByCam())
				has_pick = true;

		if(has_pick)
			RQ = GetPickResult(m_cam_xform.c, m_cam_xform.k, 1000.0f, m_pcar);
	}

	auto TGIt = m_turret_guns.begin();
	for(;TGIt!=m_turret_guns.end();++TGIt)
	{
		(*TGIt)->UpdateCL();

		if((*TGIt)->IsDirByCam() && m_pcar->Camera()->tag == CCar::ECarCamType::EctFirstCam && has_pick)
		{
			Fvector cam_dir;
			cam_dir.sub(Fvector(m_cam_xform.c).add(Fvector(m_cam_xform.k).mul(RQ.range)), (*TGIt)->FirePoint());
			cam_dir.normalize();
			(*TGIt)->UpdateFireDir(cam_dir);
		}
	}

	//IKinematics* K				= smart_cast<IKinematics*>(m_pcar->Visual());
	//K->CalculateBones_Invalidate();
	//K->CalculateBones			(TRUE);
}

void CCarTurret::Render_internal()
{
	auto TGIt = m_turret_guns.begin();
	for(;TGIt!=m_turret_guns.end();++TGIt)
		(*TGIt)->Render_internal();

	//if (HasPhysical())
	//{
	//	IKinematics* K = smart_cast<IKinematics*>(m_pcar->Visual());
	//	Fobb BoneOBB_G = K->LL_GetBox(m_rotate_x_bone);
	//	Fmatrix BoneMatrix_G; BoneOBB_G.xform_get(BoneMatrix_G);
	//	Fmatrix BoneMatrixRes_G; BoneMatrixRes_G.mul(K->LL_GetTransform(m_rotate_x_bone), BoneMatrix_G);
	//	BoneMatrix_G.mul(m_pcar->XFORM(), BoneMatrixRes_G);
	//	
	//	float cur_gun_forcemax = dJointGetHingeParam(JointGun->GetDJoint(), dParamFMax);
	//	float cur_gun_velmax = dJointGetHingeParam(JointGun->GetDJoint(), dParamVel);
	//	float cur_gun_angle = dJointGetHingeAngle(JointGun->GetDJoint());
	//	float cur_gun_vel = dJointGetHingeAngleRate(JointGun->GetDJoint());
	//	
	//	float cur_gun_lo = dJointGetHingeParam(JointGun->GetDJoint(), dParamLoStop);
	//	float cur_gun_hi = dJointGetHingeParam(JointGun->GetDJoint(), dParamHiStop);
	//	
	//	if(Level().draw_text(BoneMatrix_G.c, Ivector().set(0, 255, 0), "angle %f", cur_gun_angle))
	//	{
	//		Level().draw_out_next_text(Ivector().set(0, 255, 0), "angle_lo %f", cur_gun_lo);
	//		Level().draw_out_next_text(Ivector().set(0, 255, 0), "angle_hi %f", cur_gun_hi);
	//		Level().draw_out_next_text(Ivector().set(0, 255, 0), "forcemax %f", cur_gun_forcemax);
	//		Level().draw_out_next_text(Ivector().set(0, 255, 0), "velmax %f", cur_gun_velmax);
	//		Level().draw_out_next_text(Ivector().set(0, 255, 0), "velcurr %f", cur_gun_vel);
	//	}
	//
	//	Fobb BoneOBB_T = K->LL_GetBox(m_rotate_y_bone);
	//	Fmatrix BoneMatrix_T; BoneOBB_T.xform_get(BoneMatrix_T);
	//	Fmatrix BoneMatrixRes_T; BoneMatrixRes_T.mul(K->LL_GetTransform(m_rotate_y_bone), BoneMatrix_T);
	//	BoneMatrix_T.mul(m_pcar->XFORM(), BoneMatrixRes_T);
	//	
	//	float cur_turr_forcemax = dJointGetHingeParam(JointTurret->GetDJoint(), dParamFMax);
	//	float cur_turr_velmax = dJointGetHingeParam(JointTurret->GetDJoint(), dParamVel);
	//	float cur_turr_angle = dJointGetHingeAngle(JointTurret->GetDJoint());
	//	float cur_turrb_angle, p, b;
	//	K->LL_GetTransform(m_rotate_y_bone).getHPB(cur_turrb_angle, p, b);
	//	float cur_turr_vel = dJointGetHingeAngleRate(JointTurret->GetDJoint());
	//	float cur_turr_lo = dJointGetHingeParam(JointTurret->GetDJoint(), dParamLoStop);
	//	float cur_turr_hi = dJointGetHingeParam(JointTurret->GetDJoint(), dParamHiStop);
	//	
	//	if(Level().draw_text(BoneMatrix_T.c, Ivector().set(0, 255, 0), "angle %f", cur_turr_angle))
	//	{
	//		Level().draw_out_next_text(Ivector().set(0, 255, 0), "angleb %f", cur_turrb_angle);
	//		Level().draw_out_next_text(Ivector().set(0, 255, 0), "angle_lo %f", cur_turr_lo);
	//		Level().draw_out_next_text(Ivector().set(0, 255, 0), "angle_hi %f", cur_turr_hi);
	//		Level().draw_out_next_text(Ivector().set(0, 255, 0), "forcemax %f", cur_turr_forcemax);
	//		Level().draw_out_next_text(Ivector().set(0, 255, 0), "velmax %f", cur_turr_velmax);
	//		Level().draw_out_next_text(Ivector().set(0, 255, 0), "velcurr %f", cur_turr_vel);
	//	}
	//}
}
#include "Actor.h"
#include "ActorEffector.h"
void CCarTurret::cam_Update(float fov)
{
	if(!inited)return;
	if(m_cam_bone != BI_NONE)
	{
		IKinematics* K		= PKinematics(m_pcar->Visual());
		K->CalculateBones			(TRUE);
		Fmatrix bone_xform;
		K->Bone_GetAnimPos(bone_xform, m_cam_bone, u8(-1), false);
		m_cam_xform = set_pos_dir_rel(m_pcar->XFORM(), bone_xform, m_cam_pos_rel, m_cam_dir_rel);
	}
    camera->vDirection = m_cam_xform.k;
    camera->vNormal = m_cam_xform.j;
    camera->vPosition = m_cam_xform.c;
	camera->f_fov = fov;
	m_pcar->OwnerActor()->Cameras().UpdateFromCamera(camera);
	m_pcar->OwnerActor()->Cameras().ApplyDevice(Device.fViewportNear);
}

void CCarTurret::OnMouseMove(int dx, int dy)
{
	CCameraBase* C	= camera;
	float scale		= (C->f_fov/g_fov) * psMouseSens * psMouseSensScale/50.f;
	float h,p;
	Fmatrix(m_pcar->XFORM()).invert().transform_dir(m_destEnemyDir);
	m_destEnemyDir.getHP(h,p);
	if (dx)
	{
		float d		= float(dx)*scale;
		if(HasPhysical())
		{
			if(_abs(rad2deg(dJointGetHingeParam(JointTurret->GetDJoint(), dParamLoStop)))+_abs(rad2deg(dJointGetHingeParam(JointTurret->GetDJoint(), dParamHiStop))) < 360.0f && _valid(dJointGetHingeParam(JointTurret->GetDJoint(), dParamLoStop)))
				clamp(h, dJointGetHingeParam(JointTurret->GetDJoint(), dParamLoStop), dJointGetHingeParam(JointTurret->GetDJoint(), dParamHiStop));
			clamp(d,-PI_DIV_8,PI_DIV_8);
		}
		else
		{
			if(_abs(rad2deg(m_lim_y_rot.y))+_abs(rad2deg(m_lim_y_rot.x)) < 360.0f)
				clamp(h,m_lim_y_rot.x, m_lim_y_rot.y);
		}
		h			-= d;
		m_bMouseMoving_x = true;
	}
	if (dy)
	{
		float d		= ((psMouseInvert)?-1:1)*float(dy)*scale*3.f/4.f;
		if(HasPhysical())
		{
			if(_abs(rad2deg(dJointGetHingeParam(JointGun->GetDJoint(), dParamLoStop)))+_abs(rad2deg(dJointGetHingeParam(JointGun->GetDJoint(), dParamHiStop))) < 360.0f)
				clamp(p, dJointGetHingeParam(JointGun->GetDJoint(), dParamLoStop)+EPS_L, dJointGetHingeParam(JointGun->GetDJoint(), dParamHiStop));
			clamp(d,-PI_DIV_8,PI_DIV_8);
		}
		else
		{
			if(_abs(rad2deg(m_lim_x_rot.y))+_abs(rad2deg(m_lim_x_rot.x)) < 360.0f)
				clamp(p, m_lim_x_rot.x, m_lim_x_rot.y);
		}
		p			-= d;
		m_bMouseMoving_y = true;
	}
	m_pcar->m_pPhysicsShell->Enable();
	m_destEnemyDir.setHP(h,p);
	Fmatrix(m_pcar->XFORM()).transform_dir(m_destEnemyDir);
	mouse_moving_time = Device.dwTimeGlobal;
}

// UNUSED: decreasetozero
// float decreasetozero(float val, int stop_speed)
// {
// 	float decrement = _abs(val) / 10.f;
//
// 	if (val > 0.0000000f)
// 	    val -= decrement*stop_speed*fixed_step;
// 	if (val < 0.0000000f)
// 	    val += decrement*stop_speed*fixed_step;
//
// 	return val;
// }

#include "../xrPhysics/ODE/PHWorld.h"

void CCarTurret::PhDataUpdate()
{
	if(HasPhysical())
	{
		m_cur_x_rot = dJointGetHingeAngle(JointGun->GetDJoint());
		m_cur_y_rot = dJointGetHingeAngle(JointTurret->GetDJoint());

		if(!m_bRelative_link_y)m_bMouseMoving_x = true;
		if(!m_bRelative_link_x)m_bMouseMoving_y = true;
		if(!m_bActive)
		{
			m_bMouseMoving_x = false;
			m_bMouseMoving_y = false;
		}
		IKinematics* K = PKinematics(m_pcar->Visual());
		if(m_cur_x_rot!=m_tgt_x_rot||m_cur_y_rot!=m_tgt_y_rot)
			K->CalculateBones(TRUE);

		if(m_bMouseMoving_x)
			m_saved_x_xform = K->LL_GetTransform(m_rotate_x_bone);
		if(m_bMouseMoving_y)
			m_saved_y_xform = K->LL_GetTransform(m_rotate_y_bone);

		Fmatrix x_xform_l, y_xform_l;
		float yh, yp, yb, xh, xp, xb;
		if(m_rotate_y_bone != BI_NONE)
		{
			y_xform_l.set(m_saved_y_xform); 
			y_xform_l.mulA_43(m_pcar->XFORM());
			y_xform_l.getHPB(yh, yp, yb);
			if(m_cam_bone == BI_NONE)
				m_cam_xform = y_xform_l;
		}
		else
			yh = m_destEnemyDir.getH();

		if(m_rotate_x_bone != BI_NONE)
		{
			x_xform_l.set(m_saved_x_xform);
			x_xform_l.mulA_43(m_pcar->XFORM()); 
			x_xform_l.getHPB(xh, xp, xb);
			if(m_cam_bone == BI_NONE)
				m_cam_xform = x_xform_l;
		}
		else
			xp = m_destEnemyDir.getP();

		m_destEnemyDir.setHP(m_bMouseMoving_x ? m_destEnemyDir.getH() : yh, m_bMouseMoving_y ? m_destEnemyDir.getP() : xp);


		if(_abs(dJointGetHingeAngleRate(JointGun->GetDJoint())) > EPS_L)
			JointGun->PSecond_element()->Enable();

		if(_abs(dJointGetHingeAngleRate(JointTurret->GetDJoint())) > EPS_L)
			JointTurret->PSecond_element()->Enable();

		float world_gravity = physics_world()->Gravity();
		// UNUSED: total_mass
		// float total_mass = JointGun->PSecond_element()->getMass()+JointTurret->PSecond_element()->getMass();

		// Physical turrets use joint velocity directly. Apply the same
		// min/max speed limits that non-physical turrets use in UpdateBarrelDir.
		auto CalculateJointVelocity = [&](float AngleDiff, float VelocityMultiplier, float Mass)
		{
			if (m_min_gun_speed > 0.0f && m_max_gun_speed >= m_min_gun_speed)
			{
				const float AbsDiff = std::abs(AngleDiff);
				const float Speed = m_min_gun_speed + (m_max_gun_speed - m_min_gun_speed) * std::min(AbsDiff / PI, 1.0f);

				// Do not overshoot the target when the remaining angle is small;
				// the joint motor has no built-in arrival damping.
				const float ClampedSpeed = std::min(Speed * VelocityMultiplier, AbsDiff / fixed_step);
				return (AngleDiff < 0.0f ? -1.0f : 1.0f) * ClampedSpeed;
			}

			return VelocityMultiplier * AngleDiff * Mass * world_gravity * fixed_step;
		};

		Fvector dep;
		Fmatrix(m_pcar->XFORM()).invert().transform_dir	(dep,m_destEnemyDir);
		///GUN
		if(m_bMouseMoving_y)
		{// x angle
			m_i_bind_x_xform.transform_dir(dep); dep.normalize();
			m_tgt_x_rot = angle_normalize_signed(m_bind_x_rot+dep.getP());
			if(_abs(rad2deg(dJointGetHingeParam(JointGun->GetDJoint(), dParamLoStop)))+_abs(rad2deg(dJointGetHingeParam(JointGun->GetDJoint(), dParamHiStop))) < 360.0f)
				clamp(m_tgt_x_rot,dJointGetHingeParam(JointGun->GetDJoint(), dParamLoStop)+EPS_L, dJointGetHingeParam(JointGun->GetDJoint(), dParamHiStop));
		}

		dJointSetHingeParam(JointGun->GetDJoint(), dParamFMax, m_fGunForcMult_x*m_pcar->m_pPhysicsShell->getMass()*world_gravity*0.5f);
		dJointSetHingeParam(JointGun->GetDJoint(), dParamVel, CalculateJointVelocity(
			angle_difference_signed(m_tgt_x_rot, dJointGetHingeAngle(JointGun->GetDJoint())),
			m_fGunVelMult_x,
			JointGun->PSecond_element()->getMass()));

		///TURRET
		if(m_bMouseMoving_x)
		{// x angle
			m_i_bind_y_xform.transform_dir(dep); dep.normalize();
			m_tgt_y_rot = angle_normalize_signed(m_bind_y_rot+dep.getH());
			if(_abs(rad2deg(dJointGetHingeParam(JointTurret->GetDJoint(), dParamLoStop)))+_abs(rad2deg(dJointGetHingeParam(JointTurret->GetDJoint(), dParamHiStop))) < 360.0f && _valid(dJointGetHingeParam(JointTurret->GetDJoint(), dParamLoStop)))
				clamp(m_tgt_y_rot, dJointGetHingeParam(JointTurret->GetDJoint(), dParamLoStop), dJointGetHingeParam(JointTurret->GetDJoint(), dParamHiStop));
		}

		dJointSetHingeParam(JointTurret->GetDJoint(), dParamFMax, m_fGunForcMult_y*m_pcar->m_pPhysicsShell->getMass()*world_gravity*0.5f);
		dJointSetHingeParam(JointTurret->GetDJoint(), dParamVel, CalculateJointVelocity(
			angle_difference_signed(m_tgt_y_rot, dJointGetHingeAngle(JointTurret->GetDJoint())),
			m_fGunVelMult_y,
			JointTurret->PSecond_element()->getMass()));

		if(Device.dwTimeGlobal > mouse_moving_time+100)
		{
			m_bMouseMoving_x = false;
			m_bMouseMoving_y = false;
		}
	}
}


void CCarTurret::UpdateBarrelDir()
{
	IKinematics* K		= PKinematics(m_pcar->Visual());

	if(!HasPhysical())
	{
		if(!m_bRelative_link_y)m_bMouseMoving_x = true;
		if(!m_bRelative_link_x)m_bMouseMoving_y = true;
		if(m_pcar->Camera()->tag != CCar::ECarCamType::EctFirstCam)
		{
			m_bMouseMoving_x = true;
			m_bMouseMoving_y = true;
			mouse_moving_time = Device.dwTimeGlobal;
		}
		if(!m_bActive)
		{
			m_bMouseMoving_x = false;
			m_bMouseMoving_y = false;
		}

		if(m_bMouseMoving_x)
		{
			K->CalculateBones(TRUE);
			m_saved_x_xform = K->LL_GetTransform(m_rotate_x_bone);
		}
		if(m_bMouseMoving_y)
		{
			K->CalculateBones(TRUE);
			m_saved_y_xform = K->LL_GetTransform(m_rotate_y_bone);
		}

		Fmatrix x_xform_l, y_xform_l;
		float yh, yp, yb, xh, xp, xb;
		if(m_rotate_y_bone != BI_NONE)
		{
			y_xform_l.set(m_saved_y_xform); 
			y_xform_l.mulA_43(m_pcar->XFORM());
			y_xform_l.getHPB(yh, yp, yb);
			if(m_cam_bone == BI_NONE)
				m_cam_xform = y_xform_l;
		}
		else
			yh = m_destEnemyDir.getH();

		if(m_rotate_x_bone != BI_NONE)
		{
			x_xform_l.set(m_saved_x_xform);
			x_xform_l.mulA_43(m_pcar->XFORM()); 
			x_xform_l.getHPB(xh, xp, xb);
			if(m_cam_bone == BI_NONE)
				m_cam_xform = x_xform_l;
		}
		else
			xp = m_destEnemyDir.getP();

		m_destEnemyDir.setHP(m_bMouseMoving_x ? m_destEnemyDir.getH() : yh, m_bMouseMoving_y ? m_destEnemyDir.getP() : xp);


		Fvector dep;
		Fmatrix(m_pcar->XFORM()).invert().transform_dir	(dep,m_destEnemyDir);
		if(m_bMouseMoving_y)
		{// x angle
			m_i_bind_x_xform.transform_dir(dep); dep.normalize();
			m_tgt_x_rot		= angle_normalize_signed(m_bind_x_rot-dep.getP());
			if(_abs(rad2deg(m_lim_x_rot.y))+_abs(rad2deg(m_lim_x_rot.x)) < 360.0f)
				clamp(m_tgt_x_rot,-m_lim_x_rot.y,-m_lim_x_rot.x);
		}
		if(m_bMouseMoving_x)
		{// y angle
			m_i_bind_y_xform.transform_dir(dep); dep.normalize();
			m_tgt_y_rot		= angle_normalize_signed(m_bind_y_rot-dep.getH());
			if(_abs(rad2deg(m_lim_y_rot.y))+_abs(rad2deg(m_lim_y_rot.x)) < 360.0f)
				clamp(m_tgt_y_rot,-m_lim_y_rot.y,-m_lim_y_rot.x);
		}

		if(m_min_gun_speed == 0.0f || m_max_gun_speed == 0.0f)
		{
			m_cur_x_rot		= m_tgt_x_rot;
			m_cur_y_rot		= m_tgt_y_rot;
		}
		else
		{
			m_cur_x_rot		= angle_inertion_var(m_cur_x_rot,m_tgt_x_rot,m_min_gun_speed,m_max_gun_speed,PI,Device.fTimeDelta);
			m_cur_y_rot		= angle_inertion_var(m_cur_y_rot,m_tgt_y_rot,m_min_gun_speed,m_max_gun_speed,PI,Device.fTimeDelta);
		}
	}

	if(GetGunsSize())
	{
		static float dir_eps = deg2rad(5.0f);
		if( !fsimilar(m_cur_x_rot,m_tgt_x_rot,dir_eps) || !fsimilar(m_cur_y_rot,m_tgt_y_rot,dir_eps))
			m_cur_gun->m_allow_fire = false;
		else
			m_cur_gun->m_allow_fire = true;
	}

	if(!HasPhysical() && Device.dwTimeGlobal > mouse_moving_time+500)
	{
		m_bMouseMoving_x = false;
		m_bMouseMoving_y = false;
	}
}

float CCarTurret::FireDirDiff()
{
	Fvector d1,d2;
	d1.set(m_cur_x_rot,m_cur_y_rot,0).normalize_safe();
	d2.set(m_tgt_x_rot,m_tgt_y_rot,0).normalize_safe();
	return rad2deg( acos(d1.dotproduct(d2)) );
}

void CCarTurret::Action(int id, u32 flags)
{
	switch (id)
	{
		case eWpnFire:
		{
			if(GetGunsSize())
			{
				if(flags==1)
					m_cur_gun->StartFire();
				else
					m_cur_gun->EndFire();
			}
		}break;

		case eWpnActivate:
		{
			if(flags==1)
				m_bActive = true;
			else
			{
				m_bActive = false;
				if(GetGunsSize())
				{
					m_cur_gun->EndFire();
					m_cur_gun->LightStop();
				}
			}
		}break;

		case eWpnAutoFire:
		{
			if(GetGunsSize())
			{
				if(flags==1)
					m_cur_gun->m_bAutoFire = true;
				else
					m_cur_gun->m_bAutoFire = false;
			}
		}break;

		case eWpnToDefaultDir:
			SetParam(eWpnDesiredDir,Fvector2().set(m_bind_y_rot,m_bind_x_rot));
			break;
	}
}

void CCarTurret::SetParam(int id, Fvector2 val)
{
	switch (id)
	{
		case eWpnDesiredDir:
			m_destEnemyDir.setHP(val.x,val.y);
			break;
	}
	m_bMouseMoving_x = true;
	m_bMouseMoving_y = true;
	mouse_moving_time = Device.dwTimeGlobal;
}

void CCarTurret::SetParam(int id, Fvector val)
{
	if(!m_pcar->m_pPhysicsShell->isEnabled())
		m_pcar->m_pPhysicsShell->Enable();

	Fvector Fpoint = Fvector(GetGunsSize() ? m_cur_gun->FirePoint() : ViewCameraPos()).mad(Fvector(GetGunsSize() ? m_cur_gun->FireDir() : ViewCameraDir()), -10.0f);

	switch (id)
	{
		case eWpnDesiredPos:
			m_destEnemyDir.sub(val, m_pcar->Owner() ? Fpoint : m_cur_gun ? m_cur_gun->FirePoint() : ViewCameraPos()).normalize_safe();
			break;
	}
	m_bMouseMoving_x = true;
	m_bMouseMoving_y = true;
	mouse_moving_time = Device.dwTimeGlobal;
}

void CCarTurret::SetGun(int val)
{
	if (GetGunsSize() == 0)
	{
		return;
	}

	val = std::clamp(val, 0, GetGunsSize() - 1);
	if (m_cur_gun == m_turret_guns[val])
	{
		m_icur_gun_idx = val;
		m_cur_gun->m_curr_idx = val;
		return;
	}

	const bool has_fire = m_cur_gun != nullptr && m_cur_gun->HasFire();
	if (m_cur_gun != nullptr)
	{
		m_cur_gun->EndFire();
		m_cur_gun->LightStop();
	}

	m_cur_gun = m_turret_guns[val];
	m_icur_gun_idx = val;
	m_cur_gun->m_curr_idx = val;
	m_cur_gun->RefreshAmmoFromCar();

	if (has_fire)
	{
		m_cur_gun->StartFire();
	}
}

// UNUSED: SetPrevGun
// void CCarTurret::SetPrevGun()
// {
// 	if (GetGunsSize() == 0)
// 	{
// 		return;
// 	}
//
// 	--m_icur_gun_idx;
// 	clamp(m_icur_gun_idx, 0, GetGunsSize() - 1);
// 	SetGun(m_icur_gun_idx);
// }

// UNUSED: SetNextGun
// void CCarTurret::SetNextGun()
// {
// 	if (GetGunsSize() == 0)
// 	{
// 		return;
// 	}
//
// 	++m_icur_gun_idx;
// 	clamp(m_icur_gun_idx, 0, GetGunsSize() - 1);
// 	SetGun(m_icur_gun_idx);
// }

void CCarTurret::SetGunCircle()
{
	if (GetGunsSize() == 0)
	{
		return;
	}

	++m_icur_gun_idx;
	if (m_icur_gun_idx >= GetGunsSize())
	{
		m_icur_gun_idx = 0;
	}
	SetGun(m_icur_gun_idx);
}

void CCarTurret::AppendAmmoToGuns(CWeaponAmmo* ammo)
{
	if(ammo->getDestroy() || ammo->m_boxCurr <= 0) return;
	auto TGIt = m_turret_guns.begin();
	for(;TGIt!=m_turret_guns.end();++TGIt)
		(*TGIt)->AddAmmo(ammo);
}

void CCarTurret::DropEmptyAmmoBoxes()
{
	auto TGIt = m_turret_guns.begin();
	for(;TGIt!=m_turret_guns.end();++TGIt)
		(*TGIt)->DropEmptyAmmoBoxes();
}

#include "../xrEngine/string_table.h"
void CCarTurret::GetBriefInfo(xr_string& str_name, xr_string& icon_sect_name, xr_string& str_count)
{
	if(!m_cur_gun)
		return;

	icon_sect_name	= m_cur_gun->m_ammoTypes[m_cur_gun->m_ammoTypeCurrent].c_str();

	string256 tmpstr = {'\0'};

	xr_strcpy(tmpstr, CStringTable().translate(pSettings->r_string(icon_sect_name.c_str(), "inv_name_short")).c_str());
	str_name = tmpstr;

	if (m_cur_gun->unlimited_ammo())
		xr_sprintf(tmpstr, "--");
	else
	{
		const int ammo_count = m_cur_gun->GetAmmoCount(m_cur_gun->m_ammoTypeCurrent);

		// Only magazine-style guns need the reload timer in the readout.
		// Belt/box turrets keep showing the real remaining count while the
		// ammo-type switch sound is playing.
		if (m_cur_gun->m_aftershot_delay)
		{
			if (ammo_count && Device.fTimeGlobal > m_cur_gun->m_sndReloadSTime + m_cur_gun->m_sndReloadTTotal)
			{
				xr_sprintf(tmpstr, "1/%d", ammo_count == 1 ? 0 : ammo_count - 1);
			}
			else
			{
				xr_sprintf(tmpstr, "0/%d", ammo_count == 1 ? 0 : ammo_count);
			}
		}
		else
		{
			xr_sprintf(tmpstr, "%d", ammo_count);
		}
	}

	str_count				= tmpstr;
}

CCarWeapon::CCarWeapon(CCar* pcar, LPCSTR definition, int idx)
{
	inited		= false;
	m_pcar		= pcar;
	m_Ammo		= NULL;
	m_weaponsect = "";
	m_bAutoFire	= false;
	m_AvlblMgun	= false;
	m_AvlblRockets	= false;
	m_curr_idx = 0;
	m_sndReloadTTotal = 0;
	m_sndReloadSTime = 0;
	m_iShotsCount = 0;
	IKinematics* K			= smart_cast<IKinematics*>(m_pcar->Visual());
	CInifile* pUserData		= K->LL_UserData(); 

	string128 line_prefix;
	xr_sprintf(line_prefix, "fire_bone_%d", idx);
	m_fire_bone				= pUserData->line_exist(definition, idx ? line_prefix : "fire_bone") ? K->LL_BoneID(pUserData->r_string(definition, idx ? line_prefix : "fire_bone")) : K->LL_GetBoneRoot();
	xr_sprintf(line_prefix, "fire_pos_rel_%d", idx);
	m_fire_pos_rel			= READ_IF_EXISTS(pUserData, r_fvector3, definition, idx ? line_prefix : "fire_pos_rel", Fvector().set(0.0f, 0.0f, 0.0f));
	xr_sprintf(line_prefix, "fire_dir_rel_%d", idx);
	m_fire_dir_rel			= READ_IF_EXISTS(pUserData, r_fvector3, definition, idx ? line_prefix : "fire_dir_rel", Fvector().set(0.0f, 0.0f, 0.0f));

	m_fire_dir_by_cam		= READ_IF_EXISTS(pUserData, r_bool, definition, "fire_dir_by_cam", true);

	xr_sprintf(line_prefix, "aftershot_delay_%d", idx);
	m_aftershot_delay	= READ_IF_EXISTS(pUserData, r_bool, definition, idx ? line_prefix : "aftershot_delay", false);
	xr_sprintf(line_prefix, "unlimited_ammo_%d", idx);
	m_unlimited_ammo = READ_IF_EXISTS(pUserData, r_bool, definition, idx ? line_prefix : "unlimited_ammo", false);
	//mgun
	xr_sprintf(line_prefix, "wpn_section_%d", idx);
	if(pUserData->line_exist(definition, idx ? line_prefix : "wpn_section"))
	{
		
		m_weaponsect = pUserData->r_string(definition, idx ? line_prefix : "wpn_section");
		if(pSettings->section_exist(m_weaponsect))
		{
			if (pSettings->line_exist(m_weaponsect, "fire_modes"))
			{
				shared_str FireModesList = pSettings->r_string(m_weaponsect, "fire_modes");
				int ModesCount = _GetItemCount(FireModesList.c_str());
				m_aFireModes.clear();
				
				for (int i=0; i<ModesCount; i++)
				{
					string16 sItem;
					_GetItem(FireModesList.c_str(), i, sItem);
					m_aFireModes.push_back(atoi(sItem));
				}
				
				m_iCurFireMode = ModesCount - 1;
			}
			else
			{
				m_iCurFireMode = 0;
				m_aFireModes.push_back	(-1);
			}

			LoadParams(m_weaponsect);
			m_AvlblMgun = true;
		}
	}

	//rocket launcher
	xr_sprintf(line_prefix, "rocket_class_%d", idx);
	m_AvlblRockets			= !!((m_AvlblMgun && pSettings->line_exist(m_weaponsect, "rocket_class")) || (m_AvlblMgun && pSettings->line_exist(*m_ammoTypes[m_ammoTypeCurrent], "fake_grenade_name")));
	if(m_AvlblRockets)
	{
		if((m_AvlblMgun && pSettings->line_exist(m_weaponsect, "rocket_class")))
			m_sRocketSection		= pSettings->r_string(m_weaponsect, "rocket_class");
		if((m_AvlblMgun && pSettings->line_exist(*m_ammoTypes[m_ammoTypeCurrent], "fake_grenade_name")))
			m_sRocketSection		= pSettings->r_string(*m_ammoTypes[m_ammoTypeCurrent], "fake_grenade_name");

		xr_sprintf(line_prefix, "rocket_launch_speed_%d", idx);
		if((m_AvlblMgun && pSettings->line_exist(m_weaponsect, "launch_speed")))
			m_fRocketLaunchSpeed = pSettings->r_float(m_weaponsect, "launch_speed");
		
		xr_sprintf(line_prefix, "rocket_rpm_%d", idx);
	}
	if(m_AvlblMgun || m_AvlblRockets)
		inited = true;
}

CCarWeapon::~CCarWeapon()
{
	if(m_Ammo)delete_data(m_Ammo);
}

void CCarWeapon::LoadParams(LPCSTR section)
{
	m_Ammo = new CCartridge();
	
	ISO::Load(section);

	HUD_SOUND_ITEM::LoadSound(section,"snd_shoot", m_sndShot, SOUND_TYPE_WEAPON_SHOOTING);
	HUD_SOUND_ITEM::LoadSound(section,"snd_empty", m_sndEmptyClick, SOUND_TYPE_WEAPON_EMPTY_CLICKING);
	HUD_SOUND_ITEM::LoadSound(section,"snd_reload", m_sndReload, SOUND_TYPE_WEAPON_RECHARGING);

	LPCSTR				S = pSettings->r_string(section,"ammo_class");
	if (S && S[0]) 
	{
		string128		_ammoItem;
		int				count		= _GetItemCount	(S);
		for (int it=0; it<count; ++it)	
		{
			_GetItem				(S,it,_ammoItem);
			m_ammoTypes.push_back	(_ammoItem);
		}
		m_ammoTypesSize = count;
	}
	m_ammoTypeCurrent = 0;
	m_Ammo->Load(*m_ammoTypes[m_ammoTypeCurrent], 0);
}

void CCarWeapon::SwitchAmmoType()
{
	if(m_ammoTypes.size() < 2)
		return;

	// Re-scan the trunk before choosing the next type. Ammo may have been
	// inserted or removed without reaching the turret registration path.
	RefreshAmmoFromCar();

	int tmpcurrtype = m_ammoTypeCurrent;

	++tmpcurrtype;

	if(tmpcurrtype >= m_ammoTypesSize)
		tmpcurrtype = 0;

	if(!GetAmmoCount(tmpcurrtype) && !unlimited_ammo())
		return;

	m_ammoTypeCurrent = tmpcurrtype;

	m_Ammo->Load(*m_ammoTypes[m_ammoTypeCurrent], 0);
	if(m_AvlblRockets)
	{
		if(m_pcar->getRocketCount())
		{
			if(CExplosiveRocket* pGrenade = smart_cast<CExplosiveRocket*>(m_pcar->getCurrentRocket()))
				m_pcar->DetachRocket(pGrenade->ID(), false);
		}

		if((m_AvlblMgun && pSettings->line_exist(*m_ammoTypes[m_ammoTypeCurrent], "fake_grenade_name")))
			m_sRocketSection = pSettings->r_string(*m_ammoTypes[m_ammoTypeCurrent], "fake_grenade_name");
	}

	if(GetAmmoCount(m_ammoTypeCurrent) || unlimited_ammo())
	{
		HUD_SOUND_ITEM::PlaySound(m_sndReload, Fvector(m_fire_xform.c), m_pcar, m_pcar->Camera()->tag == CCar::ECarCamType::EctFirstCam);
		if(m_sndReload.m_activeSnd)
		{
			m_sndReloadTTotal = m_sndReload.m_activeSnd->snd.get_length_sec();
			m_sndReloadSTime = Device.fTimeGlobal;
		}
	}

	EndFire();
}

void CCarWeapon::UpdateFireDir(const Fvector &dir)
{
	if(!inited)return;
	m_fire_xform.k = dir;
}

void CCarWeapon::UpdateCL()
{
	if(!inited)return;
	IKinematics* K = PKinematics(m_pcar->Visual());
	Fmatrix fire_mtx = set_pos_dir_rel(m_pcar->XFORM(), K->LL_GetTransform(m_fire_bone), m_fire_pos_rel, m_fire_dir_rel);
	m_fire_xform.c.set(fire_mtx.c);
	m_fire_xform.j.set(fire_mtx.j);
	m_fire_xform.i.set(fire_mtx.i);
	if(!m_fire_dir_by_cam || m_pcar->Camera()->tag != CCar::ECarCamType::EctFirstCam || !m_pcar->Owner())
		m_fire_xform.k.set(fire_mtx.k);
	if(HasMGun())
	{
		if(m_pcar->Camera()->tag == CCar::ECarCamType::EctFirstCam)
		{
			m_sndShot.set_position(Fvector().set(0.f,0.f,0.f));
			m_sndEmptyClick.set_position(Fvector().set(0.f,0.f,0.f));
			m_sndReload.set_position(Fvector().set(0.f,0.f,0.f));
		}
		else
		{
			m_sndShot.set_position(m_fire_xform.c);
			m_sndEmptyClick.set_position(m_fire_xform.c);
			m_sndReload.set_position(m_fire_xform.c);
		}

		UpdateFire();
	}
	if(HasRockets())
	{
		if(!m_pcar->getRocketCount())
			m_pcar->SpawnRocket(*m_sRocketSection, m_pcar);
	}
}

void CCarWeapon::UpdateFire()
{
	fShotTimeCounter -= Device.fTimeDelta;

	ISO::UpdateEffects();

	if(m_bAutoFire)
	{
		if(m_allow_fire)
			ISO::FireStart();
		else
			ISO::FireEnd();
	}

	if(!ISO::IsWorking())
	{
		clamp(fShotTimeCounter, 0.0f, flt_max);
		m_iShotsCount = 0;
		return;
	}

	if (m_iShotsCount > 0 && GetFireMode() > 0 && m_iShotsCount == GetFireMode())
	{
		EndFire();
		m_iShotsCount = 0;
		fShotTimeCounter = 0.0f;
		return;
	}

	if(fShotTimeCounter <= 0)
	{
		if(Device.fTimeGlobal < m_sndReloadSTime + m_sndReloadTTotal)
		{
			//Msg("%f", m_sndReload.m_activeSnd->snd.get_length_sec());
			//Msg("%f", Device.fTimeGlobal);
			return;
		}

		if(!unlimited_ammo())
		{
			if(!GetAmmoCount(m_ammoTypeCurrent))
			{
				SwitchAmmoType();
				HUD_SOUND_ITEM::PlaySound(m_sndEmptyClick, Fvector(m_fire_xform.c), m_pcar, m_pcar->Camera()->tag == CCar::ECarCamType::EctFirstCam);
				EndFire();
				return;
			}
			
			DecreaseAmmo();
		}

		fShotTimeCounter += fOneShotTime;

		OnShot();
		if(HasRockets())
			startRocket();

		++m_iShotsCount;

		if(m_aftershot_delay && (GetAmmoCount(m_ammoTypeCurrent) >= 1 || unlimited_ammo()))
		{
			HUD_SOUND_ITEM::PlaySound(m_sndReload, Fvector(m_fire_xform.c), m_pcar, m_pcar->Camera()->tag == CCar::ECarCamType::EctFirstCam);
			if(m_sndReload.m_activeSnd)
			{
				m_sndReloadTTotal = m_sndReload.m_activeSnd->snd.get_length_sec();
				m_sndReloadSTime = Device.fTimeGlobal;
			}
		}
	}
}

void CCarWeapon::startRocket()
{
	if(m_pcar->getRocketCount())
	{
		if(CExplosiveRocket* pGrenade = smart_cast<CExplosiveRocket*>(m_pcar->getCurrentRocket()))
		{
			Fvector launch_dir = m_fire_xform.k;
			Fvector launch_pos = m_fire_xform.c;
			Fvector launch_force = Fvector().mul(launch_dir,m_fRocketLaunchSpeed);
			Fmatrix launch_xform;
			launch_xform.identity();
			launch_xform.k.set(launch_dir);
			Fvector::generate_orthonormal_basis(launch_xform.k,launch_xform.j,launch_xform.i);
			launch_xform.c = launch_pos;

			m_pcar->LaunchRocket(launch_xform, launch_force, zero_vel);
			pGrenade->SetInitiator(GetInitiatorID());
			if (OnServer())
			{
				NET_Packet P;
				m_pcar->u_EventGen(P,GE_LAUNCH_ROCKET,m_pcar->ID());
				P.w_u16(pGrenade->ID());
				m_pcar->u_EventSend(P);
			}
			else
				m_pcar->dropCurrentRocket();
		}
	}
}

u16 CCarWeapon::GetInitiatorID()
{
	if(m_pcar)
	{
		if(m_pcar->Owner())
			return m_pcar->Owner()->ID();
		else
			return m_pcar->ID();
	}

	return u16(-1);
}

void CCarWeapon::OnShot()
{
	if(HasRockets())
	{
		m_fire_xform.k = Fvector(m_fire_xform.k);
		m_fire_xform.c = Fvector(m_fire_xform.c);
	}
	else
	{
		if(HasMGun())
		{
			for(int i = 0; i < m_Ammo->param_s.buckShot; ++i) 
				ISO::FireBullet(Fvector(m_fire_xform.c), Fvector(m_fire_xform.k), ISO::fireDispersionBase * m_Ammo->param_s.kDisp, *m_Ammo, GetInitiatorID(), m_pcar->ID(), ISO::SendHitAllowed(m_pcar));
		}
	}

	if(ISO::m_bLightShotEnabled) 
		ISO::Light_Start();

	ISO::StartFlameParticle(); ISO::StartSmokeParticle(zero_vel);
	ISO::StartFlameParticle();
	ISO::StartSmokeParticle(zero_vel);
//	OnShellDrop				(m_fire_pos, zero_vel);

	HUD_SOUND_ITEM::PlaySound(m_sndShot, Fvector(m_fire_xform.c), m_pcar, m_pcar->Camera()->tag == CCar::ECarCamType::EctFirstCam);
}

bool CCarWeapon::unlimited_ammo()
{ 
	return !!((psActorFlags.test(AF_UNLIMITED_AMMO) && m_Ammo->m_flags.test(CCartridge::cfCanBeUnlimited)) || m_unlimited_ammo || m_pcar->GetScriptControl()); 
}

void CCarWeapon::AddAmmo(CWeaponAmmo *ammo)
{
	if(ammo->getDestroy() || ammo->m_boxCurr <= 0) return;

	for (int i=0; i<m_ammoTypes.size(); ++i)
	{
		if(!xr_strcmp(ammo->cNameSect_str(), m_ammoTypes[i].c_str()))
		{
			auto& AmmoObjects = m_ammo_objects[i];
			if (std::find(AmmoObjects.begin(), AmmoObjects.end(), ammo) == AmmoObjects.end())
			{
				AmmoObjects.push_back(ammo);
			}
		}
	}
}

void CCarWeapon::RefreshAmmoFromCar()
{
	if (m_pcar == nullptr)
	{
		return;
	}

	CInventory& Inventory = m_pcar->inventory();
	for (const shared_str& AmmoType : m_ammoTypes)
	{
		xr_vector<PIItem> Items;
		Inventory.GetAll(AmmoType.c_str(), Items);
		for (PIItem Item : Items)
		{
			if (CWeaponAmmo* Ammo = smart_cast<CWeaponAmmo*>(Item))
			{
				AddAmmo(Ammo);
			}
		}
	}
}

void CCarWeapon::DecreaseAmmo()
{
	if(m_ammo_objects[m_ammoTypeCurrent].empty())
		return;

	--m_ammo_objects[m_ammoTypeCurrent].back()->m_boxCurr;
}

void CCarWeapon::DropEmptyAmmoBoxes()
{
	for (int i=0; i<m_ammoTypes.size(); ++i)
	{
		for (int k=0; k<(int)m_ammo_objects[i].size(); ++k)
		{
			if (CWeaponAmmo* A = m_ammo_objects[i][k])
			{
				bool ammo_box_empty_and_my = !!(A->m_boxCurr <= 0 && A->H_Parent() && A->H_Parent()->ID() == m_pcar->ID());
				bool ammo_box_nonempty_and_nonmy = !!(A->m_boxCurr > 0 && (!A->H_Parent() || (A->H_Parent() && A->H_Parent()->ID() != m_pcar->ID())));

				if(ammo_box_empty_and_my || ammo_box_nonempty_and_nonmy)
				{
					if(ammo_box_empty_and_my && !A->getDestroy())
						A->SetDropManual(TRUE);

					m_ammo_objects[i].erase(m_ammo_objects[i].begin()+k);
					--k;
				}
			}
		}
	}
}

int CCarWeapon::GetAmmoCount(int ammotype)
{
	int ammocount = 0;
	if (ammotype == -1)
	{
		for (int i=0; i<m_ammoTypes.size(); ++i)
		{
			for (auto A = m_ammo_objects[i].begin(); A != m_ammo_objects[i].end(); ++A)
			{
				if((*A)->getDestroy()) continue;
				ammocount += (*A)->m_boxCurr;
			}
		}
	}
	else
	{
		for (auto A = m_ammo_objects[ammotype].begin(); A != m_ammo_objects[ammotype].end(); ++A)
		{
			if((*A)->getDestroy()) continue;
			ammocount += (*A)->m_boxCurr;
		}
	}
	return ammocount;
}

void CCarWeapon::OnNextFireMode()
{
	m_iCurFireMode = (m_iCurFireMode+1+m_aFireModes.size()) % m_aFireModes.size();
}

void CCarWeapon::OnPrevFireMode()
{
	m_iCurFireMode = (m_iCurFireMode-1+m_aFireModes.size()) % m_aFireModes.size();
}
