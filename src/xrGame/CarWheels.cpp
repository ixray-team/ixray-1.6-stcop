#include "stdafx.h"
collide::rq_result GetPickResult(Fvector pos, Fvector dir, float range, CObject* ignore);
#ifdef DEBUG
#include "ode_include.h"
#include "../xrEngine/StatGraph.h"
#include "PHDebug.h"
#endif
#include "alife_space.h"
#include "hit.h"
#include "PHDestroyable.h"
#include "car.h"
#include "../Include/xrRender/Kinematics.h"
#include "ExtendedGeom.h"
#include "mathutilsode.h"

BOOL rs_car_wheel_friction_direction = false;

CCar::SWheel::SWheelCollisionParams::SWheelCollisionParams()
{
		spring_factor								=1.0f	;
		damping_factor								=1.0f	;
		mu_factor									=1.0f	;
		surface_friction							=1.0f	;
		surface_flotation							=1.0f	;
		surface_material							=nullptr;
		friction_slip_factor						=1.0f	;
		friction2_slip_factor						=1.0f	;
		friction_nitro_factor						=1.0f	;
		current_friction_factor						=1.0f	;
		current_friction2_factor					=1.0f	;
		friction_rain_factor						=1.0f	;
		break_start									=0.f	;
		last_contact_time							=0		;
		slide = false;
		buxe = false;
		breaking = false;
		breaking_abs = false;
		traction = false;
		pwheel = NULL;
}

IC void CCar::SWheel::applywheelCollisionParams(const dxGeomUserData *ud,bool& do_colide,CPHContact& cphc,SGameMtl* material_1,SGameMtl* material_2)
{
	dContact& c = cphc.Native();
	if(ud&&ud->object_callbacks&&ud->object_callbacks->HasCallback(WheellCollisionCallback))
	{
		SWheelCollisionParams	&cp	=		*((SWheelCollisionParams*)(ud->callback_data))	;
		dSurfaceParameters		&sp	=		c.surface										;
		//if(cp.pwheel->drive_wheel)
			sp.mode |= dContactFDir1 | dContactMu2;
		//else
		Fmatrix wheel_bind_xform = cp.pwheel->car->bones_bind_forms[cp.pwheel->bone_id];
		wheel_bind_xform.mulA_43(cp.pwheel->car->XFORM());

		Fvector fdir;
		if (rs_car_wheel_friction_direction || (cp.pwheel->car->b_awd && cp.pwheel->car->e_state_drive == drive))
		{
			fdir.crossproduct(cp.pwheel->joint->PSecond_element()->XFORM().k, wheel_bind_xform.i);
		}
		else
		{
			fdir.set(cp.pwheel->car->XFORM().k);
		}

		// ODE expects fdir1 to be a unit tangent to the contact plane. The car
		// forward/right axes are not tangent on slopes; using them as-is makes
		// the friction rows degenerate and can trigger an LCP solver warning on
		// the first wheel contact after a car is spawned.
		Fvector contact_normal = cast_fv(c.geom.normal);
		if (safe_normalize(contact_normal))
		{
			fdir.mad(contact_normal, -contact_normal.dotproduct(fdir));
			if (!safe_normalize(fdir))
			{
				fdir = cp.pwheel->car->XFORM().i;
				fdir.mad(contact_normal, -contact_normal.dotproduct(fdir));
				if (!safe_normalize(fdir))
				{
					fdir.crossproduct(contact_normal, cp.pwheel->car->XFORM().j);
					if (!safe_normalize(fdir))
					{
						const Fvector axis = _abs(contact_normal.x) < 0.9f ? Fvector().set(1.f, 0.f, 0.f) : Fvector().set(0.f, 1.f, 0.f);
						fdir.crossproduct(contact_normal, axis);
						safe_normalize(fdir);
					}
				}
			}
		}
		else
		{
			safe_normalize(fdir);
		}

		c.fdir1[0] = fdir.x;
		c.fdir1[1] = fdir.y;
		c.fdir1[2] = fdir.z;

		if (!(cp.buxe || cp.breaking || cp.slide))
			cp.pwheel->tire_particle_scale = cp.pwheel->tire_particle_scale + (cp.pwheel->tire_particle_scale_min - cp.pwheel->tire_particle_scale) * 2.f * 0.4f * fixed_step;
		else
			if (cp.buxe)
				cp.pwheel->tire_particle_scale = cp.pwheel->tire_particle_scale + (cp.pwheel->tire_particle_scale_max - cp.pwheel->tire_particle_scale) * 0.4f * fixed_step;
			else
				cp.pwheel->tire_particle_scale = cp.pwheel->tire_particle_scale + (cp.pwheel->tire_particle_scale_max - cp.pwheel->tire_particle_scale) * 0.5f * 0.4f * fixed_step;

		const dxGeomUserData* Geom1UserData = retrieveGeomUserData(c.geom.g1);
		const bool OwnerIsFirst = (ud == Geom1UserData);
		SGameMtl* SurfaceMaterial = OwnerIsFirst ? material_2 : material_1;
		cp.surface_material = SurfaceMaterial;
		cp.surface_flotation = SurfaceMaterial ? SurfaceMaterial->fFlotationFactor : 1.f;

		float MaterialFriction = 1.f;
		if (material_1)
			MaterialFriction *= material_1->fPHFriction;
		if (material_2)
			MaterialFriction *= material_2->fPHFriction;
		cp.surface_friction = MaterialFriction;

		const bool LiquidSurface = SurfaceMaterial && SurfaceMaterial->Flags.test(SGameMtl::flLiquid);
		if (LiquidSurface)
		{
			// Water drag is applied by CCar::UpdateWaterDrag() from the wheel
			// immersion, including the gradual factor for a wheel that only
			// touches the surface. Do not add the full per-surface flotation
			// from vehicles.ltx as a contact override.
			cp.surface_flotation = 1.f;
			cphc.SetFlotation(OwnerIsFirst, SurfaceMaterial, 1.f);
			if (cp.pwheel && cp.pwheel->car)
				cp.pwheel->car->WheelsInWater = true;
		}

		const bool LevelContact = (cphc.GetObject(!OwnerIsFirst) == nullptr);
		const SSurfaceParams* Params = (LevelContact && cp.pwheel && cp.pwheel->car) ? cp.pwheel->car->FindSurfaceParams(SurfaceMaterial) : nullptr;
		if (Params)
		{
			if (Params->HasFriction)
			{
				MaterialFriction = Params->Friction;
				cp.surface_friction = Params->Friction;
			}
			if (Params->HasFlotation && !LiquidSurface)
			{
				cp.surface_flotation = Params->Flotation;
				cphc.SetFlotation(OwnerIsFirst, SurfaceMaterial, Params->Flotation);
			}
		}

		sp.mu						=		MaterialFriction								;
		sp.mu						*=		cp.mu_factor									;
		if (material_2 || !material_1)
			sp.mu						*=		cp.friction_rain_factor * cp.friction_nitro_factor;
		if (cp.pwheel->car && cp.pwheel && material_2 && material_1 && !material_2->Flags.test(SGameMtl::flPassable) && !material_1->Flags.test(SGameMtl::flPassable))
		{
			cp.pwheel->car->m_last_contact_time = Device.dwTimeGlobal;
			cp.pwheel->car->mb_traction = true;
			cp.last_contact_time = Device.dwTimeGlobal;
			cp.traction = true;
			cp.pwheel->Play(cast_fv(c.geom.pos), material_1->GetID(), material_2->GetID(), !!(cp.buxe || cp.breaking || cp.slide));
		}

		if (!cp.pwheel->steer_wheel)
		{
			//if(vel_current > 0.f && vel_total > 0.f && _abs(dJointGetHinge2Param(cp.pwheel->joint->GetDJoint(), dParamVel2)) > 5.0f)

			sp.mu2 = cp.pwheel->car->m_friction2_nonsteering_wheels * sp.mu;//если ведущее то 2 направления трения одно в направлении колеса другое по направлению движения
		}
		else
		{
			sp.mu2 = cp.pwheel->car->m_friction2_steering_wheels * sp.mu;
			if (cp.pwheel->car->brp && (cp.pwheel->wheel_state == wheel_left || cp.pwheel->wheel_state == wheel_right))
				sp.mu2 *= cp.pwheel->car->m_friction2_brp_boost;
		}

		sp.mu *= cp.friction_slip_factor;
		sp.mu2 *= cp.friction2_slip_factor;

		cp.current_friction_factor = sp.mu;
		cp.current_friction2_factor = sp.mu2;
		MulSprDmp(sp.soft_cfm,sp.soft_cfm,cp.spring_factor,cp.damping_factor)				;
	}
}

void  CCar::SWheel::WheellCollisionCallback(bool& do_colide,bool bo1,CPHContact& cphc,SGameMtl* material_1,SGameMtl* material_2)
{
	dContact& c = cphc.Native();
	dxGeomUserData					*ud1			=			retrieveGeomUserData(c.geom.g1)	;
	dxGeomUserData					*ud2			=			retrieveGeomUserData(c.geom.g2)	;

	applywheelCollisionParams		(ud1,do_colide,cphc,material_1,material_2)						;
	applywheelCollisionParams		(ud2,do_colide,cphc,material_1,material_2)						;
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
bool CCar::WheelHit(float P,s16 element,ALife::EHitType hit_type)
{
	xr_map   <u16,SWheel>::iterator i=m_wheels_map.find(element);
	if(i!=m_wheels_map.end())
	{
		i->second.Hit(P);
		return true;
	}
	else return false;
}
void CCar::SWheel::Init()
{
	if(inited) return;
	BONE_P_PAIR_CIT bone= car->bone_map.find(bone_id);
	R_ASSERT2(bone->second.element,"No Element was created for wheel. Check collision is set");
	CPhysicsElement	*e=bone->second.element	;
	CPhysicsJoint	*j=bone->second.joint	;
	radius=e->getRadius()*radius_factor;
	R_ASSERT2(j,"No wheel joint was set for a wheel");
	joint=j;
	joint->SetBackRef(&joint);
	R_ASSERT2(dJointGetType(joint->GetDJoint())==dJointTypeHinge2,"No wheel join was set for a wheel, only wheel-joint valid!!!");
	ApplyDriveAxisVelTorque(0.f,0.f);
	e->add_ObjectContactCallback(WheellCollisionCallback);
	e->set_ContactCallback(NULL);
	e->set_CallbackData((void*)&collision_params);
	e->set_DynamicLimits(default_l_limit, default_w_limit * 100.f);
	e->SetAirResistance(0.0f, 0.0f);
	e->set_ApplyByGravity(FALSE);
	tire_particle_scale = 1.f;


	// The driving loop is cloned from the surface-specific sound in Play().
	inited=true;
	collision_params.pwheel = this;
	// gyroscopic_mode is parsed in Load() for config compatibility, but has no
	// effect: IXRAY's ODE fork does not expose dBodySetGyroscopicMode.
	WheelFlags.set(eWheelType::wheel, TRUE);

	joint->GetJointSDfactors(spring_factor_default, damping_factor_default);
	dJointGetHinge2Axis2(joint->GetDJoint(), (float*)&default_axis2);
}

void CCar::SWheel::Load(LPCSTR section)
{
	IKinematics		*K			=PKinematics(car->Visual())		;
	CInifile		*ini		=K->LL_UserData()				;
	VERIFY						(ini)							;

	suspension_distance = 0.0f;
	suspension_force_inc = 0.0f;
	suspension_spring_factor = 1.0f;
	suspension_damping_factor = 1.0f;
	suspension_inertia = 0.5f;
	radius_factor = 1.f;
	wheel_vel_factor = 1.f;
	wheel_trq_factor = 1.f;
	gyroscopic_mode = false;

	if (ini->section_exist("wheels_params"))
	{
		collision_params.damping_factor = READ_IF_EXISTS(ini, r_float, "wheels_params", "damping_factor", collision_params.damping_factor);
		collision_params.spring_factor = READ_IF_EXISTS(ini, r_float, "wheels_params", "spring_factor", collision_params.spring_factor);
		collision_params.mu_factor = READ_IF_EXISTS(ini, r_float, "wheels_params", "friction_factor", collision_params.mu_factor);
		suspension_distance = READ_IF_EXISTS(ini, r_float, "wheels_params", "suspension_distance", suspension_distance);
		suspension_force_inc = READ_IF_EXISTS(ini, r_float, "wheels_params", "suspension_force_inc", suspension_force_inc);
		suspension_inertia = READ_IF_EXISTS(ini, r_float, "wheels_params", "suspension_inertia", suspension_inertia);
		suspension_spring_factor = READ_IF_EXISTS(ini, r_float, "wheels_params", "suspension_spring_factor", suspension_spring_factor);
		suspension_damping_factor = READ_IF_EXISTS(ini, r_float, "wheels_params", "suspension_damping_factor", suspension_damping_factor);
		radius_factor = READ_IF_EXISTS(ini, r_float, "wheels_params", "radius_factor", radius_factor);
		wheel_vel_factor = READ_IF_EXISTS(ini, r_float, "wheels_params", "velocity_factor", wheel_vel_factor);
		wheel_trq_factor = READ_IF_EXISTS(ini, r_float, "wheels_params", "torque_factor", wheel_trq_factor);
		gyroscopic_mode = READ_IF_EXISTS(ini, r_bool, "wheels_params", "gyroscopic_mode", gyroscopic_mode);
	}
	if(ini->section_exist(section))
	{
		collision_params.damping_factor	=READ_IF_EXISTS(ini,r_float,section,"damping_factor",collision_params.damping_factor);
		collision_params.spring_factor	=READ_IF_EXISTS(ini,r_float,section,"spring_factor",collision_params.spring_factor);
		collision_params.mu_factor		=READ_IF_EXISTS(ini,r_float,section,"friction_factor",collision_params.mu_factor);
		radius_factor = READ_IF_EXISTS(ini, r_float, section, "radius_factor", radius_factor);
		wheel_vel_factor = READ_IF_EXISTS(ini, r_float, section, "velocity_factor", wheel_vel_factor);
		wheel_trq_factor = READ_IF_EXISTS(ini, r_float, section, "torque_factor", wheel_trq_factor);
		gyroscopic_mode = READ_IF_EXISTS(ini, r_bool, section, "gyroscopic_mode", gyroscopic_mode);
		wheel_diff_power = 1.f;
		suspension_distance = READ_IF_EXISTS(ini, r_float, section, "suspension_distance", suspension_distance);
		suspension_force_inc = READ_IF_EXISTS(ini, r_float, section, "suspension_force_inc", suspension_force_inc);
		suspension_inertia = READ_IF_EXISTS(ini, r_float, "wheels_params", "suspension_inertia", suspension_inertia);
		suspension_spring_factor = READ_IF_EXISTS(ini, r_float, section, "suspension_spring_factor", suspension_spring_factor);
		suspension_damping_factor = READ_IF_EXISTS(ini, r_float, section, "suspension_damping_factor", suspension_damping_factor);
	} 
	if (!ini->line_exist(section, "bind_position_add") && ini->line_exist("wheels_params", "bind_position_add"))
	{
		xr_vector<Fmatrix> BindGlobals;
		K->LL_GetBindTransform(BindGlobals);
		car->ApplyBindPositionAdd(K, BindGlobals, bone_id, ini->r_fvector3("wheels_params", "bind_position_add"));
	}
	if (!ini->line_exist(section, "bind_direction_add") && ini->line_exist("wheels_params", "bind_direction_add"))
		car->ApplyBindDirectionAdd(K, bone_id, ini->r_fvector3("wheels_params", "bind_direction_add"));
	clamp(suspension_inertia, 0.f, 1.f);
	tire_trail_wallmark_size = READ_IF_EXISTS(pSettings, r_float, car->cNameSect_str(), "tire_trail_wallmark_size", 0.15f);
	tire_trail_simulars_factor = READ_IF_EXISTS(pSettings, r_float, car->cNameSect_str(), "tire_trail_similars",  0.005f);
	tire_trail_wallmark_avlbl = READ_IF_EXISTS(pSettings, r_bool, car->cNameSect_str(), "tire_trails", true);
	tire_particle_scale_min = READ_IF_EXISTS(pSettings, r_float, car->cNameSect_str(), "tire_particle_scale_min", 1.f);
	tire_particle_scale_max = READ_IF_EXISTS(pSettings, r_float, car->cNameSect_str(), "tire_particle_scale_max", 3.f);
	tire_sounds = READ_IF_EXISTS(pSettings, r_bool, car->cNameSect_str(), "tire_sounds", true);
}

void CCar::SWheel::UpdateSounds()
{
	BONE_P_PAIR_CIT bone = car->bone_map.find(bone_id);

	if (CPhysicsElement* element = bone->second.element)
	{
		if(tire_drive._p && tire_drive.is_playing())
			tire_drive.set_position(element->mXFORM.c);
	}
}

void CCar::SWheel::Play(Fvector c_pos, u16 m1_id, u16 m2_id, bool tire_fulleffects)
{
	Fvector velocity = zero_vel;
	BONE_P_PAIR_CIT bone = car->bone_map.find(bone_id);

	if (CPhysicsElement* element = bone->second.element)
	{
		element->get_LinearVel(velocity);
		float radius = element->getRadius();
		Fmatrix Wheel_Mtx = element->XFORM();
		Fvector wheel_center = Wheel_Mtx.c;
		Fmatrix wheel_bind_xform = car->bones_bind_forms[bone_id];
		wheel_bind_xform.mulA_43(car->XFORM());
		Fvector wheel_dir = Fvector().crossproduct(Wheel_Mtx.k, wheel_bind_xform.i);
		Wheel_Mtx.k = velocity.magnitude() ? Fvector(velocity).normalize().invert() : wheel_dir.invert();
		Wheel_Mtx.c.set(c_pos);

		SGameMtlPair* mtl_pair = GMLib.GetMaterialPair(GMLib.GetMaterialIdx(m2_id), GMLib.GetMaterialIdx(m1_id));
		if (mtl_pair)
		{
			collide::rq_result	result = GetPickResult(wheel_center, Fvector(car->XFORM().j).invert(), radius + 0.1f, car);
			BOOL reach_ground = !!(result.element >= 0);
			Fvector end_point = Fvector().mad(wheel_center, Fvector(car->XFORM().j).invert(), result.range);

			if (tire_fulleffects && tire_sounds)
			{
				if (car->m_wheels_sound_timer < Device.fTimeGlobal)
					car->m_wheels_sound_timer = Device.fTimeGlobal + car->m_wheels_sound_delay;

				if (car->m_wheels_sound_timer <= Device.fTimeGlobal + (car->m_wheels_sound_delay * 0.5f))
				{
					ref_sound* pSound = car->GetTireSkidSound(m1_id, m2_id);
					if (pSound && pSound->_p)
					{
						Fvector ang_vel;
						element->get_AngularVel(ang_vel);
						float ang = ang_vel.magnitude();
						float volume = 0.5f * 1.f + 0.25f * (ang * radius / car->GetVelocityM());
						clamp(volume, volume, 1.f);

						float vol_start = 0.4f + (Device.fTimeGlobal - car->m_slip_start_time) * 0.5f;
						clamp(vol_start, vol_start, 1.f);
						volume *= vol_start;
						float freq = ::Random.randF(0.89f, 1.11f) * (0.9f + 0.1f * volume);
						pSound->play_no_feedback(car, 0, 0.f, &Wheel_Mtx.c, &volume, &freq);
					}
					car->m_wheels_sound_timer = Device.fTimeGlobal - car->m_wheels_sound_delay;
				}
				if (reach_ground)
				{
					Fmatrix particle_xform = Fmatrix(Wheel_Mtx);
					Fvector particle_c = end_point;
					particle_xform.k.mul(tire_particle_scale);//тут можно немного увеличить или уменьшить партиклы скольжения колеса
					particle_xform.j.mul(tire_particle_scale);//тут можно немного увеличить или уменьшить партиклы скольжения колеса
					particle_xform.i.mul(tire_particle_scale);//тут можно немного увеличить или уменьшить партиклы скольжения колеса
					particle_xform.c = particle_c;

					if (!mtl_pair->CollideParticles.empty())
						Level().ph_commander().add_call(new CPHOnesCondition(), new CCarParticlesPlayCall(particle_xform, velocity, *mtl_pair->CollideParticles[::Random.randI(0, mtl_pair->CollideParticles.size())]));
					else
						Level().ph_commander().add_call(new CPHOnesCondition(), new CCarParticlesPlayCall(particle_xform, velocity, "phy_fx\\generic_body_dust_01"));
				}
			}
			if (tire_sounds)
			{
				float wheelspeedvol = _abs(GetDriveAngleRate()) * radius * 3.6f;
				clamp(wheelspeedvol, 0.0f, 140.f);
				wheelspeedvol /= 140.f;
				wheelspeedvol = sinf(0.5f * PI * powf(wheelspeedvol, 0.5f));
				float wheelspeedfreq = _abs(GetDriveAngleRate()) * radius * 3.6f;
				clamp(wheelspeedfreq, 0.0f, 140.f);
				wheelspeedfreq /= 140.f;
				wheelspeedfreq = sinf(0.5f * PI * powf(wheelspeedfreq, 0.5f));

				clamp(wheelspeedvol, 0.0f, 1.f);
				clamp(wheelspeedfreq, 0.0f, 2.f);

				ref_sound* pSound = car->GetTireSound(m1_id, m2_id);
				if (pSound && pSound->_p)
				{
					if (!tire_drive._p || tire_drive_source != pSound)
					{
						tire_drive.destroy();
						tire_drive.clone(*pSound, st_Effect, sg_SourceType);
						tire_drive_source = pSound;
					}

					if (tire_drive._p)
					{
						if (!tire_drive.is_playing())
							tire_drive.play_at_pos(car, wheel_center, sm_Looped);

						if (tire_drive.is_playing())
						{
							tire_drive.set_volume(wheelspeedvol);
							tire_drive.set_frequency(wheelspeedfreq);
							tire_drive.set_position(wheel_center);
						}
					}
				}
				else if (tire_drive._p)
				{
					tire_drive.stop();
					tire_drive_source = nullptr;
				}
			}
			if (tire_trail_wallmark_avlbl && tire_fulleffects)
			{
				IWallMarkArray* marks = &*mtl_pair->m_pCollideMarks;
				if (marks->empty())
					marks = car->GetTireWallmarks(m1_id, m2_id);

				// Tire tracks belong to the static ground. Do not create
				// skeleton wallmarks on another car or dynamic object: the
				// queued call may outlive that object's visual.
				if (reach_ground && !result.O && marks && !marks->empty())
				{
					CDB::TRI* pTri = Level().ObjectSpace.GetStaticTris().data() + result.element;
					Level().ph_commander().add_call(new CPHOnesCondition(),new CPHWallMarksCallStatic(marks, end_point, tire_trail_wallmark_size, pTri, tire_trail_simulars_factor, Fvector(Wheel_Mtx.k).invert()));
				}
			}
		}
	}
}
void CCar::SWheel::Neutral()
{
	ApplyDriveAxisVelTorque(0.f, car->m_axle_friction);
}
float CCar::SWheel::GetDriveAngleRate()
{
	if (!joint) return 0.0f;
	return dJointGetHinge2Angle2Rate(joint->GetDJoint());
}
float CCar::SWheel::GetSteerAngleRate()
{
	if (!joint) return 0.0f;
	return dJointGetHinge2Angle1Rate(joint->GetDJoint());
}
float CCar::SWheel::GetSteerAngle()
{
	if (!joint) return 0.0f;
	return dJointGetHinge2Angle1(joint->GetDJoint());
}
void CCar::SWheel::ApplyDriveAxisTorque(float torque)
{
	if(!joint) return;
	dJointSetHinge2Param(joint->GetDJoint(), dParamFMax2,torque);//car->m_axle_friction
}
void CCar::SWheel::ApplyDriveAxisVel(float vel)
{
	if(!joint) return;
	dJointSetHinge2Param(joint->GetDJoint(), dParamVel2, vel);
}


void CCar::SWheel::ApplyDriveAxisVelTorque(float vel,float torque)
{
	ApplyDriveAxisVel(vel);
	ApplyDriveAxisTorque(torque);

}
void CCar::SWheel::ApplySteerAxisVel(float vel)
{
	if(!joint) return;
	dJointSetHinge2Param(joint->GetDJoint(), dParamVel, vel);
}

void CCar::SWheel::ApplySteerAxisTorque(float torque)
{
	if(!joint) return;
	dJointSetHinge2Param(joint->GetDJoint(), dParamFMax, torque);
}

void CCar::SWheel::ApplySteerAxisVelTorque(float vel,float torque)
{
	ApplySteerAxisVel(vel);
	ApplySteerAxisTorque(torque);
}

void CCar::SWheel::SetSteerHiLimit(float hi)
{
	if(!joint) return;
	dJointSetHinge2Param(joint->GetDJoint(), dParamHiStop, hi);
}
void CCar::SWheel::SetSteerLoLimit(float lo)
{
	if(!joint) return;
	dJointSetHinge2Param(joint->GetDJoint(), dParamLoStop, lo);
}
void CCar::SWheel::SetSteerLimits(float hi,float lo)
{
	SetSteerHiLimit(hi);
	SetSteerLoLimit(lo);
}

void CCar::SWheel::ApplyDamage(u16 level)
{
	inherited::ApplyDamage(level);
	if(!joint) return;
	if(level == 0 )return;
	float sf,df;
	dJointID dj=joint->GetDJoint();
	switch(level) {

	case 1:
		joint->GetJointSDfactors(sf,df);
		sf/=20.f;df*=4.f;
		joint->SetJointSDfactors(sf,df);
		car->m_damage_particles.PlayWheel1(car,bone_id);
		break;
	case 2:
		
		dVector3 v;
	
		dJointGetHinge2Axis2(dj,v);
		v[0]+=0.1f;v[1]+=0.1f;v[2]+=0.1f;
		accurate_normalize(v);
		dJointSetHinge2Axis2(dj,v[0],v[1],v[2]);
		joint->GetJointSDfactors(sf,df);
		sf/=30.f;df*=8.f;
		joint->SetJointSDfactors(sf,df);
		car->m_damage_particles.PlayWheel2(car,bone_id);
		break;
	default: NODEFAULT;
	}
	
}

void CCar::SWheel::Repair()
{
	const u16 damage_level = m_level_applied;
	inherited::Repair();

	if (joint)
	{
		dJointID dj = joint->GetDJoint();
		if (damage_level >= 1)
		{
			joint->SetJointSDfactors(spring_factor_default, damping_factor_default);
		}
		if (damage_level >= 2)
		{
			dJointSetHinge2Axis2(dj, default_axis2.x, default_axis2.y, default_axis2.z);
		}
	}

	if (damage_level >= 1)
	{
		car->m_damage_particles.StopWheel1(car, bone_id);
	}
	if (damage_level >= 2)
	{
		car->m_damage_particles.StopWheel2(car, bone_id);
	}
}

void CCar::SWheel::SaveNetState(NET_Packet& P)
{
	CSE_ALifeCar::SWheelState ws;
	ws.health=Health();
	ws.write(P);
}

void CCar::SWheel::RestoreNetState(const CSE_ALifeCar::SWheelState& a_state)
{
	SetHealth(a_state.health);
	RestoreEffect();
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void CCar::SWheelDrive::Init()
{
	pwheel->Init();
	gear_factor=pwheel->radius/pwheel->car->m_ref_radius;
	CBoneData& bone_data= smart_cast<IKinematics*>(pwheel->car->Visual())->LL_GetData(u16(pwheel->bone_id));
	switch(bone_data.IK_data.type)
	{
	case jtWheel:
		// Use the bind-space wheel orientation. The physics element transform
		// is already rotated with the car, so the raw k.x flips when a car is
		// loaded facing south and reverses the drive direction.
		pos_fvd = pwheel->car->bones_bind_forms[pwheel->bone_id].k.x;
		break;

	default: NODEFAULT;
	}

	pos_fvd=pos_fvd>0.f ? -1.f : 1.f;
	pwheel->drive_wheel = true;
	pwheel->WheelFlags.set(SWheel::eWheelType::wheel_drive, TRUE);
	pwheel->max_control_velocity = _abs(ASpeedMaximum());
}
void CCar::SWheelDrive::Drive()
{
	float cur_speed = pwheel->car->RefWheelMaxSpeed() / gear_factor;// *(1.f / DiffSpeed());
	pwheel->ApplyDriveAxisVel((pos_fvd*cur_speed)* pwheel->wheel_vel_factor);
}

float CCar::SWheelDrive::ASpeedMaximum()
{
	float cur_speed = pwheel->car->RefWheelMaxSpeedByLastGear() / gear_factor;// *(1.f / DiffSpeed());
	return (pos_fvd*cur_speed)* pwheel->wheel_vel_factor;
}


void CCar::SWheelDrive::UpdatePower()
{	
	float lock = 1.f;
	if (pwheel->car->b_diff_lock)
	{
		float size = float(pwheel->car->m_driving_wheels.size());
		lock = DiffSpeed() - (0.5f * size - 1.f);
		clamp(lock, 0.f, size);
		pwheel->wheel_diff_power = pwheel->wheel_diff_power + (lock - pwheel->wheel_diff_power) * 50.f * fixed_step;
	}
	float Torque = (pwheel->car->RefWheelCurTorque()/gear_factor) * pwheel->wheel_trq_factor * pwheel->wheel_diff_power;
	pwheel->EngineTorque = Torque;
	pwheel->ApplyDriveAxisTorque(Torque);
}
void CCar::SWheelDrive::Neutral()
{
	pwheel->ApplyDriveAxisVelTorque(0.f,pwheel->car->m_axle_friction);
}

float CCar::SWheelDrive::DiffSpeed()
{
	float diff = 0.5f * float(pwheel->car->m_driving_wheels.size()) + (pwheel->car->DriveWheelsMeanAngleRate() - ASpeed()) / pwheel->car->m_max_rpm * pwheel->car->m_current_gear_ratio * pwheel->car->m_diff_lock_power;


	return diff;
}

float CCar::SWheelDrive::ASpeed()
{
	CPhysicsJoint* J=pwheel->joint;
	if(!J) return 0.f;
	return ((dJointGetHinge2Angle2Rate(J->GetDJoint()))*pos_fvd)* gear_factor;//dFabs
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void CCar::SWheelSteer::Init()
{
	IKinematics* pKinematics=smart_cast<IKinematics*>(pwheel->car->Visual());
	pwheel->Init();
	(pwheel->car->bone_map.find(pwheel->bone_id))->second.joint->GetLimits(lo_limit,hi_limit,0);
	CBoneData& bone_data= pKinematics->LL_GetData(u16(pwheel->bone_id));
	switch(bone_data.IK_data.type)
	{
	case jtWheel:	
		pos_right= pwheel->car->bone_map.find(pwheel->bone_id)->second.element->mXFORM.i.y;//.dotproduct(pwheel->car->m_root_transform.j);
		break;

	default: NODEFAULT;
	}
	pwheel->wheel_state = wheel_idle;
	pos_right=pos_right>0.f ? -1.f : 1.f;
	float steering_torque = READ_IF_EXISTS(pKinematics->LL_UserData(), r_float, "car_definition", "steering_torque", 1.0f);
	pwheel->ApplySteerAxisTorque(steering_torque);
	dJointSetHinge2Param(pwheel->joint->GetDJoint(), dParamFudgeFactor, (1.f/steering_torque) * 70.f);
	pwheel->ApplySteerAxisVel(0.f);
	limited=false;
	pwheel->steer_wheel = true;
	pwheel->WheelFlags.set(SWheel::eWheelType::wheel_steering, TRUE);
}

void CCar::SWheelSteer::SteerRight()
{
	pwheel->wheel_state = wheel_right;

	if (pwheel->car->m_steer_invert && hi_limit - pwheel->car->m_steer_threshold < pwheel->GetSteerAngle())
	{
		pwheel->SetSteerLimits(hi_limit, hi_limit);
		pwheel->ApplySteerAxisVel(0.f);
		return;
	}
	if (!pwheel->car->m_steer_invert && lo_limit + pwheel->car->m_steer_threshold > pwheel->GetSteerAngle())
	{
		pwheel->SetSteerLimits(lo_limit, lo_limit);
		pwheel->ApplySteerAxisVel(0.f);
		return;
	}

	float wheel_steer_speed = pwheel->car->m_steering_speed;
	if (pwheel->car->m_steer_nonlinear)
	{
		float time = (Device.fTimeGlobal - pwheel->car->m_steer_start_time) / pwheel->car->m_steering_speed;
		clamp(time, 0.f, 1.f);
		float back = 1.f;
		if (GetSteerAngle() > 0.f && !pwheel->car->m_steer_invert)
		{
			time = 1.f;
			back = 1.5f;
		}
		if (GetSteerAngle() < 0.f && pwheel->car->m_steer_invert)
		{
			time = 1.f;
			back = 1.5f;
		}

		float sliding = 0.85f + pwheel->car->m_current_slip_sliding * 1.5f;
		clamp(sliding, 1.f, 3.f);

		wheel_steer_speed = sliding * back * (0.05f + 1.15f * sinf(0.5f * PI * powf(time, 0.5f)));

		if ((!pwheel->car->m_steer_invert && pos_right < 0.f) || (pwheel->car->m_steer_invert && pos_right > 0.f))
			wheel_steer_speed *= pwheel->car->m_steering_speed;
		else
			wheel_steer_speed *= pwheel->car->m_steering_speed;
	}

	limited=true;						//no need to limit wheels when steering
	if (pwheel->car->m_steer_invert)
	{
		pwheel->SetSteerLimits(hi_limit, lo_limit);
		pwheel->ApplySteerAxisVel(wheel_steer_speed);
	}
	else
	{
		pwheel->SetSteerLimits(hi_limit, lo_limit);
		pwheel->ApplySteerAxisVel( -wheel_steer_speed);
	}
}
void CCar::SWheelSteer::SteerLeft()
{
	pwheel->wheel_state = wheel_left;

	if (!pwheel->car->m_steer_invert && hi_limit - pwheel->car->m_steer_threshold < pwheel->GetSteerAngle())
	{
		pwheel->SetSteerLimits(hi_limit, hi_limit);
		pwheel->ApplySteerAxisVel(0.f);
		return;
	}
	if (pwheel->car->m_steer_invert && lo_limit + pwheel->car->m_steer_threshold > pwheel->GetSteerAngle())
	{
		pwheel->SetSteerLimits(lo_limit, lo_limit);
		pwheel->ApplySteerAxisVel(0.f);
		return;
	}

	float wheel_steer_speed = pwheel->car->m_steering_speed;
	if (pwheel->car->m_steer_nonlinear)
	{
		float time = (Device.fTimeGlobal - pwheel->car->m_steer_start_time) / pwheel->car->m_steering_speed;
		clamp(time, 0.f, 1.f);
		float back = 1.f;
		if (GetSteerAngle() < 0.f && !pwheel->car->m_steer_invert)
		{
			time = 1.f;
			back = 1.5f;
		}
		if (GetSteerAngle() > 0.f && pwheel->car->m_steer_invert)
		{
			time = 1.f;
			back = 1.5f;
		}

		float sliding = 0.85f + pwheel->car->m_current_slip_sliding * 1.5f;
		clamp(sliding, 1.f, 3.f);

		wheel_steer_speed = sliding * back * (0.05f + 1.15f * sinf(0.5f * PI * powf(time, 0.5f)));

		if ((!pwheel->car->m_steer_invert && pos_right < 0.f) || (pwheel->car->m_steer_invert && pos_right > 0.f))
			wheel_steer_speed *= pwheel->car->m_steering_speed;
		else
			wheel_steer_speed *= pwheel->car->m_steering_speed;
	}
		
	limited=true;						//no need to limit wheels when steering
	if(!pwheel->car->m_steer_invert)
	{
		pwheel->SetSteerLimits(hi_limit, lo_limit);
		pwheel->ApplySteerAxisVel(wheel_steer_speed);
	}
	else
	{
		pwheel->SetSteerLimits(hi_limit, lo_limit);
		pwheel->ApplySteerAxisVel( -wheel_steer_speed);
	}
}
void CCar::SWheelSteer::SteerIdle()
{
	pwheel->wheel_state = wheel_idle;

	if (-pwheel->car->m_steer_threshold < pwheel->GetSteerAngle() && pwheel->GetSteerAngle() < pwheel->car->m_steer_threshold)
	{
		pwheel->SetSteerLimits(0.f, 0.f);
		pwheel->ApplySteerAxisVel(0.f);
		return;
	}

	limited = false;
	float wheel_steer_speed = pwheel->car->m_steering_speed;
	if (pwheel->car->m_steer_nonlinear)
	{
		float cur_steer = 0.f;
		if (hi_limit > 0.f)
			cur_steer = (fabsf(GetSteerAngle()) / hi_limit) * (1.f / 0.4f);
		clamp(cur_steer, cur_steer, 1.f);
		float return_speed = 0.05f + 0.95f * cur_steer;

		float time = 2.f * (Device.fTimeGlobal - pwheel->car->m_steer_start_time) / pwheel->car->m_steering_speed;
		clamp(time, 0.f, 0.8f);
		wheel_steer_speed = (0.1f + 1.1f * sinf(1.5f * powf(time, 0.5f)));
		//float sliding = 0.9f + pwheel->car->m_current_slip_sliding * 2.5f; // replace with angular acceleration
		Fvector ang_vel;
		pwheel->car->m_pPhysicsShell->get_AngularVel(ang_vel);
		float angular = (ang_vel.magnitude() - 0.1f) * 2.f;
		clamp(angular, 1.f, 5.f);

		float spd = 1.f + (_abs(pwheel->GetDriveAngleRate()) * pwheel->radius * 0.03f);
		wheel_steer_speed = wheel_steer_speed * pwheel->car->m_steering_speed * angular * spd * return_speed;
	}

	if (pwheel->GetSteerAngle() < 0.f)
	{
		pwheel->SetSteerLimits(0.f, lo_limit + 0.01f);
		pwheel->ApplySteerAxisVel(wheel_steer_speed);
	}
	else
	{
		pwheel->SetSteerLimits(hi_limit - 0.01f, 0.f);
		pwheel->ApplySteerAxisVel(-wheel_steer_speed);
	}

}

void CCar::SWheelSteer::Limit()
{
	CPhysicsJoint* J=pwheel->joint;
	if(!J) return;
	pwheel->wheel_state = wheel_limit;
	dJointID joint=J->GetDJoint();
	if(!limited)
	{
		dReal angle = dJointGetHinge2Angle1(joint);
		if(dFabs(angle)<M_PI/180.f)
		{
			pwheel->SetSteerLimits(0.f,0.f);
			pwheel->ApplySteerAxisVel(0.f);
			limited=true;
		}
	}
	pwheel->car->b_wheels_limited=pwheel->car->b_wheels_limited&&limited;
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void CCar::SWheelBreak::Init()
{
	pwheel->Init();
	float k=pwheel->radius/pwheel->car->m_ref_radius;

	break_torque			*=k;
	hand_break_torque		*=k;
	pwheel->break_wheel = true;
	pwheel->WheelFlags.set(SWheel::eWheelType::wheel_breaking, TRUE);
}
void CCar::SWheelBreak::Load(LPCSTR section)
{
	IKinematics		*K			=PKinematics(pwheel->car->Visual())												;
	CInifile		*ini		=K->LL_UserData()																;
	VERIFY						(ini)																			;
	break_torque		=		READ_IF_EXISTS(ini, r_float, "car_definition", "break_torque", break_torque)	;
	hand_break_torque	=		READ_IF_EXISTS(ini,r_float,"car_definition","hand_break_torque",break_torque)	;
	if(ini->section_exist(section))
	{	
		break_torque					=READ_IF_EXISTS(ini,r_float,section,"break_torque",break_torque);
		hand_break_torque				=READ_IF_EXISTS(ini,r_float,section,"hand_break_torque",hand_break_torque);
	}
}
void CCar::SWheelBreak::Break(float k)
{
	float current_wheel_block_factor = 100000.f * break_torque * k;
	float max_wheel_block_factor = 100000.f * hand_break_torque;
	if (!pwheel->car->brp)
	{
		if (!pwheel->car->m_break_abs_individual || !pwheel->car->m_break_abs)
		{
			pwheel->BrakeTorque = current_wheel_block_factor;
			pwheel->ApplyDriveAxisVelTorque(0.f, current_wheel_block_factor);
			if (pwheel->collision_params.breaking_abs && pwheel->car->m_break_abs)
			{
				if (Device.fTimeGlobal - pwheel->car->m_break_start > pwheel->car->m_break_time)
					pwheel->car->m_break_start = Device.fTimeGlobal - pwheel->car->m_break_time;
				if (pwheel->car->m_break_time > 0.f)
				{
					pwheel->car->m_break_start += 0.015f * pwheel->car->m_break_abs_power * fixed_step * FixedStepCompensation;
					if (pwheel->car->m_break_start > Device.fTimeGlobal)
						pwheel->car->m_break_start = Device.fTimeGlobal;
				}
			}
		}
		else
		{
			float n = 1.f;
			float time = (Device.fTimeGlobal - pwheel->collision_params.break_start);
			if (time < pwheel->car->m_break_time)
			{
				n *= (time / pwheel->car->m_break_time);
			}
			float current_wheel_individual_block_factor = 100000.f * break_torque * n;
			pwheel->BrakeTorque = current_wheel_individual_block_factor;
			pwheel->ApplyDriveAxisVelTorque(0.f, current_wheel_individual_block_factor);
			if (Device.fTimeGlobal - pwheel->collision_params.break_start > pwheel->car->m_break_time)
				pwheel->collision_params.break_start = Device.fTimeGlobal - pwheel->car->m_break_time;
			if (pwheel->car->m_break_time > 0.f && pwheel->collision_params.breaking_abs)
			{
				pwheel->collision_params.break_start += 0.03f * pwheel->car->m_break_abs_power * fixed_step * FixedStepCompensation;
				if (pwheel->collision_params.break_start > Device.fTimeGlobal)
					pwheel->collision_params.break_start = Device.fTimeGlobal;
			}
		}
	}
	else
	{
		pwheel->BrakeTorque = current_wheel_block_factor + max_wheel_block_factor;
		pwheel->ApplyDriveAxisVelTorque(0.f, current_wheel_block_factor + max_wheel_block_factor);
	}
}

void CCar::SWheelBreak::HandBreak(float k)
{
	float current_wheel_block_factor = 100000.f * break_torque * k;
	float max_wheel_block_factor = 100000.f * hand_break_torque;
	if (!pwheel->car->b_breaks)
	{
		pwheel->BrakeTorque = max_wheel_block_factor;
		pwheel->ApplyDriveAxisVelTorque(0.f, max_wheel_block_factor);
	}
	else
	{
		pwheel->BrakeTorque = max_wheel_block_factor + current_wheel_block_factor;
		pwheel->ApplyDriveAxisVelTorque(0.f, max_wheel_block_factor + current_wheel_block_factor);
	}
}

void CCar::SWheelBreak::Neutral()
{
	pwheel->ApplyDriveAxisVelTorque(0.f, pwheel->car->m_axle_friction);
}
