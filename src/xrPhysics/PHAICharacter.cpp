#include "stdafx.h"

#include "PHDynamicData.h"
#include "Physics.h"
#include "ExtendedGeom.h"
#include "../xrEngine/cl_intersect.h"
#include "tri-colliderKNoOPC\__aabb_tri.h"

#include "phaicharacter.h"
#include "../xrengine/device.h"

#ifdef DEBUG
#	include "debug_output.h"
#endif

CPHAICharacter::CPHAICharacter()
{
	m_forced_physics_control=false;
}

void CPHAICharacter::Create(dVector3 sizes)
{
	inherited::Create(sizes);
	m_forced_physics_control=false;
}

bool CPHAICharacter::TryPosition(Fvector pos, bool exact_state)
{
	if (!b_exist)
		return false;

	if (m_forced_physics_control || JumpState())
		return false;

	if (DoCollideObj())	return false;
	Fvector	current_pos;
	GetPosition(current_pos);
	Fvector	cur_vel; GetVelocity(cur_vel);

	Fvector	displace; displace.sub(pos, current_pos);
	float	disp_mag = displace.magnitude();

	if (fis_zero(disp_mag) || fis_zero(Device.fTimeDelta))
		return true;

	const	u32		max_steps = 15;
	const	float	fmax_steps = float(max_steps);
	float	fsteps_num = 1.f;
	u32		steps_num = 1;
	float	disp_pstep = FootRadius();
	float	rest = 0.f;

	float	parts = disp_mag / disp_pstep;
	fsteps_num = floor(parts);
	steps_num = iFloor(parts);
	if (steps_num > max_steps)
	{
		steps_num = max_steps;
		fsteps_num = fmax_steps;
		disp_pstep = disp_mag / fsteps_num;
	}
	rest = disp_mag - fsteps_num * disp_pstep;

	Fvector	vel; vel.mul(displace, disp_pstep / fixed_step / disp_mag);
	bool	ret = true;
	int save_gm = dBodyGetGravityMode(m_body);
	dBodySetGravityMode(m_body, 0);
	for (u32 i = 0; steps_num > i; ++i)
	{
		SetVelocity(vel);
		Enable();
		if (!step_single(fixed_step))
		{
			SetVelocity(cur_vel);
			ret = false;
			break;
		}
	}

	vel.mul(displace, rest / fixed_step / disp_mag);
	SetVelocity(vel);
	Enable();
	ret = step_single(fixed_step);


	dBodySetGravityMode(m_body, save_gm);
	SetVelocity(cur_vel);
	Fvector	pos_new; GetPosition(pos_new);

	SetPosition(pos_new);
	m_last_move.sub(pos_new, current_pos).mul(1.f / Device.fTimeDelta);
	m_body_interpolation.UpdatePositions();
	m_body_interpolation.UpdatePositions();

	if (ret)
		Disable();

	m_collision_damage_info.m_contact_velocity = 0.f;
	return ret;
}

void CPHAICharacter::Jump(const Fvector& jump_velocity)
{
	b_jump = true;
	m_jump_accel.set(jump_velocity);
}

void CPHAICharacter::ValidateWalkOn()
{
	inherited::ValidateWalkOn();
}

void CPHAICharacter::InitContact(dContact* c, bool& do_collide, u16 material_idx_1, u16 material_idx_2)
{
	SGameMtl* material_1 = GMLibrary().GetMaterialByIdx(material_idx_1);
	SGameMtl* material_2 = GMLibrary().GetMaterialByIdx(material_idx_2);
	if ((material_1 && material_1->Flags.test(SGameMtl::flActorObstacle)) || (material_2 && material_2->Flags.test(SGameMtl::flActorObstacle)))
		do_collide = true;
	inherited::InitContact(c, do_collide, material_idx_1, material_idx_2);
	if (is_control || b_lose_control || b_jumping)
		c->surface.mu = 0.00f;
	dxGeomUserData* D1 = retrieveGeomUserData(c->geom.g1);
	dxGeomUserData* D2 = retrieveGeomUserData(c->geom.g2);
	if (D1 && D2 && D1->ph_object && D2->ph_object && D1->ph_object->CastType() == tpCharacter && D2->ph_object->CastType() == tpCharacter)
	{
		b_on_object = true;
		b_valide_wall_contact = false;
	}
#ifdef DEBUG
	if (debug_output().ph_dbg_draw_mask().test(phDbgNeverUseAiPhMove))do_collide = false;
#endif
}

#ifdef DEBUG_DRAW
void CPHAICharacter::OnRender()	
{
	inherited::OnRender();
#if	0
	if(!b_exist) return;

	Fvector pos;
	GetDesiredPosition(pos);
	pos.y+=m_radius;


	Fvector scale;
	scale.set(0.35f,0.35f,0.35f);
	Fmatrix M;
	M.identity();
	M.scale(scale);
	M.c.set(pos);


	Level().debug_renderer().draw_ellipse(M, 0xffffffff);
#endif
}
#endif