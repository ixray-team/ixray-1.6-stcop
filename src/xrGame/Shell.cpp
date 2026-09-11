#include "StdAfx.h"

#include "Shell.h"

#include "Level.h"
#include "Weapon.h"
#include "HUDManager.h"
#include "PhysicsShellHolder.h"
#include "../xrEngine/xr_ioc_cmd.h"
#include "../xrPhysics/PHShell.h"
#include "../xrPhysics/ExtendedGeom.h"
#include "../xrPhysics/PhysicsExternalCommon.h"
#include "../xrEngine/GameMtlLib.h"

bool CShell::net_Spawn(CSE_Abstract* e)
{
	if (!inherited::net_Spawn(e))
	{
		return false;
	}

	if (CSE_Shell* se = smart_cast<CSE_Shell*>(e))
	{
		params.weapon_id = se->weapon_id;
		params.dir = se->eject_dir;
		params.lin_vel = se->parent_vel;
		params.speed = se->eject_speed;
		params.dispersion = se->eject_dispersion_angle;
	}

	bounce = .5f;
	bounce_vel = 1.5f;

	CPhysicsShell* ph_shell = PPhysicsShell();
	ph_shell->DisableCharacterCollision();
	ph_shell->SetAirResistance(0.f, 0.f);
	ph_shell->set_DynamicLimits(default_l_limit, 100.f);
	ph_shell->SetSmall();
	ph_shell->set_ObjectContactCallback(ContactCallback);
	need_eject = true;

	return true;
}

void CShell::UpdateCL()
{
	if (need_eject)
	{
		Eject();
		need_eject = false;
	}

	inherited::UpdateCL();
}

void CShell::PH_A_CrPr()
{
}

void CShell::PH_B_CrPr()
{
}

void CShell::ContactCallback(bool& do_collide, bool bo1, dContact& c, SGameMtl* /*material_1*/, SGameMtl* /*material_2*/)
{
	dxGeomUserData* data = retrieveGeomUserData(bo1 ? c.geom.g1 : c.geom.g2);

	if (data == nullptr || data->ph_ref_object == nullptr)
	{
		return;
	}

	CShell* shell = smart_cast<CShell*>(data->ph_ref_object);

	if (shell == nullptr)
	{
		return;
	}

	c.surface.bounce = shell->bounce;
	c.surface.bounce_vel = shell->bounce_vel;
	c.surface.mode |= dContactBounce;
}

void CShell::Eject()
{
	if (CPhysicsShell* physic_shell = PPhysicsShell())
	{
		Fvector impulse_point, impulse_dir, impulse_eject_offset;

		impulse_point.set(sin(Random.randF(PI_DIV_8, PI_DIV_3)), 0.f, cos(Random.randF(PI_DIV_8, PI_DIV_3)));

		impulse_dir.set(params.dir);
		impulse_eject_offset.random_dir(impulse_dir, deg2rad(params.dispersion));
		impulse_dir.add(impulse_eject_offset);
		impulse_dir.normalize();

		impulse_dir.mul(params.speed);
		impulse_dir.add(Fvector().set(params.lin_vel).mul(physic_shell->getMass()));

		if (auto p_sync_obj = PHGetSyncItem(0))
		{
			if (auto object = Level().Objects.net_Find(params.weapon_id))
			{
				if (auto weapon = object->cast_weapon())
				{
					SPHNetState state;
					p_sync_obj->get_State(state);
					state.position = weapon->get_CurrentShellPoint(true);
					state.previous_position = Position();
					p_sync_obj->set_State(state);
				}
			}
		}

		physic_shell->applyImpulseTrace(impulse_point, impulse_dir, 1.f);
		Level().ShellManager().Push(this);
	}
}
