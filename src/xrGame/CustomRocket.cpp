//////////////////////////////////////////////////////////////////////
// CustomRocket.cpp:	ракета, которой стреляет RocketLauncher 
//						(умеет лететь, светиться и отыгрывать партиклы)
//////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "CustomRocket.h"
#include "ParticlesObject.h"
#include "../xrPhysics/PhysicsShell.h"
#include "../xrPhysics/ExtendedGeom.h"
#include "../xrPhysics/CalculateTriangle.h"
#include "../xrPhysics/tri-colliderknoopc/dcTriangle.h"

#include "Level.h"
#include "xrMessages.h"
#include "../xrEngine/GameMtlLib.h"
#include "../Include/xrRender/RenderVisual.h"
#include "Actor.h"
#ifdef DEBUG
#include "PHDebug.h"
#include "game_base_space.h"
#endif

#define CHOOSE_MAX(x,inst_x,y,inst_y,z,inst_z)\
	if(x>y)\
	if(x>z){inst_x;}\
		else{inst_z;}\
	else\
	if(y>z){inst_y;}\
		else{inst_z;}

CCustomRocket::CCustomRocket()
{
	m_LaunchXForm.identity();
}

CCustomRocket::~CCustomRocket()
{
	m_pTrailLight.destroy();
}

void CCustomRocket::reinit()
{
	inherited::reinit();

	m_pTrailLight.destroy();
	m_pTrailLight = ::Render->light_create();
	m_pTrailLight->set_shadow(true);

	m_pEngineParticles = nullptr;
	m_pFlyParticles = nullptr;

	m_pOwner = nullptr;

	m_vPrevVel.set(0, 0, 0);
}

BOOL CCustomRocket::net_Spawn(CSE_Abstract* DC)
{
	m_eState = eInactive;
	BOOL result = inherited::net_Spawn(DC);
	m_LaunchXForm.set(XFORM());
	SpatialComponent->spatial.type |= ESPATIAL_TYPE::ROCKET;
	return result;
}

void CCustomRocket::net_Destroy()
{
	CPHUpdateObject::Deactivate();
	inherited::net_Destroy();

	StopEngine();
}

void CCustomRocket::SetLaunchParams(const Fmatrix& xform, const Fvector& vel, const Fvector& angular_vel)
{
	VERIFY2(_valid(xform), "SetLaunchParams. Invalid xform argument!");
	m_LaunchXForm = xform;
	m_vLaunchVelocity = vel;

	m_vLaunchAngularVelocity = angular_vel;
	m_time_to_explode = Device.fTimeGlobal + pSettings->r_float(cNameSect(), "force_explode_time") / 1000.0f;
#ifdef DEBUG
	gbg_rocket_speed1 = 0.0f;
	gbg_rocket_speed2 = 0.0f;
#endif	
}

void CCustomRocket::activate_physic_shell()
{
	R_ASSERT(H_Parent());
	R_ASSERT(!m_pPhysicsShell);
	create_physic_shell();

	R_ASSERT(m_pPhysicsShell);
	if (m_pPhysicsShell->isActive())
	{
		return;
	}

	VERIFY2(_valid(m_LaunchXForm), "CCustomRocket::activate_physic_shell. Invalid m_LaunchXForm!");

	m_pPhysicsShell->Activate(m_LaunchXForm, m_vLaunchVelocity, m_vLaunchAngularVelocity);
	m_pPhysicsShell->Update();

	XFORM().set(m_pPhysicsShell->mXFORM);
	Position().set(m_pPhysicsShell->mXFORM.c);
	m_pPhysicsShell->set_PhysicsRefObject(this);
	m_pPhysicsShell->set_ObjectContactCallback(ObjectContactCallback);
	m_pPhysicsShell->set_ContactCallback(nullptr);
	m_pPhysicsShell->SetAirResistance(0.0f, 0.0f);
	m_pPhysicsShell->set_DynamicScales(1.0f, 1.0f);
	m_pPhysicsShell->SetAllGeomTraced();
}

void CCustomRocket::create_physic_shell()
{
	R_ASSERT(!m_pPhysicsShell);
	Fobb obb;
	Visual()->getVisData().box.get_CD(obb.m_translate, obb.m_halfsize);
	obb.m_rotate.identity();

	// Physics (Elements)
	CPhysicsElement* E = P_create_Element();
	R_ASSERT(E);

	Fvector ax = zero_vel;
	float radius = 0.0f;
	CHOOSE_MAX(
		obb.m_halfsize.x, ax.set(obb.m_rotate.i); ax.mul(obb.m_halfsize.x); radius = std::min(obb.m_halfsize.y, obb.m_halfsize.z); obb.m_halfsize.y /= 2.0f; obb.m_halfsize.z /= 2.0f,
		obb.m_halfsize.y, ax.set(obb.m_rotate.j); ax.mul(obb.m_halfsize.y); radius = std::min(obb.m_halfsize.x, obb.m_halfsize.z); obb.m_halfsize.x /= 2.0f; obb.m_halfsize.z /= 2.0f,
		obb.m_halfsize.z, ax.set(obb.m_rotate.k); ax.mul(obb.m_halfsize.z); radius = std::min(obb.m_halfsize.y, obb.m_halfsize.x); obb.m_halfsize.y /= 2.0f; obb.m_halfsize.x /= 2.0f
	)

	Fsphere	sphere1, sphere2;
	sphere1.P.add(obb.m_translate, ax);
	sphere1.R = radius * 1.4142f;

	sphere2.P.sub(obb.m_translate, ax);
	sphere2.R = radius / 2.0f;

	E->add_Box(obb);
	E->add_Sphere(sphere1);
	E->add_Sphere(sphere2);

	// Physics (Shell)
	m_pPhysicsShell = P_create_Shell();
	R_ASSERT(m_pPhysicsShell);
	m_pPhysicsShell->add_Element(E);
	m_pPhysicsShell->setMass(7.f);
	m_pPhysicsShell->SetAirResistance();
}

//////////////////////////////////////////////////////////////////////////
// Rocket specific functions
//////////////////////////////////////////////////////////////////////////


void CCustomRocket::ObjectContactCallback(bool& do_colide, bool bo1, dContact& c, SGameMtl* material_1, SGameMtl* material_2)
{
	do_colide = false;

	dxGeomUserData* l_pUD1 = nullptr;
	dxGeomUserData* l_pUD2 = nullptr;
	l_pUD1 = PHRetrieveGeomUserData(c.geom.g1);
	l_pUD2 = PHRetrieveGeomUserData(c.geom.g2);

	SGameMtl* material = 0;
	CCustomRocket* l_this = l_pUD1 != nullptr ? smart_cast<CCustomRocket*>(l_pUD1->ph_ref_object) : nullptr;
	Fvector vUp;
	if (!l_this)
	{
		l_this = l_pUD2 ? smart_cast<CCustomRocket*>(l_pUD2->ph_ref_object) : nullptr;
		vUp.invert(*(Fvector*)&c.geom.normal);
		material = material_1;

	}
	else
	{
		vUp.set(*(Fvector*)&c.geom.normal);
		material = material_2;

	}

	VERIFY(material);
	if (material->Flags.is(SGameMtl::flPassable))
	{
		return;
	}

	if (!l_this || l_this->m_contact.contact)
	{
		return;
	}

	CGameObject* l_pOwner = l_pUD1 ? smart_cast<CGameObject*>(l_pUD1->ph_ref_object) : nullptr;
	if (!l_pOwner || l_pOwner == (CGameObject*)l_this)
	{
		l_pOwner = l_pUD2 ? smart_cast<CGameObject*>(l_pUD2->ph_ref_object) : nullptr;
	}

	if (!l_pOwner || l_pOwner != l_this->m_pOwner)
	{
		if (l_this->m_pOwner)
		{
			Fvector l_pos; l_pos.set(l_this->Position());
			dxGeomUserData* l_pMYU = bo1 ? l_pUD1 : l_pUD2;
			VERIFY(l_pMYU);
			if (l_pMYU->last_pos[0] != -dInfinity)
			{
				l_pos = cast_fv(l_pMYU->last_pos);
			}
#ifdef DEBUG
			bool corrected_pos = false;
#endif
			if (!l_pUD1 || !l_pUD2)
			{
				dGeomID g = nullptr;
				dxGeomUserData*& l_pUD = l_pUD1 ? l_pUD1 : l_pUD2;
				if (l_pUD1)
				{
					g = c.geom.g1;
				}
				else
				{
					g = c.geom.g2;
				}

				if (l_pUD->pushing_neg)
				{
					Fvector velocity;
					l_this->PHGetLinearVell(velocity);
					if (velocity.square_magnitude() > EPS)
					{	//. desync?
						velocity.normalize();
						Triangle neg_tri;
						CalculateTriangle(l_pUD->neg_tri, g, neg_tri, Level().ObjectSpace.GetStaticVerts().data());
						float cosinus = velocity.dotproduct(*((Fvector*)neg_tri.norm));
						VERIFY(_valid(neg_tri.dist));
						float dist = neg_tri.dist / cosinus;
						velocity.mul(dist * 1.1f);
						l_pos.sub(velocity);
#ifdef DEBUG
						corrected_pos = true;
#endif
					}
				}
			}
#ifdef DEBUG
			if (ph_dbg_draw_mask.test(phDbgDrawExplosionPos))
			{
				DBG_DrawPoint(l_pos, 0.05f, color_xrgb(255, 255, (!corrected_pos) * 255));
		}
#endif

			l_this->Contact(l_pos, vUp);

			R_ASSERT(l_this->m_pPhysicsShell);
			l_this->m_pPhysicsShell->DisableCollision();
			l_this->m_pPhysicsShell->set_LinearVel(zero_vel);
			l_this->m_pPhysicsShell->set_AngularVel(zero_vel);
			l_this->m_pPhysicsShell->setForce(zero_vel);
			l_this->m_pPhysicsShell->setTorque(zero_vel);
			l_this->m_pPhysicsShell->set_ApplyByGravity(false);
			l_this->setEnabled(FALSE);

		}
	}
}

void CCustomRocket::Load(LPCSTR section)
{
	inherited::Load(section);

	reload(section);
}

void CCustomRocket::reload(LPCSTR section)
{
	inherited::reload(section);
	m_eState = eInactive;

	m_bEnginePresent = !!pSettings->r_bool(section, "engine_present");
	if (m_bEnginePresent)
	{
		m_dwEngineWorkTime = pSettings->r_u32(section, "engine_work_time");
		m_fEngineImpulse = pSettings->r_float(section, "engine_impulse");
		m_fEngineImpulseUp = pSettings->r_float(section, "engine_impulse_up");
	}

	m_bLightsEnabled = !!pSettings->r_bool(section, "lights_enabled");
	if (m_bLightsEnabled)
	{
		m_TrailLightColor = pSettings->r_fcolor(section, "trail_light_color");
		m_fTrailLightRange = pSettings->r_float(section, "trail_light_range");
	}

	if (pSettings->line_exist(section, "engine_particles"))
	{
		m_sEngineParticles = pSettings->r_string(section, "engine_particles");
	}

	if (pSettings->line_exist(section, "fly_particles"))
	{
		m_sFlyParticles = pSettings->r_string(section, "fly_particles");
	}

	if (pSettings->line_exist(section, "snd_fly_sound"))
	{
		m_flyingSound.create(pSettings->r_string(section, "snd_fly_sound"), st_Effect, sg_SourceType);
	}
}

void CCustomRocket::Contact(const Fvector& pos, const Fvector& normal)
{
	m_contact.contact = true;
	m_contact.pos.set(pos);
	m_contact.up.set(normal);
}

void CCustomRocket::PlayContact()
{
	if (!m_contact.contact)
	{
		return;
	}

	if (eCollide == m_eState)
	{
		return;
	}

	StopEngine();

	m_eState = eCollide;

	//дективировать физическую оболочку,чтоб ракета не летела дальше
	if (m_pPhysicsShell)
	{
		m_pPhysicsShell->set_LinearVel(zero_vel);
		m_pPhysicsShell->set_AngularVel(zero_vel);
		m_pPhysicsShell->set_ObjectContactCallback(nullptr);
		m_pPhysicsShell->Disable();
	}

	Position().set(m_contact.pos);
	m_contact.contact = false;
}

void CCustomRocket::OnH_B_Chield()
{
	VERIFY(m_eState == eInactive);
	inherited::OnH_B_Chield();
}

void CCustomRocket::OnH_A_Chield()
{
	VERIFY(m_eState == eInactive);
	inherited::OnH_A_Chield();
}

void CCustomRocket::OnH_B_Independent(bool just_before_destroy)
{
	inherited::OnH_B_Independent(just_before_destroy);

	m_pOwner = H_Parent() != nullptr ? H_Parent()->H_Root()->cast_game_object() : nullptr;
}

void CCustomRocket::OnH_A_Independent()
{
	inherited::OnH_A_Independent();

	if (!g_pGameLevel->bReady || !m_bLaunched)
	{
		return;
	}

	setVisible(true);
	StartEngine();
}

void CCustomRocket::UpdateCL()
{
	inherited::UpdateCL();

	PlayContact();

	if (m_eState == eEngine || m_eState == eFlying)
	{
		if (m_flyingSound.is_playing())
			m_flyingSound.set_position(XFORM().c);

		UpdateLights();
		UpdateParticles();

		if (m_time_to_explode < Device.fTimeGlobal)
		{
			Contact(Position(), Direction());
		}
	}
}

void CCustomRocket::StartEngine()
{
	VERIFY(nullptr == H_Parent());

	if (!m_bEnginePresent)
	{
		m_eState = eFlying;
		return;
	}

	m_eState = eEngine;
	m_dwEngineTime = m_dwEngineWorkTime;

	if (m_flyingSound.handle())
		m_flyingSound.play_at_pos(0, XFORM().c, sm_Looped);

	StartLights();
	StartParticles();

	R_ASSERT(m_pPhysicsShell);
	CPHUpdateObject::Activate();
}

void CCustomRocket::StopEngine()
{
	m_eState = eFlying;

	m_dwEngineTime = 0;

	if (m_bStopLightsWithEngine)
	{
		StopLights();
	}

	if (m_flyingSound.is_playing())
		m_flyingSound.stop();

	StopLights();
	StopParticles();

	CPHUpdateObject::Deactivate();
}

void CCustomRocket::UpdateEnginePh()
{
	if (Level().In_NetCorrectionPrediction())
	{
		return;
	}

	float force = m_fEngineImpulse * fixed_step;
	float k_back = 1.0f;
	Fvector l_pos = zero_vel, l_dir = zero_vel;
	l_pos.set(0.0f, 0.0f, -2.0f);
	l_dir.set(XFORM().k);

	l_dir.normalize();

	R_ASSERT(m_pPhysicsShell);
	m_pPhysicsShell->applyImpulse(l_dir, (1.0f + k_back) * force);
	m_pPhysicsShell->get_LinearVel(l_dir);
	l_dir.normalize_safe();
	l_dir.invert();
	m_pPhysicsShell->applyImpulseTrace(l_pos, l_dir, force);
	l_dir.set(0.0f, 1.0f, 0.0f);
	force = m_fEngineImpulseUp * fixed_step;
	m_pPhysicsShell->applyImpulse(l_dir, force);
}


void CCustomRocket::UpdateEngine()
{
	if (!m_pPhysicsShell)
	{
		Msg("! CCustomRocket::UpdateEngine called, but m_pPhysicsShell is nullptr");
	}

	if (!getVisible())
	{
		Msg("! CCustomRocket::UpdateEngine called, but false==getVisible() id[%d] frame[%d]", ID(), Device.dwFrame);
	}

	if (m_dwEngineTime <= 0)
	{
		StopEngine();
		return;
	}

	m_dwEngineTime -= Device.dwTimeDelta;
}

//////////////////////////////////////////////////////////////////////////
//	Lights
//////////////////////////////////////////////////////////////////////////
void CCustomRocket::StartLights()
{
	if (!m_bLightsEnabled)
	{
		return;
	}

	//включить световую подсветку от двигателя
	m_pTrailLight->set_color(m_TrailLightColor.r, m_TrailLightColor.g, m_TrailLightColor.b);

	m_pTrailLight->set_range(m_fTrailLightRange);
	m_pTrailLight->set_position(Position());
	m_pTrailLight->set_active(true);
}

void CCustomRocket::StopLights()
{
	if (!m_bLightsEnabled)
	{
		return;
	}

	m_pTrailLight->set_active(false);
}

void CCustomRocket::UpdateLights()
{
	if (!m_bLightsEnabled || !m_pTrailLight->get_active())
	{
		return;
	}

	m_pTrailLight->set_position(Position());
}

void CCustomRocket::PhDataUpdate(float step) {}

void CCustomRocket::PhTune(float step)
{
	UpdateEnginePh();
}

//////////////////////////////////////////////////////////////////////////
//	Particles
//////////////////////////////////////////////////////////////////////////

void CCustomRocket::UpdateParticles()
{
	if (m_pEngineParticles || m_pFlyParticles)
	{
		Fvector vel;
		PHGetLinearVell(vel);

		vel.add(m_vPrevVel, vel);
		vel.mul(0.5f);
		m_vPrevVel.set(vel);

		Fmatrix particles_xform;
		particles_xform.identity();
		particles_xform.k.set(XFORM().k);
		particles_xform.k.mul(-1.f);
		Fvector dir = particles_xform.k;
		Fvector::generate_orthonormal_basis(particles_xform.k, particles_xform.j, particles_xform.i);
		particles_xform.c.set(XFORM().c);
		dir.normalize_safe();
		particles_xform.c.add(dir);

		if (m_pEngineParticles)
			m_pEngineParticles->UpdateParent(particles_xform, vel);

		if (m_pFlyParticles)
			m_pFlyParticles->UpdateParent(particles_xform, vel);
	}
}

void CCustomRocket::StartParticles()
{
	m_pFlyParticles = m_sFlyParticles ? Particles::Details::Create(*m_sFlyParticles, FALSE) : nullptr;
	m_pEngineParticles = m_sEngineParticles ? Particles::Details::Create(*m_sEngineParticles, FALSE) : nullptr;
	UpdateParticles();
	if (m_pFlyParticles)
		m_pFlyParticles->Play(false);
	if (m_pEngineParticles)
		m_pEngineParticles->Play(false);
}
void CCustomRocket::StopParticles()
{
	if (m_pFlyParticles)
		m_pFlyParticles->Destroy();

	if (m_pEngineParticles)
		m_pEngineParticles->Destroy();
}

void CCustomRocket::OnEvent(NET_Packet& P, u16 type)
{
	switch (type)
	{
	case GE_GRENADE_EXPLODE:
	{
		if (m_eState != eCollide && OnClient())
		{
			CCustomRocket::Contact(Position(), Direction());
		};
	}break;
	}

	inherited::OnEvent(P, type);
};
#ifdef DEBUG
void CCustomRocket::deactivate_physics_shell()
{
	R_ASSERT(!CPHUpdateObject::IsActive());
	inherited::deactivate_physics_shell();
}
#endif