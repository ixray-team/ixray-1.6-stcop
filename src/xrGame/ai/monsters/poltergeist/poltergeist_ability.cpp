#include "stdafx.h"
#include "poltergeist.h"
#include "../../../../xrPhysics/PhysicsShell.h"
#include "../../../Level.h"
#include "../../../material_manager.h"
#include "../../../level_debug.h"
#include "../../../ParticlesObject.h"

IPolter::IPolter(CPoltergeist* polter) : m_particles_hidden(nullptr),
                                         m_particles_damage(nullptr),
                                         m_particles_death(nullptr),
                                         m_particles_idle(nullptr),
                                         m_last_hit_frame(0)
{
	poltergeist = polter;

	m_particles_object = nullptr;
	m_particles_object_electro = nullptr;
}


IPolter::~IPolter()
{
	Particles::Details::Destroy	(m_particles_object);
	Particles::Details::Destroy	(m_particles_object_electro);
}

void IPolter::load(const char* section)
{
	m_particles_hidden					= pSettings->r_string(section,"Particles_Hidden");
	m_particles_damage					= pSettings->r_string(section,"Particles_Damage");
	m_particles_death					= pSettings->r_string(section,"Particles_Death");
	m_particles_idle					= pSettings->r_string(section,"Particles_Idle");

	m_sound_base.create					(pSettings->r_string(section,"Sound_Idle"), st_Effect, SOUND_TYPE_MONSTER_TALKING);

	m_last_hit_frame					= 0;
}

void IPolter::update_schedule()
{
	if (poltergeist->g_Alive()) {
		if (!m_sound_base.is_playing()) m_sound_base.play_at_pos(poltergeist, poltergeist->Position());
		else m_sound_base.set_position(poltergeist->Position());
	}
}

void IPolter::on_hide()
{
	VERIFY(m_particles_object == 0);
	if (!poltergeist->g_Alive())
		return;

 	m_particles_object			= poltergeist->PlayParticles	(m_particles_hidden, poltergeist->Position(),Fvector().set(0.0f,0.1f,0.0f), false);
 	m_particles_object_electro	= poltergeist->PlayParticles	(m_particles_idle, poltergeist->Position(),Fvector().set(0.0f,0.1f,0.0f), false);
}

void IPolter::on_show()
{
	if (m_particles_object)			Particles::Details::Destroy(m_particles_object);
	if (m_particles_object_electro) Particles::Details::Destroy(m_particles_object_electro);
}

void IPolter::update_frame()
{
	if (m_particles_object)			m_particles_object->SetXFORM		(poltergeist->XFORM());
	if (m_particles_object_electro)	m_particles_object_electro->SetXFORM(poltergeist->XFORM());
}

void IPolter::on_die()
{
	Fvector particles_position	= poltergeist->m_current_position;
	particles_position.y		+= poltergeist->target_height;

	poltergeist->PlayParticles			(m_particles_death, particles_position, Fvector().set(0.0f,1.0f,0.0f), true, false);

	Particles::Details::Destroy		(m_particles_object_electro);
	Particles::Details::Destroy		(m_particles_object);
}

void IPolter::on_hit(SHit* pHDS)
{
	if (poltergeist->g_Alive() && (pHDS->hit_type == ALife::eHitTypeFireWound) && (Device.dwFrame != m_last_hit_frame)) {
		if(BI_NONE != pHDS->bone()) {

			//вычислить координаты попадания
			IKinematics* V = PKinematics(poltergeist->Visual());

			Fvector start_pos = pHDS->bone_space_position();
			Fmatrix& m_bone = V->LL_GetBoneInstance(pHDS->bone()).mTransform;
			m_bone.transform_tiny	(start_pos);
			poltergeist->XFORM().transform_tiny	(start_pos);

			poltergeist->PlayParticles(m_particles_damage, start_pos, Fvector().set(0.f,1.f,0.f));
		}
	} 

	m_last_hit_frame = Device.dwFrame;
}


//////////////////////////////////////////////////////////////////////////
// Other
//////////////////////////////////////////////////////////////////////////

void CPoltergeist::PhysicalImpulse	(const Fvector &position)
{
	g_SpatialSpace->q_sphere(m_nearest,0,ESPATIAL_TYPE::COLLIDEABLE,position,IMPULSE_RADIUS);
	if (m_nearest.empty()) return;
	
	u32 index = Random.randI(m_nearest.size());

	ISpatial* S = m_nearest[index].get();
	if (!S) return;
	CObject* O = S->dcast_CObject();
	if (!O || O->getDestroy()) return;
	
	CPhysicsShellHolder  *obj = O->cast_physics_shell_holder();
	if (!obj || !obj->m_pPhysicsShell) return;

	Fvector dir;
	dir.sub(obj->Position(), position);
	dir.normalize();
	
	CPhysicsElement* E=obj->m_pPhysicsShell->get_ElementByStoreOrder(u16(Random.randI(obj->m_pPhysicsShell->get_ElementsNumber())));
	//E->applyImpulse(dir,IMPULSE * obj->m_pPhysicsShell->getMass());
	E->applyImpulse(dir,IMPULSE * E->getMass());
}

void CPoltergeist::StrangeSounds(const Fvector &position)
{
	if (m_strange_sound.is_playing()) return;
	
	for (u32 i = 0; i < TRACE_ATTEMPT_COUNT; i++) {
		Fvector dir;
		dir.random_dir();

		collide::rq_result	l_rq;
		if (Level().ObjectSpace.RayPick(position, dir, TRACE_DISTANCE, collide::rqtStatic, l_rq, nullptr)) {
			if (l_rq.IsStatic() && l_rq.range < TRACE_DISTANCE) {

				// Получить пару материалов
				const CDB::TRI&	pTri = l_rq.GetStatic()->tris[l_rq.element];
				SGameMtlPair* mtl_pair = GMLib.GetMaterialPair(material().self_material_idx(),pTri.material);
				if (!mtl_pair) continue;

				// Играть звук
				if (!mtl_pair->CollideSounds.empty()) {
					CLONE_MTL_SOUND(m_strange_sound, mtl_pair, CollideSounds);
					Fvector pos;
					pos.mad(position, dir, ((l_rq.range - 0.1f > 0) ? l_rq.range - 0.1f  : l_rq.range));
					m_strange_sound.play_at_pos(this,pos);
					return;
				}			
			}
		}
	}
}

