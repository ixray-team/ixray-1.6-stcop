#include "stdafx.h"
#include "particle_effect.h"

using namespace PAPI;

xrCriticalSection ParticleEffect::mParticleRemoveCS;

ParticleEffect::ParticleEffect(int mp)
{
	owner = 0;
	param = 0;
	b_cb = 0;
	d_cb = 0;
	p_count = 0;
	max_particles = mp;
	particles_allocated = max_particles;
	particles = xr_alloc<Particle>(max_particles);
}

ParticleEffect::~ParticleEffect()
{
	xr_free(particles);
}

int ParticleEffect::Resize(u32 max_count)
{
	// Reducing max
	if (particles_allocated >= max_count)
	{
		max_particles = max_count;

		// May have to kill particles.
		if (p_count > max_particles)
			p_count = max_particles;

		return max_count;
	}

	// Allocate particles.
	Particle* new_particles = xr_alloc<Particle>(max_count);

	std::memcpy(new_particles, particles, p_count * sizeof(Particle));
	xr_free(particles);

	particles = new_particles;

	max_particles = max_count;
	particles_allocated = max_count;
	return max_count;
}

void ParticleEffect::Remove(int i)
{
	xrCriticalSectionGuard RemoveGuard(mParticleRemoveCS);

	if (0 == p_count)
		return;

	Particle& m = particles[i];
	if (d_cb)
		d_cb(owner, param, m, i);

	// не менять правило удаления !!! (dependence ParticleGroup)
	m = particles[--p_count];
}

BOOL ParticleEffect::Add(const pVector& pos, const pVector& posB,
	const pVector& size, const pVector& rot, const pVector& vel, u32 color,
	const float age, u16 frame, u16 flags)
{
	if (p_count >= max_particles)
		return FALSE;

	Particle& P = particles[p_count];
	P.pos 		= pos;
	P.posI		= pos;
	P.posB 		= posB;
	P.size 		= size;
	P.sizeI 	= size;
	P.rot.x 	= rot.x;
	P.rotI.x 	= rot.x;
	P.vel 		= vel;
	P.velI 		= vel;
	P.color 	= color;
	P.age 		= age;
	P.frame 	= frame;
	P.flags.assign(flags);

	if (b_cb)
		b_cb(owner, param, P, p_count);

	p_count++;

	return TRUE;
}