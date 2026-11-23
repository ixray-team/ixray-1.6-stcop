
#ifndef pholderH
#define pholderH
#include "particle_actions_collection.h"
namespace PAPI
{
	// A effect of particles - Info and an array of Particles and Actions
	struct ParticleHolder
	{
		xr_vector<ParticleAction*> m_actions;
		xr_vector<ParticleAction*> m_animators;

		Particle* particles = nullptr;		// Actually, num_particles in size
		OnBirthParticleCB b_cb = nullptr;
		OnDeadParticleCB d_cb = nullptr;
		void* owner = nullptr;

		u32 p_count = 0U;				// Number of particles currently existing.
		u32 max_particles = 1U;			// Max particles allowed in effect.
		u32 particles_allocated = 1U;	// Actual allocated size.
		u32 param = 0U;

		IC ParticleHolder()
		{
			particles = xr_alloc<Particle>(max_particles);
		}

		virtual ~ParticleHolder()
		{
			xr_free(particles);
			for (ParticleAction* pPAction : m_actions)
				xr_delete(pPAction);
		}

		ICF void SetMaxParticles(u32 max_count)
		{
			// Reducing max
			if (particles_allocated >= max_count)
			{
				max_particles = max_count;

				// May have to kill particles.
				if (p_count > max_particles)
					p_count = max_particles;

				return;
			}

			// Allocate particles.
			Particle* new_particles = xr_alloc<Particle>(max_count);
			//std::memcpy(new_particles, particles, p_count * sizeof(Particle));

			for (u32 i = 0; i < p_count; i++)
				new_particles[i] = particles[i];
			
			xr_free(particles);

			particles = new_particles;

			max_particles = max_count;
			particles_allocated = max_count;
		}

		ICF void RemoveParticle(u32 i)
		{
			if (0 == p_count)
				return;

			if (d_cb)
				d_cb(owner, param, particles[i], i);

			particles[i] = std::move(particles[--p_count]);
		}

		ICF bool AddParticle(const Fvector& pos, const Fvector& posB,
			const Fvector& size, const Fvector& rot, const Fvector& vel, const Fvector& rot_vel,
			u32 color, const float age = 0.0f, u16 frame = 0, u16 flags = 0)
		{
			if (p_count >= max_particles)
				return false;

			VERIFY(age >=0.0f);
			Particle& P = particles[p_count];
			P.pos = pos;
			P.posI = pos;
			P.posB = posB;
			P.size = size;
			P.sizeI = size;
			P.sizeMod = { 1.0f, 1.0f, 1.0f };
			P.rot.x = rot.x;
			P.rotI.x = rot.x;
			P.vel = vel;
			P.velI = vel;
			P.rot_vel = rot_vel;
			P.rot_velS = rot_vel;
			P.color = color;
			P.colorMod = { 1.0f, 1.0f, 1.0f, 1.0f };
			P.age = age;
			P.frame = frame;
			P.flags.assign(flags);

			if (b_cb)
				b_cb(owner, param, P, p_count);

			p_count++;

			return true;
		}

		static ParticleAction* CreateAction(PActionEnum Id)
		{
			switch (Id)
			{
				case PAAvoidID:				return new PAAvoid();
				case PABounceID:    		return new PABounce();
				case PACopyVertexBID:    	return new PACopyVertexB();
				case PADampingID:    		return new PADamping();
				case PAExplosionID:    		return new PAExplosion();
				case PAFollowID:    		return new PAFollow();
				case PAGravitateID:    		return new PAGravitate();
				case PAGravityID:    		return new PAGravity();
				case PAJetID:    			return new PAJet();
				case PAKillOldID:    		return new PAKillOld();
				case PAMatchVelocityID:    	return new PAMatchVelocity();
				case PAMoveID:    			return new PAMove();
				case PAOrbitLineID:    		return new PAOrbitLine();
				case PAOrbitPointID:    	return new PAOrbitPoint();
				case PARandomAccelID:    	return new PARandomAccel();
				case PARandomDisplaceID:    return new PARandomDisplace();
				case PARandomVelocityID:    return new PARandomVelocity();
				case PARestoreID:    		return new PARestore();
				case PASinkID:    			return new PASink();
				case PASinkVelocityID:    	return new PASinkVelocity();
				case PASourceID:    		return new PASource();
				case PASpeedLimitID:    	return new PASpeedLimit();
				case PATargetColorID:    	return new PATargetColor();
				case PATargetSizeID:    	return new PATargetSize();
				case PATargetRotateID:    	return new PATargetRotate();
				case PATargetRotateDID:    	return new PATargetRotate();
				case PATargetVelocityID:    return new PATargetVelocity();
				case PATargetVelocityDID:   return new PATargetVelocity();
				case PAVortexID:    		return new PAVortex();
				case PATurbulenceID:		return new PATurbulence();
				case PAScatterID:  			return new PAScatter();

				case PABindVelocityValueID: return new PABindVelocityValue();
				case PABindRotationValueID: return new PABindRotationValue();
				case PABindSizeValueID:  	return new PABindSizeValue();
				case PABindColorValueID:  	return new PABindColorValue();
				case PABindColorAlphaID:  	return new PABindColorAlpha();

				case PAColorAnimatorID:  	return new PAColorAnimator();
				case PASizeAnimatorID:  	return new PASizeAnimator();
				case PAVelocityAnimatorID:  return new PAVelocityAnimator();
				case PAVelocityRotationAnimatorID: return new PAVelocityRotationAnimator();
				default: return nullptr;
			}
		}

		static bool IsAnimatorAction(PActionEnum Id)
		{
			return Id == PAColorAnimatorID || Id == PASizeAnimatorID || Id == PAVelocityAnimatorID || Id == PAVelocityRotationAnimatorID;
		}

		ICF void LoadActions(IReader& R)
		{
			if (R.length())
			{
				u32 cnt = R.r_u32();
				m_animators.clear();
				m_actions.clear(); // without this in SDK effects are broken
				m_actions.reserve(cnt);

				for (u32 k = 0; k < cnt; ++k)
				{
					const PActionEnum Id = (PActionEnum)R.r_u32();
					ParticleAction* PA = CreateAction(Id);
					if (!PA)
					{
						string256 Message;
						xr_sprintf(Message, "Unknown particle action id %u (action %u of %u)", u32(Id), k, cnt);
						FATAL(Message);
					}
					if (IsAnimatorAction(Id))
					{
						m_animators.push_back(PA);
					}
					PA->Load(R);
					m_actions.push_back(PA);
				}
			}
		}

		ICF void SaveActions(IWriter& W)
		{
			W.w_u32((u32)m_actions.size());

			for (ParticleAction* PA : m_actions)
				PA->Save(W);
		}

		ICF void SetCallback(OnBirthParticleCB _b, OnDeadParticleCB _d, void* ow, u32 par)
		{
			b_cb = _b;
			d_cb = _d;
			owner = ow;
			param = par;
		}

		ICF void GetParticles(Particle*& pvec, u32& cnt)
		{
			pvec = particles;
			cnt = p_count;
		}

		ICF u32 GetParticlesCount() { return p_count; }

		ICF void Transform(const Fmatrix& full, const Fvector& vel)
		{
			Fmatrix mT;
			mT.translate(full.c);

			// Step through all the actions in the action list.
			for (ParticleAction* PA : m_actions)
			{
				bool r = PA->m_Flags.is(ParticleAction::ALLOW_ROTATE);
				const Fmatrix& m = r ? full : mT;
				PA->Transform(m);
				switch (PA->type)
				{
				case PASourceID:
					static_cast<PASource*>(PA)->parent_vel = vel * static_cast<PASource*>(PA)->parent_motion;
					break;
				}
			}
		}

		ICF void Update(float dt)
		{
			// Step through all the actions in the action list.


			for (auto PA : m_animators)
				PA->Animate(this);

			float kill_old_time = 1.0f;
			for (auto PA : m_actions)
				PA->Execute(this, dt, kill_old_time);

		}

		ICF void StopEffect(bool deffered)
		{
			for (ParticleAction* PA : m_actions)
			{
				switch (PA->type)
				{
				case PASourceID:
					static_cast<PASource*>(PA)->m_Flags.set(PASource::flSilent, true);
					break;
				}
			}

			if (!deffered)
				p_count = 0;
		}

		ICF void PlayEffect()
		{
			// Step through all the actions in the action list.
			for (ParticleAction* PA : m_actions)
			{
				switch (PA->type)
				{
				case PASourceID:
					static_cast<PASource*>(PA)->m_Flags.set(PASource::flSilent, false);
					break;
				case PAExplosionID:
					static_cast<PAExplosion*>(PA)->age = 0.f;
					break;
				case PATurbulenceID:
					static_cast<PATurbulence*>(PA)->age = 0.f;
					break;
				}
			}
		}
	};
}
#endif