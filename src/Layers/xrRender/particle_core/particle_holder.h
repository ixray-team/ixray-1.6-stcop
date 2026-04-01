
#ifndef pholderH
#define pholderH
#include "particle_actions_collection.h"
namespace PAPI
{
	// A effect of particles - Info and an array of Particles and Actions
	struct ParticleHolder
	{
		xr_vector<ParticleAction*> m_actions;

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

		IC void SetMaxParticles(u32 max_count)
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

			for (u32 i = 0; i < max_count; i++)
			{
				if (i < p_count)
					new_particles[i] = particles[i];
				else
					new_particles[i].Reset();
			}
			
			xr_free(particles);

			particles = new_particles;

			max_particles = max_count;
			particles_allocated = max_count;
		}

		IC void RemoveParticle(u32 i)
		{
			if (0 == p_count)
				return;

			if (d_cb)
				d_cb(owner, param, particles[i], i);

			particles[i] = particles[--p_count];
			particles[p_count].Reset();
		}

		IC BOOL AddParticle(const Fvector& pos, const Fvector& posB,
			const Fvector& size, const Fvector& rot, const Fvector& vel, const Fvector& rot_vel,
			u32 color, const float age = 0.0f, u16 frame = 0, u16 flags = 0)
		{
			if (p_count >= max_particles)
				return FALSE;

			VERIFY(age >=0.0f);
			Particle& P = particles[p_count];
			P.pos = pos;
			P.posI = pos;
			P.posB = posB;
			P.size = size;
			P.sizeMod = {1.0f, 1.0f, 1.0f};
			P.sizeI = size;
			P.rot.x = rot.x;
			P.rotI.x = rot.x;
			P.vel = vel;
			P.velI = vel;
			P.rot_vel 	= rot_vel;
			P.rot_velS 	= rot_vel;
			P.color = color;
			P.colorMod = { 1.0f, 1.0f, 1.0f, 1.0f };
			P.age = age;
			P.frame = frame;
			P.flags.assign(flags);

			if (b_cb)
				b_cb(owner, param, P, p_count);

			p_count++;

			return TRUE;
		}

		IC u32 LoadActions(IReader& R)
		{
			if (R.length())
			{
				u32 cnt = R.r_u32();
				m_actions.clear(); // without this in SDK effects are broken
				m_actions.reserve(cnt);

				for (u32 k = 0; k < cnt; ++k)
				{
					PActionEnum type = (PActionEnum)R.r_u32();

					ParticleAction* PA = nullptr;
					switch (type)
					{
						case PAAvoidID:				PA = new PAAvoid();				break;
						case PABounceID:    		PA = new PABounce();			break;
						case PACopyVertexBID:    	PA = new PACopyVertexB();		break;
						case PADampingID:    		PA = new PADamping();			break;
						case PAExplosionID:    		PA = new PAExplosion();			break;
						case PAFollowID:    		PA = new PAFollow();			break;
						case PAGravitateID:    		PA = new PAGravitate();			break;
						case PAGravityID:    		PA = new PAGravity();			break;
						case PAJetID:    			PA = new PAJet();				break;
						case PAKillOldID:    		PA = new PAKillOld();			break;
						case PAMatchVelocityID:    	PA = new PAMatchVelocity();		break;
						case PAMoveID:    			PA = new PAMove();				break;
						case PAOrbitLineID:    		PA = new PAOrbitLine();			break;
						case PAOrbitPointID:    	PA = new PAOrbitPoint();		break;
						case PARandomAccelID:    	PA = new PARandomAccel();		break;
						case PARandomDisplaceID:    PA = new PARandomDisplace();	break;
						case PARandomVelocityID:    PA = new PARandomVelocity();	break;
						case PARestoreID:    		PA = new PARestore();			break;
						case PASinkID:    			PA = new PASink();				break;
						case PASinkVelocityID:    	PA = new PASinkVelocity();		break;
						case PASourceID:    		PA = new PASource();			break;
						case PASpeedLimitID:    	PA = new PASpeedLimit();		break;
						case PATargetColorID:    	PA = new PATargetColor();		break;
						case PATargetSizeID:    	PA = new PATargetSize();		break;
						case PATargetRotateID:    	PA = new PATargetRotate();		break;
						case PATargetRotateDID:    	PA = new PATargetRotate();		break;
						case PATargetVelocityID:    PA = new PATargetVelocity(); 	break;
						case PATargetVelocityDID:   PA = new PATargetVelocity();	break;
						case PAVortexID:    		PA = new PAVortex();			break;
						case PATurbulenceID:		PA = new PATurbulence();		break;
						case PAScatterID:  			PA = new PAScatter();			break;
						case PABindVelocityValueID: PA = new PABindVelocityValue();			break;
						case PABindRotationValueID: PA = new PABindRotationValue();			break;
						case PABindSizeValueID:  	PA = new PABindSizeValue();			break;
						case PABindColorValueID:  	PA = new PABindColorValue();			break;
						case PABindColorAlphaID:  	PA = new PABindColorAlpha();			break;
						case PAColorAnimatorID:  	PA = new PAColorAnimator();			break;
						case PASizeAnimatorID:  	PA = new PASizeAnimator();			break;
						case PAVelocityAnimatorID:  PA = new PAVelocityAnimator();			break;
						case PAVelocityRotationAnimatorID: PA = new PAVelocityRotationAnimator();	break;
						default: NODEFAULT;
					}
					R_ASSERT(PA);
					PA->type = type;

					PA->Load(R);
					m_actions.push_back(PA);
				}
			}
			return (u32)m_actions.size();
		}

		IC void SaveActions(IWriter& W)
		{
			W.w_u32((u32)m_actions.size());

			for (ParticleAction* PA : m_actions)
				PA->Save(W);
		}

		IC void SetCallback(OnBirthParticleCB _b, OnDeadParticleCB _d, void* ow, u32 par)
		{
			b_cb = _b;
			d_cb = _d;
			owner = ow;
			param = par;
		}

		IC void GetParticles(Particle*& pvec, u32& cnt)
		{
			pvec = particles;
			cnt = p_count;
		}

		IC u32 GetParticlesCount() { return p_count; }

		IC void Transform(const Fmatrix& full, const Fvector& vel)
		{
			Fmatrix mT;
			mT.translate(full.c);

			// Step through all the actions in the action list.
			for (ParticleAction* PA : m_actions)
			{
				BOOL r = PA->m_Flags.is(ParticleAction::ALLOW_ROTATE);
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

		IC void Update(float dt)
		{
			// Step through all the actions in the action list.
			float kill_old_time = 3.0f;
			for (auto PA : m_actions)
			{
				PA->PreExecute(this);
			}			
			for (auto PA : m_actions)
			{
				PA->Execute(this, dt, kill_old_time);
			}
		}

		IC void StopEffect(BOOL deffered)
		{
			for (ParticleAction* PA : m_actions)
			{
				switch (PA->type)
				{
				case PASourceID:
					static_cast<PASource*>(PA)->m_Flags.set(PASource::flSilent, TRUE);
					break;
				}
			}

			if (!deffered)
				p_count = 0;
		}

		IC void PlayEffect()
		{
			// Step through all the actions in the action list.
			for (ParticleAction* PA : m_actions)
			{
				switch (PA->type)
				{
				case PASourceID:
					static_cast<PASource*>(PA)->m_Flags.set(PASource::flSilent, FALSE);
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