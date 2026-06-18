#pragma once
#include "particle_actions_collection.h"

namespace PAPI
{
	// A effect of particles - Info and an array of Particles and Actions
	struct ParticleHolder
	{
		xr_vector<ParticleAction*> m_actions;
		xr_vector<ParticleAction*> m_animators;

		Particles particles;		// Actually, num_particles in size
		OnBirthParticleCB b_cb = nullptr;
		OnDeadParticleCB d_cb = nullptr;
		void* owner = nullptr;

		u32 p_count = 0U;				// Number of particles currently existing.
		u32 max_particles = 1U;			// Max particles allowed in effect.
		u32 particles_allocated = 1U;	// Actual allocated size.
		u32 param = 0U;

		IC ParticleHolder()
		{
			particles.Realloc(max_particles);
		}

		virtual ~ParticleHolder()
		{
			for (ParticleAction* pPAction : m_actions)
			{
				xr_delete(pPAction);
			}
		}

		ICF void SetMaxParticles(u32 max_count)
		{
			// Reducing max
			if (particles_allocated >= max_count)
			{
				max_particles = max_count;

				// May have to kill particles.
				if (p_count > max_particles)
				{
					p_count = max_particles;
				}

				return;
			}

			// Allocate particles.
			{
				Particles new_particles;
				new_particles.Realloc(max_count);
				new_particles.CopyData(particles, p_count);
				particles.Free();
				particles = std::move(new_particles);
			}

			max_particles = max_count;
			particles_allocated = max_count;
		}

		ICF void RemoveParticle(u32 i)
		{
			if (0 == p_count)
			{
				return;
			}

			if (d_cb)
			{
				d_cb(owner, param, particles, i, i);
			}

			particles.SwapWithLast(i, p_count--);
		}

		ICF bool AddParticle(const Fvector& pos, const Fvector& posB,
			const Fvector& size, const Fvector& rot, const Fvector& vel, const Fvector& rot_vel,
			u32 color, const float age = 0.0f, u16 frame = 0, u16 flags = 0)
		{
			if (p_count >= max_particles)
			{
				return false;
			}

			VERIFY(age >=0.0f);
			particles.Add(p_count, pos, posB, size, rot, vel, rot_vel, color, age, frame, flags);

			if (b_cb)
			{
				b_cb(owner, param, particles, p_count, p_count);
			}

			p_count++;

			return true;
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
					ParticleAction* PA = nullptr;
					switch ((PActionEnum)R.r_u32())
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

						case PABindVelocityValueID: PA = new PABindVelocityValue();	break;
						case PABindRotationValueID: PA = new PABindRotationValue();	break;
						case PABindSizeValueID:  	PA = new PABindSizeValue();		break;
						case PABindColorValueID:  	PA = new PABindColorValue();	break;
						case PABindColorAlphaID:  	PA = new PABindColorAlpha();	break;

						case PAColorAnimatorID:  	PA = new PAColorAnimator(); m_animators.push_back(PA);		break;
						case PASizeAnimatorID:  	PA = new PASizeAnimator(); m_animators.push_back(PA);		break;
						case PAVelocityAnimatorID:  PA = new PAVelocityAnimator(); m_animators.push_back(PA);	break;
						case PAVelocityRotationAnimatorID: PA = new PAVelocityRotationAnimator(); m_animators.push_back(PA);	break;
						default: NODEFAULT;
					}
					R_ASSERT(PA);
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

		ICF Particles& GetParticles(u32& cnt)
		{
			cnt = p_count;
			return particles;
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