#include "StdAfx.h"
#include "../xrEngine/GameMtlLib.h"
#include "Level.h"
#include "GamePersistent.h"
#include "PhysicsGamePars.h"
#include "../xrPhysics/PhysicsExternalCommon.h"
#include "PHSoundPlayer.h"
#include "PhysicsShellHolder.h"
#include "PHCommander.h"
#include "../xrPhysics/MathUtils.h"
#include "../xrPhysics/IPHWorld.h"

#include "PHReqComparer.h"

#include "../Include/xrRender/FactoryPtr.h"
#include "../Include/xrRender/WallMarkArray.h"
#include "ParticlesObject.h"

static const float PARTICLE_EFFECT_DIST=70.f;
static const float SOUND_EFFECT_DIST=70.f;
const float			mass_limit										= 10000.f;//some conventional value used as evaluative param (there is no code restriction on mass)

static const float SQUARE_PARTICLE_EFFECT_DIST=PARTICLE_EFFECT_DIST*PARTICLE_EFFECT_DIST;
static const float SQUARE_SOUND_EFFECT_DIST=SOUND_EFFECT_DIST*SOUND_EFFECT_DIST;

class CPHParticlesPlayCall :
	public CPHAction
{
	const char* ps_name;

protected:
	Fvector c_pos;
	Fvector c_normal;

public:
	CPHParticlesPlayCall(const SPHShotMarkContact& contact, bool invert_n, const char* psn)
	{
		ps_name = psn;
		c_pos.set(contact.Position);
		c_normal.set(contact.Normal);
		if (invert_n)
		{
			c_normal.invert();
		}
	}

	virtual void run() override
	{
		xr_shared_ptr<CParticlesObject> ps = Particles::Details::Create(ps_name, true);

		Fmatrix pos;
		Fvector zero_vel_ = {0.f, 0.f, 0.f};
		pos.k.set(c_normal);
		Fvector::generate_orthonormal_basis(pos.k, pos.j, pos.i);
		pos.c.set(c_pos);

		ps->UpdateParent(pos, zero_vel_);
		GamePersistent().ps_needtoplay.push_back(ps);
	};
	virtual bool obsolete() const override { return false; }
};

static const float minimal_plane_distance_between_liquid_particles = 0.2f;
class CPHLiquidParticlesPlayCall :
	public CPHParticlesPlayCall,
	public CPHReqComparerV
{
	u32 remove_time;
	bool b_called;
public:
	CPHLiquidParticlesPlayCall(const SPHShotMarkContact &contact,bool invert_n,const char* psn):
	  CPHParticlesPlayCall( contact, invert_n, psn ), b_called ( false )
	{
		static const u32 time_to_call_remove  = 3000;
		remove_time = Device.dwTimeGlobal + time_to_call_remove;
	}
	const Fvector 	&position() const
	{
		return c_pos;
	}
private:
	virtual bool			compare							(const	CPHReqComparerV* v)					const override {return v->compare(this);}

	virtual void 			run								() override
	{
		if( b_called )
			return;
		b_called = true;
		CPHParticlesPlayCall::run();
	}

	virtual bool 			obsolete						()const override {return Device.dwTimeGlobal > remove_time ;}
};


class CPHLiquidParticlesCondition:
	public CPHCondition,
	public CPHReqComparerV
{


private:
	virtual bool			compare							(const	CPHReqComparerV* v)					const override {return v->compare(this);}
	virtual bool 			is_true							() override {return true;}
	virtual bool 			obsolete						()const override {return false;}
};
class CPHFindLiquidParticlesComparer:
	public CPHReqComparerV
{
	Fvector m_position;
public:
	CPHFindLiquidParticlesComparer( const Fvector &position ): m_position(position) {}
private:
	virtual bool			compare							(const	CPHReqComparerV* v)					const	{return v->compare(this);}
	virtual bool			compare							(const	CPHLiquidParticlesCondition* v)		const override {return true;}

	virtual bool			compare							(const	CPHLiquidParticlesPlayCall* v )		const override
	{
		VERIFY( v );
		
		Fvector	disp = Fvector().sub( m_position, v->position() );
		return disp.x*disp.x + disp.z*disp.z < ( minimal_plane_distance_between_liquid_particles * minimal_plane_distance_between_liquid_particles ) ;
	}
};

class CPHWallMarksCall :
	public CPHAction
{
	wm_shader pWallmarkShader;
	Fvector pos;
	CDB::TRI* T;

public:
	CPHWallMarksCall(const Fvector& p, CDB::TRI* Tri, const wm_shader& s)
	{
		pWallmarkShader = s;
		pos.set(p);
		T = Tri;
	}

	virtual void run() override
	{
		// добавить отметку на материале
		::Render->add_StaticWallmark(pWallmarkShader, pos, 0.09f, T, Level().ObjectSpace.GetStaticVerts().data());
	};
	virtual bool obsolete() const override { return false; }
};

static void play_object(IPhysicsShellHolder* RefObject, SGameMtlPair* mtl_pair, const SPHShotMarkContact& c)
{
	VERIFY(mtl_pair);

	if (!RefObject || RefObject->ObjectGetDestroy())
	{
		return;
	}

	CPHSoundPlayer* sp = nullptr;
	try
	{
		sp = RefObject->ObjectPhSoundPlayer();
	}
	catch (...)
	{
		Msg("! Physics callbacks sound player play_object error");
		sp = nullptr;
	}

	if (sp)
	{
		sp->Play(mtl_pair, c.Position);
	}
}

template<class Pars>
IC bool play_liquid_particle_criteria(const SPHShotMarkContact& c, float vel_cret)
{
	if (vel_cret > Pars::vel_cret_particles)
	{
		return true;
	}

	return !c.IsCharacter && vel_cret > Pars::vel_cret_particles / 4.f;
}

template<class Pars>
void play_particles(float vel_cret, const SPHShotMarkContact& c, bool b_invert_normal, const SGameMtl* static_mtl, const char* ps_name )
{
	VERIFY( static_mtl );
	bool liquid = !!static_mtl->Flags.test( SGameMtl::flLiquid );

	bool play_liquid =		liquid && play_liquid_particle_criteria<Pars>( c, vel_cret );
	bool play_not_liquid = !liquid && vel_cret > Pars::vel_cret_particles;

	if( play_not_liquid )
		Level().ph_commander().add_call(new CPHOnesCondition(),new CPHParticlesPlayCall(c,b_invert_normal,ps_name));
	else if( play_liquid )
	{
		CPHFindLiquidParticlesComparer find( c.Position );
		if( !Level().ph_commander().has_call( &find, &find ) )
			Level().ph_commander().add_call(new CPHLiquidParticlesCondition(),new CPHLiquidParticlesPlayCall(c,b_invert_normal,ps_name));
	}
}

template<class Pars>
void TContactShotMark(CDB::TRI* T, const SPHShotMarkContact& c)
{
	if (!c.HasEffect)
	{
		return;
	}

	float vel_cret = c.VelCret;
	bool b_invert_normal = c.InvertNormal;

	Fvector to_camera;
	to_camera.sub(c.Position, Device.vCameraPosition);
	float square_cam_dist = to_camera.square_magnitude();
	if (c.HasUserData)
	{
		SGameMtlPair* mtl_pair = GMLib.GetMaterialPair(T->material, c.Material);
		if (mtl_pair)
		{
			if (vel_cret > Pars::vel_cret_wallmark && !mtl_pair->m_pCollideMarks->empty())
			{
				wm_shader WallmarkShader = mtl_pair->m_pCollideMarks->GenerateWallmark();
				Level().ph_commander().add_call(new CPHOnesCondition(), new CPHWallMarksCall(c.Position, T, WallmarkShader));
			}

			if (square_cam_dist < SQUARE_SOUND_EFFECT_DIST)
			{
				SGameMtl* static_mtl = GMLib.GetMaterialByIdx(T->material);
				VERIFY(static_mtl);
				if (!static_mtl->Flags.test(SGameMtl::flPassable))
				{
					if (vel_cret > Pars::vel_cret_sound)
					{
						if (!mtl_pair->CollideSounds.empty())
						{
							float volume = collide_volume_min + vel_cret * (collide_volume_max - collide_volume_min) / (_sqrt(mass_limit) * default_l_limit - Pars::vel_cret_sound);
							Fvector sound_pos = c.Position;
							GET_RANDOM(mtl_pair->CollideSounds).play_no_feedback(0, 0, 0, &sound_pos, &volume);
						}
					}
				}
				else
				{
					if (c.Object && !mtl_pair->CollideSounds.empty())
					{
						play_object(c.Object, mtl_pair, c);
					}
				}
			}

			if (square_cam_dist < SQUARE_PARTICLE_EFFECT_DIST && !mtl_pair->CollideParticles.empty())
			{
				SGameMtl* static_mtl = GMLib.GetMaterialByIdx(T->material);
				VERIFY(static_mtl);
				const char* ps_name = *mtl_pair->CollideParticles[::Random.randI(0, (int)mtl_pair->CollideParticles.size())];
				play_particles<Pars>(vel_cret, c, b_invert_normal, static_mtl, ps_name);
			}
		}
	}
}

ContactCallbackFun *ContactShotMark = &TContactShotMark<EffectPars>;
ContactCallbackFun *CharacterContactShotMark = &TContactShotMark<CharacterEffectPars>;
