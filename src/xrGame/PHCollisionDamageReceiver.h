#pragma once

#include "../xrPhysics/icollisiondamagereceiver.h"

class CPhysicsShellHolder;
//struct SGameMtl;

class CPHCollisionDamageReceiver:
	public ICollisionDamageReceiver
{
typedef std::pair<u16,float> SControledBone;

using DAMAGE_CONTROLED_BONES_V = xr_vector<SControledBone>;
using DAMAGE_BONES_I = DAMAGE_CONTROLED_BONES_V::iterator;

struct SFind{u16 id;SFind(u16 _id){id=_id;};bool operator () (const SControledBone& cb){return cb.first==id;}};
DAMAGE_CONTROLED_BONES_V m_controled_bones;
float						m_hit_threshold = 5.f;
float						m_hit_cooldown = 0.f;
u32							m_last_hit_time = 0;

public:
			void						SetCollisionHitThreshold	(float v) { m_hit_threshold = v; }
			float						CollisionHitThreshold		() const { return m_hit_threshold; }
			void						SetCollisionHitCooldown		(float seconds) { m_hit_cooldown = seconds; m_last_hit_time = 0; }
			float						CollisionHitCooldown		() const { return m_hit_cooldown; }

protected:
	virtual CPhysicsShellHolder*		PPhysicsShellHolder			()																		=0;
			void						Init						()																		;

			void						Clear						()																		;
	// Returns true when this collision hit may start the cooldown. Derived
	// classes can reject hits that do no damage or should not suppress the
	// next body hit (for example, wheel contacts on a car).
	virtual bool						CollisionHitStartsCooldown	(u16 bone_id, float power)								{ return true; }
private:
			void						BoneInsert					(u16 id,float k)														;

	IC		DAMAGE_BONES_I				FindBone					(u16 id)
	{
		return std::find_if(m_controled_bones.begin(),m_controled_bones.end(),SFind(id));
	}
	virtual void						CollisionHit							(ALife::_OBJECT_ID source_id,u16 bone_id,float power,const Fvector &dir,Fvector &pos) override;
};
