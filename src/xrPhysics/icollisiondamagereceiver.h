#pragma once

class ICollisionDamageReceiver
{
public:

	virtual		void	CollisionHit( ALife::_OBJECT_ID source_id, u16 bone_id, float power, const Fvector &dir, Fvector &pos )	=0;
protected:
	virtual				~ICollisionDamageReceiver()	{};
};

struct dContact;
struct SGameMtl;
XRPHYSICS_API void 	DamageReceiverCollisionCallback	(bool& do_colide,bool bo1,dContact& c,SGameMtl* material_1,SGameMtl* material_2);
XRPHYSICS_API void 	BreakableObjectCollisionCallback(bool& do_colide,bool bo1,dContact& c,SGameMtl* material_1,SGameMtl* material_2);