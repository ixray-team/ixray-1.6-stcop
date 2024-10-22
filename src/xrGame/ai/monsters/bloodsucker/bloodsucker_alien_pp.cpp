#include "stdafx.h"
#include "bloodsucker.h"
#include "../../../level.h"
#include "../../../Actor.h"
#include "../../../ActorEffector.h"
#include "../../../inventory.h"
#include "../../../HudItem.h"
#include "../../../../xrEngine/CustomHUD.h"

#include "bloodsucker_alien_pp.h"

CustomBloodsuckerAlienEffectorPP::CustomBloodsuckerAlienEffectorPP(const SPPInfo& ppi, EEffectorPPType type) :
	CEffectorPP(type, flt_max, false)
{
	state = ppi;
	factor = 0.f;
	target_factor = 1.f;
}

CustomBloodsuckerAlienEffectorPP::~CustomBloodsuckerAlienEffectorPP()
{

}

BOOL CustomBloodsuckerAlienEffectorPP::Process(SPPInfo& pp)
{
	inherited::Process(pp);

	if (fsimilar(factor, target_factor)) 
	{
		target_factor = (target_factor > 0.5f) ? .3f : .6f;
	}

	def_lerp(factor, target_factor, EntityDefinitions::CBloodsuckerBase::PERIOD_SPEED, Device.fTimeDelta);
	pp.lerp(pp_identity, state, factor);

	return TRUE;
}

void CustomBloodsuckerAlienEffectorPP::Destroy()
{
	fLifeTime = 0.f;
	CustomBloodsuckerAlienEffectorPP* self = this;
	xr_delete(self);
}
