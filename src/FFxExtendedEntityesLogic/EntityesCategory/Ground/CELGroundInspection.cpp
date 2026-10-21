#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/CELGroundInspection.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/ai/monsters/control_animation_base.h"

// Связывает временное направление видового осмотра с необходимыми службами и состоянием владельца.
CELGroundInspection::CELGroundInspection(CBaseMonster& OwnerMonster) : Owner(OwnerMonster) {}

// Временно включает существующую анимацию обнюхивания и снимает её по завершении.
bool CELGroundInspection::Update(bool Enabled)
{
	if (!Enabled)
	{
		Clear();
		return false;
	}
	if (!HasOverride && s32(Device.dwTimeGlobal - NextInspection) >= 0)
	{
		MotionID Motion;
		float Duration = 0.f;
		NextInspection = Device.dwTimeGlobal + Random.randI(3500u, 5001u);
		// Query registered native motions; never install a missing animation on a species.
		if (Owner.anim().get_animation_info(eAnimSniff, 0, Motion, Duration) && Motion.valid() && _valid(Duration) && Duration > 0.f)
		{
			Owner.anim().set_override_animation(eAnimSniff, 0);
			Until = Device.dwTimeGlobal + u32(Duration * 1000.f);
			HasOverride = true;
		}
	}
	if (HasOverride && s32(Device.dwTimeGlobal - Until) >= 0)
	{
		Clear();
	}
	return HasOverride;
}

// Снимает временную подмену штатной анимации осмотра.
void CELGroundInspection::Clear()
{
	if (HasOverride)
	{
		Owner.anim().clear_override_animation();
		HasOverride = false;
	}
}
