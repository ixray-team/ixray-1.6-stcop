#pragma once
#include "../Interfaces/IELLogic.h"
class CBaseMonster;

class CELSharedFactory final
{
public:
	static xr_unique_ptr<IELLogic> Create(CBaseMonster& OwnerMonster);
};
