#pragma once
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/CELFlyingEntityBase.h"

class CELDuten final : public CELFlyingEntityBase
{
public:
	char* get_monster_class_name() override { return const_cast<char*>("duten"); }
};
