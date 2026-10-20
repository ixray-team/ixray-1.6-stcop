#pragma once
#include "../../../CELFlyingEntityBase.h"

class CELStrekoza final : public CELFlyingEntityBase
{
public:
	char* get_monster_class_name() override { return const_cast<char*>("strekoza"); }
};
