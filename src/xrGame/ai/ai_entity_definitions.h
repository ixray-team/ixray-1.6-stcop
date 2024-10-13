#pragma once

namespace EntityDefinitions 
{
	struct CSnorkBaseDef 
	{
		static constexpr float TRACE_RANGE = 30.f;
		static constexpr float JUMP_DISTANCE = 10.f;
	};

	struct CTushkanoBaseDef
	{

	};

	struct CZombieBaseDef
	{
		static constexpr int TIME_FAKE_DEATH = 5000;
		static constexpr int FAKE_DEATH_TYPES_COUNT = 4;
		static constexpr int TIME_RESURRECT_RESTORE = 2000;
	};
}