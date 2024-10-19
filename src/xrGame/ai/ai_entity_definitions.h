#pragma once

namespace EntityDefinitions 
{
	struct CPseudoDogBase
	{
		struct CPseudoPsyDogPhantomBase
		{
			static constexpr unsigned int PMT_TIME_WAIT_PARENT = 10000;
		};

		struct CPseudoPsyDogBase
		{
			static constexpr unsigned int s_phantom_immediate_respawn_flag = 0;
			static constexpr unsigned int s_phantom_alive_flag = 1;
		};
	};

	struct CPseudoGiantBaseDef
	{
		static constexpr float MAX_STEP_RADIUS = 60.f;
	};

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