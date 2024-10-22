#pragma once

namespace EntityDefinitions 
{
	struct CBloodsuckerBase
	{
		static constexpr int TimeHold = 4000;           // Время удержания вампира в миллисекундах

		static constexpr float PI_DIV_2 = 3.14f / 2.0f;
		static constexpr float PI_MUL_2 = 2.0f * 3.14f;
		static constexpr float TimeAttack = 0.2f;

		static constexpr int Periods = 2;

		static constexpr float RadToPerc(float rad)
		{
			return (rad - PI_DIV_2) / (Periods * PI_MUL_2);
		}

		static constexpr float PercToRad(float perc)
		{
			return perc * (Periods * PI_MUL_2) + PI_DIV_2;
		}

		static constexpr float DeltaAngleX = 10.0f * 3.14f / 180.0f;
		static constexpr float DeltaAngleY = DeltaAngleX;
		static constexpr float DeltaAngleZ = DeltaAngleX;

		static constexpr float AngleSpeed = 0.2f;

		static constexpr float BestDistance = 0.3f;

		static constexpr float  RUN_AWAY_DISTANCE = 50.f;

		static constexpr int TIME_TO_RESELECT_CAMP = 15000;

		static constexpr int encircle_time = 3000;
		static constexpr float loose_health_diff = 0.15f;
		static constexpr int change_behaviour_time = 1000;

		static constexpr float PERIOD_SPEED = 0.3f;

		static constexpr float MinFov = 70.0f;
		static constexpr float MaxFov = 175.0f;
		static constexpr float FovSpeed = 80.0f;
		static constexpr float MaxCameraDist = 3.5f;

		static constexpr int defaultVisibilityStateChangeMinDelay = 1000;

		static constexpr int defaultPartialVisibilityRadius = 10;
		static constexpr int defaultRunawayInvisibleTime = 3000;

		static constexpr const char* fullVisibilityRadiusString = "full_visibility_radius";
		static constexpr const char* partialVisibilityRadiusString = "partial_visibility_radius";
		static constexpr const char* visibilityStateChangeMinDelayString = "visibility_state_change_min_delay";
	};

	struct CBurerBase
	{
		static constexpr int SCAN_STATE_TIME = 4000;
		static constexpr float GOOD_DISTANCE_FOR_TELE = 15.f;
		static constexpr int MAX_TIME_CHECK_FAILURE = 6000;

		static constexpr float MIN_DIST_MELEE_ATTACK	= 5.f;
		static constexpr float MAX_DIST_MELEE_ATTACK	= 9.f;
	};

	struct CChimeraBase
	{
		static constexpr float DISTANCE_TO_ENEMY		= 5.f;
		static constexpr float MIN_DISTANCE_TO_ENEMY = 8.f;
		static constexpr float MAX_DISTANCE_TO_ENEMY	= 8.f;

		static constexpr int STATE_TIME_OUT	= 4000;

		static constexpr float MIN_DIST_TO_ENEMY	= 3.f;
		static constexpr float MORALE_THRESHOLD	= 0.8f;
		static constexpr int THREATEN_DELAY		= 10000;
	};

	struct CControllerBase
	{
		static constexpr int SEE_ENEMY_DURATION = 1000;
		static constexpr float GOOD_DISTANCE_FOR_CONTROL_HIT = 8.f;
		static constexpr int CONTROL_PREPARE_TIME = 2900;

		static constexpr int MAX_STATE_TIME	= 10000;
		static constexpr int DEFAULT_LOOK_POINT_CHANGE_DELAY = 2000;
		static constexpr int LOOK_COVER_PROBABILITY	= 30;

		static constexpr float MIN_ENEMY_DISTANCE	 = 10.f;
		static constexpr int STATE_MAX_TIME	= 3000;
		static constexpr int STATE_EXECUTE_DELAY = 5000;

		static constexpr float ANGLE_DISP = 1.57f;
		static constexpr float ANGLE_DISP_STEP = (10.f * 3.14f / 180.f);
		static constexpr float TRACE_STATIC_DIST = 3.0f;
		static constexpr int TIME_POINT_CHANGE_MIN = 2000;
		static constexpr int TIME_POINT_CHANGE_MAX = 5000;

		static constexpr float PMT_HEAD_BONE_LIMIT = 0.5f;
		static constexpr float PMT_TORSO_BONE_LIMIT = 1.0f;
		static constexpr float PMT_ROTATION_SPEED = 9.4f;
		static constexpr float PMT_MIN_SPEED = (10 * 3.14 / 180);

		static constexpr float PMT_PSY_ATTACK_TIME = 0.5f;

		static constexpr unsigned int PMT_PSY_ATTACK_DELAY = 2000;
		static constexpr float PMT_PSY_ATTACK_MIN_ANGLE = (5.f * 3.14f / 180);

		static constexpr unsigned int DEFAULT_TUBE_CONDITION_SEE_DURATION = 50;
		static constexpr unsigned int DEFAULT_TUBE_CONDITION_MIN_DELAY = 10000;
		static constexpr float DEFAULT_TUBE_CONDITION_MIN_DISTANCE = 10.0f;
		static constexpr float DEFAULT_STAMINA_HIT = 0.2f;

		static constexpr int	FAKE_AURA_DURATION	= 3000;
		static constexpr int	FAKE_AURA_DELAY		= 8000;
		static constexpr float FAKE_MAX_ADD_DIST	= 90.f;
		static constexpr float FAKE_MIN_ADD_DIST	= 20.f;
	};

	struct CPoltergeistBase
	{
		static constexpr int FIND_POINT_ATTEMPT_COUNT = 5;
	};

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