#pragma once

#include "associative_vector.h"

#define MIN_LEGS_COUNT	1
#define MAX_LEGS_COUNT	4 

#include "../include/xrRender/animation_motion.h"

struct SStepParam {
	struct{
		float	time;
		float	power;
	} step[MAX_LEGS_COUNT];

	u8			cycles;
};

//using STEPS_MAP = xr_map<MotionID, SStepParam>; 
//using STEPS_MAP_IT = STEPS_MAP::iterator;

using STEPS_MAP = associative_vector<MotionID, SStepParam>;
using STEPS_MAP_IT = STEPS_MAP::iterator;

struct SStepInfo {
	struct {
		bool			handled;		// обработан
		u8				cycle;			// цикл в котором отработан
	} activity[MAX_LEGS_COUNT];

	SStepParam		params;
	bool			disable;

	u8				cur_cycle;

	SStepInfo()		{disable = true;}
};

enum ELegType {
	eFrontLeft,
	eFrontRight,
	eBackRight,
	eBackLeft
};

