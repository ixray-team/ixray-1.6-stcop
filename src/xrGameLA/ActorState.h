#pragma once
#ifndef __ACTOR_STATE_H__
#define __ACTOR_STATE_H__

enum EActorState
{
	eJammedInactive = 0,
	eJammedRed,
	eJammedYellow,
	eJammedGreen,
	eRadiationInactive,
	eRadiationRed,
	eRadiationYellow,
	eRadiationGreen,
	eBleedingInactive,
	eBleedingRed,
	eBleedingYellow,
	eBleedingGreen,
	eHungerInactive,
	eHungerRed,
	eHungerYellow,
	eHungerGreen,
	eThirstInactive,
	eThirstRed,
	eThirstYellow,
	eThirstGreen,
	ePsyHealthInactive,
	ePsyHealthRed,
	ePsyHealthYellow,
	ePsyHealthGreen,
	eSleepInactive,
	eSleepRed,
	eSleepYellow,
	eSleepGreen,
};

#endif