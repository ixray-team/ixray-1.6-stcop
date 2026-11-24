#pragma once

#include "light.h"

class	CLight_DB
{
private:
	xr_vector<ref_light>	v_static;
public:
	ref_light				sun_original;
	ref_light				sun_adapted;
	// The direction the level's light was baked with, and the one the frame lights by: the baked frame keeps to
	// the first, so what the sun casts stays where the lightmaps have it
	Fvector					SunBaked = { 0.f, -1.f, 0.f };
	Fvector					SunDirection() const;
	// The sun lights from where the level's lightmaps were baked with it, not from where the weather has it
	bool IsSunBaked() const;
	light*					rain_light = nullptr;
public:
	void					add_light			(light*		L	);

	void					Load				(IReader*	fs	);
	// Sun and rain lights for the editors, which have no level light chunk
	void					LoadEditor			();
	void					Unload				();

	light*					Create				();
	void					Update				();

	CLight_DB				();
	~CLight_DB				();
};
