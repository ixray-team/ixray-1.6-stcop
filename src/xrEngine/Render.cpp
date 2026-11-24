#include "stdafx.h"
#include "Render.h"
#include "IGame_Persistent.h"
#include "Environment.h"

//ENGINE_API	IRender_interface*	Render		= nullptr;

// resources
IRender_Light::~IRender_Light	()			{	
	::Render->light_destroy		(this	);
}
IRender_Glow::~IRender_Glow		()			{	
	::Render->glow_destroy		(this	);	
}

Fvector IRender_interface::SunDirection()
{
	return g_pGamePersistent->Environment().CurrentEnv->sun_dir;
}
