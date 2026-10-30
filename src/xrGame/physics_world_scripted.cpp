#include "StdAfx.h"
#include "pch_script.h"

#include "physics_world_scripted.h"

#include "Level.h"

#include "PHCommander.h"

void	cphysics_world_scripted::	AddCall			(	CPHCondition*c, CPHAction*a )	
{
	Level().ph_commander_physics_worldstep().add_call(c,a);
}
