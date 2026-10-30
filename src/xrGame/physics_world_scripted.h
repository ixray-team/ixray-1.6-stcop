#pragma once

#include "../xrPhysics/iphysics_scripted.h"
#include "../xrPhysics/IPHWorld.h"
#include "../xrScripts/script_export_space.h"
#include "PHCommander.h"

XREFLECT(LuaExport, Name="physics_world")
class cphysics_world_scripted:
public cphysics_game_scripted<IPHWorld>
{
public:
					cphysics_world_scripted		(IPHWorld* imp ):cphysics_game_scripted<IPHWorld>(imp){}				
			
	XPROPERTY(LuaCallable, Name="gravity")
		float		Gravity						( )									{	return physics_impl().Gravity(); }
	XPROPERTY(LuaCallable, Name="set_gravity")
		void		SetGravity					( float	g )							{	return physics_impl().SetGravity( g ); }
	XPROPERTY(LuaCallable, Name="add_call")
		void		AddCall						(	CPHCondition*c, CPHAction*a )	;
};