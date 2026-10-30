////////////////////////////////////////////////////////////////////////////
//	Module 		: script_zone.h
//	Created 	: 10.10.2003
//  Modified 	: 10.10.2003
//	Author		: Dmitriy Iassenev
//	Description : Script zone object
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "space_restrictor.h"
#include "../xrScripts/script_export_space.h"

XREFLECT(LuaExport, Name="ce_smart_zone", Constructible)
class CSmartZone : 
	public CSpaceRestrictor
{
public:
	virtual bool	register_schedule	() const override {return true;}
};