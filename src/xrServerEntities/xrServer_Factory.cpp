////////////////////////////////////////////////////////////////////////////
//	Module 		: xrServer_Factory.cpp
//	Created 	: 19.09.2002
//  Modified 	: 04.06.2003
//	Author		: Oles Shyshkovtsov, Alexander Maksimchuk, Victor Reutskiy and Dmitriy Iassenev
//	Description : Server objects factory
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "object_factory.h"

CSE_Abstract* F_entity_Create(const char* section)
{
	// A missing section (or one without a server 'class') is a data problem,
	// not a fatal one: log and skip so a bad spawn request can never take the
	// game down (viewer/sandbox spawns hit this with arbitrary section names).
	if (pSettings->section_exist(section) && pSettings->line_exist(section, "class"))
	{
		return (object_factory().server_object(pSettings->r_clsid(section, "class"), section));
	}

	Msg("! Error: cannot spawn - section '%s' is missing or has no 'class' line", section);
	return nullptr;
}
