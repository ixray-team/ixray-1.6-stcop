////////////////////////////////////////////////////////////////////////////
//	Module 		: object_item_abstract.h
//	Created 	: 27.05.2004
//  Modified 	: 30.06.2004
//	Author		: Dmitriy Iassenev
//	Description : Object item abstract class
////////////////////////////////////////////////////////////////////////////

#ifndef object_item_abstractH
#define object_item_abstractH

#pragma once

#include "object_factory_space.h"

class CObjectItemAbstract
{
protected:
	CLASS_ID									m_clsid;
	shared_str									m_script_clsid;

public:
	IC											CObjectItemAbstract	(const CLASS_ID &clsid, const char* script_clsid);
	virtual ~CObjectItemAbstract() = default;

	IC		const CLASS_ID						&clsid				() const;
	IC		shared_str							script_clsid		() const;
#ifndef NO_XR_GAME
	virtual ObjectFactory::CLIENT_BASE_CLASS	*client_object		() const = 0;

	// The ltx schemas the client class reads its section with, for the editor
	using IniSchemaCollector = void (*)(xr_vector<const char*>& Out);
	IniSchemaCollector							ini_schemas			= nullptr;
	virtual const std::type_info*				client_type			() const { return nullptr; }
#endif
	virtual ObjectFactory::SERVER_BASE_CLASS	*server_object		(const char* section) const = 0;
};

#include "object_item_abstract_inline.h"
#endif