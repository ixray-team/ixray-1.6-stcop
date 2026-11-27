////////////////////////////////////////////////////////////////////////////
//	Module 		: object_item_script.h
//	Created 	: 27.05.2004
//  Modified 	: 30.06.2004
//	Author		: Dmitriy Iassenev
//	Description : Object item script class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "object_factory_space.h"
#include "object_item_abstract.h"

class CScriptEngine;

class CObjectItemScript : public CObjectItemAbstract {
protected:
	typedef CObjectItemAbstract inherited;

	struct SCreators
	{
#ifndef NO_XR_GAME
		luabind::object Client;
#endif
		luabind::object Server;
	};

	xr_map<const CScriptEngine*, SCreators> Creators;
	const SCreators* FindCreators() const;

public:
	void AddCreators(const CObjectItemScript& other);
	void ReleaseCreators(const CScriptEngine* engine);
	bool HasCreators(const CScriptEngine* engine) const;

public:
												CObjectItemScript	(
#ifndef NO_XR_GAME
													luabind::object client_creator, 
#endif
													luabind::object server_creator, 
													const CLASS_ID &clsid, 
													const char* script_clsid
												);
#ifndef NO_XR_GAME
												CObjectItemScript	(
													luabind::object creator, 
													const CLASS_ID &clsid, 
													const char* script_clsid
												);
	virtual ObjectFactory::CLIENT_BASE_CLASS	*client_object		() const override;
	virtual const std::type_info*				client_type			() const override;
#endif
	virtual ObjectFactory::SERVER_BASE_CLASS	*server_object		(const char* section) const override;
};
