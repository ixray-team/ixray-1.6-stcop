////////////////////////////////////////////////////////////////////////////
//	Module 		: object_item_script.cpp
//	Created 	: 27.05.2004
//  Modified 	: 30.06.2004
//	Author		: Dmitriy Iassenev
//	Description : Object item script class
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "object_item_script.h"
#include "object_factory.h"

#ifndef NO_XR_GAME
#include "ai_space.h"
#endif

const CObjectItemScript::SCreators* CObjectItemScript::FindCreators() const
{
#ifndef NO_XR_GAME
	const CScriptEngine* engine = g_pScriptEngine;
#else
	const CScriptEngine* engine = nullptr;
#endif
	const auto it = Creators.find(engine);
	return it == Creators.end() ? nullptr : &it->second;
}

void CObjectItemScript::AddCreators(const CObjectItemScript& other)
{
	for (const auto& [engine, creators] : other.Creators)
		Creators[engine] = creators;
}

void CObjectItemScript::ReleaseCreators(const CScriptEngine* engine)
{
	Creators.erase(engine);
}

bool CObjectItemScript::HasCreators(const CScriptEngine* engine) const
{
	return Creators.find(engine) != Creators.end();
}

#ifndef NO_XR_GAME
#	include "attachable_item.h"

ObjectFactory::CLIENT_BASE_CLASS *CObjectItemScript::client_object	() const
{
	const SCreators* creators = FindCreators();
	if (creators == nullptr)
		return nullptr;

	ObjectFactory::CLIENT_SCRIPT_BASE_CLASS	*object;
	try {
		object	= luabind::object_cast<ObjectFactory::CLIENT_SCRIPT_BASE_CLASS*>(creators->Client(),luabind::adopt<0>());
	}
	catch(...) {
		return	(0);
	}
	R_ASSERT	(object);
	return		(object->_construct());
}

const std::type_info* CObjectItemScript::client_type() const
{
	const SCreators* creators = FindCreators();
	if (creators == nullptr || !creators->Client.is_valid())
		return nullptr;
	lua_State* L = creators->Client.lua_state();
	if (L == nullptr)
	{
		return nullptr;
	}

	creators->Client.pushvalue();
	const luabind::detail::class_rep* Class = nullptr;
	if (lua_getmetatable(L, -1) != 0)
	{
		lua_getfield(L, -1, "__luabind_classrep");
		if (lua_toboolean(L, -1))
		{
			Class = static_cast<luabind::detail::class_rep*>(lua_touserdata(L, -3));
		}
		lua_pop(L, 2);
	}
	lua_pop(L, 1);

	while (Class != nullptr && Class->get_class_type() != luabind::detail::class_rep::cpp_class)
	{
		Class = Class->bases().empty() ? nullptr : Class->bases().front().base;
	}
	return Class != nullptr ? Class->type() : nullptr;
}

#endif

ObjectFactory::SERVER_BASE_CLASS *CObjectItemScript::server_object	(const char* section) const
{
	typedef ObjectFactory::SERVER_SCRIPT_BASE_CLASS		SERVER_SCRIPT_BASE_CLASS;
	typedef ObjectFactory::SERVER_BASE_CLASS			SERVER_BASE_CLASS;
	SERVER_SCRIPT_BASE_CLASS	*object;
	const SCreators* creators = FindCreators();
	if (creators == nullptr)
		return nullptr;

	try {
		luabind::object	*instance = 0;
		try {
			instance	= new luabind::object((luabind::object)(creators->Server(section)));
		}
		catch(const std::exception& e) {
			Msg			("Exception [%s] raised while creating server object from section [%s]", e.what(),section);
			return		(0);
		}
		catch(...) {
			Msg			("Exception raised while creating server object from section [%s]",section);
			return		(0);
		}
		object			= luabind::object_cast<ObjectFactory::SERVER_SCRIPT_BASE_CLASS*>(*instance,luabind::adopt<0>());
		xr_delete		(instance);
	}
	catch(const std::exception& e) {
		Msg				("Exception [%s] raised while casting and adopting script server object from section [%s]", e.what(),section);
		return			(0);
	}
	catch(...) {
		Msg				("Exception raised while creating script server object from section [%s]", section);
		return			(0);
	}

	R_ASSERT			(object);
	SERVER_BASE_CLASS	*o = object->init();
	R_ASSERT			(o);
	return				(o);
}

CObjectItemScript::CObjectItemScript	(
#ifndef NO_XR_GAME
	luabind::object		client_creator, 
#endif
	luabind::object		server_creator, 
	const CLASS_ID		&clsid, 
	const char*				script_clsid
) : 
	inherited								(clsid,script_clsid)
{
#ifndef NO_XR_GAME
	Creators[g_pScriptEngine].Client = client_creator;
	Creators[g_pScriptEngine].Server = server_creator;
#else
	Creators[nullptr].Server = server_creator;
#endif
}

#ifndef NO_XR_GAME

CObjectItemScript::CObjectItemScript	(
	luabind::object		unknown_creator, 
	const CLASS_ID		&clsid, 
	const char*				script_clsid
) : 
	inherited								(clsid,script_clsid)
{
	Creators[g_pScriptEngine].Client = unknown_creator;
	Creators[g_pScriptEngine].Server = unknown_creator;
}

#endif
