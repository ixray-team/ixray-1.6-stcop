////////////////////////////////////////////////////////////////////////////
//	Module 		: script_export_space.h
//	Created 	: 22.09.2003
//  Modified 	: 01.04.2004
//	Author		: Dmitriy Iassenev
//	Description : XRay Script export space 
////////////////////////////////////////////////////////////////////////////

#ifndef script_export_spaceH
#define script_export_spaceH

#pragma once

struct lua_State;

#define DECLARE_SCRIPT_REGISTER_FUNCTION public: static void script_register(lua_State *);
#define DECLARE_SCRIPT_REGISTER_FUNCTION_STRUCT static void script_register(lua_State *);

template <typename T> struct enum_exporter{DECLARE_SCRIPT_REGISTER_FUNCTION};
template <typename T> struct class_exporter{DECLARE_SCRIPT_REGISTER_FUNCTION};

template <typename T> struct SCRIPTS_API enum_exporter_lib { DECLARE_SCRIPT_REGISTER_FUNCTION };
template <typename T> struct SCRIPTS_API class_exporter_lib { DECLARE_SCRIPT_REGISTER_FUNCTION };

#endif