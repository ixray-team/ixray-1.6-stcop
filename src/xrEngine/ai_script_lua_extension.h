////////////////////////////////////////////////////////////////////////////
//	Module 		: ai_script_lua_extension.h
//	Created 	: 19.09.2003
//  Modified 	: 22.09.2003
//	Author		: Dmitriy Iassenev
//	Description : XRay Script extensions
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "ai_script_space.h"
//struct CLuaVirtualMachine;

namespace Script {
#ifndef ENGINE_BUILD
	void				vfExportGlobals				(CLuaVirtualMachine *tpLuaVM);
	void				vfExportFvector				(CLuaVirtualMachine *tpLuaVM);
	void				vfExportFmatrix				(CLuaVirtualMachine *tpLuaVM);
	void				vfExportGame				(CLuaVirtualMachine *tpLuaVM);
	void				vfExportLevel				(CLuaVirtualMachine *tpLuaVM);
	void				vfExportDevice				(CLuaVirtualMachine *tpLuaVM);
	void				vfExportParticles			(CLuaVirtualMachine *tpLuaVM);
	void				vfExportSound				(CLuaVirtualMachine *tpLuaVM);
	void				vfExportHit					(CLuaVirtualMachine *tpLuaVM);
	void				vfExportActions				(CLuaVirtualMachine *tpLuaVM);
	void				vfExportObject				(CLuaVirtualMachine *tpLuaVM);
	void				vfExportEffector			(CLuaVirtualMachine *tpLuaVM);
	void				vfExportArtifactMerger		(CLuaVirtualMachine *tpLuaVM);
	void				vfLoadStandardScripts		(CLuaVirtualMachine *tpLuaVM);
	void				vfExportMemoryObjects		(CLuaVirtualMachine *tpLuaVM);
	void				vfExportToLua				(CLuaVirtualMachine *tpLuaVM);
	void				vfExportActionManagement	(CLuaVirtualMachine *tpLuaVM);
	void				vfExportMotivationManagement(CLuaVirtualMachine *tpLuaVM);
	bool				bfLoadFile					(CLuaVirtualMachine *tpLuaVM, const char*	caScriptName,	bool	bCall = true);
	void				LuaHookCall					(CLuaVirtualMachine *tpLuaVM, lua_Debug *tpLuaDebug);
	int					LuaPanic					(CLuaVirtualMachine *tpLuaVM);
#endif
	bool				bfPrintOutput				(CLuaVirtualMachine *tpLuaVM, const char*	caScriptName, int iErorCode = 0);
	const char*				cafEventToString			(int				iEventCode);
	void				vfPrintError				(CLuaVirtualMachine *tpLuaVM, int		iErrorCode);
	bool				bfListLevelVars				(CLuaVirtualMachine *tpLuaVM, int		iStackLevel);
	bool				bfLoadBuffer				(CLuaVirtualMachine *tpLuaVM, const char*	caBuffer,		size_t	tSize,				const char*	caScriptName, const char* caNameSpaceName = 0);
	bool				bfLoadFileIntoNamespace		(CLuaVirtualMachine *tpLuaVM, const char*	caScriptName,	const char*	caNamespaceName,	bool	bCall);
	bool				bfGetNamespaceTable			(CLuaVirtualMachine *tpLuaVM, const char*	caName);
	CLuaVirtualMachine	*get_namespace_table		(CLuaVirtualMachine *tpLuaVM, const char*	caName);
	bool				bfIsObjectPresent			(CLuaVirtualMachine *tpLuaVM, const char*	caIdentifier,	int type);
	bool				bfIsObjectPresent			(CLuaVirtualMachine *tpLuaVM, const char*	caNamespaceName, const char*	caIdentifier, int type);
	luabind::object		lua_namespace_table			(CLuaVirtualMachine *tpLuaVM, const char* namespace_name);
};
