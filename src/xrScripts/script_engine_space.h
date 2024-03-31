////////////////////////////////////////////////////////////////////////////
//	Module 		: script_engine_space.h
//	Created 	: 01.04.2004
//  Modified 	: 18.03.2005
//	Author		: Dmitriy Iassenev
//	Description : Script engine space
////////////////////////////////////////////////////////////////////////////

#pragma once

namespace ScriptEngine {
	enum EScriptProcessors {
		eScriptProcessorLevel = u32(0),
		eScriptProcessorGame,
		eScriptProcessorHelper,
		eScriptProcessorDummy = u32(-1),
	};
};