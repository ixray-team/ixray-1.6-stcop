#pragma once

enum
{
	AF_GODMODE					= (1 << 0),
	AF_NO_CLIP					= (1 << 1),
	AF_UNLIMITEDAMMO			= (1 << 3),
	AF_RUN_BACKWARD				= (1 << 4),
	AF_AUTOPICKUP				= (1 << 5),
	AF_DYNAMIC_MUSIC			= (1 << 7),
	AF_DISABLE_CONDITION_TEST	= (1 << 8),
	AF_IMPORTANT_SAVE			= (1 << 9),
	AF_CROUCH_TOGGLE			= (1 << 10),
	AF_RIGHT_SHOULDER			= (1 << 11),
	AF_DISPLAY_VOICE_ICON		= (1 << 12),
	AF_SPRINT_TOGGLE			= (1 << 13),
};

extern Flags32	psActorFlags;
extern BOOL		GodMode	();	
