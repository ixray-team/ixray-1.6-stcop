#pragma once

enum
{
		AF_GODMODE				= (1<<0),
		AF_INVISIBLE			= (1<<1),
		AF_ALWAYSRUN			= (1<<2),
		AF_UNLIMITEDAMMO		= (1<<3),
		AF_RUN_BACKWARD			= (1<<4),
		AF_AUTOPICKUP			= (1<<5),
		AF_PSP					= (1<<6),
		AF_USE_CROSSHAIR		= (1<<7),
		AF_WPN_BOBBING			= (1<<8),
		AF_COLLISION			= (1<<9),
		AF_STRAFE_INERT			= (1<<10),
		AF_FST_PSN_DEATH		= (1<<11),
		AF_ACTOR_BODY			= (1<<12),
		AF_HEAD_BOBBING			= (1<<13),
		AF_CORRECT_FIREPOS		= (1<<14),
};

extern Flags32 psActorFlags;

extern BOOL		GodMode	();	

