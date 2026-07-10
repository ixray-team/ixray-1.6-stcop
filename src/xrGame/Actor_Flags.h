#pragma once

enum
{
	AF_GOD_MODE					= (1 << 0),
	AF_NO_CLIP					= (1 << 1),
	AF_UNLIMITED_AMMO			= (1 << 3),
	AF_RUN_BACKWARD				= (1 << 4),
	AF_AUTO_PICKUP				= (1 << 5),
	AF_DYNAMIC_MUSIC			= (1 << 7),
	AF_DISABLE_CONDITION_TEST	= (1 << 8),
	AF_IMPORTANT_SAVE			= (1 << 9),
	AF_CROUCH_TOGGLE			= (1 << 10),
	AF_RIGHT_SHOULDER			= (1 << 11),
	AF_DISPLAY_VOICE_ICON		= (1 << 12),
	AF_INFINITE_FIRE			= (1 << 13),
	AF_INFINITE_DURABILITY		= (1 << 14),
	AF_HIT_SLOWMO				= (1 << 15),
	AF_3D_ICONS_INV				= (1 << 16),
};

extern Flags32 psActorFlags;
extern ICF bool GodMode();	
