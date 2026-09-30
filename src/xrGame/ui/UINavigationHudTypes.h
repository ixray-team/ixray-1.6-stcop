#pragma once

enum class ENavigationHudMode : u8
{
	Minimap,
	CompassBar,
};

enum class ENavigationHudState : u8
{
	Minimap,
	Compass,
	Transitioning,
	FailedInit,
};
