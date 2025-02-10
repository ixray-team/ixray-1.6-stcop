#include	"pch.h"

#include	"animation.h"

using	namespace animation;

const channal_rule channels::rules[channels::max] = { { mix_type::lerp, mix_type::lerp},
	{ mix_type::lerp, mix_type::lerp},
	{ mix_type::lerp, mix_type::add},
	{ mix_type::lerp, mix_type::add} };

channels::channels( )
{
	init( );
}

void	channels::init( )
{
	std::fill( factors, factors + max, 1.f );
}

void channels::	set_factor	(  u16 channel, float factor )
{
	VERIFY( channel < max );
	factors[channel] = factor;
}