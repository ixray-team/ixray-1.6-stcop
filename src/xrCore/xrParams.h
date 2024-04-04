#pragma once

enum ECoreParams
{
	// Core
	auto_load_arch = 1<<0,
	overlaypath    = 1<<1,
	nolog          = 1<<2,
	build          = 1<<3,
	ebuild         = 1<<4,
	mem_debug      = 1<<5,

	// Engine
	xclsx               = 1<<6,
	no_center_screen    = 1<<7,
	dxdebug             = 1<<8,
	perfhud_hack        = 1<<9,
	nes_texture_storing = 1<<10,
	noprefetch          = 1<<11,
	demomode            = 1<<12,
	nosound             = 1<<13,
	r2                  = 1<<14,
	r4                  = 1<<15,

	// Render
	nocolormap = 1<<16,
	skinw      = 1<<17,
	disasm     = 1<<18,
	nonvs      = 1<<19,
	tsh        = 1<<20,
	noshadows  = 1<<21,
	ss_tga     = 1<<22,
	no_occq    = 1<<23,
	nodistort  = 1<<24,

	// Game
	use_callstack = 1<<25,
	debug_ge      = 1<<26,
	
	// API
	renderdoc = 1<<27,
};

void LoadParams();