#pragma once

enum class ECoreParams : u64
{
	// Core
	auto_load_arch = 1 << 0,
	overlaypath    = 1 << 1,
	nolog          = 1 << 2, // Отключить логирование в файл
	build          = 1 << 3,
	ebuild         = 1 << 4,
	//mem_debug      = 1<<5,

	// Engine
	xclsx               = 1 << 6, // Отключение отрисовки красного текста в дебаге
	no_center_screen	= 1 << 7, // Не центрировать окно при старте
	dxdebug				= 1 << 8, // Enable DirectX debug device
	perfhud_hack        = 1 << 9, // Принудительно активировать все рендеры
	nes_texture_storing = 1 << 10,
	noprefetch          = 1 << 11, // Отключить префетч
	demomode			= 1 << 12,		// Проигрывание анимации деморекорда
	nosound				= 1 << 13, // Отключить звук
	r2					= 1 << 14, // Принудительно включить рендерер R2
	r4					= 1 << 15, // Принудительно включить рендерер R4

	// Render
	nocolormap = 1<<16,
	skinw      = 1 << 17,
	disasm     = 1 << 18, // Enable shader disasm
	nonvs      = 1 << 19, // No nvidia depthstencil (dx9 only)
	tsh        = 1 << 20,
	noshadows  = 1 << 21, // Отключить тени
	//ss_tga     = 1<<22,
	no_occq    = 1 << 23, // Отключить очередь окклюзии
	nodistort  = 1 << 24, // Отключить искажения

	// Game
	use_callstack = 1 << 25, // Включить вывод callstack при ошибках LUA
	debug_ge      = 1 << 26,
	
	// API
	renderdoc = 1 << 27, // Enable RenderDoc capture

	// Lua
	keep_lua = 1 << 28, // Не выгружать Lua при смене уровней
	no_debug_panel = 1 << 29, // Отключить ImGui панель отладки

	// Test
	autotest = 1 << 30, // Прогнать N кадров без окна и записать отчёт
	
	// SDK
	no_silent_errors = u64(1) << 31, // Включить назад работу брейкпоинтов в SDK
};

void LoadParams();