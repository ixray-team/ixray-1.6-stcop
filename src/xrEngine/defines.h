#pragma once

#ifdef ENGINE_API
#ifdef	DEBUG_DRAW
	ENGINE_API	extern BOOL			bDebug;
#else
	#define bDebug 0
#endif

#define _RELEASE(x)			{ if(x) { (x)->Release();       (x)=NULL; } }
#define _SHOW_REF(msg, x)   { if(x) { x->AddRef(); Msg("%s %d", msg,u32(x->Release()));}}

// textures
ENGINE_API extern	int		psTextureLOD		;

// psGameFlags
enum {
	rsActorShadow					= (1ul<<0ul),
	rsDrawPortals					= (1ul<<1ul),
};


ENGINE_API extern	u32			psCurrentBPP		;
ENGINE_API extern	Flags32		psGameFlags			;

#endif 

// game path definition
#define _game_data_				"$game_data$"
#define _game_textures_			"$game_textures$"
#define _game_levels_			"$game_levels$"
#define _game_sounds_			"$game_sounds$"
#define _game_meshes_			"$game_meshes$"
#define _game_shaders_			"$game_shaders$"
#define _game_config_			"$game_config$"
#define _game_fonts_			"$game_fonts$"

// editor path definition
#define _server_root_		    "$server_root$"
#define _server_data_root_	    "$server_data_root$"
#define _local_root_		    "$local_root$"
#define _import_			    "$import$"
#define _sounds_			    "$sounds$"
#define _textures_			    "$textures$"
#define _objects_			    "$objects$"
#define _maps_				    "$maps$"
#define _temp_				    "$temp$"
#define _omotion_			    "$omotion$"
#define _omotions_			    "$omotions$"
#define _smotion_			    "$smotion$"
#define _detail_objects_	    "$detail_objects$"