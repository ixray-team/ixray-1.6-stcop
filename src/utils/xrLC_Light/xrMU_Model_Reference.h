#pragma once

#include "base_color.h"
#include "mu_model_face.h"

class xrMU_Model;
struct FaceDataEmbree;

namespace CDB { class CollectorPacked; }
  
class XRLC_LIGHT_API xrMU_Reference
{
public:
	xrMU_Model*				model;
	u32 ModelID = u32(-1);
    Fmatrix					xform;
    Flags32					flags;
	u16						sector;

	xr_vector<base_color>	color;

	base_color_c			c_scale;
	base_color_c			c_bias;

	xr_stack_string256		debug_name;
	
public:
 	xr_concurrent_unordered_map<size_t, base_color_c> colors_cuda;

public:
							xrMU_Reference		(): model(0), sector(u16(-1)), flags(Flags32().assign(0)), xform(Fidentity){}
							~xrMU_Reference() 
							{ 
 								colors_cuda.clear(); 
 
								color.clear();
								color.shrink_to_fit();
							}

	void					Load				( IReader& fs, xr_vector<xrMU_Model*>& mu_models );
	
	void					calc_lighting		();
#ifdef LCCUDA_BUILD
	void					calc_lighting_cuda_1  ();
	void					calc_lighting_cuda_2  ();
	void					calc_lighting_cuda_3  ();
#endif
	void					export_cform_game	(CDB::CollectorPacked& CL);
	void					export_cform_rcast	(CDB::CollectorPacked& CL); 

	void					export_cform_rcast_new(xr_vector<FaceDataEmbree>& faces);
	void					export_cform_game_new(xr_vector<FaceDataEmbree>& faces);

};
 