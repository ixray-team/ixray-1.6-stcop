#ifndef r_backend_hemiH
#define r_backend_hemiH
#pragma once

class ECORE_API	R_hemi
{
public:
	RHIShaderConstant*		c_pos_faces;
	RHIShaderConstant*		c_neg_faces;
	RHIShaderConstant*		c_material;

	RHIShaderConstant*		c_lit_color;
	RHIShaderConstant*		c_lit_dir;

public:
	R_hemi		();
	void			unmap		();

	void			set_c_pos_faces		(RHIShaderConstant* C) {c_pos_faces = C;}
	void			set_c_neg_faces		(RHIShaderConstant* C) {c_neg_faces = C;}
	void			set_c_material		(RHIShaderConstant* C) {c_material  = C;}

	void			set_c_lit_color		(RHIShaderConstant* C) { c_lit_color = C; }
	void			set_c_lit_dir		(RHIShaderConstant* C) { c_lit_dir = C; }

	void			set_pos_faces		(float posx, float posy, float posz);
	void			set_neg_faces		(float negx, float negy, float negz);
	void			set_material		(float x, float y, float z, float w);

	void			set_lit_color(Fvector color, Fvector dir);
};
#endif
