#pragma once
#ifndef __MAYA_XRAY_MATERIAL_H__
#define __MAYA_XRAY_MATERIAL_H__

#include <maya/MPxNode.h>

class MFnPlugin;

class maya_xray_material: public MPxNode {
public:
	virtual			~maya_xray_material();

	virtual void		postConstructor();
	virtual MStatus		compute(const MPlug& plug, MDataBlock& data_block);

	static void*		creator();
	static MStatus		init();

	static MStatus		initialize(MFnPlugin& plugin_fn);
	static MStatus		uninitialize(MFnPlugin& plugin_fn);

private:
	static const MTypeId	k_type_id;
	static const MString	k_type_name;

	static MObject		g_tc;
	static MObject		g_drfl;
	static MObject		g_cr, g_cg, g_cb, g_c;
	static MObject		g_ir, g_ig, g_ib, g_ic;
	static MObject		g_itr, g_itg, g_itb, g_it;
	static MObject		g_ocr, g_ocg, g_ocb, g_oc;
	static MObject		g_otr, g_otg, g_otb, g_ot;
	static MObject		g_nx, g_ny, g_nz, g_n;
	static MObject		g_ldx, g_ldy, g_ldz, g_ld;
	static MObject		g_lir, g_lig, g_lib, g_li;
	static MObject		g_la, g_ldf, g_ls;
	static MObject		g_lsf;
	static MObject		g_psi;
	static MObject		g_lbld;
	static MObject		g_ltd;

	static MObject		g_xrd;
	static MObject		g_xre;
	static MObject		g_xrc;
	static MObject		g_xrm;
};

#endif
