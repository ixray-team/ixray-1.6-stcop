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
#if 0
	virtual MStatus		legalConnection(const MPlug& plug, const MPlug& other_plug, bool as_src, bool& is_legal) const;
	virtual MStatus		connectionMade(const MPlug& plug, const MPlug& other_plug, bool as_src);
	virtual MStatus		connectionBroken(const MPlug& plug, const MPlug& other_plug, bool as_src);
#endif

	static void*		creator();
	static MStatus		init();

	static MStatus		initialize(MFnPlugin& plugin_fn);
	static MStatus		uninitialize(MFnPlugin& plugin_fn);

private:
	static const MTypeId	k_type_id;
	static const MString	k_type_name;

	static MObject		g_tc;				// translucenceCoeff
	static MObject		g_drfl;				// diffuseReflectivity
	static MObject		g_cr, g_cg, g_cb, g_c;		// color
	static MObject		g_ir, g_ig, g_ib, g_ic;		// incandescence
	static MObject		g_itr, g_itg, g_itb, g_it;	// transparency
	static MObject		g_ocr, g_ocg, g_ocb, g_oc;	// color
	static MObject		g_otr, g_otg, g_otb, g_ot;	// outTransparency
	static MObject		g_nx, g_ny, g_nz, g_n;		// normalCamera
	static MObject		g_ldx, g_ldy, g_ldz, g_ld;	// lightDirection
	static MObject		g_lir, g_lig, g_lib, g_li;	// lightIntensity
	static MObject		g_la, g_ldf, g_ls;		// lightAmbient, lightDiffuse, lightSpecular
	static MObject		g_lsf;				// lightShadowFraction
	static MObject		g_psi;				// preShadowIntensity;
	static MObject		g_lbld;				// lightBlindData
	static MObject		g_ltd;				// lightDataArray

	static MObject		g_xrd;		// xrayDoubleSide
	static MObject		g_xre;		// xrayEngineShader
	static MObject		g_xrc;		// xrayCompilerShader
	static MObject		g_xrm;		// xrayGameMaterial
};

#endif
