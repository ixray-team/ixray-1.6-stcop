#ifndef dxThunderboltRender_included
#define dxThunderboltRender_included
#pragma once

#include "../../Include/xrRender/ThunderboltRender.h"
#include "IRenderDetailModel.h"

class dxThunderboltRender : public IThunderboltRender
{
public:
	dxThunderboltRender();
	virtual ~dxThunderboltRender() override;

	virtual void Copy(IThunderboltRender &_in) override;

	virtual void Render(CEffect_Thunderbolt &owner) override;

	// What a frame draws of the bolt, taken from the game's with the frame's collect; Draw reads nothing else
	static void Take(CEffect_Thunderbolt& Owner);
	static void Draw();

private:
	struct FrameState
	{
		struct SThunderboltDesc* current;
		Fmatrix current_xform;
		Fvector lightning_center;
		float lightning_size, lightning_phase;
	};

	void DrawState();

	static dxThunderboltRender* Frame;
	FrameState State;
	ref_geom			  		hGeom_model;
	ref_geom			  		hGeom_gradient;
	// The model as the stream draws it: its triangles' vertices one after another
	xr_vector<IRender_DetailModel::fvfVertexOut> ModelVertices;
	xr_vector<u16> ModelIndices;
};

#endif	//	ThunderboltRender_included