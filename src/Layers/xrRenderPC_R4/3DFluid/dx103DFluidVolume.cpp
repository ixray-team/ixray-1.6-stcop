#include "stdafx.h"
#include "dx103DFluidVolume.h"

#include "dx103DFluidManager.h"
#include "../../../xrEngine/EngineAPI.h"

dx103DFluidVolume::dx103DFluidVolume()
{
}

dx103DFluidVolume::~dx103DFluidVolume()
{

}

void dx103DFluidVolume::Load( const char* N, IReader *data, u32 dwFlags )
{
	//	Uncomment this if choose to read from OGF
//	dxRender_Visual::Load		(N,data,dwFlags);

	//	Create shader for correct sort while rendering
	//	shader name can't start from a digit
	shader.create("fluid3d_stub","water\\water_ryaska1");

	Type = MT_3DFLUIDVOLUME;

	//	Version 3>
	m_FluidData.Load(data);

	//	Prepare transform
	const Fmatrix		&Transform = m_FluidData.GetTransform();

	//	Update visibility data
	vis.box.min = Fvector3().set(-0.5f, -0.5f, -0.5f);
	vis.box.max = Fvector3().set( 0.5f,  0.5f,  0.5f);

	vis.box.xform(Transform);

	vis.box.getcenter(vis.sphere.P);
	vis.sphere.R = vis.box.getradius();

/*
	//	Version 2
	//	Prepare transform
	Fmatrix		Transform;
	data->r( &Transform, sizeof(Transform) );
	m_FluidData.SetTransform(Transform);

	//	Update visibility data
	vis.box.min = Fvector3().set(-0.5f, -0.5f, -0.5f);
	vis.box.max = Fvector3().set( 0.5f,  0.5f,  0.5f);

	vis.box.xform(Transform);

	vis.box.getcenter(vis.sphere.P);
	vis.sphere.R = vis.box.getradius();

	//	Read obstacles
	u32 uiObstCnt = data->r_u32();
	for(u32 i=0; i<uiObstCnt; ++i)
	{
		Fmatrix		ObstTransform;
		data->r( &ObstTransform, sizeof(ObstTransform) );
		m_FluidData.AddObstacle(ObstTransform);
	}
	*/

	//	Version 0
	/*
	Fbox	B;
	data->r( &B, sizeof(B) );

	Fmatrix		Transform;
	Fvector3	temp;

	B.getsize(temp);
	Transform.scale(temp);

	B.getcenter(temp);
	Transform.translate_over(temp);

	m_FluidData.SetTransform(Transform);

	vis.box.set(B);
	//vis.box.min = Fvector3().set(-0.5f, -0.5f, -0.5f);
	//vis.box.max = Fvector3().set( 0.5f,  0.5f,  0.5f);

	vis.box.getcenter(vis.sphere.P);
	vis.sphere.R = vis.box.getradius();
	*/
}

void dx103DFluidVolume::Render( GfxCmdList& Cmd, float LOD )		// LOD - Level Of Detail  [0.0f - min, 1.0f - max], Ignored ?
{
	if (LightingModeIsStaticRender())
		return;
	//float fTimeStep = Device.fTimeDelta*30*2.0f;
	const float fTimeStep = 2.0f;

	//FluidManager.Update( m_FluidData, 2.0f);
	FluidManager.Update( Cmd, m_FluidData, fTimeStep);
	FluidManager.RenderFluid( Cmd, m_FluidData );
}

void dx103DFluidVolume::Copy( dxRender_Visual *pFrom )
{
	dxRender_Visual::Copy(pFrom);
}

void dx103DFluidVolume::Release()
{
	dxRender_Visual::Release();
}