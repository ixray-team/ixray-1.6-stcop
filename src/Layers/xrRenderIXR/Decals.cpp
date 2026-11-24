/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "stdafx.h"
#include "ixr.h"
#include "ShaderBindings.h"

namespace
{
	constexpr u32 LayerSize = 256;
	constexpr u32 LayerMips = 6;
}

// A mark's texture into the layer of its slot. The layers are filled as marks first use them: a slot keeps its
// texture until the marks are cleared
void CRender::FillDecalLayer(GfxCmdList& Cmd, u32 Layer, const shared_str& Texture)
{
	if (!DecalLayerTarget[Layer])
	{
		RHIRenderTargetViewDesc View{};
		View.Format = ERHI_FORMAT::R8G8B8A8_UNORM;
		View.ViewDimension = ERHI_RTV_DIMENSION::TEXTURE2DARRAY;
		View.FirstArraySlice = Layer;
		DecalLayerTarget[Layer] = GRHI->CreateRenderTargetView(DecalArray, View);
	}

	ref_shader Copy;
	Copy.create("decal_copy", Texture.c_str());

	// A texture still loading shows its placeholder: the layer is copied again every frame until it has loaded, and
	// its marks are not drawn before that
	bool Pending = false;
	u64 Revision = 0;
	(*Copy->E[0]->passes[0]->T)[0].second->CanCacheShadow(Pending, Revision);

	Target->u_setrt(Cmd, LayerSize, LayerSize, DecalLayerTarget[Layer], nullptr, nullptr, nullptr);

	// A target given by its view keeps the viewport of the one before it
	RHIViewport Whole(0.0f, 0.0f, float(LayerSize), float(LayerSize));
	Cmd.SetViewport(Whole);
	Target->DrawPassSQ(Cmd, Copy, 0);

	if (!Pending)
	{
		DecalLayerTexture[Layer] = Texture;
	}
}

// The wallmarks in view as decals, nearest first, in view space as the lights are, and the tiles they reach. A
// mark's box is as deep as a third of its half size: it wraps a corner a little and reaches no wall behind
u32 CRender::BuildForwardDecals(GfxCmdList& Cmd)
{
	GPU_EVENT(ForwardDecals);
	CreateForwardLists();

	if (!DecalArray)
	{
		RHITextureDesc Desc(LayerSize, LayerSize, ERHI_FORMAT::R8G8B8A8_UNORM);
		Desc.ArraySize = DecalLayers;
		Desc.MipLevels = LayerMips;
		Desc.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE | ERHI_BIND_FLAG::RENDER_TARGET;
		Desc.MiscFlags = u32(ERHI_RESOURCE_MISC_FLAG::GENERATE_MIPS);
		DecalArray = GRHI->CreateRenderTarget(Desc);

		RHIShaderResourceViewDesc View{};
		View.Format = Desc.Format;
		View.ViewDimension = ERHI_SRV_DIMENSION::TEXTURE2DARRAY;
		View.MipLevels = LayerMips;
		View.ArraySize = DecalLayers;
		DecalArrayView = GRHI->CreateShaderResourceView(DecalArray, &View);
	}

	static CWallmarksEngine::DecalSource Sources[SCENE_DECALS];
	const u32 Decals = Wallmarks->CollectDecals(Sources, SCENE_DECALS, RFrame.vCameraPosition);
	u64 Filled = 0;

	for (u32 Each = 0; Each < Decals; ++Each)
	{
		const CWallmarksEngine::DecalSource& Source = Sources[Each];

		if (Source.Slot < DecalLayers && Source.Texture.size() && DecalLayerTexture[Source.Slot] != Source.Texture)
		{
			FillDecalLayer(Cmd, Source.Slot, Source.Texture);
			Filled |= 1ull << Source.Slot;
		}
	}

	if (Filled)
	{
		NriPasses.GenerateMips(Cmd, DecalArray, Filled);
	}

	Fmatrix FromView;
	FromView.invert(RFrame.mView);

	RHIMappedSubresource Mapped;
	R_ASSERT(ForwardDecalBuffer->Map(ERHI_BUFFER_MAP::WRITE_DISCARD, 0, &Mapped));
	Hlsl::SceneDecal* Records = static_cast<Hlsl::SceneDecal*>(Mapped.pData);
	Records[0] = {};
	Records[0].sphere.set(float(Decals), 0.0f, 0.0f, 0.0f);

	for (u32 Each = 0; Each < Decals; ++Each)
	{
		const CWallmarksEngine::DecalSource& Source = Sources[Each];
		Hlsl::SceneDecal& Record = Records[Each + 1];
		const bool HasLayer = Source.Slot < DecalLayers && DecalLayerTexture[Source.Slot] == Source.Texture;
		Fmatrix ToBox;
		ToBox.mul(Source.Project, FromView);
		Fvector Centre;
		RFrame.mView.transform_tiny(Centre, Source.Bounds.P);

		const float Depth = 3.0f;
		Record.sphere.set(Centre.x, Centre.y, Centre.z, Source.Bounds.R);
		Record.to_box[0].set(ToBox._11, ToBox._21, ToBox._31, ToBox._41);
		Record.to_box[1].set(ToBox._12, ToBox._22, ToBox._32, ToBox._42);
		Record.to_box[2].set(ToBox._13 * Depth, ToBox._23 * Depth, ToBox._33 * Depth, ToBox._43 * Depth);
		Record.opacity = HasLayer ? Source.Opacity : 0.0f;
		Record.layer = HasLayer ? Source.Slot : 0;
		Record.mode = 0;
		Record.unused = 0;
	}

	Scene.SetForwardRecords(1, Records, (Decals + 1) * sizeof(Hlsl::SceneDecal));
	// The tiles hold the lights too: marked while there are any, and once more when the last one is gone
	const bool IsTiled = Decals || !Target->rt_LightTiles || TiledLights || (!TileLights.empty() && TileLights[0].flags);
	ForwardDecalRecords = NriPasses.StreamRecords(Records, (Decals + 1) * sizeof(Hlsl::SceneDecal), sizeof(Hlsl::SceneDecal));
	ForwardDecalBuffer->Unmap();
	ForwardDecals = Decals;

	if (IsTiled)
	{
		DispatchLightTiles(Cmd, ForwardDecalRecords);
	}

	return Decals;
}

void CRender::BindForwardDecals(GfxCmdList& Cmd)
{
	Cmd.SetResource(ERHI_SHADER_TYPE::PS, ShaderBindings::Texture("g_forward_decals"), ForwardDecalView, ForwardDecalRecords);
	Cmd.SetResource(ERHI_SHADER_TYPE::PS, ShaderBindings::Texture("s_light_tiles"), Target->rt_LightTiles->pTexture->get_SRView(), Target->rt_LightTiles->pSurface);
	Cmd.SetResource(ERHI_SHADER_TYPE::PS, ShaderBindings::Texture("s_decals"), DecalArrayView, DecalArray);

	// The colour passes read the rain beside the decals
	BindRainTextures(Cmd);
}
