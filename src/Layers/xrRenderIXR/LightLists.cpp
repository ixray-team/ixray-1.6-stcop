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
#include "blenders/Blender_Recorder.h"
#include "blenders/Blender.h"

namespace
{
	class CBlenderLightTiles : public IBlender
	{
	public:
		const char* getComment() override { return "INTERNAL: light tiles"; }
		bool canBeDetailed() override { return false; }
		bool canBeLMAPped() override { return false; }

		void Compile(CBlender_Compile& C) override
		{
			IBlender::Compile(C);
			C.r_ComputePass("light_tiles");
			C.r_End();
		}
	};

	IRHIShaderResourceView* MakeRecords(IRHIBuffer*& Buffer, u32 Stride, u32 Count)
	{
		RHIBufferDesc Desc{};
		Desc.Usage = ERHI_USAGE::USAGE_DYNAMIC;
		Desc.Type = ERHI_BUFFER_TYPE::STRUCTURED;
		Desc.CPUAccessFlags = ERHI_CPU_ACCESS_FLAG::ERHI_CPU_ACCESS_FLAG_WRITE;
		Desc.StructureByteStride = Stride;
		Desc.Size = Stride * Count;
		Buffer = GRHI->CreateBuffer(Desc, nullptr);

		RHIShaderResourceViewDesc View{};
		View.Format = ERHI_FORMAT::UNKNOWN;
		View.ElementWidth = Count;
		return GRHI->CreateShaderResourceView(Buffer, &View);
	}
}

void CRender::CreateForwardLists()
{
	// The targets are made anew on a reset, the buffers are not
	if (!Target->s_light_tiles)
	{
		CBlenderLightTiles Blender;
		Target->s_light_tiles.create(&Blender, "r2\\light_tiles");
	}

	if (ForwardLightBuffer)
	{
		return;
	}

	// The tiles' pass reads the lights beside the decals: the record of none, the frame's lights are SurfaceLights
	ForwardLightView = MakeRecords(ForwardLightBuffer, sizeof(Hlsl::SceneLight), SCENE_LIGHTS + 1);
	ForwardDecalView = MakeRecords(ForwardDecalBuffer, sizeof(Hlsl::SceneDecal), SCENE_DECALS + 1);

	RHIMappedSubresource Mapped;
	R_ASSERT(ForwardLightBuffer->Map(ERHI_BUFFER_MAP::WRITE_DISCARD, 0, &Mapped));
	*static_cast<Hlsl::SceneLight*>(Mapped.pData) = {};
	ForwardLightBuffer->Unmap();
}

// The tiles of both lists from the records as they are uploaded now. The decals are known before the lights: the
// pass runs for them first, with the lights of the frame before, and again once this frame's lights are in. The old
// backend's records are streams of its upload ring on D3D12: NRI reads its own copies
void CRender::DispatchLightTiles(GfxCmdList& Cmd, nri::Descriptor* DecalRecords)
{
	const u32 TilesX = (Target->rt_Generic_0->dwWidth + SCENE_LIGHT_TILE - 1) / SCENE_LIGHT_TILE;
	const u32 TilesY = (Target->rt_Generic_0->dwHeight + SCENE_LIGHT_TILE - 1) / SCENE_LIGHT_TILE;
	const u32 Words = SCENE_TILE_WORDS;

	if (!Target->rt_LightTiles || Target->rt_LightTiles->dwWidth != TilesX * Words || Target->rt_LightTiles->dwHeight != TilesY)
	{
		Target->rt_LightTiles = nullptr;
		Target->rt_LightTiles.create("$user$light_tiles", TilesX * Words, TilesY, ERHI_FORMAT::R32_UINT, 1, CRT::USE_UAV_FLAG);
	}

	// The pass reads the screen's size and the main view: whatever drew last may have left a shadow map's
	Target->u_setrt(Cmd, Target->rt_Generic_0->ViewWidth(), Target->rt_Generic_0->ViewHeight(), nullptr, nullptr, nullptr, nullptr);
	Cmd.SetView(RFrame.mView, RFrame.mProject);

	SPass& Pass = *(Target->s_light_tiles->E[0]->passes[0]);
	Cmd.SetStates(Pass.state);
	Cmd.SetConstantTable(Pass.constants);
	Cmd.SetTextures(Pass.T);

	const Hlsl::SceneLight None = {};
	const bool HasLights = !TileLights.empty();
	nri::Descriptor* LightRecords = NriPasses.StreamRecords(HasLights ? TileLights.data() : &None, u32((HasLights ? TileLights.size() : 1) * sizeof(None)), sizeof(None));
	TiledLights = HasLights ? TileLights[0].flags : 0;
	const NriBinding Records[2] = { { ShaderBindings::Texture("g_forward_lights"), nullptr, 0, LightRecords }, { ShaderBindings::Texture("g_forward_decals"), nullptr, 0, DecalRecords } };
	const NriBinding Written = { 0, Target->rt_LightTiles->pSurface };

	NriPasses.Dispatch(Cmd, Pass.cs._get(), Records, 2, &Written, 1, (TilesX + 7) / 8, (TilesY + 7) / 8, 1);
}
