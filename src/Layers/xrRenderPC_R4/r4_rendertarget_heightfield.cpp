#include "stdafx.h"
#include "r4_rendertarget.h"
#include "ShaderBindings.h"

namespace HeightfieldRendering
{
constexpr u32 BlockSize = 64; // Matches HEIGHTFIELD_BLOCK_SIZE in the compute shader.

class Blender final : public IBlender
{
public:
	Blender() { description.CLS = 0; }
	const char* getComment() override { return "INTERNAL: heightfield sun prefix scan"; }
	bool canBeDetailed() override { return false; }
	bool canBeLMAPped() override { return false; }
	void Compile(CBlender_Compile& Compiler) override
	{
		IBlender::Compile(Compiler);
		switch (Compiler.iElement)
		{
			case 0:
			{
				Compiler.r_ComputePass("heightfield_shadow");
				Compiler.AddDefine("HEIGHTFIELD_PASS", "0");
				Compiler.r_dx10Texture("s_heightfield_input", "$user$heightfield_height");
				Compiler.r_End();
				break;
			}
			case 1:
			{
				Compiler.r_ComputePass("heightfield_shadow");
				Compiler.AddDefine("HEIGHTFIELD_PASS", "1");
				Compiler.r_dx10Texture("s_heightfield_input", "$user$heightfield_blocks");
				Compiler.r_End();
				break;
			}
			case 2:
			{
				Compiler.r_ComputePass("heightfield_shadow");
				Compiler.AddDefine("HEIGHTFIELD_PASS", "2");
				Compiler.r_dx10Texture("s_heightfield_input", "$user$heightfield_local");
				Compiler.r_dx10Texture("s_heightfield_blocks", "$user$heightfield_prefix");
				Compiler.r_End();
				break;
			}
		}
	}
};

static void Dispatch(GfxCmdList& Cmd, const ref_shader& Shader, u32 Element, u32 GroupsX, u32 GroupsY, IRHIUnorderedAccessView* Output, IRHIUnorderedAccessView* Blocks = nullptr)
{
	SPass& Pass = *Shader->E[Element]->passes[0];
	Cmd.SetStates(Pass.state);
	Cmd.SetConstantTable(Pass.constants);
	Cmd.SetTextures(Pass.T);
	Cmd.SetCS(Pass.cs);
	// Commit readers from the previous pass before binding the next outputs.
	GRHI->ShaderResourceCache->Apply();
	IRHIUnorderedAccessView* Outputs[] = {Output, Blocks};
	Cmd.SetComputeUAVs(0, 2, Outputs);
	Cmd.Dispatch(GroupsX, GroupsY, 1);
	IRHIUnorderedAccessView* Empty[2] = {};
	Cmd.SetComputeUAVs(0, 2, Empty);
	Cmd.UnbindComputeTextures();
}
} // namespace HeightfieldRendering

void CRenderTarget::PhaseHeightfieldShadows(GfxCmdList& Cmd, const Fvector& SunDirection)
{
	CBLight& Light = Cmd.Constants.Write<CBLight>();
	Light.HeightfieldParams.set(0, 0, 0, 0);
	const HeightfieldShadowMap& Map = RImplementation.HeightfieldShadows;
	if (!g_HeightfieldShadows || !Map.IsValid() || !std::isfinite(SunDirection.x) || !std::isfinite(SunDirection.y) || !std::isfinite(SunDirection.z) || SunDirection.y >= 0.0f)
	{
		return;
	}
	const u32 Width = u32(Map.GetGrid().x);
	const u32 Height = u32(Map.GetGrid().y);
	const u32 BlockSize = HeightfieldRendering::BlockSize;
	const u32 Blocks = (std::max(Width, Height) + BlockSize - 1) / BlockSize;
	const float Dx = SunDirection.x * Map.GetWorldToGrid().z;
	const float Dz = SunDirection.z * Map.GetWorldToGrid().w;
	const bool MajorZ = std::abs(Dz) > std::abs(Dx);
	const float Major = MajorZ ? Dz : Dx;
	const float Minor = MajorZ ? Dx : Dz;
	const float Extent = std::abs(Major);
	const float Shear = Extent > 0.0f ? Minor / Extent : 0.0f;
	const u32 MajorSize = MajorZ ? Height : Width;
	const u32 MinorSize = MajorZ ? Width : Height;
	const float Offset = std::max(Shear, 0.0f) * float(MajorSize - 1);
	const u32 Rows = std::min(Width + Height, MinorSize + u32(std::ceil(std::abs(Shear) * float(MajorSize - 1))) + 1);
	// Beyond the entire height span no upstream sample can shadow the next texel.
	// This also handles vertical sun without a near-zero direction division.
	const float MaxDrop = Map.GetDecode().x + 1.0f;
	const float Drop = Extent > 0.0f ? std::min(MaxDrop, -SunDirection.y / Extent) : MaxDrop;
	if (!std::isfinite(Drop * MajorSize))
	{
		return;
	}
	if (!HeightfieldShadowShader)
	{
		CreateEffectSQ<HeightfieldRendering::Blender>(HeightfieldShadowShader);
	}
	Cmd.SetResource(ERHI_SHADER_TYPE::PS, ShaderBindings::Texture("s_heightfield_shadow"), nullptr);
	if (HeightfieldShadowTarget && (HeightfieldShadowTarget->dwWidth != Width || HeightfieldShadowTarget->dwHeight != Height))
	{
		HeightfieldShadowTarget.destroy();
		HeightfieldShadowLocal.destroy();
		HeightfieldShadowBlocks.destroy();
		HeightfieldShadowPrefix.destroy();
	}
	if (!HeightfieldShadowTarget)
	{
		HeightfieldShadowTarget.create("$user$heightfield_shadow", Width, Height, ERHI_FORMAT::R32_FLOAT, 1, CRT::USE_UAV_FLAG);
		HeightfieldShadowLocal.create("$user$heightfield_local", Blocks * BlockSize, Width + Height, ERHI_FORMAT::R32_FLOAT, 1, CRT::USE_UAV_FLAG);
		HeightfieldShadowBlocks.create("$user$heightfield_blocks", Blocks, Width + Height, ERHI_FORMAT::R32_FLOAT, 1, CRT::USE_UAV_FLAG);
		HeightfieldShadowPrefix.create("$user$heightfield_prefix", Blocks, Width + Height, ERHI_FORMAT::R32_FLOAT, 1, CRT::USE_UAV_FLAG);
		if (!HeightfieldShadowTarget->pUAView || !HeightfieldShadowLocal->pUAView || !HeightfieldShadowBlocks->pUAView || !HeightfieldShadowPrefix->pUAView)
		{
			Msg("! Heightfield shadows: cannot create scan UAVs; using existing sun shadows");
		}
	}
	if (!HeightfieldShadowTarget->pUAView || !HeightfieldShadowLocal->pUAView || !HeightfieldShadowBlocks->pUAView || !HeightfieldShadowPrefix->pUAView)
	{
		return;
	}
	Light.HeightfieldWorldToGrid = Map.GetWorldToGrid();
	Light.HeightfieldDecode = Map.GetDecode();
	Light.HeightfieldGrid.set(float(Width), float(Height), float(MajorSize), float(Rows));
	Light.HeightfieldParams.set(1.0f, Drop, g_HeightfieldBias + Map.GetDecode().z, float(g_HeightfieldFiltering));
	Light.HeightfieldScan.set(MajorZ ? 1.0f : 0.0f, Major >= 0.0f ? 1.0f : -1.0f, Shear, Offset);

	// Rebuild every frame, including unchanged sunlight, so this event measures
	// all three passes. Neither the scan nor its buffers depend on screen size.
	GPU_EVENT(heightfield_shadows);
	u_setrt(Cmd, Width, Height, nullptr, nullptr, nullptr, nullptr);
	Cmd.ApplyRenderTargets();
	{
		GPU_EVENT(heightfield_sun_local_scan);
		HeightfieldRendering::Dispatch(Cmd, HeightfieldShadowShader, 0, (MajorSize + BlockSize - 1) / BlockSize, Rows, HeightfieldShadowLocal->pUAView, HeightfieldShadowBlocks->pUAView);
	}
	{
		GPU_EVENT(heightfield_sun_block_scan);
		HeightfieldRendering::Dispatch(Cmd, HeightfieldShadowShader, 1, 1, Rows, HeightfieldShadowPrefix->pUAView);
	}
	{
		GPU_EVENT(heightfield_sun_resolve);
		HeightfieldRendering::Dispatch(Cmd, HeightfieldShadowShader, 2, (Width + 7u) / 8u, (Height + 7u) / 8u, HeightfieldShadowTarget->pUAView);
	}
}
