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

namespace
{
	// surface_light of shaders/common/surface_forward.hlsli; the first record holds the count and the eye
	struct SurfaceLightRecord
	{
		Fvector4 Position;
		Fvector4 Colour;
		Fvector4 Direction;
	};

	constexpr u32 SurfaceLightLimit = 63;
	// A shadow entry is two records after the lights: the matrix from the world into the atlas, then .x - whether
	// the entry has a map, .y - its page, .z and .w - what a spot projects: the scale from the atlas to its texture
	// and the texture's slot plus one, with the middle of its place in the atlas in the next two numbers. A spot
	// has an entry for its texture alone as well. GpuScene's copy of the buffer is as large
	constexpr u32 SurfaceShadowLimit = 64;
	constexpr u32 SurfaceProjectorLimit = 8;
	constexpr u32 SurfaceRecordLimit = SurfaceLightLimit + 1 + SurfaceShadowLimit * 2;

	void WriteShadow(SurfaceLightRecord* Entry, const light& Part, u32 Projector = 0)
	{
		const bool IsMapped = Part.ShadowFrame == RFrame.dwFrame;
		const float Size = float(RImplementation.o.smapsize);
		// Without a map the matrix leads straight to the texture the light projects
		const float Extent = IsMapped ? float(Part.X.S.size - 2) / Size * 0.5f : 0.5f;
		const float Left = IsMapped ? float(Part.X.S.posX + 1) / Size + 0.5f / Size : 0.0f;
		const float Top = IsMapped ? float(Part.X.S.posY + 1) / Size + 0.5f / Size : 0.0f;
		Fmatrix Adjust = {
			Extent, 0.0f, 0.0f, 0.0f,
			0.0f, -Extent, 0.0f, 0.0f,
			0.0f, 0.0f, ps_r2_ls_depth_scale, 0.0f,
			Extent + Left, Extent + Top, ps_r2_ls_depth_bias, 1.0f
		};

		Fmatrix Project;
		Project.mul(Adjust, Part.X.S.project);
		Fmatrix ToAtlas;
		ToAtlas.set(Part.X.S.view);
		ToAtlas.mulA_44(Project);

		static_assert(sizeof(ToAtlas) == sizeof(SurfaceLightRecord) + sizeof(Fvector4));
		std::memcpy(Entry, &ToAtlas, sizeof(ToAtlas));
		Entry[1].Colour.set(IsMapped ? 1.0f : 0.0f, float(Part.vis.smap_ID), 0.5f / Extent, float(Projector));
		Entry[1].Direction.set(Extent + Left, Extent + Top, 0.0f, 0.0f);
	}
}

void CRender::UploadSurfaceLights(GfxCmdList& Cmd)
{
	if (!SurfaceLightBuffer)
	{
		RHIBufferDesc Desc{};
		Desc.Usage = ERHI_USAGE::USAGE_DYNAMIC;
		Desc.Type = ERHI_BUFFER_TYPE::STRUCTURED;
		Desc.CPUAccessFlags = ERHI_CPU_ACCESS_FLAG::ERHI_CPU_ACCESS_FLAG_WRITE;
		Desc.StructureByteStride = sizeof(SurfaceLightRecord);
		Desc.Size = SurfaceRecordLimit * sizeof(SurfaceLightRecord);
		SurfaceLightBuffer = GRHI->CreateBuffer(Desc, nullptr);

		RHIShaderResourceViewDesc View{};
		View.Format = ERHI_FORMAT::UNKNOWN;
		View.ElementWidth = SurfaceRecordLimit;
		SurfaceLightView = GRHI->CreateShaderResourceView(SurfaceLightBuffer, &View);
	}

	RHIMappedSubresource Mapped;
	R_ASSERT(SurfaceLightBuffer->Map(ERHI_BUFFER_MAP::WRITE_DISCARD, 0, &Mapped));
	SurfaceLightRecord* Records = static_cast<SurfaceLightRecord*>(Mapped.pData);
	u32 Count = 0;
	u32 Shadows = 0;
	SurfaceProjectors.clear();
	TileLights.assign(1, {});

	for (light* Light : SurfaceLights)
	{
		if (Count == SurfaceLightLimit)
		{
			break;
		}

		if (!Light->flags.bActive || Light->range <= EPS_L)
		{
			continue;
		}

		const bool IsSpot = Light->flags.type == IRender_Light::SPOT;
		SurfaceLightRecord& Record = Records[++Count];
		Record.Position.set(Light->position.x, Light->position.y, Light->position.z, 1.0f / (Light->range * Light->range));
		Fvector Seen;
		RFrame.mView.transform_tiny(Seen, Light->position);
		TileLights.emplace_back().position.set(Seen.x, Seen.y, Seen.z, Record.Position.w);
		// A shadowed light fades out with distance, as in the frame that shades a pixel once
		const float Fade = Light->get_LOD();
		Record.Colour.set(Light->color.r * Fade, Light->color.g * Fade, Light->color.b * Fade, 0.0f);
		Record.Direction.set(Light->direction.x, Light->direction.y, Light->direction.z, IsSpot ? std::cos(Light->cone * 0.5f) : -2.0f);

		const light* Parts[6] = { Light };
		u32 PartCount = 1;

		if (!IsSpot)
		{
			PartCount = Light->omnipart[0] ? 6 : 0;
			std::copy_n(Light->omnipart, PartCount, Parts);

			// The game may be making or dropping the parts this very moment
			if (std::find(Parts, Parts + PartCount, nullptr) != Parts + PartCount)
			{
				PartCount = 0;
			}
		}

		const bool IsShadowed = std::any_of(Parts, Parts + PartCount, [](const light* Part) { return Part->ShadowFrame == RFrame.dwFrame; }) && PartCount != 0;
		const bool IsProjecting = IsSpot && !Light->flags.bHudMode;

		if ((!IsShadowed && !IsProjecting) || Shadows + PartCount > SurfaceShadowLimit)
		{
			continue;
		}

		u32 Projector = 0;

		if (IsProjecting)
		{
			// The matrix of a spot that drew no map this frame
			Light->xform_calc();

			if (!SurfaceProjectorDefault)
			{
				SurfaceProjectorDefault.create("lights\\lights_spot01");
			}

			CTexture* Texture = Light->Projector ? Light->Projector._get() : SurfaceProjectorDefault._get();
			Texture->EnsureLoaded();
			const size_t Slot = std::find(SurfaceProjectors.begin(), SurfaceProjectors.end(), Texture) - SurfaceProjectors.begin();

			if (Texture->pSurface && Slot < SurfaceProjectorLimit)
			{
				if (Slot == SurfaceProjectors.size())
				{
					SurfaceProjectors.push_back(Texture);
				}

				Projector = u32(Slot) + 1;
			}
		}

		SurfaceLightRecord* Entries = Records + SurfaceLightLimit + 1 + Shadows * 2;
		Record.Colour.w = float(Entries - Records);

		for (u32 Part = 0; Part < PartCount; ++Part)
		{
			WriteShadow(Entries + Part * 2, *Parts[Part], Projector);
		}

		Shadows += PartCount;
	}

	Records[0].Position.set(float(Count), RFrame.vCameraPosition.x, RFrame.vCameraPosition.y, RFrame.vCameraPosition.z);
	TileLights[0].flags = Count;
	const u32 Bytes = (Shadows ? SurfaceLightLimit + 1 + Shadows * 2 : Count + 1) * sizeof(SurfaceLightRecord);
	Scene.SetForwardRecords(0, Records, Bytes);
	nri::Descriptor* NriRecords = NriPasses.StreamRecords(Records, Bytes, sizeof(SurfaceLightRecord));
	SurfaceLightBuffer->Unmap();
	Cmd.SetResource(ERHI_SHADER_TYPE::PS, ShaderBindings::Texture("g_surface_lights"), SurfaceLightView, NriRecords);

	for (u32 Slot = 0; Slot < SurfaceProjectors.size(); ++Slot)
	{
		string64 Name;
		xr_sprintf(Name, "s_forward_projector%u", Slot);
		Cmd.SetTexture(ERHI_SHADER_TYPE::PS, ShaderBindings::Texture(Name), SurfaceProjectors[Slot]);
	}
}
