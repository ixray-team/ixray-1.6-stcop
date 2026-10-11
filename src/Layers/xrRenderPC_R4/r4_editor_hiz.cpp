#include "stdafx.h"

#include "r4_editor_hiz.h"

namespace
{
	class CBlender_EditorHiZ final : public IBlender
	{
	public:
		CBlender_EditorHiZ()
		{
			description.CLS = 0;
		}

		const char* getComment() override
		{
			return "INTERNAL: editor occlusion depth grid";
		}

		bool canBeDetailed() override
		{
			return false;
		}

		bool canBeLMAPped() override
		{
			return false;
		}

		void Compile(CBlender_Compile& C) override
		{
			IBlender::Compile(C);

			C.r_ComputePass("editor_hiz");
			C.r_dx10Texture("s_position", r2_RT_P);
			C.r_End();
		}
	};

	// The camera may differ from the read-back frame this much before the data is ignored
	// (fast moves, another viewport)
	constexpr float MaxCameraShift = 2.f;
	constexpr float MinCameraDirectionDot = 0.97f;
}

CEditorHiZ::~CEditorHiZ()
{
	Destroy();
}

void CEditorHiZ::Destroy()
{
	for (SReadback& Readback : Readbacks)
	{
		_RELEASE(Readback.Texture);
		Readback.Pending = false;
	}

	_RELEASE(GridUAV);
	_RELEASE(GridTexture);

	Shader.destroy();
	xr_delete(Blender);

	ViewWidth = 0;
	ViewHeight = 0;
	GridHeight = 0;
	HasData = false;
	Usable = false;
}

bool CEditorHiZ::CreateResources(u32 Width, u32 Height)
{
	if (GridTexture != nullptr && Width == ViewWidth && Height == ViewHeight)
	{
		return true;
	}

	for (SReadback& Readback : Readbacks)
	{
		_RELEASE(Readback.Texture);
		Readback.Pending = false;
	}

	_RELEASE(GridUAV);
	_RELEASE(GridTexture);
	HasData = false;
	Usable = false;

	ViewWidth = Width;
	ViewHeight = Height;
	GridHeight = std::clamp(u32(float(GridWidth) * float(Height) / float(Width) + 0.5f), 1u, GridWidth);

	D3D11_TEXTURE2D_DESC Desc = {};
	Desc.Width = GridWidth;
	Desc.Height = GridHeight;
	Desc.MipLevels = 1;
	Desc.ArraySize = 1;
	Desc.Format = DXGI_FORMAT_R32_FLOAT;
	Desc.SampleDesc.Count = 1;
	Desc.Usage = D3D11_USAGE_DEFAULT;
	Desc.BindFlags = D3D11_BIND_UNORDERED_ACCESS;

	if (FAILED(RDevice->CreateTexture2D(&Desc, nullptr, &GridTexture)))
	{
		return false;
	}

	if (FAILED(RDevice->CreateUnorderedAccessView(GridTexture, nullptr, &GridUAV)))
	{
		_RELEASE(GridTexture);
		return false;
	}

	Desc.Usage = D3D11_USAGE_STAGING;
	Desc.BindFlags = 0;
	Desc.CPUAccessFlags = D3D11_CPU_ACCESS_READ;

	for (SReadback& Readback : Readbacks)
	{
		if (FAILED(RDevice->CreateTexture2D(&Desc, nullptr, &Readback.Texture)))
		{
			return false;
		}
	}

	// CPU pyramid down to one cell
	Levels.clear();
	u32 LevelWidth = GridWidth;
	u32 LevelHeight = GridHeight;
	while (true)
	{
		SLevel& Level = Levels.emplace_back();
		Level.Width = LevelWidth;
		Level.Height = LevelHeight;
		Level.Depth.resize(LevelWidth * LevelHeight);

		if (LevelWidth == 1 && LevelHeight == 1)
		{
			break;
		}

		LevelWidth = (LevelWidth + 1) / 2;
		LevelHeight = (LevelHeight + 1) / 2;
	}

	return true;
}

void CEditorHiZ::Build()
{
	const u32 Width = (u32)RCache.get_width();
	const u32 Height = (u32)RCache.get_height();
	if (Width == 0 || Height == 0 || !CreateResources(Width, Height))
	{
		return;
	}

	if (!Shader)
	{
		Blender = new CBlender_EditorHiZ();
		Shader.create(Blender);
	}

	SReadback& Readback = Readbacks[NextReadback];
	if (Readback.Texture == nullptr)
	{
		return;
	}

	GPU_EVENT(editor_hiz);

	SPass& P = *(Shader->E[0]->passes[0]);
	RCache.set_States(P.state);
	RCache.set_Constants(P.constants);
	RCache.set_Textures(P.T);
	RCache.set_CS(P.cs);
	RCache.set_c("editor_hiz_params", float(Width), float(Height), 0.f, 0.f);

	UINT InitialCount = 0;
	RContext->CSSetUnorderedAccessViews(0, 1, &GridUAV, &InitialCount);
	RCache.Compute((GridWidth + 7) / 8, (GridHeight + 7) / 8, 1);

	ID3D11UnorderedAccessView* NullUAV = nullptr;
	RContext->CSSetUnorderedAccessViews(0, 1, &NullUAV, &InitialCount);

	RContext->CopyResource(Readback.Texture, GridTexture);
	Readback.ViewProject = Device.mFullTransform;
	Readback.CameraPosition = Device.vCameraPosition;
	Readback.CameraDirection = Device.vCameraDirection;
	Readback.Pending = true;

	NextReadback = (NextReadback + 1) % ReadbackCount;
}

void CEditorHiZ::Fetch()
{
	// Oldest first: the newest finished read-back wins
	for (u32 Offset = 0; Offset < ReadbackCount; ++Offset)
	{
		SReadback& Readback = Readbacks[(NextReadback + Offset) % ReadbackCount];
		if (!Readback.Pending)
		{
			continue;
		}

		D3D11_MAPPED_SUBRESOURCE Mapped = {};
		if (RContext->Map(Readback.Texture, 0, D3D11_MAP_READ, D3D11_MAP_FLAG_DO_NOT_WAIT, &Mapped) != S_OK)
		{
			continue;
		}

		SLevel& Level = Levels[0];
		for (u32 Y = 0; Y < Level.Height; ++Y)
		{
			CopyMemory(Level.Depth.data() + Y * Level.Width, (const u8*)Mapped.pData + Y * Mapped.RowPitch, Level.Width * sizeof(float));
		}
		RContext->Unmap(Readback.Texture, 0);

		Readback.Pending = false;
		ViewProject = Readback.ViewProject;
		CameraPosition = Readback.CameraPosition;
		CameraDirection = Readback.CameraDirection;
		HasData = true;

		BuildMips();
	}

	Usable = HasData
		&& Device.vCameraPosition.distance_to_sqr(CameraPosition) < _sqr(MaxCameraShift)
		&& Device.vCameraDirection.dotproduct(CameraDirection) > MinCameraDirectionDot;
}

void CEditorHiZ::BuildMips()
{
	for (size_t Index = 1; Index < Levels.size(); ++Index)
	{
		const SLevel& Src = Levels[Index - 1];
		SLevel& Dst = Levels[Index];

		for (u32 Y = 0; Y < Dst.Height; ++Y)
		{
			const u32 Y0 = Y * 2;
			const u32 Y1 = std::min(Y0 + 1, Src.Height - 1);

			for (u32 X = 0; X < Dst.Width; ++X)
			{
				const u32 X0 = X * 2;
				const u32 X1 = std::min(X0 + 1, Src.Width - 1);

				const float* Row0 = Src.Depth.data() + Y0 * Src.Width;
				const float* Row1 = Src.Depth.data() + Y1 * Src.Width;
				Dst.Depth[Y * Dst.Width + X] = std::max(std::max(Row0[X0], Row0[X1]), std::max(Row1[X0], Row1[X1]));
			}
		}
	}
}

bool CEditorHiZ::IsVisible(const Fbox& Box) const
{
	if (!Usable)
	{
		return true;
	}

	float MinX = flt_max, MinY = flt_max, MaxX = -flt_max, MaxY = -flt_max, MinZ = flt_max;
	const Fmatrix& M = ViewProject;

	for (u32 Corner = 0; Corner < 8; ++Corner)
	{
		const float X = (Corner & 1) ? Box.max.x : Box.min.x;
		const float Y = (Corner & 2) ? Box.max.y : Box.min.y;
		const float Z = (Corner & 4) ? Box.max.z : Box.min.z;

		const float W = X * M._14 + Y * M._24 + Z * M._34 + M._44;

		// Crosses the near plane: cannot be tested in screen space
		if (W < EPS_L)
		{
			return true;
		}

		const float InvW = 1.f / W;
		const float PX = (X * M._11 + Y * M._21 + Z * M._31 + M._41) * InvW;
		const float PY = (X * M._12 + Y * M._22 + Z * M._32 + M._42) * InvW;
		const float PZ = (X * M._13 + Y * M._23 + Z * M._33 + M._43) * InvW;

		MinX = std::min(MinX, PX);
		MaxX = std::max(MaxX, PX);
		MinY = std::min(MinY, PY);
		MaxY = std::max(MaxY, PY);
		MinZ = std::min(MinZ, PZ);
	}

	// Partly outside the read-back view: that part was not tested in that frame and can be on
	// screen now (the camera turned)
	if (MinX < -1.f || MaxX > 1.f || MinY < -1.f || MaxY > 1.f)
	{
		return true;
	}

	// Grid cells under the box (y goes down in the grid)
	const SLevel& Base = Levels[0];
	const float U0 = std::clamp((MinX * 0.5f + 0.5f) * float(Base.Width), 0.f, float(Base.Width - 1));
	const float U1 = std::clamp((MaxX * 0.5f + 0.5f) * float(Base.Width), 0.f, float(Base.Width - 1));
	const float V0 = std::clamp((0.5f - MaxY * 0.5f) * float(Base.Height), 0.f, float(Base.Height - 1));
	const float V1 = std::clamp((0.5f - MinY * 0.5f) * float(Base.Height), 0.f, float(Base.Height - 1));

	// Coarsest level that keeps the box within a few cells
	const float Extent = std::max(U1 - U0, V1 - V0);
	u32 LevelIndex = 0;
	while (LevelIndex + 1 < Levels.size() && Extent > float(2u << LevelIndex))
	{
		++LevelIndex;
	}

	const SLevel& Level = Levels[LevelIndex];
	const u32 X0 = std::min(u32(U0) >> LevelIndex, Level.Width - 1);
	const u32 X1 = std::min(u32(U1) >> LevelIndex, Level.Width - 1);
	const u32 Y0 = std::min(u32(V0) >> LevelIndex, Level.Height - 1);
	const u32 Y1 = std::min(u32(V1) >> LevelIndex, Level.Height - 1);

	for (u32 Y = Y0; Y <= Y1; ++Y)
	{
		const float* Row = Level.Depth.data() + Y * Level.Width;
		for (u32 X = X0; X <= X1; ++X)
		{
			// Something under the box is farther than its nearest point
			if (MinZ <= Row[X])
			{
				return true;
			}
		}
	}

	return false;
}
