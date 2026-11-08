#include "stdafx.h"
#include "dxUIRender.h"

namespace
{
void ClearRoundedTextures()
{
	static STextureList EmptyTextures;
	RCache.set_Textures(&EmptyTextures);
	GRHI->ShaderResourceCache->Apply();
}
} // namespace

dxUIRender::RoundedLayer::~RoundedLayer()
{
	ReleaseResources();
}

void dxUIRender::RoundedLayer::ReleaseResources()
{
	Shader = nullptr;
	if (Background)
	{
		Background->surface_set(nullptr);
	}
	if (Content)
	{
		Content->surface_set(nullptr);
	}
	Background = nullptr;
	Content = nullptr;
	_RELEASE(ContentView);
	_RELEASE(BackgroundSurface);
	_RELEASE(ContentSurface);
	_RELEASE(DepthView);
	_RELEASE(DepthSurface);
	Width = Height = Samples = 0;
}

bool dxUIRender::BeginRoundedRect(const UIRoundedRect& Rect)
{
	IRHIRenderTargetView* Target = GRHI->GetRenderTargetView(0);
	if (!Target || !Target->GetSurface())
	{
		return false;
	}
	IRHISurface* Surface = Target->GetSurface();
	// Reuse resources by nesting depth, independently of widget and Lua lifetimes.
	if (RoundedDepth == RoundedLayers.size())
	{
		RoundedLayers.emplace_back(xr_make_unique<RoundedLayer>());
	}
	RoundedLayer& Layer = *RoundedLayers[RoundedDepth];
	IRHIDepthStencilView* Depth = GRHI->GetDepthStencilView();
	const ERHI_FORMAT DepthFormat = Depth ? Depth->GetSurface()->GetFormat() : ERHI_FORMAT::UNKNOWN;
	const u32 DepthWidth = Depth ? Depth->GetSurface()->GetWidth() : 0;
	const u32 DepthHeight = Depth ? Depth->GetSurface()->GetHeight() : 0;
	const u32 Width = Surface->GetWidth(), Height = Surface->GetHeight(), Samples = Surface->GetSampleDescCount();
	if (Layer.Width != Width || Layer.Height != Height || Layer.Samples != Samples ||
		Layer.Format != Surface->GetFormat() || Layer.DepthFormat != DepthFormat ||
		Layer.DepthWidth != DepthWidth || Layer.DepthHeight != DepthHeight)
	{
		Layer.ReleaseResources();
		Layer.Format = Surface->GetFormat();
		Layer.DepthFormat = DepthFormat;
		RHITextureDesc Desc(Width, Height, Layer.Format);
		Desc.SampleDescCount = Samples;
		Desc.BindFlags = ERHI_BIND_FLAG::RENDER_TARGET | ERHI_BIND_FLAG::SHADER_RESOURCE;
		Layer.BackgroundSurface = GRHI->CreateRenderTarget(Desc);
		Layer.ContentSurface = GRHI->CreateRenderTarget(Desc);
		if (!Layer.BackgroundSurface || !Layer.ContentSurface)
		{
			Layer.ReleaseResources();
			return false;
		}
		RHIRenderTargetViewDesc ViewDesc;
		ViewDesc.Format = Layer.Format;
		ViewDesc.ViewDimension = Samples > 1 ? ERHI_RTV_DIMENSION::TEXTURE2DMS : ERHI_RTV_DIMENSION::TEXTURE2D;
		Layer.ContentView = GRHI->CreateRenderTargetView(Layer.ContentSurface, ViewDesc);
		if (!Layer.ContentView)
		{
			Layer.ReleaseResources();
			return false;
		}
		if (Depth)
		{
			Desc.Width = DepthWidth;
			Desc.Height = DepthHeight;
			Desc.Format = DepthFormat;
			Desc.BindFlags = ERHI_BIND_FLAG::DEPTH_STENCIL;
			Layer.DepthSurface = GRHI->CreateRenderTarget(Desc);
			RHIDepthStencilViewDesc DepthDesc;
			DepthDesc.ViewDimension = Depth->GetDimension();
			DepthDesc.Format = DepthFormat;
			if (DepthFormat == ERHI_FORMAT::R24G8_TYPELESS)
			{
				DepthDesc.Format = ERHI_FORMAT::D24_UNORM_S8_UINT;
			}
			if (DepthFormat == ERHI_FORMAT::R32_TYPELESS)
			{
				DepthDesc.Format = ERHI_FORMAT::D32_FLOAT;
			}
			if (DepthFormat == ERHI_FORMAT::R32G8X24_TYPELESS)
			{
				DepthDesc.Format = ERHI_FORMAT::D32_FLOAT_S8X24_UINT;
			}
			if (DepthFormat == ERHI_FORMAT::R16_TYPELESS)
			{
				DepthDesc.Format = ERHI_FORMAT::D16_UNORM;
			}
			if (Layer.DepthSurface)
			{
				Layer.DepthView = GRHI->CreateDepthStencilView(Layer.DepthSurface, DepthDesc);
			}
			if (!Layer.DepthView)
			{
				Layer.ReleaseResources();
				return false;
			}
		}
		string64 BackgroundName, ContentName;
		xr_sprintf(BackgroundName, "$user$ui_round_background_%zu", RoundedDepth);
		xr_sprintf(ContentName, "$user$ui_round_content_%zu", RoundedDepth);
		Layer.Background.create(BackgroundName);
		Layer.Background->surface_set(Layer.BackgroundSurface);
		Layer.Content.create(ContentName);
		Layer.Content->surface_set(Layer.ContentSurface);
		xr_string Textures = xr_string(ContentName) + "," + BackgroundName;
		Layer.Shader.create(Samples > 1 ? "hud\\ui_rounded_ms" : "hud\\ui_rounded", Textures.c_str());
		Layer.Width = Width;
		Layer.Height = Height;
		Layer.Samples = Samples;
		Layer.DepthWidth = DepthWidth;
		Layer.DepthHeight = DepthHeight;
	}
	Layer.Rect = Rect;
	Layer.DepthZ.set(0.0f, 0.0f, 0.0f, 0.0f);
	Layer.DepthW.set(0.0f, 0.0f, 1.0f, 0.0f);
	Layer.Viewport = GRHI->GetViewport();
	Layer.Scissor = GetScissor();
	if (!ScissorEnabled)
	{
		Layer.Scissor.set(0, 0, Width, Height);
	}
	Layer.ScissorEnabled = ScissorEnabled;
	Layer.World = RCache.get_xform_world();
	Layer.View = RCache.get_xform_view();
	Layer.Projection = RCache.get_xform_project();
	Layer.CullMode = GRHI->StateManager->GetCullMode();
	// Inverse homography maps raster coordinates into the rounded rectangle,
	// including perspective projection of a 3D UI plane.
	double H[3][3] = {};
	const UITransform2D& T = Rect.Transform;
	if (Rect.ScreenSpace)
	{
		H[0][0] = T.XX;
		H[0][1] = T.XY;
		H[0][2] = T.TX;
		H[1][0] = T.YX;
		H[1][1] = T.YY;
		H[1][2] = T.TY;
		H[2][2] = 1.0;
	}
	else
	{
		const Fmatrix& M = RCache.xforms.m_wvp;
		Layer.DepthZ.set(M._13 * T.XX + M._23 * T.YX, M._13 * T.XY + M._23 * T.YY, M._13 * T.TX + M._23 * T.TY + M._43, 0.0f);
		Layer.DepthW.set(M._14 * T.XX + M._24 * T.YX, M._14 * T.XY + M._24 * T.YY, M._14 * T.TX + M._24 * T.TY + M._44, 0.0f);
		const double X[3] = {M._11 * T.XX + M._21 * T.YX, M._11 * T.XY + M._21 * T.YY, M._11 * T.TX + M._21 * T.TY + M._41};
		const double Y[3] = {M._12 * T.XX + M._22 * T.YX, M._12 * T.XY + M._22 * T.YY, M._12 * T.TX + M._22 * T.TY + M._42};
		const double W[3] = {M._14 * T.XX + M._24 * T.YX, M._14 * T.XY + M._24 * T.YY, M._14 * T.TX + M._24 * T.TY + M._44};
		const RHIViewport& V = Layer.Viewport;
		for (int I = 0; I < 3; ++I)
		{
			H[0][I] = X[I] * V.Width * 0.5 + W[I] * (V.TopLeftX + V.Width * 0.5);
			H[1][I] = -Y[I] * V.Height * 0.5 + W[I] * (V.TopLeftY + V.Height * 0.5);
			H[2][I] = W[I];
		}
	}
	// Restrict composition to the projected control bounds, including its AA fringe.
	Layer.Bounds.set(0, 0, Width, Height);
	double MinX = double(Width), MinY = double(Height), MaxX = 0.0, MaxY = 0.0;
	bool FiniteBounds = true;
	for (int Corner = 0; Corner < 4; ++Corner)
	{
		const double X = (Corner & 1) ? Rect.Width : 0.0;
		const double Y = (Corner & 2) ? Rect.Height : 0.0;
		const double W = H[2][0] * X + H[2][1] * Y + H[2][2];
		if (W <= 1e-8)
		{
			FiniteBounds = false;
			break;
		}
		const double PX = (H[0][0] * X + H[0][1] * Y + H[0][2]) / W;
		const double PY = (H[1][0] * X + H[1][1] * Y + H[1][2]) / W;
		if (!std::isfinite(PX) || !std::isfinite(PY))
		{
			FiniteBounds = false;
			break;
		}
		MinX = std::min(MinX, PX);
		MinY = std::min(MinY, PY);
		MaxX = std::max(MaxX, PX);
		MaxY = std::max(MaxY, PY);
	}
	if (FiniteBounds)
	{
		Layer.Bounds.set(
			int(std::clamp(std::floor(MinX) - 2.0, 0.0, double(Width))),
			int(std::clamp(std::floor(MinY) - 2.0, 0.0, double(Height))),
			int(std::clamp(std::ceil(MaxX) + 2.0, 0.0, double(Width))),
			int(std::clamp(std::ceil(MaxY) + 2.0, 0.0, double(Height)))
		);
	}
	double Inv[3][3];
	for (int Row = 0; Row < 3; ++Row)
	{
		for (int Col = 0; Col < 3; ++Col)
		{
			const int A = (Col + 1) % 3, B = (Col + 2) % 3, C = (Row + 1) % 3, D = (Row + 2) % 3;
			Inv[Row][Col] = H[A][C] * H[B][D] - H[A][D] * H[B][C];
		}
	}
	const double Det = H[0][0] * Inv[0][0] + H[0][1] * Inv[1][0] + H[0][2] * Inv[2][0];
	if (std::isfinite(Det) && std::abs(Det) > 1e-12)
	{
		for (int I = 0; I < 3; ++I)
		{
			Layer.Inverse[I].set(float(Inv[I][0] / Det), float(Inv[I][1] / Det), float(Inv[I][2] / Det), 0.0f);
		}
	}
	else
	{
		Layer.Rect.Width = Layer.Rect.Height = 0.0f;
		for (Fvector4& Row : Layer.Inverse)
		{
			Row.set(0.0f, 0.0f, 0.0f, 0.0f);
		}
	}
	for (u32 I = 0; I < RHI_MAX_RENDER_TARGETS; ++I)
	{
		Layer.SavedTargets[I] = GRHI->GetRenderTargetView(I);
		if (Layer.SavedTargets[I])
		{
			Layer.SavedTargets[I]->AddRef();
		}
	}
	Layer.SavedDepth = Depth;
	if (Depth)
	{
		Depth->AddRef();
	}
	ClearRoundedTextures();
	GRHI->CopySurface(Layer.BackgroundSurface, Surface);
	GRHI->CopySurface(Layer.ContentSurface, Surface);
	if (Depth)
	{
		GRHI->CopySurface(Layer.DepthSurface, Depth->GetSurface());
	}
	for (u32 I = 0; I < RHI_MAX_RENDER_TARGETS; ++I)
	{
		GRHI->SetRenderTargetView(I == 0 ? Layer.ContentView : nullptr, I);
	}
	GRHI->SetDepthStencilView(Layer.DepthView);
	GRHI->ApplyRenderTargetChange();
	++RoundedDepth;
	return true;
}

void dxUIRender::EndRoundedRect()
{
	VERIFY(RoundedDepth != 0);
	RoundedLayer& Layer = *RoundedLayers[--RoundedDepth];
	ClearRoundedTextures();
	GRHI->SetRenderTargetView(Layer.SavedTargets[0], 0);
	for (u32 I = 1; I < RHI_MAX_RENDER_TARGETS; ++I)
	{
		GRHI->SetRenderTargetView(nullptr, I);
	}
	GRHI->SetDepthStencilView(Layer.Rect.ScreenSpace ? nullptr : Layer.SavedDepth);
	GRHI->ApplyRenderTargetChange();
	RHIViewport FullViewport(0.0f, 0.0f, float(Layer.Width), float(Layer.Height));
	GRHI->SetViewport(FullViewport);
	Irect CompositeScissor = Layer.Scissor;
	CompositeScissor.x1 = std::max(CompositeScissor.x1, Layer.Bounds.x1);
	CompositeScissor.y1 = std::max(CompositeScissor.y1, Layer.Bounds.y1);
	CompositeScissor.x2 = std::max(CompositeScissor.x1, std::min(CompositeScissor.x2, Layer.Bounds.x2));
	CompositeScissor.y2 = std::max(CompositeScissor.y1, std::min(CompositeScissor.y2, Layer.Bounds.y2));
	CompositeScissor.x1 = std::max(CompositeScissor.x1, iFloor(Layer.Viewport.TopLeftX));
	CompositeScissor.y1 = std::max(CompositeScissor.y1, iFloor(Layer.Viewport.TopLeftY));
	CompositeScissor.x2 = std::max(CompositeScissor.x1, std::min(CompositeScissor.x2, iCeil(Layer.Viewport.TopLeftX + Layer.Viewport.Width)));
	CompositeScissor.y2 = std::max(CompositeScissor.y1, std::min(CompositeScissor.y2, iCeil(Layer.Viewport.TopLeftY + Layer.Viewport.Height)));
	SetScissor(&CompositeScissor);
	RCache.set_Shader(Layer.Shader);
	RCache.set_Stencil(FALSE);
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_ColorWriteEnable();
	RCache.set_c("ui_round_target", float(Layer.Width), float(Layer.Height), 1.0f / Layer.Width, 1.0f / Layer.Height);
	RCache.set_c("ui_round_shape", Layer.Rect.Width, Layer.Rect.Height, Layer.Rect.Radius, Layer.Rect.Thickness);
	RCache.set_c("ui_round_color", color_get_R(Layer.Rect.Color) / 255.0f, color_get_G(Layer.Rect.Color) / 255.0f, color_get_B(Layer.Rect.Color) / 255.0f, color_get_A(Layer.Rect.Color) / 255.0f);
	RCache.set_c("ui_round_inverse0", Layer.Inverse[0]);
	RCache.set_c("ui_round_inverse1", Layer.Inverse[1]);
	RCache.set_c("ui_round_inverse2", Layer.Inverse[2]);
	RCache.set_c("ui_round_depth_z", Layer.DepthZ);
	RCache.set_c("ui_round_depth_w", Layer.DepthW);
	RCache.set_c("ui_round_depth_range", Layer.Viewport.MinDepth, Layer.Viewport.MaxDepth - Layer.Viewport.MinDepth, 0.0f, 0.0f);
	// The compositor uses target coordinates directly, without a second tree transform.
	u32 Offset;
	FVF::TL* Vertices = static_cast<FVF::TL*>(RCache.Vertex.Lock(4, hGeom_TL.stride(), Offset));
	Vertices[0].set(-0.5f, -0.5f, 0xffffffff, 0.0f, 0.0f);
	Vertices[1].set(-0.5f, Layer.Height - 0.5f, 0xffffffff, 0.0f, 1.0f);
	Vertices[2].set(Layer.Width - 0.5f, -0.5f, 0xffffffff, 1.0f, 0.0f);
	Vertices[3].set(Layer.Width - 0.5f, Layer.Height - 0.5f, 0xffffffff, 1.0f, 1.0f);
	RCache.Vertex.Unlock(4, hGeom_TL.stride());
	RCache.set_Geometry(hGeom_TL);
	RCache.set_Z(!Layer.Rect.ScreenSpace);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_STRIP, Offset, 2);
	ClearRoundedTextures();
	for (u32 I = 0; I < RHI_MAX_RENDER_TARGETS; ++I)
	{
		GRHI->SetRenderTargetView(Layer.SavedTargets[I], I);
	}
	GRHI->SetDepthStencilView(Layer.SavedDepth);
	GRHI->ApplyRenderTargetChange();
	for (auto*& Target : Layer.SavedTargets)
	{
		_RELEASE(Target);
	}
	_RELEASE(Layer.SavedDepth);
	GRHI->SetViewport(Layer.Viewport);
	SetScissor(Layer.ScissorEnabled ? &Layer.Scissor : nullptr);
	RCache.set_xform_world(Layer.World);
	RCache.set_xform_view(Layer.View);
	RCache.set_xform_project(Layer.Projection);
	GRHI->StateManager->SetCullMode(Layer.CullMode);
}
