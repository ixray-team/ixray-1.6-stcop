#include "stdafx.h"
#include <DirectXMath.h>
using namespace DirectX;

#include "../../xrCore/Collision/Frustum.h"

void GfxCmdList::OnFrameEnd	()
{
	if (!g_dedicated_server)
	{
		Invalidate			();
	}
//#endif
}

void GfxCmdList::OnFrameBegin	()
{
	if (!g_dedicated_server)
	{
		if (Heap)
		{
			Heap->Flush();
		}
		Invalidate();
		RImplementation.rmNormal();
		if (!EditorMode)
		{
			SetRenderTarget(RImplementation.Target->rt_BackbufferLUT->pRT);
		}
		Constants.UpdateFrame();
		SetView(ViewState);
		Stats = {};
		RImplementation.Skinning.BeginFrame();
		SetStencil(false);
	}
}

void GfxCmdList::UnbindComputeTextures()
{
	for (u32 Slot = 0; Slot < mtMaxComputeShaderTextures; ++Slot)
	{
		GRHI->ShaderResourceCache->SetCSResource(Slot, nullptr);
	}
	GRHI->ShaderResourceCache->Apply();
	std::memset(Textures[5], 0, sizeof(Textures[5]));
	TextureMask[5] = 0;
	PendingTextureMask[5] = 0;
	T = nullptr;
}

void GfxCmdList::Invalidate	()
{
	if (GRHI != nullptr)
	{
		GRHI->SetRenderTargetView(nullptr, 0);
		GRHI->SetRenderTargetView(nullptr, 1);
		GRHI->SetRenderTargetView(nullptr, 2);
		GRHI->SetRenderTargetView(nullptr, 3);
		GRHI->SetRenderTargetView(nullptr, 4);
		GRHI->SetRenderTargetView(nullptr, 5);
		GRHI->SetDepthStencilView(nullptr, true);
	}

	ib							= nullptr;
	GeomSource					= EGeomSource::Static;
	GeomPage					= 0;

	state						= nullptr;
	std::fill(std::begin(States), std::end(States), nullptr);
	ctable						= nullptr;

	T							= nullptr;
	M							= nullptr;

	m_pInputLayout				= nullptr;
	m_PrimitiveTopology			= ERHI_PRIMITIVE_TOPOLOGY::UNDEFINED;
	m_bChangedRTorZB			= false;
	StatesDirty = true;
	if (GRHI != nullptr)
	{
		GRHI->StateManager->Reset();
	}

	if (GRHI && GRHI->ShaderResourceCache)
	{
		GRHI->ShaderResourceCache->ResetDeviceState();
		if (Heap)
		{
			Heap->Bind(*this);
		}
		RImplementation.Skinning.Bind(*this);
	}

	std::memset(Textures, 0, sizeof(Textures));
	std::memset(HandViews, 0, sizeof(HandViews));
	// The cache below forgets them too
	std::memset(RawViews, 0, sizeof(RawViews));
	std::memset(TextureMask, 0, sizeof(TextureMask));
	std::memset(PendingTextureMask, 0, sizeof(PendingTextureMask));
	HasPendingTextures = false;
	for (u32 m_it =0; m_it< 8;)		matrices	[m_it++]	= nullptr;
}

namespace
{
	using StageResourceFn = void (IRHIShaderResourceStateCache::*)(u32, IRHIShaderResourceView*);
	constexpr StageResourceFn StageResource[] = { &IRHIShaderResourceStateCache::SetPSResource, &IRHIShaderResourceStateCache::SetVSResource, &IRHIShaderResourceStateCache::SetGSResource,
		&IRHIShaderResourceStateCache::SetHSResource, &IRHIShaderResourceStateCache::SetDSResource, &IRHIShaderResourceStateCache::SetCSResource };
}

void GfxCmdList::SetConstantTable(R_constant_table* C_)
{
	ctable = C_;
	if (C_ && C_->DetailScale)
	{
		const float Scale = *C_->DetailScale;
		Constants.Write<CBMaterial>().dt_params.set(Scale, Scale, Scale, 1 / r__dtex_range);
	}
}

void GfxCmdList::SetResource(ERHI_SHADER_TYPE Stage, u32 Slot, IRHIShaderResourceView* View)
{
	BindByHand(Stage, Slot, View, {});
}

void GfxCmdList::SetResource(ERHI_SHADER_TYPE Stage, u32 Slot, IRHIShaderResourceView* View, IRHISurface* Surface)
{
	BindByHand(Stage, Slot, View, { Surface, nullptr });
}

void GfxCmdList::SetResource(ERHI_SHADER_TYPE Stage, u32 Slot, IRHIShaderResourceView* View, nri::Descriptor* NriView)
{
	BindByHand(Stage, Slot, View, { nullptr, NriView });
}

void GfxCmdList::SetTexture(ERHI_SHADER_TYPE Stage, u32 Slot, CTexture* Texture)
{
	IRHIShaderResourceView* View = Texture->get_SRView();
	nri::Descriptor* NriView = Texture->BoundView();
	BindByHand(Stage, Slot, View, { NriView ? nullptr : Texture->pSurface, NriView });
}

void GfxCmdList::BindByHand(ERHI_SHADER_TYPE Stage, u32 Slot, IRHIShaderResourceView* View, HandView Nri)
{
	const u32 Kind = u32(Stage);

	if (Kind < std::size(HandViews) && Slot < std::size(HandViews[0]))
	{
		HandViews[Kind][Slot] = View ? Nri : HandView{};
		RawViews[Kind] = View ? RawViews[Kind] | 1ull << Slot : RawViews[Kind] & ~(1ull << Slot);
	}

	// The list that was applied last no longer holds: a pass that uses it again, with a texture of its own in this
	// slot, must bind it anew
	T = nullptr;
	Textures[Kind][Slot] = nullptr;
	TextureMask[Kind] &= ~(1ull << Slot);
	PendingTextureMask[Kind] &= ~(1ull << Slot);
	(GRHI->ShaderResourceCache->*StageResource[Kind])(Slot, View);
}

void GfxCmdList::SetTextures(STextureList* _T)
{
	PROF_EVENT("set_Textures");
	if (T == _T && !HasPendingTextures)
	{
		return;
	}
	T = _T;

	u64 Used[6] = {};
	for (auto& [Stage, Texture] : *_T)
	{
		const u32 Kind = Stage / CTexture::rstVertex;
		const u32 Slot = Stage % CTexture::rstVertex;
		const u64 Bit = 1ull << Slot;
		CTexture* Surface = Texture._get();
		if (!Surface)
		{
			continue;
		}
		Used[Kind] |= Bit;
		if (Textures[Kind][Slot] != Surface || (PendingTextureMask[Kind] & Bit))
		{
			Textures[Kind][Slot] = Surface;
			Surface->bind(Stage);
			if (Stage == 0)
			{
				Constants.Write<CBMaterial>().BaseIsBC1 = Surface->BindsBC1() ? 1u : 0u;
			}
			if (Surface->IsLoadPending())
			{
				PendingTextureMask[Kind] |= Bit;
			}
			else
			{
				PendingTextureMask[Kind] &= ~Bit;
			}
		}
	}

	HasPendingTextures = false;
	for (u32 Kind = 0; Kind < 6; ++Kind)
	{
		for (u64 Stale = TextureMask[Kind] & ~Used[Kind]; Stale; Stale &= Stale - 1)
		{
			const u32 Slot = u32(std::countr_zero(Stale));
			Textures[Kind][Slot] = nullptr;
			(GRHI->ShaderResourceCache->*StageResource[Kind])(Slot, nullptr);
		}
		TextureMask[Kind] = Used[Kind];
		PendingTextureMask[Kind] &= Used[Kind];
		HasPendingTextures |= PendingTextureMask[Kind] != 0;
	}
}

void GfxCmdList::FlushStates()
{
	if (StatesDirty)
	{
		const auto Resolve = [this](void*& Cached, const auto& Desc) { return Cached || !state ? Cached : (Cached = GRHI->GetState(Desc)); };
		IRHIStateManager& Manager = *GRHI->StateManager;
		Manager.SetRasterizerState(Resolve(States[0], Raster));
		Manager.SetDepthStencilState(Resolve(States[1], Depth));
		Manager.SetBlendState(Resolve(States[2], Blend));
		Manager.SetStencilRef(StencilRef);
		StatesDirty = false;
	}
	GRHI->StateManager->Apply();
}

void GfxCmdList::SetStencil(u32 Enable, u32 Func, u32 Ref, u32 Mask, u32 WriteMask, u32 Fail, u32 Pass, u32 ZFail)
{
	RHIDepthStencilDesc& Desc = EditDepth();
	Desc.StencilEnable = !!Enable;
	Desc.StencilReadMask = u8(Mask);
	Desc.StencilWriteMask = u8(WriteMask);
	Desc.FrontFace = { ERHI_STENCIL_OP(Fail), ERHI_STENCIL_OP(ZFail), ERHI_STENCIL_OP(Pass), ERHI_COMPARISON(Func) };
	Desc.BackFace = Desc.FrontFace;
	SetStencilRef(Ref);
}

void GfxCmdList::SetBlend(bool Enable, u32 Src, u32 Dst)
{
	for (RHIRenderTargetBlendDesc& Target : EditBlend().RenderTarget)
	{
		Target.BlendEnable = Enable;
		Target.SrcBlend = ERHI_BLEND(Src);
		Target.DestBlend = ERHI_BLEND(Dst);
	}
}

void GfxCmdList::SetColorWriteEnable(u32 Mask)
{
	for (RHIRenderTargetBlendDesc& Target : EditBlend().RenderTarget)
	{
		Target.RenderTargetWriteMask = u8(Mask);
	}
}

void GfxCmdList::SetScissor(Irect* Rect)
{
	if (Raster.ScissorEnable != (Rect != nullptr))
	{
		EditRaster().ScissorEnable = Rect != nullptr;
	}
	if (Rect)
	{
		LastScissor = *Rect;
	}
	GRHI->SetScissorRect(Rect);
}

void GfxCmdList::SetScissorOverride(bool Override)
{
	ScissorOverride = Override;
	EditRaster().ScissorEnable = Override || (state && state->Raster.ScissorEnable);
}

void GfxCmdList::SetFillModeOverride(bool Override, u32 Mode)
{
	FillOverride = Override;
	FillOverrideMode = Mode == RHI_FILL_SOLID ? RHI_FILL_SOLID : RHI_FILL_WIREFRAME;
	EditRaster().FillMode = Override ? FillOverrideMode : (state ? state->Raster.FillMode : RHI_FILL_SOLID);
}

void GfxCmdList::SetView(const GfxView& View)
{
	ViewState = View;
	Constants.UpdateView(ViewState);
	UpdateObject();
}

void GfxCmdList::SetObjectTransform(const Fmatrix& W, const Fmatrix& WOld)
{
	World = W;
	WorldOld = WOld;
	UpdateObject();
}

void GfxCmdList::UpdateObject()
{
	Fmatrix WorldView, WorldViewOld, WorldViewProjectOld;
	WorldView.mul_43(ViewState.View, World);
	WorldViewProject.mul(ViewState.Project, WorldView);
	WorldViewOld.mul_43(ViewState.ViewOld, WorldOld);
	WorldViewProjectOld.mul(ViewState.ProjectOld, WorldViewOld);
	Constants.UpdateObject(World, WorldView, WorldViewProject, WorldViewProjectOld);
}

void GfxCmdList::SetRenderTargets(std::initializer_list<IRHIRenderTargetView*> Targets, IRHIDepthStencilView* Depth)
{
	VERIFY(Targets.size() <= 4);
	u32 Index = 0;
	for (IRHIRenderTargetView* Target : Targets)
	{
		GRHI->SetRenderTargetView(Target, Index++);
	}
	for (; Index < 4; ++Index)
	{
		GRHI->SetRenderTargetView(nullptr, Index);
	}
	GRHI->SetDepthStencilView(Depth);
}
