#include "stdafx.h"
#include "dxRainRender.h"
#include "../../xrEngine/Rain.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "dxEnvironmentRender.h"
#include "xrRender_console.h"

// Buffers survive a D3D11 swapchain resize; streams and geometry are released with the effect.
dxRainRender::InstanceStream::~InstanceStream()
{
	_RELEASE(View);
	_RELEASE(Buffer);
}

bool dxRainRender::InstanceStream::Upload(const void* Data, u32 Count, u32 Stride)
{
	if (!Data || !Count || !Stride || Count > u32(-1) / Stride)
	{
		return false;
	}
	if (Count > Capacity)
	{
		_RELEASE(View);
		_RELEASE(Buffer);
		Capacity = 0;
		RHIBufferDesc Desc{};
		Desc.Usage = ERHI_USAGE::USAGE_DYNAMIC;
		Desc.Type = ERHI_BUFFER_TYPE::STRUCTURED;
		Desc.CPUAccessFlags = ERHI_CPU_ACCESS_FLAG::ERHI_CPU_ACCESS_FLAG_WRITE;
		Desc.StructureByteStride = Stride;
		// Grow in chunks to avoid reallocating on every increase in visible count.
		const u64 RoundedCount = ((u64(Count) + 255) / 256) * 256;
		const u32 NewCapacity = RoundedCount <= u32(-1) / Stride ? u32(RoundedCount) : Count;
		Desc.Size = NewCapacity * Stride;
		Buffer = GRHI->CreateBuffer(Desc);
		if (!Buffer)
		{
			return false;
		}
		RHIShaderResourceViewDesc ViewDesc{};
		ViewDesc.Format = ERHI_FORMAT::UNKNOWN;
		ViewDesc.ElementWidth = NewCapacity;
		View = GRHI->CreateShaderResourceView(Buffer, &ViewDesc);
		if (!View)
		{
			_RELEASE(Buffer);
			return false;
		}
		Capacity = NewCapacity;
	}
	RHIMappedSubresource Mapped{};
	if (!Buffer->Map(ERHI_BUFFER_MAP::WRITE_DISCARD, 0, &Mapped))
	{
		return false;
	}
	memcpy(Mapped.pData, Data, Count * Stride);
	Buffer->Unmap();
	return true;
}

dxRainRender::dxRainRender()
{
	AirShader.create("effects\\rain_instanced", "fx\\hud_rain");
	ImpactShader.create("effects\\rain_impact", "fx\\hud_rain");
	Geometry.create(EGeomSource::Ring);
	DropBounds.set(Fvector().set(0.f, 0.f, 0.f), 1.f);
}

dxRainRender::~dxRainRender() = default;

void dxRainRender::Copy(IRainRender& Source)
{
	// This object already owns its resources. No simulation state lives in the renderer.
	(void)Source;
}

dxRainRender* dxRainRender::Frame = nullptr;

void dxRainRender::Take(CEffect_Rain& Owner)
{
	// The drops are simulated on another thread
	xrCriticalSectionGuard Guard(Owner.rainCS);
	// The console setting for the next worker update
	Owner.RainDropsNearDistance = g_RainDropsNear;
	Frame = Owner.IsEnabled ? static_cast<dxRainRender*>(&*Owner.m_pRender) : nullptr;

	if (Frame)
	{
		Owner.BuildInstances();
		FrameState& State = Frame->State;
		State.RainInstances.assign(Owner.RainInstances.begin(), Owner.RainInstances.end());
		State.ImpactInstances.assign(Owner.ImpactInstances.begin(), Owner.ImpactInstances.end());
		State.RainColor = Owner.RainColor;
		State.RainLength = Owner.RainLength;
		State.RainWidth = Owner.RainWidth;
		State.RainDropsOpacity = Owner.RainDropsOpacity;
	}
}

void dxRainRender::Draw()
{
	if (Frame)
	{
		Frame->DrawState();
	}
}

// For a caller outside the frame's collect
void dxRainRender::Render(CEffect_Rain& Owner)
{
	Take(Owner);
	Draw();
}

void dxRainRender::DrawState()
{
	FrameState& Owner = State;
	if (Owner.RainInstances.empty() && Owner.ImpactInstances.empty())
	{
		return;
	}
	GPU_EVENT(Rain);
	GfxCmdList& Cmd = RImplementation.Imm();
	CRenderTarget& Target = *RImplementation.Target;
	const bool Pbr = g_RainShading == 2;

	// Rain has color-only output; leave the opaque scene's velocity untouched.
	Target.u_setrt(Cmd, Target.rt_Generic_0, nullptr, RDepth);
	Cmd.SetObjectTransform(Fidentity);
	Cmd.SetStencil(FALSE);
	Cmd.SetColorWriteEnable();
	Cmd.SetCullMode(ERHI_CULLMODE::NONE);
	u32 VertexOffset = 0;
	DynamicGeometry& Dynamic = RImplementation.Dynamic;
	auto* Vertices = static_cast<FVF::LIT*>(Dynamic.Lock(6, EVertexFormat::LIT, VertexOffset));
	Vertices[0].set(-1.f, -1.f, 0.f, 0xffffffff, 0.f, 1.f);
	Vertices[1].set(-1.f, 1.f, 0.f, 0xffffffff, 0.f, 0.f);
	Vertices[2].set(1.f, -1.f, 0.f, 0xffffffff, 1.f, 1.f);
	Vertices[3].set(1.f, 1.f, 0.f, 0xffffffff, 1.f, 0.f);
	Vertices[4] = Vertices[2];
	Vertices[5] = Vertices[1];
	Dynamic.Unlock(6);

	auto Draw = [&](ref_shader& Shader, InstanceStream& Stream, const void* Data, u32 Count, u32 Stride, u32 Children, float Radius)
	{
		if (!Stream.Upload(Data, Count, Stride))
		{
			return;
		}
		Cmd.SetPass(Shader);
		Cmd.SetStencil(FALSE);
		Cmd.SetColorWriteEnable();
		Cmd.SetCullMode(ERHI_CULLMODE::NONE);
		const u32 Slot = Shader->E[0]->passes[0]->vs->RainInstanceSlot;
		if (Slot == u32(-1))
		{
			return;
		}
		Cmd.SetGeometry(Geometry);
		CBPost& Post = Cmd.Constants.Write<CBPost>();
		Post.RainShape.set(Owner.RainWidth * (2.f / 3.f), Owner.RainLength * 0.5f, Radius, g_RainDropsFeather);
		Post.RainColor.set(Owner.RainColor.x, Owner.RainColor.y, Owner.RainColor.z, Owner.RainDropsOpacity * g_RainDropsOpacity);
		Post.RainShading.set(Pbr ? 1.f : 0.f, g_RainDropsColorBlend, g_RainDropsTransmission, g_RainDropsRefraction);
		// The same instances for a draw through NRI
		Cmd.SetResource(ERHI_SHADER_TYPE::VS, Slot, Stream.View, RImplementation.NriPasses.StreamRecords(Data, Count * Stride, Stride));
		Dynamic.DrawInstanced(Cmd, ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 6, Count * Children, VertexOffset);
		Cmd.SetResource(ERHI_SHADER_TYPE::VS, Slot, nullptr);
		GRHI->ShaderResourceCache->Apply();
	};
	Draw(AirShader, AirStream, Owner.RainInstances.data(), u32(Owner.RainInstances.size()), sizeof(CEffect_Rain::RainInstance), 1, g_RainDropsRadius);
	Draw(ImpactShader, ImpactStream, Owner.ImpactInstances.data(), u32(Owner.ImpactInstances.size()), sizeof(CEffect_Rain::ImpactInstance), 4, g_RainDropsImpactRadius);

	Cmd.SetCullMode(ERHI_CULLMODE::BACK);
}

const Fsphere& dxRainRender::GetDropBounds() const
{
	return DropBounds;
}
