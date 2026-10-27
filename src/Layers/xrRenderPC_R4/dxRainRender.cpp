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
	Geometry.create(FVF::F_LIT, RCache.Vertex.Buffer(), RCache.QuadIB);
	DropBounds.set(Fvector().set(0.f, 0.f, 0.f), 1.f);
}

dxRainRender::~dxRainRender() = default;

void dxRainRender::Copy(IRainRender& Source)
{
	// This object already owns its resources. No simulation state lives in the renderer.
	(void)Source;
}

void dxRainRender::Render(CEffect_Rain& Owner)
{
	// Render holds rainCS; publish the console setting for the next worker update.
	Owner.RainDropsNearDistance = g_RainDropsNear;
	if (Owner.RainInstances.empty() && Owner.ImpactInstances.empty())
	{
		return;
	}
	GPU_EVENT(Rain);
	CRenderTarget& Target = *RImplementation.Target;
	const bool Dynamic = LightingModeIsDynamicRender();
	if (!Dynamic)
	{
		// Static lighting does not run phase_combine, which binds these aliases in dynamic lighting.
		auto& Environment = g_pGamePersistent->Environment();
		auto& Mixer = static_cast<dxEnvDescriptorMixerRender&>(*Environment.CurrentEnv->m_pDescriptorMixer);
		IRHISurface* Previous = Mixer.sky_r_textures_env.empty() ? nullptr : Mixer.sky_r_textures_env[0].second->surface_get();
		IRHISurface* Next = Mixer.sky_r_textures_env.size() < 2 ? nullptr : Mixer.sky_r_textures_env[1].second->surface_get();
		Target.t_envmap_0->surface_set(Previous);
		Target.t_envmap_1->surface_set(Next);
		_RELEASE(Previous);
		_RELEASE(Next);
	}
	const bool Pbr = g_RainShading == 2 || (g_RainShading == 0 && Dynamic && !RImplementation.o.dx11_use_legacy_light);

	// Rain has color-only output; leave the opaque scene's velocity untouched.
	Target.u_setrt(Target.rt_Generic_0, nullptr, RDepth);
	RCache.set_xform_world(Fidentity);
	RCache.set_Stencil(FALSE);
	RCache.set_ColorWriteEnable();
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	u32 VertexOffset = 0;
	auto* Vertices = static_cast<FVF::LIT*>(RCache.Vertex.Lock(4, sizeof(FVF::LIT), VertexOffset));
	Vertices[0].set(-1.f, -1.f, 0.f, 0xffffffff, 0.f, 1.f);
	Vertices[1].set(-1.f, 1.f, 0.f, 0xffffffff, 0.f, 0.f);
	Vertices[2].set(1.f, -1.f, 0.f, 0xffffffff, 1.f, 1.f);
	Vertices[3].set(1.f, 1.f, 0.f, 0xffffffff, 1.f, 0.f);
	RCache.Vertex.Unlock(4, sizeof(FVF::LIT));

	auto Draw = [&](ref_shader& Shader, InstanceStream& Stream, const void* Data, u32 Count, u32 Stride, u32 Children, float Radius)
	{
		if (!Stream.Upload(Data, Count, Stride))
		{
			return;
		}
		RCache.set_Shader(Shader);
		RCache.set_Stencil(FALSE);
		RCache.set_ColorWriteEnable();
		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
		const u32 Slot = Shader->E[0]->passes[0]->vs->RainInstanceSlot;
		if (Slot == u32(-1))
		{
			return;
		}
		RCache.set_Geometry(Geometry);
		RCache.set_c("RainShape", Owner.RainWidth * (2.f / 3.f), Owner.RainLength * 0.5f, Radius, g_RainDropsFeather);
		RCache.set_c("RainColor", Owner.RainColor.x, Owner.RainColor.y, Owner.RainColor.z, Owner.RainDropsOpacity * g_RainDropsOpacity);
		RCache.set_c("RainShading", Pbr ? 1.f : 0.f, g_RainDropsColorBlend, g_RainDropsTransmission, g_RainDropsRefraction);
		GRHI->ShaderResourceCache->SetVSResource(Slot, Stream.View);
		RCache.RenderInstancedIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, VertexOffset, 0, 4, 0, 2, Count * Children, 0, true);
		GRHI->ShaderResourceCache->SetVSResource(Slot, nullptr);
		RCache.InvalidateVSTexture(Slot);
		GRHI->ShaderResourceCache->Apply();
	};
	Draw(AirShader, AirStream, Owner.RainInstances.data(), u32(Owner.RainInstances.size()), sizeof(CEffect_Rain::RainInstance), 1, g_RainDropsRadius);
	Draw(ImpactShader, ImpactStream, Owner.ImpactInstances.data(), u32(Owner.ImpactInstances.size()), sizeof(CEffect_Rain::ImpactInstance), 4, g_RainDropsImpactRadius);

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);
	if (Dynamic && !RImplementation.o.dx11_disable_motion_vectors)
	{
		Target.u_setrt(Target.rt_Generic_0, Target.rt_Velocity, RDepth);
	}
}

const Fsphere& dxRainRender::GetDropBounds() const
{
	return DropBounds;
}
