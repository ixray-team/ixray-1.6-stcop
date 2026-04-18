#include "StdAfx.h"
#include "DynamicWallmarkZone.h"
#include "../Include/xrRender/WallMarkArray.h"

#include "xrServer_Objects_ALife.h"
#include "Level.h"

CDynamicWallmarkRegistry::CDynamicWallmarkRegistry()
{
}

CDynamicWallmarkRegistry& CDynamicWallmarkRegistry::Instance()
{
    static CDynamicWallmarkRegistry Instance;
    return Instance;
}

wm_shader CDynamicWallmarkRegistry::GetWallmarkShader(shared_str shader, shared_str texture)
{
    auto shader_registry = registry.find(shader);
    if (shader_registry == registry.end())
    {
        shader_registry = registry.emplace(shader, xr_hash_map<shared_str, FactoryPtr<IWallMarkArray>>()).first;
    }
    auto& shader_registry_ref = shader_registry->second;
    auto texture_registry = shader_registry_ref.find(texture);
    if (texture_registry == shader_registry_ref.end())
    {
        texture_registry = shader_registry_ref.emplace(texture, FactoryPtr<IWallMarkArray>()).first;
    }
    auto& texture_registry_ref = texture_registry->second;
    if (texture_registry_ref->empty())
    {
        texture_registry_ref->AppendMark(shader.c_str(), texture.c_str());
    }
    return texture_registry_ref->GenerateWallmark();
}

void CDynamicWallmarkRegistry::ClearWallmarks()
{
    registry.clear();
}

bool CDynamicWallmarkZone::trace_callback(collide::rq_result& result, LPVOID params)
{
    VERIFY(!result.O);
    rq_data* wm_trace_data = (rq_data*)params;
    VERIFY(!wm_trace_data->self->handler);
	
    Fvector collide_position = Fvector().mad(wm_trace_data->StartPos, wm_trace_data->Dir, result.range);
	
    xr_vector<Fvector>& pVerts	= Level().ObjectSpace.GetStaticVerts();
    CDB::TRI&	pTri	= Level().ObjectSpace.GetStaticTris()[result.element];

    wm_trace_data->self->handler = ::Render->add_DynamicWallmark(
        CDynamicWallmarkRegistry::Instance().GetWallmarkShader(wm_trace_data->self->shader, wm_trace_data->self->texture),
        collide_position,
        wm_trace_data->self->w,
        wm_trace_data->self->h,
        wm_trace_data->self->r,
        &pTri, pVerts.data());

	return false;
}

bool CDynamicWallmarkZone::test_callback(const collide::ray_defs& rd, CObject* object, LPVOID params)
{
    return true;
}

bool CDynamicWallmarkZone::net_Spawn(CSE_Abstract* DC)
{
    auto result = inherited::net_Spawn(DC);

    auto Casted = smart_cast<CSE_ALifeDynamicWallmark*>(DC);

    shader = Casted->shader;
    texture = Casted->texture;
    w = Casted->w;
    h = Casted->h;
    r = Casted->r;
	
    return result;
}

void CDynamicWallmarkZone::save(NET_Packet& output_packet)
{
    inherited::save(output_packet);
    output_packet.w_u8(CurrentStatus);
    output_packet.w_stringZ(shader);
    output_packet.w_stringZ(texture);
    output_packet.w_float(h);
    output_packet.w_float(w);
    output_packet.w_float(r);
}

void CDynamicWallmarkZone::load(IReader& input_packet)
{
    inherited::load(input_packet);
    bool LocCurrentStatus = input_packet.r_u8();
    input_packet.r_stringZ(shader);
    input_packet.r_stringZ(texture);
    h = input_packet.r_float();
    w = input_packet.r_float();
    r = input_packet.r_float();
    SwitchWallmark(LocCurrentStatus);
}

void CDynamicWallmarkZone::SwitchWallmark(bool isOn)
{
    if (isOn == CurrentStatus)
    {
        return;
    }
    CurrentStatus = isOn;
    if (isOn)
    {
        rq_data wm_trace_data;
        wm_trace_data.self = this;
        wm_trace_data.StartPos = XFORM().c;
        wm_trace_data.Dir = XFORM().j;
        wm_trace_data.StartPos.add(wm_trace_data.Dir);
        wm_trace_data.Dir.invert();
		
        collide::rq_results storage;
        collide::ray_defs RD(wm_trace_data.StartPos, wm_trace_data.Dir, 1500.0f, CDB::OPT_FULL_TEST, collide::rqtBoth);
        VERIFY(Level().ObjectSpace.RayQuery(storage, RD, CDynamicWallmarkZone::trace_callback, &wm_trace_data, CDynamicWallmarkZone::test_callback, nullptr));
    } else
    {
        if (!handler || !handler->IsValid())
        {
            return;
        }
        handler->Destroy();
        handler = nullptr;
    }
}
