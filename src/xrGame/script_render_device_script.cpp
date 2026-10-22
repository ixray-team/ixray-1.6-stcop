////////////////////////////////////////////////////////////////////////////
//	Module 		: script_render_device_script.cpp
//	Created 	: 28.06.2004
//  Modified 	: 28.06.2004
//	Author		: Dmitriy Iassenev
//	Description : Script render device script export
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "script_render_device.h"

using namespace luabind;

bool is_device_paused(CRenderDeviceData* d)
{
	return !!Device.Paused();
}

void set_device_paused(CRenderDeviceData* d, bool b)
{
	Device.Pause(b, true, false,"set_device_paused_script");
}

extern ENGINE_API bool g_appLoaded;
bool is_app_ready()
{
	return !!g_appLoaded;
}

struct SScriptVideoMemoryInfo
{
	bool Valid = false;
	float Budget = 0.f;
	float Usage = 0.f;
	float RenderTargets = 0.f;
	float ShadowMaps = 0.f;
	float Textures = 0.f;
	u32 RenderWidth = 0;
	u32 RenderHeight = 0;
};

SScriptVideoMemoryInfo get_video_memory_info()
{
	SScriptVideoMemoryInfo Result;
	IRenderDeviceRender::SVideoMemoryInfo Info;

	if (Device.m_pRender && Device.m_pRender->GetVideoMemoryInfo(Info))
	{
		constexpr float Megabyte = 1024.f * 1024.f;

		Result.Valid = true;
		Result.Budget = float(Info.Budget) / Megabyte;
		Result.Usage = float(Info.Usage) / Megabyte;
		Result.RenderTargets = float(Info.RenderTargets) / Megabyte;
		Result.ShadowMaps = float(Info.ShadowMaps) / Megabyte;
		Result.Textures = float(Info.Textures) / Megabyte;
		Result.RenderWidth = Info.RenderWidth;
		Result.RenderHeight = Info.RenderHeight;
	}

	return Result;
}

u32 time_global(const CRenderDeviceData *self_)
{
	THROW		(self_);
	return		(self_->dwTimeGlobal);
}

#pragma optimize("s",on)
template<>
void CScriptRenderDevice::script_register(lua_State *L)
{
	module(L)
	[
		class_<CRenderDeviceData>("render_device")
			.def_readonly("width",			static_cast<u32 CRenderDeviceData::*>(&CRenderDeviceData::TargetWidth))
			.def_readonly("height",			static_cast<u32 CRenderDeviceData::*>(&CRenderDeviceData::TargetHeight))
			.def_readonly("time_delta",		static_cast<u32 CRenderDeviceData::*>(&CRenderDeviceData::dwTimeDelta))
			.def_readonly("f_time_delta",	static_cast<float CRenderDeviceData::*>(&CRenderDeviceData::fTimeDelta))
			.def_readonly("cam_pos",		static_cast<Fvector CRenderDeviceData::*>(&CRenderDeviceData::vCameraPosition))
			.def_readonly("cam_dir",		static_cast<Fvector CRenderDeviceData::*>(&CRenderDeviceData::vCameraDirection))
			.def_readonly("cam_top",		static_cast<Fvector CRenderDeviceData::*>(&CRenderDeviceData::vCameraTop))
			.def_readonly("cam_right",		static_cast<Fvector CRenderDeviceData::*>(&CRenderDeviceData::vCameraRight))
//			.def_readonly("view",			&CRenderDevice::mView)
//			.def_readonly("projection",		&CRenderDevice::mProject)
//			.def_readonly("full_transform",	&CRenderDevice::mFullTransform)
			.def_readonly("fov",			static_cast<float CRenderDeviceData::*>(&CRenderDeviceData::fFOV))
			.def_readonly("aspect_ratio",	static_cast<float CRenderDeviceData::*>(&CRenderDeviceData::fASPECT))
			.def("time_global",				&time_global)
			.def_readonly("precache_frame",	static_cast<u32 CRenderDeviceData::*>(&CRenderDeviceData::dwPrecacheFrame))
			.def_readonly("frame",			static_cast<u32 CRenderDeviceData::*>(&CRenderDeviceData::dwFrame))
			.def_readonly("ftime_delta",		static_cast<float CRenderDeviceData::*>(&CRenderDeviceData::fTimeDelta))
			.def_readonly("dwtime_continual",	static_cast<u32 CRenderDeviceData::*>(&CRenderDeviceData::dwTimeContinual)),

		def("app_ready",				&is_app_ready),

		class_<SScriptVideoMemoryInfo>("video_memory_info")
			.def_readonly("valid",			&SScriptVideoMemoryInfo::Valid)
			.def_readonly("budget",			&SScriptVideoMemoryInfo::Budget)
			.def_readonly("usage",			&SScriptVideoMemoryInfo::Usage)
			.def_readonly("render_targets",	&SScriptVideoMemoryInfo::RenderTargets)
			.def_readonly("shadow_maps",	&SScriptVideoMemoryInfo::ShadowMaps)
			.def_readonly("textures",		&SScriptVideoMemoryInfo::Textures)
			.def_readonly("render_width",	&SScriptVideoMemoryInfo::RenderWidth)
			.def_readonly("render_height",	&SScriptVideoMemoryInfo::RenderHeight),

		def("get_video_memory_info",	&get_video_memory_info),

		class_<CRenderDevice, CRenderDeviceData>("engine_device")
			.def("is_paused", &is_device_paused)
			.def("pause", &set_device_paused)
	];
}
