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

bool is_device_paused(CRenderDevice* d)
{
	return !!Device.Paused();
}

void set_device_paused(CRenderDevice* d, bool b)
{
	Device.Pause(b, TRUE, FALSE,"set_device_paused_script");
}

extern ENGINE_API BOOL g_appLoaded;
bool is_app_ready()
{
	return !!g_appLoaded;
}

u32 time_global(const CRenderDevice *self_)
{
	THROW		(self_);
	return		(self_->dwTimeGlobal);
}

#pragma optimize("s",on)
void CScriptRenderDevice::script_register(lua_State *L)
{
	module(L)
	[
		class_<CRenderDevice>("render_device")
			.def_readonly("width",			static_cast<u32 CRenderDevice::*>(&CRenderDevice::TargetWidth))
			.def_readonly("height",			static_cast<u32 CRenderDevice::*>(&CRenderDevice::TargetHeight))
			.def_readonly("time_delta",		static_cast<u32 CRenderDevice::*>(&CRenderDevice::dwTimeDelta))
			.def_readonly("f_time_delta",	static_cast<float CRenderDevice::*>(&CRenderDevice::fTimeDelta))
			.def_readonly("cam_pos",		static_cast<Fvector CRenderDevice::*>(&CRenderDevice::vCameraPosition))
			.def_readonly("cam_dir",		static_cast<Fvector CRenderDevice::*>(&CRenderDevice::vCameraDirection))
			.def_readonly("cam_top",		static_cast<Fvector CRenderDevice::*>(&CRenderDevice::vCameraTop))
			.def_readonly("cam_right",		static_cast<Fvector CRenderDevice::*>(&CRenderDevice::vCameraRight))
//			.def_readonly("view",			&CRenderDevice::mView)
//			.def_readonly("projection",		&CRenderDevice::mProject)
//			.def_readonly("full_transform",	&CRenderDevice::mFullTransform)
			.def_readonly("fov",			static_cast<float CRenderDevice::*>(&CRenderDevice::fFOV))
			.def_readonly("aspect_ratio",	static_cast<float CRenderDevice::*>(&CRenderDevice::fASPECT))
			.def("time_global",				&time_global)
			.def_readonly("precache_frame",	static_cast<u32 CRenderDevice::*>(&CRenderDevice::dwPrecacheFrame))
			.def_readonly("frame",			static_cast<u32 CRenderDevice::*>(&CRenderDevice::dwFrame))
			.def("is_paused",				&is_device_paused)
			.def("pause",					&set_device_paused),
			def("app_ready",				&is_app_ready)
	];
}
