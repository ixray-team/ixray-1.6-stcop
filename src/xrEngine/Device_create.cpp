#include "stdafx.h"

//#include "resourcemanager.h"
#include "../Include/xrRender/DrawUtils.h"
//#include "xr_effgamma.h"
#include "Render.h"
#include "../xrCore/Collision/xrCDB.h"

extern XRCORE_API bool *cdb_bDebug;

void	SetupGPU(IRenderDeviceRender *pRender)
{
	// Command line
	char *lpCmdLine		= Core.Params;

	bool bForceGPU_SW;
	bool bForceGPU_NonPure;
	bool bForceGPU_REF;

	if (strstr(lpCmdLine,"-gpu_sw")!=nullptr)		bForceGPU_SW		= true;
	else										bForceGPU_SW		= false;
	if (strstr(lpCmdLine,"-gpu_nopure")!=nullptr)	bForceGPU_NonPure	= true;
	else										bForceGPU_NonPure	= false;
	if (strstr(lpCmdLine,"-gpu_ref")!=nullptr)		bForceGPU_REF		= true;
	else										bForceGPU_REF		= false;

	pRender->SetupGPU(bForceGPU_SW, bForceGPU_NonPure, bForceGPU_REF);
}

void CRenderDevice::_SetupStates	()
{
	// General Render States
	mView.identity			();
	mProject.identity		();
	mFullTransform.identity	();
	vCameraPosition.set		(0,0,0);
	vCameraDirection.set	(0,0,1);
	vCameraTop.set			(0,1,0);
	vCameraRight.set		(1,0,0);

	m_pRender->SetupStates();
}

void CRenderDevice::_Create	(const char* shName)
{
	Memory.mem_compact			();

	// after creation
	b_is_Ready					= true;
	_SetupStates				();

	m_pRender->OnDeviceCreate(shName);

	dwFrame						= 0;
}

void CRenderDevice::ConnectToRender()
{
	R_ASSERT2(RenderFactory, "Render factory is empty!");

	if (!m_pRender)
		m_pRender = RenderFactory->CreateRenderDeviceRender();
}

void CRenderDevice::Create	() 
{
	PROF_EVENT("CRenderDevice::Create");
	if (b_is_Ready)		return;		// prevent double call
	Statistic			= new CStats();

#ifdef	DEBUG
	cdb_bDebug		= &bDebug;
#endif

	if (!m_pRender)
		m_pRender = RenderFactory->CreateRenderDeviceRender();

	SetupGPU(m_pRender);
	Log					("Starting RENDER device...");

	psCurrentVidMode[0]	= TargetWidth;
	psCurrentVidMode[1] = TargetHeight;

	fFOV = 90.f;
	fASPECT = 1.f;

	m_pRender->Create
	(
		g_AppInfo.Window,
		TargetWidth,
		TargetHeight,
		true
	);

	string_path			fname; 
	FS.update_path		(fname,_game_data_,"shaders.xr");

	//////////////////////////////////////////////////////////////////////////
	_Create				(fname);

	PreCache			(0, false, false);
}
