//---------------------------------------------------------------------------

#include "stdafx.h"
#pragma hdrstop

#include "UI_ToolsCustom.h"
#include "EditObject.h"
#include "EditMesh.h"
#include "ui_main.h"           
#include "../xrEngine/motion.h"
#include "../xrEngine/bone.h"
#include "../xrEngine/fmesh.h"
#include "library.h"
#include "d3dutils.h"

//------------------------------------------------------------------------------
CToolCustom* Tools=0;
//------------------------------------------------------------------------------
#define CHECK_SNAP(R,A,C){ R+=A; if(fabsf(R)>=C){ A=snapto(R,C); R=0; }else{A=0;}}

CToolCustom::CToolCustom()
{
    m_bReady			= false;
    m_Action			= etaSelect;
    m_Settings.assign	(etfNormalAlign|etfGSnap|etfOSnap|etfMTSnap|etfVSnap|etfASnap|etfMSnap);
    fFogness			= 0.9f;
    dwFogColor			= 0xffffffff;
    m_axis_xform		= Fidentity;
}
//---------------------------------------------------------------------------

CToolCustom::~CToolCustom()
{
}
//---------------------------------------------------------------------------

bool CToolCustom::OnCreate()
{
    m_bReady 		= true;

	SetAction		(etaSelect);
    return true;
}

void CToolCustom::OnDestroy()
{
	VERIFY					(m_bReady);
    m_bReady				= false;
}
//---------------------------------------------------------------------------

void CToolCustom::SetAction(ETAction action)
{
	switch(action)
    {
    case etaSelect: 
    	m_bHiddenMode	= false; 
        break;
    case etaAdd:
    	m_bHiddenMode	= true; 
        break;
    }
    m_Action = action;
  /*  switch(m_Action)
    {
    case etaSelect:  
    	UI->GetD3DWindow()->Cursor = crCross;     
    break;
    case etaAdd:     
    	UI->GetD3DWindow()->Cursor = crArrow;     
    break;
    case etaMove:
    	if(!EPrefs->tools_show_move_axis)    
        	UI->GetD3DWindow()->Cursor = crSizeAll;   
        else
        	UI->GetD3DWindow()->Cursor = crHandPoint;   
        
    break;
    case etaRotate:  
    	UI->GetD3DWindow()->Cursor = crSizeWE;    
    break;
    case etaScale:   
    	UI->GetD3DWindow()->Cursor = crVSplit;    
    break;
    default:         
    	UI->GetD3DWindow()->Cursor = crHelp;
    }*/

 
    UI->RedrawScene();
    ExecCommand(COMMAND_REFRESH_UI_BAR);
}


void CToolCustom::SetSettings(u32 mask, BOOL val)
{
	m_Settings.set(mask,val);
    UI->RedrawScene();
    ExecCommand(COMMAND_REFRESH_UI_BAR);
}


bool  CToolCustom::MouseStart(TShiftState Shift)
{
	switch(m_Action)
    {
        case etaSelect:	break;
        case etaAdd:	break;
   
    }
    
	return m_bHiddenMode;
}

bool  CToolCustom::MouseEnd(TShiftState Shift)
{
	switch(m_Action)
    {
        case etaSelect: break;
        case etaAdd: 	break;
    }
	return true;
}

void  CToolCustom::MouseMove(TShiftState Shift)
{
	switch(m_Action)
    {
    case etaSelect: break;
    case etaAdd: 	break;
    }
}

void CToolCustom::GetCurrentFog(u32& fog_color, float& s_fog, float& e_fog)
{
    s_fog				= psDeviceFlags.is(rsFog)?(1.0f - fFogness)* 0.85f * UI->ZFar():0.99f*UI->ZFar();
    e_fog				= psDeviceFlags.is(rsFog)?0.91f * UI->ZFar():UI->ZFar();
    fog_color 			= dwFogColor;
}

void CToolCustom::RenderEnvironment()
{
}

void CToolCustom::Clear()
{
	ClearDebugDraw		();
}

void CToolCustom::Render()
{
    // render errors
    EDevice->SetShader(EDevice->m_SelectionShader);
    RCache.set_xform_world(Fidentity);
    EDevice->RenderNearer(0.0003f);
    EDevice->SetRS(D3DRS_CULLMODE, D3DCULL_NONE);
    xr_string temp;
    temp.resize(64);

    int cnt = 0;
    for (SDebugDraw::PointIt vit = m_DebugDraw.m_Points.begin(); vit != m_DebugDraw.m_Points.end(); ++vit)
    {
        LPCSTR s = NULL;
        if (vit->i)
        {
            sprintf(temp.data(), "P: %d", cnt++);
            s = temp.c_str();
        }

        if (vit->descr.size())
        {
            s = vit->descr.c_str();
        }
        DU_impl.dbgDrawVert(vit->p[0], vit->c, s ? s : "");
    }
    EDevice->SetShader(EDevice->m_SelectionShader);

    temp.clear();
    temp.resize(64);

    cnt = 0;
    for (SDebugDraw::LineIt eit = m_DebugDraw.m_Lines.begin(); eit != m_DebugDraw.m_Lines.end(); eit++) {
        if (eit->i)        sprintf(temp.data(), "L: %d", cnt++);
        DU_impl.dbgDrawEdge(eit->p[0], eit->p[1], eit->c, eit->i ? temp.c_str() : "");
    }
    EDevice->SetShader(EDevice->m_SelectionShader);

    temp.clear();
    temp.resize(64);
    cnt = 0;
    for (SDebugDraw::FaceIt fwit = m_DebugDraw.m_WireFaces.begin(); fwit != m_DebugDraw.m_WireFaces.end(); fwit++) {
        if (fwit->i)        sprintf(temp.data(), "F: %d", cnt++);
        DU_impl.dbgDrawFace(fwit->p[0], fwit->p[1], fwit->p[2], fwit->c, fwit->i ? temp.c_str() : "");
    }
    
    cnt = 0;
    if (!m_DebugDraw.m_SolidFaces.empty()) {
        EDevice->SetShader(EDevice->m_SelectionShader);
        DU_impl.DD_DrawFace_begin(FALSE);
        for (SDebugDraw::FaceIt fsit = m_DebugDraw.m_SolidFaces.begin(); fsit != m_DebugDraw.m_SolidFaces.end(); fsit++)
            DU_impl.DD_DrawFace_push(fsit->p[0], fsit->p[1], fsit->p[2], fsit->c);
        DU_impl.DD_DrawFace_end();
    }
    EDevice->SetShader(EDevice->m_SelectionShader);

    temp.clear();
    temp.resize(64);
    cnt = 0;
    for (SDebugDraw::OBBVecIt oit = m_DebugDraw.m_OBB.begin(); oit != m_DebugDraw.m_OBB.end(); oit++)
    {
        sprintf(temp.data(), "OBB: %d", cnt++);
        DU_impl.DrawOBB(Fidentity, *oit, 0x2F00FF00, 0xFF00FF00);
        DU_impl.OutText(oit->m_translate, temp.c_str(), 0xffff0000, 0x0000000);
    }

    EDevice->SetRS(D3DRS_CULLMODE, D3DCULL_CCW);
    EDevice->ResetNearer();
}
//------------------------------------------------------------------------------

