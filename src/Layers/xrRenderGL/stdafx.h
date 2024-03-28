// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently

#pragma once

#pragma warning(disable:4995)
#include "../xrEngine/stdafx.h"
#pragma warning(default:4995)
#pragma warning(disable:4714)
#pragma warning( 4 : 4018 )
#pragma warning( 4 : 4244 )
#pragma warning(disable:4237)

#define GLEW_STATIC
#include <gl\glew.h>
#include <gl\GL.h>
#include <gl\GLU.h>
#include <gl\glext.h>
#include <gl\wglext.h>

#define CHK_GL(a) a

#define DEFINE_MAP_PRED(K, T, N, I, P)\
    typedef xr_map<K, T, P> N;\
    typedef N::iterator I;


#define DEFINE_VECTOR(T, N, I)\
    using N = xr_vector<T>;\
    using I = N::iterator;

#include "xrRenderOpenGL/xrD3DDefs.h"
#include "xrRenderOpenGL/Debug/dxPixEventWrapper.h"

#define		R_GL	0
#define		R_R1	1
#define		R_R2	2
#define		R_R3	3
#define		R_R4	4
#define		RENDER	R_GL

#include "../xrParticles/psystem.h"

#include "xrRenderOpenGL/HW.h"
#include "xrRenderOpenGL/Shader.h"
#include "xrRenderOpenGL/R_Backend.h"
#include "xrRenderOpenGL/R_Backend_Runtime.h"

#include "xrRenderOpenGL/ResourceManager.h"

#include "xrRenderOpenGL/dxRenderDeviceRender.h"

#include "../xrEngine/vis_common.h"
#include "../xrEngine/Render.h"
#include "../xrEngine/_d3d_extensions.h"
#include "../xrEngine/IGame_Level.h"
#include "xrRenderOpenGL/blenders\blender.h"
#include "xrRenderOpenGL/blenders\blender_clsid.h"
#include "xrRenderOpenGL/xrRender_console.h"
#include "xrRenderPC_GL/rgl.h"

IC	void	jitter(CBlender_Compile& C)
{
	C.r_Sampler("jitter0", JITTER(0), true, D3DTADDRESS_WRAP, D3DTEXF_POINT, D3DTEXF_NONE, D3DTEXF_POINT);
	C.r_Sampler("jitter1", JITTER(1), true, D3DTADDRESS_WRAP, D3DTEXF_POINT, D3DTEXF_NONE, D3DTEXF_POINT);
	C.r_Sampler("jitter2", JITTER(2), true, D3DTADDRESS_WRAP, D3DTEXF_POINT, D3DTEXF_NONE, D3DTEXF_POINT);
	C.r_Sampler("jitter3", JITTER(3), true, D3DTADDRESS_WRAP, D3DTEXF_POINT, D3DTEXF_NONE, D3DTEXF_POINT);
}