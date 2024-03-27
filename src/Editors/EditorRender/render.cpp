#include "stdafx.h"
#pragma hdrstop

#include "render.h"
#include "../../Layers/xrRender/ResourceManager.h"
#include <d3dcompiler.h>

//---------------------------------------------------------------------------
float ssaDISCARD		= 4.f;
float ssaDONTSORT		= 32.f;

ECORE_API float r_ssaDISCARD;
ECORE_API float	g_fSCREEN;

CRender   			RImplementation;
ECORE_API CRender* 	Render 		= &RImplementation;

//---------------------
IRenderFactory*	RenderFactory = NULL;
//---------------------------------------------------------------------------

u32 GetGpuNum()
{
	return 1;
}

CRender::CRender	()
{
	m_skinning					= 0;
}

CRender::~CRender	()
{
	xr_delete		(Target);
}

void					CRender::Initialize				()
{
	PSLibrary.OnCreate			();
}
void					CRender::ShutDown				()
{
	PSLibrary.OnDestroy			();
}

void					CRender::OnDeviceCreate			()
{
	Models						= xr_new<CModelPool>	();
    Models->Logging				(FALSE);
}
void					CRender::OnDeviceDestroy		()
{
	xr_delete					(Models);
}

ref_shader	CRender::getShader	(int id){ return 0; }//VERIFY(id<int(Shaders.size()));	return Shaders[id];	}

BOOL CRender::occ_visible(Fbox&	B)
{
	u32 mask		= 0xff;
	return ViewBase.testAABB(B.data(),mask);
}

BOOL CRender::occ_visible(sPoly& P)
{
	return ViewBase.testPolyInside(P);
}

BOOL CRender::occ_visible(vis_data& P)
{
	return occ_visible(P.box);
}

void CRender::Calculate()
{
	// Transfer to global space to avoid deep pointer access
	g_fSCREEN = float(Device.TargetWidth * Device.TargetHeight);
	r_ssaDISCARD = (ssaDISCARD * ssaDISCARD) / g_fSCREEN;
	//	r_ssaLOD_A						=	(ssaLOD_A*ssaLOD_A)/g_fSCREEN;
	//	r_ssaLOD_B						=	(ssaLOD_B*ssaLOD_B)/g_fSCREEN;

	ViewBase.CreateFromMatrix(Device.mFullTransform, FRUSTUM_P_LRTB | FRUSTUM_P_FAR);
}

#include "../../xrEngine/IGame_Persistent.h"

void CRender::Render()
{
}

IRender_DetailModel*	CRender::model_CreateDM(IReader* F)
{
	VERIFY				(F);
	CDetail*	D		= xr_new<CDetail> ();
	D->Load				(F);
	return D;
}

IRenderVisual*	CRender::model_CreatePE(LPCSTR name)
{
	PS::CPEDef*	source		= PSLibrary.FindPED	(name);
	return Models->CreatePE	(source);
}

IRenderVisual*			CRender::model_CreateParticles	(LPCSTR name)
{
	PS::CPEDef*	SE		= PSLibrary.FindPED	(name);
	if (SE) return		Models->CreatePE	(SE);
	else{
		PS::CPGDef*	SG	= PSLibrary.FindPGD	(name);
		return			SG?Models->CreatePG	(SG):0;
	}
}

void	CRender::rmNear		()
{
	CRenderTarget* T	=	(CRenderTarget*)getTarget	();
	D3DVIEWPORT9 VP		=	{0,0,T->get_width(),T->get_height(),0,0.02f };
	CHK_DX				(RDevice->SetViewport(&VP));
}
void	CRender::rmFar		()
{
	CRenderTarget* T	= (CRenderTarget*)getTarget	();
	D3DVIEWPORT9 VP		=	{0,0,T->get_width(),T->get_height(),0.99999f,1.f };
	CHK_DX				(RDevice->SetViewport(&VP));
}
void	CRender::rmNormal	()
{
	CRenderTarget* T	= (CRenderTarget*)getTarget	();
	D3DVIEWPORT9 VP		= {0,0,T->get_width(),T->get_height(),0,1.f };
	CHK_DX				(RDevice->SetViewport(&VP));
}

void 	CRender::set_Transform	(Fmatrix* M)
{
	current_matrix.set(*M);
}

void			CRender::add_Visual   		(IRenderVisual* visual, bool)			{ Models->RenderSingle	(dynamic_cast<dxRender_Visual*>(visual),current_matrix,1.f);}
IRenderVisual*	CRender::model_Create		(LPCSTR name, IReader* data)		{ return Models->Create(name,data);		}
IRenderVisual*	CRender::model_CreateChild	(LPCSTR name, IReader* data)		{ return Models->CreateChild(name,data);}
void 			CRender::model_Delete		(IRenderVisual* &V, BOOL bDiscard)	{ Models->Delete((dxRender_Visual*&)(V),bDiscard);			}
IRenderVisual*	CRender::model_Duplicate	(IRenderVisual* V)					{ return Models->Instance_Duplicate(dynamic_cast<dxRender_Visual*>(V));	}
void 			CRender::model_Render		(IRenderVisual* m_pVisual, const Fmatrix& mTransform, int priority, bool strictB2F, float m_fLOD){Models->Render(dynamic_cast<dxRender_Visual*>(m_pVisual), mTransform, priority, strictB2F, m_fLOD);}
void 			CRender::model_RenderSingle	(IRenderVisual* m_pVisual, const Fmatrix& mTransform, float m_fLOD){Models->RenderSingle(dynamic_cast<dxRender_Visual*>(m_pVisual), mTransform, m_fLOD);}

HRESULT	CRender::shader_compile			(
	LPCSTR							name,
	DWORD const* pSrcData,
	UINT                            SrcDataLen,
	LPCSTR                          pFunctionName,
	LPCSTR                          pTarget,
	DWORD                           Flags,
	void*& result)
{
	D3D_SHADER_MACRO defines			[128];
	int def_it			= 0;

	// options
	if (m_skinning<0)		{
		defines[def_it].Name		=	"SKIN_NONE";
		defines[def_it].Definition	=	"1";
		def_it						++;
	}
	if (0==m_skinning)		{
		defines[def_it].Name		=	"SKIN_0";
		defines[def_it].Definition	=	"1";
		def_it						++;
	}
	if (1==m_skinning)		{
		defines[def_it].Name		=	"SKIN_1";
		defines[def_it].Definition	=	"1";
		def_it						++;
	}
	if (2==m_skinning)		{
		defines[def_it].Name		=	"SKIN_2";
		defines[def_it].Definition	=	"1";
		def_it						++;
	}
	// finish
	defines[def_it].Name			=	0;
	defines[def_it].Definition		=	0;
	def_it							++;

	class includer : public ID3DInclude 
	{
	public:
		HRESULT __stdcall Open(D3D_INCLUDE_TYPE IncludeType, LPCSTR pFileName, LPCVOID pParentData, LPCVOID* ppData, UINT* pBytes) 
		{
			string_path pname;
			xr_strconcat(pname, ::Render->getShaderPath(), pFileName);
			IReader* R = FS.r_open("$game_shaders$", pname);
			if (0 == R) {
				// possibly in shared directory or somewhere else - open directly
				R = FS.r_open("$game_shaders$", pFileName);
				if (0 == R) {
					return E_FAIL;
				}
			}

			// duplicate and zero-terminate
			u32 size = R->length();
			u8* data = xr_alloc<u8>(size + 1);
			CopyMemory(data, R->pointer(), size);
			data[size] = 0;
			FS.r_close(R);

			*ppData = data;
			*pBytes = size;
			return	D3D_OK;
		}

		HRESULT __stdcall Close(LPCVOID pData) {
			xr_free(pData);
			return D3D_OK;
		}
	};
	includer					Includer;
	LPD3DBLOB					pShaderBuf = nullptr;
	LPD3DBLOB					pErrorBuf = nullptr;

	HRESULT _result = D3DCompile(pSrcData, SrcDataLen, "", defines, &Includer, pFunctionName, pTarget, Flags, 0, &pShaderBuf, &pErrorBuf);
	return _result;
}

void					CRender::reset_begin			()
{
	xr_delete			(Target);
}
void					CRender::reset_end				()
{
	Target			=	xr_new<CRenderTarget>			();
}

