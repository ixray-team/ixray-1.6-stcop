#pragma once

#include "../../../xrEngine/device.h"
#include "UI_Camera.h"
#include "../../../Layers/xrRender/HWCaps.h"
#include "../../../Layers/xrRender/HW.h"
#include "../../../xrEngine/pure.h"
#include "../../../xrCore/FTimer.h"
#include "EStats.h"
#include "../../../xrEngine/Shader_xrLC.h"
#include "../../../Layers/xrRender/Shader.h"
#include "../../../Layers/xrRender/R_Backend.h"

//---------------------------------------------------------------------------
// refs
class CGameFont;
class CInifile;
class CResourceManager;
#undef CreateWindow
//------------------------------------------------------------------------------
class ECORE_API CEditorRenderDevice;
extern ECORE_API CEditorRenderDevice* EDevice;

#define REContext ((ID3D11DeviceContext*)GRHI->GetContext())
#define REDevice ((ID3D11Device*)Device.GetRenderDevice())

class ECORE_API CEditorRenderDevice :
	public CRenderDevice
{
	friend class CUI_Camera;
	friend class TUI;

private:
	float m_fNearer;

	ref_shader m_CurrentShader;
	ref_texture SearchIcon;

	void _SetupStates();
	void _Create(IReader* F);
	void _Destroy(bool bKeepTextures);

public:
	ref_shader m_WireShader;
	ref_shader m_SelectionShader;
	ref_shader ShaderTL;

	ref_texture texture_null;
	Fmaterial m_DefaultMat;

	float RenderRadius;
	float m_ScreenQuality;

	u32 dwFillMode;
	u32 dwShadeMode;

	RECT NormalWinSize;
	bool NormalWinSizeSaved = false;
	bool isZoomed = false;

public:
	CEditorRenderDevice();
	virtual ~CEditorRenderDevice();


	virtual bool Paused() const { return false; };
	void time_factor(float);
	bool Create();
	void Destroy();
	void Resize(int w, int h, bool maximized);
	void ReloadTextures();
	void UnloadTextures();

	void RenderNearer(float f_Near);
	void ResetNearer();
	bool Begin();
	void End();

	void Initialize(void);
	void ShutDown(void);
	void Reset(IReader* F, bool bKeepTextures);

	void MaximizedWindow();
	void ResoreWindow(bool moving);
	void InitWindowStyle();

	virtual void DumpResourcesMemoryUsage()
	{
	}

	// Sprite rendering
	IC float _x2real(float x)
	{
		return (x + 1) * TargetWidth * 0.5f;
	}

	IC float _y2real(float y)
	{
		return (y + 1) * TargetHeight * 0.5f;
	}

	// draw
	void SetShader(ref_shader sh);
	ref_shader GetShader() { return m_CurrentShader; }
	void DP(ERHI_PRIMITIVE_TOPOLOGY pt, ref_geom geom, u32 startV, u32 pc);
	void DIP(ERHI_PRIMITIVE_TOPOLOGY pt, ref_geom geom, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC);

	IC void SetRS(D3DRENDERSTATETYPE p1, u32 p2)
	{
		VERIFY(b_is_Ready);

		switch (p1)
		{
			case D3DRS_TEXTUREFACTOR:
				RCache.hemi.set_tfactor(p2);
			break;
			default:
				GRHI->StateManager->SetRenderState(p1, p2);
			break;
		}
	}

	IC void SetSS(u32 sampler, D3DSAMPLERSTATETYPE type, u32 value)
	{
		VERIFY(b_is_Ready);
//		VERIFY2(0, "Implement");
	}

	// light&material
	IC void LightEnable(u32 dwLightIndex, bool bEnable)
	{
//		VERIFY2(0, "Implement");
	}

	IC void SetLight(u32 dwLightIndex, Flight& lpLight)
	{
//		VERIFY2(0, "Implement");
	}

	IC void SetMaterial(Fmaterial& mat)
	{
//		VERIFY2(0, "Implement");
	}

	IC void ResetMaterial()
	{
//		VERIFY2(0, "Implement");
	}

	// update
	void UpdateView();
	void FrameMove();

	bool MakeScreenshot(U32Vec& pixels, u32 width, u32 height);


	void InitTimer();

	// Mode control
	virtual void Pause(bool bOn, bool bTimer, bool bSound, const char* reason) override
	{
	}

	virtual void PreCache(u32 amount, bool b_draw_loadscreen, bool b_wait_user_input) override
	{
	}

	virtual void Clear();

public:
	Shader_xrLC_LIB ShaderXRLC;

	// camera
	CRegistrator<pureDrawUI> seqDrawUI;

	// Dependent classes
	CResourceManager* Resources;

private:
	virtual void _BCL AddSeqFrame(pureFrame* f, bool mt);
	virtual void _BCL RemoveSeqFrame(pureFrame* f);

public:
	HWND GetHWND() const;
	void CreateWindow();
	void DestryWindow();
	virtual void Reset(bool precache);
	virtual bool IsEditorMode() override { return true; }
};

// video
enum
{
	rsFilterLinear = (1ul << 20ul),
	rsEdgedFaces = (1ul << 21ul),
	rsRenderTextures = (1ul << 22ul),
	rsFog = (1ul << 24ul),
	rsRenderRealTime = (1ul << 25ul),
	rsDrawGrid = (1ul << 26ul),
	rsDrawSafeRect = (1ul << 27ul),
	rsMuteSounds = (1ul << 28ul),
	rsEnvironment = (1ul << 29ul),
	rsDrawAxis = (1ul << 30ul),
	rsDisableAxisCube = (1ul << 31ul),
};

#define DEFAULT_CLEARCOLOR 0x00555555

#define REQ_CREATE()	if (!EDevice->bReady)	return;
#define REQ_DESTROY()	if (EDevice->bReady)	return;

#include "../../../Layers/xrRender/R_Backend_Runtime.h"
