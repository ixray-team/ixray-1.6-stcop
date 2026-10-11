#pragma once

#include "RenderAPI.h"
#include "Shader.h"

namespace PS
{
	class CPACDef;
}

struct SPBItem;
class IBlender;
class CDetailManager;

struct EditorLightDescriptor
{
    u32 Type = 0;
    Fvector Position = {};
    Fvector Direction = {};
    Fcolor Color = {};
    float Range = 0.f;
    float Cone = 0.f;
    float Attenuation0 = 1.f;
    float Attenuation1 = 0.f;
    float Attenuation2 = 0.f;
    // ELight::flAffectDynamic
    bool CastShadow = false;
};

// Editor services the editor renderer calls back into. xrECore implements it and
// registers the instance before the render device is created.
class IEditorRenderHost
{
public:
	virtual ~IEditorRenderHost() = default;

	// UI
	virtual bool IsUIReady() = 0;
	virtual bool IsPlayInEditor() = 0;
	virtual SPBItem* ProgressStart(float MaxValue, const char* Text) = 0;
	virtual void ProgressInc(SPBItem* Item) = 0;
	virtual void ProgressEnd(SPBItem*& Item) = 0;
	virtual bool Confirm(const char* Text) = 0;
	virtual void UpdateProperties() = 0;

	// Tools
	virtual bool RayPick(const Fvector& Start, const Fvector& Dir, float& Dist, Fvector* Point, Fvector* Normal) = 0;
	virtual void PlayCurrent(int Index) = 0;
	virtual void StopCurrent(bool FinishPlaying) = 0;
	virtual void SelectEffect(const char* Name) = 0;
	virtual void RemoveAction(u32 Index) = 0;
	virtual void EditPAC(PS::CPACDef* PAC) = 0;

	virtual void DrawAxis(const Fmatrix& Transform) = 0;

	virtual void SubmitScene() = 0;
	virtual void SubmitShadowCasters()
	{
		SubmitScene();
	}

	virtual IBlender* CreateBlender(CLASS_ID Class) = 0;

	virtual void DrawSceneHelpers()
	{
	}

	virtual CDetailManager* GetSceneDetails()
	{
		return nullptr;
	}

	virtual u32 CollectEditorLights(EditorLightDescriptor* Out, u32 Capacity)
	{
		(void)Out;
		(void)Capacity;
		return 0;
	}
};

extern RENDER_API IEditorRenderHost* g_EditorRenderHost;
