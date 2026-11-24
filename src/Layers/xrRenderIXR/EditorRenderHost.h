/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
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
    // What a spot projects, none for the default
    shared_str Texture;
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

	// Called once the view frustum of the frame is built, before the scene passes
	virtual void PrepareScene() {}
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
