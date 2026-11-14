#include "stdafx.h"
#include "ViewportParticle.h"

static void ViewportActivate()
{
	LUI->BeginEState(esEditCustom);
}

static void ViewportDeactivate()
{
	LUI->EndEState(esEditCustom);
}

CViewportParticle::CViewportParticle()
{
	View.GetViewport().OnActivate = (xr_delegate<void()>)ViewportActivate;
	View.GetViewport().OnDeactivate = (xr_delegate<void()>)ViewportDeactivate;
	ViewName = "Empty mesh";
}

CViewportParticle::~CViewportParticle()
{
	if (ParticleGroupView != nullptr)
	{
		dxRender_Visual* Vis = ParticleGroupView;
		((CRender*)::Render)->Models->Delete(Vis);
	}

	if (ParticleEffectView != nullptr)
	{
		dxRender_Visual* Vis = ParticleEffectView;
		((CRender*)::Render)->Models->Delete(Vis);
	}
}

void CViewportParticle::Draw()
{
	if (!ImGui::Begin(*ViewName, &bOpen))
	{
		ImGui::End();
		return;
	}

	View.DrawVP();

	ImGui::End();
}

void CViewportParticle::Render()
{
	if (!View.IsActive())
	{
		return;
	}

	if (ParticleGroupView != nullptr)
	{
		ParticleGroupView->OnFrame(Device.dwTimeDelta);
		//ParticleGroupView->Render(0);
		((CRender*)::Render)->Models->RenderSingle(ParticleGroupView, Fidentity, 1.f);
	}

	if (ParticleEffectView != nullptr)
	{
		ParticleEffectView->OnFrame(Device.dwTimeDelta);
		//ParticleEffectView->Render(0);
		((CRender*)::Render)->Models->RenderSingle(ParticleEffectView, Fidentity, 1.f);
	}
}

void CViewportParticle::OpenModel(PS::CPGDef* Part)
{
	ParticleGroupView = (PS::CParticleGroup*)((CRender*)::Render)->Models->CreatePG(0);
	ParticleGroupView->Compile(Part);
	ParticleGroupView->Play();
	ViewName = Part->m_Name;

	dxRender_Visual* Vis = ParticleEffectView;
	((CRender*)::Render)->Models->Delete(Vis);
	ParticleEffectView = nullptr;
}

void CViewportParticle::OpenModel(PS::CPEDef* Part)
{
	dxRender_Visual* Vis = ParticleGroupView;
	((CRender*)::Render)->Models->Delete(Vis);
	ParticleGroupView = nullptr;
	ViewName = Part->m_Name;

	ParticleEffectView = (PS::CParticleEffect*)((CRender*)::Render)->Models->CreatePE(0);
	ParticleEffectView->Compile(Part);
	ParticleEffectView->Play();
}