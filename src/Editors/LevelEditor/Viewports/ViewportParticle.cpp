#include "stdafx.h"
#include "ViewportParticle.h"

static void ViewportFocusCallback()
{
	LUI->EndEState(esEditLibrary);
	LUI->EndEState(esEditScene);
	LUI->BeginEState(esEditCustom);
}

CViewportParticle::CViewportParticle()
{
	View.OnFocusCallback = (xr_delegate<void()>)ViewportFocusCallback;
	ViewName = "Empty mesh";
}

CViewportParticle::~CViewportParticle()
{
}

void CViewportParticle::Draw()
{
	if (!ImGui::Begin(*ViewName, &bOpen))
	{
		ImGui::End();
		return;
	}

	if (ImGui::IsWindowFocused())
	{
		UI->ViewID = View.ViewportID;
	}

	View.DrawVP();

	ImGui::End();
}

void CViewportParticle::Render()
{
	if (UI->ViewID != View.ViewportID)
	{
		return;
	}

	// Particle simulation и draw будут выполнены renderer-owned preview pass.
	// Здесь не создаются legacy CModelPool или dynamic vertex buffers.
}

void CViewportParticle::RenderTiramisu()
{
	if (UI->ViewID != View.ViewportID || ParticleAssetName.empty())
	{
		return;
	}

	FEditorViewportCamera Camera;
	std::copy_n(EDevice->mView.mm, Camera.View.size(), Camera.View.begin());
	std::copy_n(
		EDevice->mProject.mm,
		Camera.Projection.size(),
		Camera.Projection.begin()
	);
	std::copy_n(
		EDevice->mFullTransform.mm,
		Camera.ViewProjection.size(),
		Camera.ViewProjection.begin()
	);
	const Fvector& Position = UI->CurrentView().m_Camera.GetPosition();
	Camera.WorldPosition = {Position.x, Position.y, Position.z};
	Camera.NearPlane = UI->CurrentView().m_Camera._Znear();
	Camera.FarPlane = UI->CurrentView().m_Camera._Zfar();

	FEditorParticleInstance Particle;
	Particle.ObjectId = {
		static_cast<u64>(reinterpret_cast<std::uintptr_t>(this))
	};
	if (!Particle.ObjectId.IsValid())
	{
		Particle.ObjectId.Value = 1;
	}
	Particle.AssetName = ParticleAssetName;
	Particle.AssetType = ParticleAssetType;
	Particle.Flags = EEditorParticleInstanceFlags::Playing;

	const u64 NameHash = std::hash<xr_string_view>{}(
		ParticleAssetName
	);
	FEditorViewportSceneSnapshot Snapshot;
	Snapshot.Camera = Camera;
	Snapshot.ParticleInstances = xr_span(&Particle, 1);
	Snapshot.DebugDrawRevision = NameHash == 0 ? 1 : NameHash;
	Snapshot.Revision = ++SceneRevision;
	(void)GetEditorRenderBackend().SubmitViewportScene(
		static_cast<u32>(View.ViewportID), Snapshot
	);
}

void CViewportParticle::OpenModel(
	const xr_string_view AssetName,
	const EEditorParticleAssetType AssetType
)
{
	ParticleAssetName = AssetName;
	ParticleAssetType = AssetType;
	ViewName = ParticleAssetName.c_str();
}
