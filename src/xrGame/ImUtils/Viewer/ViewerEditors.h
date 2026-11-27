#pragma once

#include "../../GameObject.h"
#include "../../ParticlesObject.h"
#include "../../../Include/xrRender/ParticleDefinition.h"

#include "ViewerModes.h"

class CGameObject;
class CParticlesObject;
class CSE_ALifeDynamicObjectVisual;
class CLevel;

namespace ImUtils::Viewer
{
	class CViewerPreviewObject : public CGameObject
	{
	public:
		bool Spawn(CSE_ALifeDynamicObjectVisual& description, CLevel& level);
		void Release();
		bool IsRegistered() const { return Registered; }
		bool ScriptCallbacksEnabled() const override { return false; }
		bool UsedAI_Locations() override { return false; }
		bool register_schedule() const override { return false; }

	protected:
		void spawn_supplies() override {}

	private:
		bool Spawned = false;
		bool Registered = false;
	};

	class CViewerOgfEditor : public CViewerEditor
	{
	public:
		const char* Name() const override { return "OGF Viewer"; }
		void Configure(SViewerPreset& preset) override;
		void OnSessionReady(CViewerSession& session) override;
		void OnExit(CViewerSession& session) override;
		void TickEditor(CViewerSession& session) override;
		bool EditorRequest(CViewerSession& session, EViewerEditorCommand command, const char* argument, float value) override;
		const char* HistoryFolder() const override { return "ogf_viewer"; }
		bool HasPersistentState() const override { return true; }
		bool WriteState(IWriter& state) override;
		bool ReadState(IReader& state) override;
		void Restored(CViewerSession& session) override;
		void PrepareHistoryTest(CViewerSession& session, u32 index) override { RotationYaw = float(index); }
		bool VerifyHistoryTestState(CViewerSession& session, u32 index) override;
		bool LoadPreview(CViewerSession& session, const char* path);
		float RotationYawAngle() const { return RotationYaw; }
		float RotationPitchAngle() const { return RotationPitch; }
		void ReleasePreview(CViewerSession& session);
		CGameObject* PreviewObject(CViewerSession& session) const;

	protected:
		void DrawEditor(CViewerSession& session) override;
		char PreviewPath[256] = "dynamics\\weapons\\wpn_ak74\\wpn_ak74.ogf";
		float RotationYaw = -PI_DIV_2;
		float RotationPitch = 0.f;
		ALife::_OBJECT_ID PreviewId = ALife::INVALID_OBJECT_ID;
		CViewerPreviewObject* Preview = nullptr;
		char OgfShaderName[128] = {};
		char OgfTextureName[256] = {};

	private:
		bool RotationChanged = false;
		bool ReplacePreview(CViewerSession& session, const char* path, bool duplicateVisual);
	};

	class CViewerMaterialEditor : public CViewerOgfEditor
	{
	public:
		const char* Name() const override { return "Material Editor"; }
		void Configure(SViewerPreset& preset) override;
		void OnSessionReady(CViewerSession& session) override;
		bool EditorRequest(CViewerSession& session, EViewerEditorCommand command, const char* argument, float value) override;
		const char* HistoryFolder() const override { return "material_editor"; }
		bool WriteState(IWriter& state) override;
		bool ReadState(IReader& state) override;
		void Restored(CViewerSession& session) override;

	protected:
		void DrawEditor(CViewerSession& session) override;

	private:
		bool ApplyMaterial(CViewerSession& session);
		char ShaderPath[128] = {};
		char TexturePath[256] = "act\\act_arm_1";
	};

	class CViewerParticleEditor : public CViewerEditor
	{
	public:
		~CViewerParticleEditor() override;
		const char* Name() const override { return "Particle Editor"; }
		void Configure(SViewerPreset& preset) override;
		void OnSessionReady(CViewerSession& session) override;
		void OnExit(CViewerSession& session) override;
		void TickEditor(CViewerSession& session) override;
		bool EditorRequest(CViewerSession& session, EViewerEditorCommand command, const char* argument, float value) override;
		const char* HistoryFolder() const override { return "particle_editor"; }
		bool HasPersistentState() const override { return true; }
		bool WriteState(IWriter& state) override;
		bool ReadState(IReader& state) override;
		void Restored(CViewerSession& session) override;
		void PrepareHistoryTest(CViewerSession& session, u32 index) override
		{
			MaximumParticles = 128 + int(index);
			Scale = 1.f + float(index) * 0.01f;
			SourceRate = 1.f + float(index);
			Rebuild(session);
		}
		bool VerifyHistoryTestState(CViewerSession& session, u32 index) override
		{
			float actualRate = -1.f;
			return MaximumParticles == 128 + int(index) && fsimilar(Scale, 1.f + float(index) * 0.01f) &&
				Definition != nullptr && Definition->GetSourceRate(actualRate) && fsimilar(actualRate, 1.f + float(index)) &&
				Effect != nullptr && Effect->renderable.visual != nullptr && Effect->IsPlaying();
		}
		bool LoadEffect(CViewerSession& session, const char* name);
		void StopEffect(CViewerSession& session, bool destroy = true);
		bool PlayingForTest() const { return Effect != nullptr && Effect->IsPlaying(); }
		float SourceRateForTest() const { return SourceRate; }

	protected:
		void DrawEditor(CViewerSession& session) override;

	private:
		void Rebuild(CViewerSession& session);
		void ReleaseDefinition(CViewerSession& session);
		bool LoadFile(CViewerSession& session, const char* path);
		bool SaveFile(CViewerSession& session, const char* path) const;
		char EffectName[256] = {};
		IParticleDefinition* Definition = nullptr;
		xr_shared_ptr<CParticlesObject> Effect;
		char ShaderName[128] = {};
		char TextureName[256] = {};
		float TimeLimit = 4.f;
		int MaximumParticles = 256;
		float Scale = 2.f;
		float SourceRate = -1.f;
		u32 NextStatsMs = 0;
	};

	class CViewerAiEditor : public CViewerEditor
	{
	public:
		const char* Name() const override { return "AI Editor"; }
		void Configure(SViewerPreset& preset) override;
		void OnSessionReady(CViewerSession& session) override;
		void TickEditor(CViewerSession& session) override;
		bool EditorRequest(CViewerSession& session, EViewerEditorCommand command, const char* argument, float value) override;
		bool NavigationProbeCompletedForTest() const { return Probe.Completed; }
		bool NavigationProbePassedForTest() const { return Probe.Passed; }

	protected:
		void DrawEditor(CViewerSession& session) override;

	private:
		struct SAiMovementProbe
		{
			ALife::_OBJECT_ID ObjectId = ALife::INVALID_OBJECT_ID;
			Fvector StartPosition = {};
			u32 Destination = 0;
			u32 PendingDestination = 0;
			u32 PendingTicks = 0;
			u32 RemainingTicks = 0;
			u32 ClosestVertex = u32(-1);
			float Distance = 0.f;
			bool Completed = false;
			bool Passed = false;
		};

		ALife::_OBJECT_ID TestActorId = ALife::INVALID_OBJECT_ID;
		SAiMovementProbe Probe;
		u32 Destination = 0;
		bool Wander = false;
	};

	xr_unique_ptr<IViewerMode> CreateViewerEditor(EViewerEditorKind kind);
}
