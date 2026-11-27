#include "StdAfx.h"
#include "ViewerEditors.h"
#include "ViewerSession.h"

#include "../../Level.h"
#include "../../ParticlesObject.h"
#include "../../GamePersistent.h"

namespace ImUtils::Viewer
{
	namespace ViewerParticleDetail
	{
		class CBoundedReader final : public IReader
		{
		public:
			CBoundedReader(const void* bytes, size_t size) : IReader(const_cast<void*>(bytes), size) {}
			bool Valid = true;

			intptr_t elapsed() const override { return IReader::elapsed(); }
			intptr_t tell() const override { return IReader::tell(); }
			intptr_t length() const override { return IReader::length(); }
			void seek(intptr_t position) override
			{
				if (position < 0 || position > length())
				{
					Valid = false;
					return;
				}
				IReader::seek(position);
			}
			void advance(intptr_t count) override
			{
				if (count < 0 || count > elapsed())
				{
					Valid = false;
					return;
				}
				IReader::advance(count);
			}
			void r(void* destination, intptr_t count) override
			{
				if (count < 0 || count > elapsed())
				{
					Valid = false;
					return;
				}
				if (count != 0)
					Memory.mem_copy(destination, pointer(), count);
				IReader::advance(count);
			}
		};

		bool HasTerminatedString(const void* data, u32 size, u32 offset = 0)
		{
			const u8* bytes = static_cast<const u8*>(data);
			for (u32 index = offset; index < size; ++index)
			{
				if (bytes[index] == 0)
					return true;
			}
			return false;
		}

		IParticleDefinition* LoadDefinition(CViewerSession& session, IReader& source)
		{
			if (session.SceneRenderer() == nullptr || !session.SceneRenderer()->validate_particle_definition(source))
			{
				Msg("! Particle Editor: invalid or truncated effect stream");
				return nullptr;
			}
			CBoundedReader reader(source.pointer(), source.elapsed());
			IParticleDefinition* definition = session.SceneRenderer()->create_particle_definition(reader);
			if (definition == nullptr || !reader.Valid || !reader.eof())
			{
				Msg("! Particle Editor: cannot decode effect definition");
				return nullptr;
			}
			source.advance(reader.tell());
			return definition;
		}

		void ResolveFile(const char* path, string_path& result)
		{
			if (path == nullptr || path[0] == 0)
			{
				result[0] = 0;
				return;
			}
			if (strchr(path, ':') != nullptr || strchr(path, '/') != nullptr || strchr(path, '\\') != nullptr)
			{
				xr_strcpy(result, path);
				return;
			}
			xr_string relative = xr_string("particle_editor\\") + path;
			if (strext(relative.c_str()) == nullptr)
				relative += ".pe";
			FS.update_path(result, _app_data_root_, relative.c_str());
		}
	}

	CViewerParticleEditor::~CViewerParticleEditor() = default;

	void CViewerParticleEditor::Configure(SViewerPreset& preset)
	{
		CViewerEditor::Configure(preset);
		preset.Terrain.Kind = Sandbox::ETerrainKind::Flat;
		preset.Resources.zero();
		preset.Resources.set(u32(ELevelResource::LevelConfig) | u32(ELevelResource::Collision) | u32(ELevelResource::PhysicsWorld) | u32(ELevelResource::SoundEnv), true);
	}

	void CViewerParticleEditor::OnSessionReady(CViewerSession& session)
	{
		CenterCamera(session, Fvector().set(0.f, 3.f, 0.f), 6.f, -0.2f);
		if (History.HasInitialState() && RestoreLatest(session))
			return;
		particles_systems::library_interface* library = session.SceneRenderer() != nullptr ? session.SceneRenderer()->particle_library() : nullptr;
		if (library != nullptr)
		{
			string256 selected = {};
			int bestScore = -1;
			for (const shared_str& name : library->vec_all_particles())
			{
				IParticleDefinition* definition = session.SceneRenderer()->find_particle_definition(name.c_str());
				float sourceRate = 0.f;
				if (definition == nullptr || definition->GetCompiledActionBytes() == 0 || !definition->GetSourceRate(sourceRate) || sourceRate <= 0.f)
					continue;
				const shared_str shader = definition->GetShaderName();
				const shared_str texture = definition->GetTextureName();
				int score = 1;
				if (strstr(name.c_str(), "distort") != nullptr || (shader.size() != 0 && strstr(shader.c_str(), "distort") != nullptr))
					score = 0;
				if (strstr(name.c_str(), "campfire") != nullptr || strstr(name.c_str(), "fire") != nullptr)
					score += 2;
				if (strstr(name.c_str(), "smoke") != nullptr)
					++score;
				if (strstr(name.c_str(), "explosion") != nullptr || strstr(name.c_str(), "explode") != nullptr ||
					(texture.size() != 0 && (strstr(texture.c_str(), "fire") != nullptr || strstr(texture.c_str(), "explotions") != nullptr)))
					score += 8;
				if (definition->GetMaxParticles() >= 16)
					++score;
				if (score > bestScore)
				{
					bestScore = score;
					xr_strcpy(selected, name.c_str());
				}
			}
			if (selected[0] != 0 && LoadEffect(session, selected))
				return;
		}
		Msg("! Particle Editor: no installed effects; load a .pe file under $appdata$/particle_editor");
	}

	void CViewerParticleEditor::TickEditor(CViewerSession& session)
	{
		if (Effect != nullptr && !Effect->IsPlaying())
			Effect->Play(false);
		if (Effect != nullptr && Device.dwTimeGlobal >= NextStatsMs)
		{
			NextStatsMs = Device.dwTimeGlobal + 1000;
			Msg("* Particle Editor: '%s' playing=%d sprites=%u rate=%.2f scale=%.2f", EffectName, Effect->IsPlaying(), Effect->GetSpriteCount(), SourceRate, Scale);
		}
	}

	void CViewerParticleEditor::OnExit(CViewerSession& session)
	{
		StopEffect(session);
		ReleaseDefinition(*EditorSession);
		CViewerEditor::OnExit(session);
	}

	void CViewerParticleEditor::ReleaseDefinition(CViewerSession& session)
	{
		if (Definition != nullptr && session.SceneRenderer() != nullptr)
			session.SceneRenderer()->destroy_particle_definition(Definition);
		Definition = nullptr;
	}

	void CViewerParticleEditor::StopEffect(CViewerSession& session, bool destroy)
	{
		if (Effect == nullptr)
			return;
		Effect->Stop(false);
		if (destroy)
			Effect->Destroy();
		GamePersistent().ps_active.erase(std::remove(GamePersistent().ps_active.begin(), GamePersistent().ps_active.end(), Effect), GamePersistent().ps_active.end());
		GamePersistent().ps_active_deffer.erase(std::remove(GamePersistent().ps_active_deffer.begin(), GamePersistent().ps_active_deffer.end(), Effect), GamePersistent().ps_active_deffer.end());
		GamePersistent().ps_needtoplay.erase(std::remove(GamePersistent().ps_needtoplay.begin(), GamePersistent().ps_needtoplay.end(), Effect), GamePersistent().ps_needtoplay.end());
		if (session.SceneRenderer() != nullptr && Effect->renderable.visual != nullptr)
			session.SceneRenderer()->model_Delete(Effect->renderable.visual);
		Effect.reset();
	}

	void CViewerParticleEditor::Rebuild(CViewerSession& session)
	{
		StopEffect(session);
		if (Definition == nullptr || session.SceneRenderer() == nullptr || session.GetLevel() == nullptr)
			return;
		MaximumParticles = clampr(MaximumParticles, 0, 1000000);
		TimeLimit = clampr(TimeLimit, 0.05f, 3600.f);
		Scale = clampr(Scale, 0.01f, 100.f);
		Definition->SetMaxParticles(MaximumParticles);
		Definition->SetTimeLimit(TimeLimit);
		Definition->DestroyShader();
		Definition->SetSprite(ShaderName, TextureName);
		Definition->CreateShader();

		Effect = xr_make_shared<CParticlesObject>(EffectName[0] != 0 ? EffectName : "viewer_particle", false, false);
		Effect->renderable.visual = session.SceneRenderer()->model_CreateParticleDefinition(Definition);
		Effect->LevelContext = { session.GetLevel(), session.GetLevel()->Services.Spatial, session.GetLevel()->Services.SpatialPhysic };
		IParticleCustom* visual = Effect->renderable.visual != nullptr ? Effect->renderable.visual->dcast_ParticleCustom() : nullptr;
		if (visual == nullptr)
		{
			Msg("! Particle Editor: no visual was compiled for '%s'", EffectName);
			StopEffect(session);
			return;
		}
		visual->SetLevelContext(Effect->LevelContext);
		Effect->m_bLooped = true;
		Effect->m_iLifeTime = 0;
		Fmatrix transform;
		transform.translate(0.f, 1.5f, 0.f);
		Effect->SetXFORM(transform);
		Effect->SetScale(Scale);
		if (SourceRate >= 0.f)
			session.SceneRenderer()->set_particle_source_rate(Effect->renderable.visual, SourceRate);
		SourceRate = session.SceneRenderer()->particle_source_rate(Effect->renderable.visual);
		GamePersistent().ps_active_deffer.push_back(Effect);
		Effect->Play(false);
	}

	bool CViewerParticleEditor::LoadEffect(CViewerSession& session, const char* name)
	{
		if (name == nullptr || name[0] == 0)
			return false;
		IParticleDefinition* source = session.SceneRenderer() != nullptr ? session.SceneRenderer()->find_particle_definition(name) : nullptr;
		if (source == nullptr)
			return LoadFile(session, name);
		CMemoryWriter stream;
		source->Save(stream);
		IReader reader(stream.pointer(), stream.size());
		IParticleDefinition* clone = ViewerParticleDetail::LoadDefinition(session, reader);
		if (clone == nullptr)
			return false;
		ReleaseDefinition(*EditorSession);
		Definition = clone;
		xr_strcpy(EffectName, Definition->Name());
		xr_strcpy(ShaderName, Definition->GetShaderName().c_str());
		xr_strcpy(TextureName, Definition->GetTextureName().c_str());
		MaximumParticles = Definition->GetMaxParticles();
		TimeLimit = Definition->HasTimeLimit() ? Definition->GetTimeLimit() : 4.f;
		SourceRate = -1.f;
		Rebuild(session);
		return CommitHistory("load effect");
	}

	bool CViewerParticleEditor::LoadFile(CViewerSession& session, const char* path)
	{
		string_path resolved;
		ViewerParticleDetail::ResolveFile(path, resolved);
		IReader* reader = FS.r_open(resolved);
		if (reader == nullptr)
		{
			Msg("! Particle Editor: effect '%s' is missing", resolved);
			return false;
		}
		IParticleDefinition* clone = ViewerParticleDetail::LoadDefinition(session, *reader);
		FS.r_close(reader);
		if (clone == nullptr)
			return false;
		ReleaseDefinition(*EditorSession);
		Definition = clone;
		xr_strcpy(EffectName, Definition->Name());
		xr_strcpy(ShaderName, Definition->GetShaderName().c_str());
		xr_strcpy(TextureName, Definition->GetTextureName().c_str());
		MaximumParticles = Definition->GetMaxParticles();
		TimeLimit = Definition->HasTimeLimit() ? Definition->GetTimeLimit() : 4.f;
		SourceRate = -1.f;
		Rebuild(session);
		return CommitHistory("load file");
	}

	bool CViewerParticleEditor::SaveFile(CViewerSession& session, const char* path) const
	{
		if (Definition == nullptr)
			return false;
		string_path resolved;
		ViewerParticleDetail::ResolveFile(path, resolved);
		IWriter* writer = FS.w_open(resolved);
		if (writer == nullptr)
			return false;
		Definition->Save(*writer);
		FS.w_close(writer);
		Msg("* Particle Editor: saved '%s'", resolved);
		return true;
	}

	bool CViewerParticleEditor::EditorRequest(CViewerSession& session, EViewerEditorCommand command, const char* argument, float value)
	{
		switch (command)
		{
		case EViewerEditorCommand::Load:
			return argument != nullptr && 0 == _stricmp(argument, "file") ? LoadFile(session, EffectName) : LoadEffect(session, argument != nullptr ? argument : EffectName);
		case EViewerEditorCommand::Play:
			if (Effect == nullptr)
				Rebuild(session);
			else
				Effect->Play(false);
			return Effect != nullptr;
		case EViewerEditorCommand::Stop:
			StopEffect(session, false);
			return true;
		case EViewerEditorCommand::Save:
			return SaveFile(session, argument != nullptr ? argument : EffectName);
		case EViewerEditorCommand::SetParameter:
			if (argument != nullptr && 0 == _stricmp(argument, "rate") && value >= 0.f)
				SourceRate = value;
			else if (argument != nullptr && 0 == _stricmp(argument, "scale") && value > 0.f)
				Scale = value;
			else if (argument != nullptr && 0 == strncmp(argument, "texture ", 8))
				xr_strcpy(TextureName, argument + 8);
			else if (argument != nullptr && 0 == strncmp(argument, "shader ", 7))
				xr_strcpy(ShaderName, argument + 7);
			Rebuild(session);
			return CommitHistory("edit effect");
		case EViewerEditorCommand::Reset:
			StopEffect(session);
			return true;
		default:
			return false;
		}
	}

	bool CViewerParticleEditor::WriteState(IWriter& state)
	{
		if (Definition == nullptr)
			return false;
		state.w_u32(0x31535056);
		state.w_float(Scale);
		Definition->Save(state);
		return true;
	}

	bool CViewerParticleEditor::ReadState(IReader& state)
	{
		const u32 signature = state.r_u32();
		if (signature == 0x31535056)
		{
			Scale = state.r_float();
			if (!_valid(Scale) || Scale <= 0.f)
				return false;
		}
		else
		{
			state.seek(0);
			Scale = 1.f;
		}
		IParticleDefinition* clone = ViewerParticleDetail::LoadDefinition(*EditorSession, state);
		if (clone == nullptr)
			return false;
		ReleaseDefinition(*EditorSession);
		Definition = clone;
		xr_strcpy(EffectName, Definition->Name());
		xr_strcpy(ShaderName, Definition->GetShaderName().c_str());
		xr_strcpy(TextureName, Definition->GetTextureName().c_str());
		MaximumParticles = Definition->GetMaxParticles();
		TimeLimit = Definition->GetTimeLimit();
		SourceRate = -1.f;
		return true;
	}

	void CViewerParticleEditor::Restored(CViewerSession& session)
	{
		Rebuild(session);
	}

	void CViewerParticleEditor::DrawEditor(CViewerSession& session)
	{
		if (!ImGui::CollapsingHeader("Particle effect", ImGuiTreeNodeFlags_DefaultOpen))
			return;
		ImGui::InputText("Effect name", EffectName, sizeof(EffectName));
		if (ImGui::Button("Load installed effect"))
			session.RequestEditorCommand(EViewerEditorCommand::Load, EffectName);
		ImGui::SameLine();
		if (ImGui::Button("Open .pe file"))
			session.RequestEditorCommand(EViewerEditorCommand::Load, "file");
		ImGui::InputText("Shader", ShaderName, sizeof(ShaderName));
		ImGui::InputText("Texture", TextureName, sizeof(TextureName));
		ImGui::InputInt("Maximum particles", &MaximumParticles);
		ImGui::DragFloat("Time limit (seconds)", &TimeLimit, 0.05f, 0.05f, 3600.f);
		ImGui::DragFloat("Scale", &Scale, 0.01f, 0.01f, 100.f);
		ImGui::DragFloat("Source rate", &SourceRate, 0.1f, 0.f, 100000.f);
		if (ImGui::Button("Apply and restart"))
			session.RequestEditorCommand(EViewerEditorCommand::SetParameter, "apply");
		ImGui::SameLine();
		const bool play = ImGui::Button("Play");
		session.UiProbeObserve("Play", play);
		if (play)
			session.RequestEditorCommand(EViewerEditorCommand::Play);
		ImGui::SameLine();
		if (ImGui::Button("Stop"))
			session.RequestEditorCommand(EViewerEditorCommand::Stop);
		if (ImGui::Button("Save .pe under $appdata$"))
			session.RequestEditorCommand(EViewerEditorCommand::Save, EffectName);
		ImGui::SameLine();
		if (ImGui::Button("Exercise history"))
			session.RequestEditorCommand(EViewerEditorCommand::TestHistory);
		ImGui::Text("Playing: %s; sprites: %u", Effect != nullptr && Effect->IsPlaying() ? "yes" : "no", Effect != nullptr ? Effect->GetSpriteCount() : 0);
	}
}
