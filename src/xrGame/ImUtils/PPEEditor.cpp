
#include "StdAfx.h"
#include "../Level.h"
#include "../Actor.h"
#include "../alife_simulator.h"
#include "../alife_object_registry.h"

#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/string_table.h"

#include "ai_space.h"

#include "ImUtils.h"
#include "../PostprocessAnimator.h"
#include <fstream>

// ==============================================================
// PPE editor.
// Edits binary .ppe postprocess files (same files the game plays
// through CPostprocessAnimator, see gamedata/configs/misc/
// postprocess.ltx which references them via pp_eff_name).
// Editor tab: load/edit/save a file (works in main menu).
// Game tab: load/edit/save + play/stop the effect on the actor.
// ==============================================================

constexpr u8 _kPPEColorParamsCount = 3;
constexpr u8 _kPPEValueParamsCount = 8;

enum class _ePPEParamKind : u8
{
	kColor,
	kValue
};

struct _SPPEParamMeta
{
	const char* name;
	_ePPEParamKind kind;
	u8 index; // index into colors[] / values[] of SPPEffectData
};

// serialization order of the .ppe format (see CPostprocessAnimator::Load)
constexpr _SPPEParamMeta _kPPEParamMetas[POSTPROCESS_PARAMS_COUNT] = {
	{"Base color", _ePPEParamKind::kColor, 0},
	{"Add color", _ePPEParamKind::kColor, 1},
	{"Gray color", _ePPEParamKind::kColor, 2},
	{"Gray value", _ePPEParamKind::kValue, 0},
	{"Blur", _ePPEParamKind::kValue, 1},
	{"Duality H", _ePPEParamKind::kValue, 2},
	{"Duality V", _ePPEParamKind::kValue, 3},
	{"Noise intensity", _ePPEParamKind::kValue, 4},
	{"Noise grain", _ePPEParamKind::kValue, 5},
	{"Noise FPS", _ePPEParamKind::kValue, 6},
	{"CM influence", _ePPEParamKind::kValue, 7},
};

// editor's own model of a .ppe file.
// CEnvelope is reused directly so serialization is byte-exact
// (CEnvelope::Save / CEnvelope::Load_2).
// WARNING: never copy it (CEnvelope owns raw st_Key pointers).
struct SPPEffectData
{
	struct SColorParam
	{
		float base = 0.0f;
		CEnvelope r, g, b;
	};

	struct SValueParam
	{
		CEnvelope v;
	};

	SColorParam colors[_kPPEColorParamsCount];   // base, add, gray
	SValueParam values[_kPPEValueParamsCount];   // gray, blur, dual_h, dual_v, noise_i/g/f, cm_influence
	xr_stack_string<sizeof(string_path)> cm_tex1;

	void Reset()
	{
		for (SColorParam& color : colors)
		{
			color.base = 0.0f;
			color.r.ClearAndFree();
			color.g.ClearAndFree();
			color.b.ClearAndFree();
		}

		for (SValueParam& value : values)
		{
			value.v.ClearAndFree();
		}

		cm_tex1.clear();

		// defaults from SPPInfo::SPPInfo()
		colors[0].base = 0.5f;   // base color
		colors[1].base = 0.0f;   // add color
		colors[2].base = 0.333f; // gray color
	}

	float GetLength()
	{
		float mn = 0.0f, mx = 0.0f;
		float result = 0.0f;

		auto length_of = [&mn, &mx, &result](CEnvelope& envelope)
		{
			if (envelope.keys.empty() == false)
			{
				result = std::max(result, envelope.GetLength(&mn, &mx));
			}
		};

		for (SColorParam& color : colors)
		{
			length_of(color.r);
			length_of(color.g);
			length_of(color.b);
		}

		for (SValueParam& value : values)
		{
			length_of(value.v);
		}

		return result;
	}

	u32 GetKeysCount() const
	{
		u32 result = 0;

		for (const SColorParam& color : colors)
		{
			result += (u32)color.r.keys.size() + (u32)color.g.keys.size() + (u32)color.b.keys.size();
		}

		for (const SValueParam& value : values)
		{
			result += (u32)value.v.keys.size();
		}

		return result;
	}
};

// shared UI state (editor tab and game tab each carry one)
struct SPPEditorUIState
{
	bool is_file_loaded{};
	int current_selected_param{};
	int current_selected_file{}; // index into combo_files, 0 = "<not selected>"
	// add-key input temps are per (param, channel), so R/G/B don't share them
	float add_key_time[POSTPROCESS_PARAMS_COUNT][3]{};
	float add_key_value[POSTPROCESS_PARAMS_COUNT][3]{};
	u32 file_version{};
	xr_stack_string<sizeof(string_path) * 2> path;       // absolute path when loaded from disk
	xr_stack_string<64> selected_file;                   // vfs name when loaded from $game_anims$

	// vfs file list ($game_anims$); storage owns the strings, combo_files points into it
	xr_vector<xr_string> combo_files_storage;
	xr_vector<const char*> combo_files;

	void Reset()
	{
		current_selected_param = 0;

		for (int param = 0; param < POSTPROCESS_PARAMS_COUNT; ++param)
		{
			for (int channel = 0; channel < 3; ++channel)
			{
				add_key_time[param][channel] = 0.0f;
				add_key_value[param][channel] = 0.0f;
			}
		}
	}
};

struct CImGuiPPEEditor : SPPEditorUIState
{
	SPPEffectData data;
};

CImGuiPPEEditor* g_pPPEEditor = nullptr;

#if IXRAY_PPE_EDITOR_TAB_GAME == 1
struct CImGuiPPEGameState : SPPEditorUIState
{
	SPPEffectData data;
	bool is_cyclic_playback{true};
};

CImGuiPPEGameState* g_pPPEGame = nullptr;
#endif

// preview effector id: AddPPEffector replaces an effector with the
// same id, so replaying overwrites the previous preview automatically
#if IXRAY_PPE_EDITOR_TAB_GAME == 1
constexpr u32 _kPPEEditorPreviewEffectorID = effCustomEffectorStartID + 9999;
#endif

constexpr const char* _kPPEEditorModalWindow_TextureSelector = "Select texture##ToolsInGameImGui_PPEEditor_TextureSelector";

// state of the texture selector modal window (shared by both tabs;
// only one modal can be open at a time).
// Loading is done on the worker thread through the request system,
// flags sync the render thread with it (data first, flag last).
struct SPPETextureBrowser
{
	bool is_open{};
	SPPEffectData* p_target_data{};

	// texture list, filled on the worker thread (kLoadFile request)
	bool is_load_started{};
	bool is_loaded{};
	xr_vector<xr_string> names_storage;
	xr_vector<const char*> names;

	// filter (render thread only)
	xr_stack_string<256> filter;
	xr_vector<int> filtered_indices;
	bool is_filter_dirty{true};

	int current_selected{-1}; // index into names, -1 = none

	// hover preview, loaded on the worker thread (kLoadTexturePreview request)
	bool is_preview_load_started{};
	bool is_preview_loaded{};
	bool is_preview_release_sent{};
	xr_stack_string<256> preview_name; // name the preview was loaded for (written by worker)
	IRHISurface* p_preview{};
	IRHIShaderResourceView* p_preview_srv{};
};

SPPETextureBrowser g_ppe_tex_browser;

void PPEEditor_OnPressed(int key)
{
	switch (key)
	{
		case SDL_Scancode::SDL_SCANCODE_ESCAPE:
		{
			if (Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_PostProcessEffectorEditor)])
			{
				if (g_pPPEEditor)
				{
					SRequestData req;
					req.editor_type = (u32)eImGuiEditorType::kPPEEditor;
					req.request_type = (u32)eRequestType_PPEditor::kDeselectCurrentSelectedOrHideWindow;

					AllEditors_SendRequest(req);
				}
			}
			break;
		}
	}
}

void PPEEditor_OnReleased(int key)
{
}

// ==============================================================
// Serialization (.ppe binary, version 0x0002)
// ==============================================================

// core parser; works on any IReader (memory, vfs)
bool PPEEditor_LoadPPE_Reader(SPPEffectData& data, u32& file_version, IReader& reader)
{
	file_version = reader.r_u32();

	if (file_version < 0x0001 || file_version > POSTPROCESS_FILE_VERSION)
	{
		return false;
	}

	for (u8 i = 0; i < _kPPEColorParamsCount; ++i)
	{
		data.colors[i].base = reader.r_float();
		data.colors[i].r.Load_2(reader);
		data.colors[i].g.Load_2(reader);
		data.colors[i].b.Load_2(reader);
	}

	// params 3..9 (gray value .. noise fps)
	for (u8 i = 0; i < _kPPEValueParamsCount - 1; ++i)
	{
		data.values[i].v.Load_2(reader);
	}

	if (file_version >= 0x0002)
	{
		data.values[_kPPEValueParamsCount - 1].v.Load_2(reader); // cm influence

		data.cm_tex1.clear();

		char symbol = 0;

		while (reader.elapsed() > 0)
		{
			reader.r(&symbol, sizeof(symbol));

			if (symbol == 0)
			{
				break;
			}

			data.cm_tex1 += symbol;
		}
	}

	return true;
}

// loads an absolute path from disk (file dialog):
// note: FS.r_open can NOT be used here, it only searches the virtual
// file system cache and absolute paths are not part of it
bool PPEEditor_LoadPPEFromFile(SPPEffectData& data, u32& file_version, const char* path)
{
	R_ASSERT(path);

	std::ifstream file(path, std::ios::binary | std::ios::ate);

	if (file.is_open() == false)
	{
		return false;
	}

	std::streamsize size = file.tellg();
	file.seekg(0, std::ios::beg);

	xr_vector<char> buffer(size);

	if (size > 0)
	{
		file.read(buffer.data(), size);
	}

	file.close();

	IReader reader(buffer.data(), (int)size);

	return PPEEditor_LoadPPE_Reader(data, file_version, reader);
}

// loads a file known to the virtual file system ($game_anims$),
// works for packed archives and unpacked folders
bool PPEEditor_LoadPPEFromVFS(SPPEffectData& data, u32& file_version, const char* vfs_name)
{
	R_ASSERT(vfs_name);

	IReader* pReader = FS.r_open("$game_anims$", vfs_name);

	if (pReader == nullptr)
	{
		return false;
	}

	bool status = PPEEditor_LoadPPE_Reader(data, file_version, *pReader);

	FS.r_close(pReader);

	return status;
}

// core serializer; works on any IWriter (file, memory)
void PPEEditor_SavePPE_Writer(SPPEffectData& data, IWriter& writer)
{
	writer.w_u32((u32)POSTPROCESS_FILE_VERSION);

	for (u8 i = 0; i < _kPPEColorParamsCount; ++i)
	{
		writer.w_float(data.colors[i].base);
		data.colors[i].r.Save(writer);
		data.colors[i].g.Save(writer);
		data.colors[i].b.Save(writer);
	}

	for (u8 i = 0; i < _kPPEValueParamsCount; ++i)
	{
		data.values[i].v.Save(writer);
	}

	writer.w_stringZ(data.cm_tex1.c_str());
}

bool PPEEditor_SavePPE(SPPEffectData& data, const char* path)
{
	R_ASSERT(path);

	IWriter* pWriter = FS.w_open(path);

	if (pWriter == nullptr)
	{
		return false;
	}

	PPEEditor_SavePPE_Writer(data, *pWriter);

	FS.w_close(pWriter);

	return true;
}

// envelopes must be sorted by time for Evaluate/saving; user edits
// can break the order, so re-sort before use (save/play)
void PPEEditor_SortKeys(SPPEffectData& data)
{
	auto sort_envelope = [](CEnvelope& envelope)
	{
		std::stable_sort(
			envelope.keys.begin(),
			envelope.keys.end(),
			[](const st_Key* left, const st_Key* right)
			{
				return left->time < right->time;
			}
		);
	};

	for (SPPEffectData::SColorParam& color : data.colors)
	{
		sort_envelope(color.r);
		sort_envelope(color.g);
		sort_envelope(color.b);
	}

	for (SPPEffectData::SValueParam& value : data.values)
	{
		sort_envelope(value.v);
	}
}

// ==============================================================
// File operations (dialogs + vfs)
// ==============================================================

void PPEEditor_LoadFileInteractive(SPPEditorUIState& state, SPPEffectData& data)
{
	if (xr_EFS == nullptr)
	{
		return;
	}

	xr_stack_tstring<sizeof(string_path)> local_path;

	if (xr_EFS->GetOpenName(local_path, XR_TEXT("PPE file\0*.ppe\0")) == false)
	{
		return;
	}

#ifdef IXR_WINDOWS
	xr_stack_string<sizeof(string_path) * 2> char_path;
	bool status = Platform::WCHAR_TO_CHAR(local_path, char_path);
	R_ASSERT2(status, "report to developers! Unable to convert your path to multibyte string");

	u32 file_version = 0;

	if (PPEEditor_LoadPPEFromFile(data, file_version, char_path.c_str()))
	{
		state.path = char_path;
		state.file_version = file_version;
		state.is_file_loaded = true;
		state.selected_file.clear();
		state.current_selected_file = 0;
		state.Reset();
	}
	else
	{
		data.Reset();
		state.is_file_loaded = false;
		ShowMessageBox(_eMessageBoxStatus::kWarning, "Warning", "failed to load file!");
	}
#endif
}

// implicit load when the user picks a file in the vfs combo
void PPEEditor_LoadFromVFSInteractive(SPPEditorUIState& state, SPPEffectData& data, const char* vfs_name)
{
	R_ASSERT(vfs_name);

	u32 file_version = 0;

	if (PPEEditor_LoadPPEFromVFS(data, file_version, vfs_name))
	{
		state.selected_file = vfs_name;
		state.file_version = file_version;
		state.is_file_loaded = true;
		state.path.clear();
		state.Reset();
	}
	else
	{
		ShowMessageBox(_eMessageBoxStatus::kWarning, "Warning", "failed to load file from virtual file system!");
	}
}

// enumerates .ppe files known to the virtual file system ($game_anims$),
// covers packed archives and unpacked folders
void PPEEditor_RefreshFileList(SPPEditorUIState& state)
{
	state.combo_files_storage.clear();
	state.combo_files.clear();

	state.combo_files_storage.emplace_back("<not selected>");

	FS_FileSet files;
	FS.file_list(files, "$game_anims$", FS_ListFiles, "*.ppe");

	for (const FS_File& file : files)
	{
		state.combo_files_storage.push_back(file.name.c_str());
	}

	state.combo_files.reserve(state.combo_files_storage.size());

	for (const xr_string& name : state.combo_files_storage)
	{
		state.combo_files.push_back(name.c_str());
	}

	if (state.current_selected_file >= (int)state.combo_files.size())
	{
		state.current_selected_file = 0;
	}
}

void PPEEditor_SaveFileInteractive(SPPEditorUIState& state, SPPEffectData& data, bool force_dialog)
{
	PPEEditor_SortKeys(data);

	// loaded from the vfs combo: write a loose override under the same
	// name into the $game_anims$ folder (shadows a packed file if any)
	if (force_dialog == false && state.selected_file.empty() == false)
	{
		string_path real_path;
		FS.update_path(real_path, "$game_anims$", state.selected_file.c_str());

		if (PPEEditor_SavePPE(data, real_path))
		{
			ShowMessageBox(_eMessageBoxStatus::kSuccess, "", "File is saved successfully!");
		}
		else
		{
			ShowMessageBox(_eMessageBoxStatus::kError, "Check writing policy for your disk", "Failed to create file for writing");
		}

		return;
	}

	xr_stack_tstring<sizeof(string_path)> local_path;

	if (force_dialog || state.path.empty())
	{
		if (xr_EFS == nullptr)
		{
			return;
		}

		if (xr_EFS->GetSaveName(local_path, XR_TEXT("PPE file\0*.ppe\0")) == false)
		{
			return;
		}
	}
	else
	{
		// note: go through a temporary stack string, direct assignment from
		// the returned pointer picks the wrong (string view) operator= overload
		xr_stack_tstring<sizeof(string_path)> converted_path = Platform::ANSI_TO_TCHAR(state.path.c_str());
		local_path = converted_path;
	}

	xr_strlwr(local_path);

	if (local_path.find(XR_TEXT(".ppe")) == xr_stack_tstring<1>::npos)
	{
		local_path.append(XR_TEXT(".ppe"));
	}

#ifdef IXR_WINDOWS
	xr_stack_string<sizeof(string_path) * 2> char_path;
	bool status = Platform::WCHAR_TO_CHAR(local_path, char_path);
	R_ASSERT2(status, "report to developers! Unable to convert your path to multibyte string");

	if (PPEEditor_SavePPE(data, char_path.c_str()))
	{
		state.path = char_path;
		state.selected_file.clear();
		ShowMessageBox(_eMessageBoxStatus::kSuccess, "", "File is saved successfully!");
	}
	else
	{
		ShowMessageBox(_eMessageBoxStatus::kError, "Check writing policy for your disk", "Failed to create file for writing");
	}
#endif
}

// ==============================================================
// Game tab playback (same API the game/scripts use:
// CEffectorPP + CCameraManager::AddPPEffector)
// ==============================================================

#if IXRAY_PPE_EDITOR_TAB_GAME == 1
// Preview effector fed from an in-memory snapshot of the edited data.
// A file round-trip is impossible: CPostprocessAnimator::Load searches
// only $level$/$game_anims$ inside the virtual file system, and files
// written after the FS init are not in the cached VFS scan (so Load
// ends in Debug.fatal "Can't find motion file"). The effector evaluates
// the envelopes directly, mirroring CPostprocessAnimator::Process.
class CPPEditorPreviewEffector : public CEffectorPP
{
public:
	CPPEditorPreviewEffector(SPPEffectData& source, bool cyclic)
		: CEffectorPP((EEffectorPPType)_kPPEEditorPreviewEffectorID, cyclic ? 100000.0f : 0.0f, true),
		  m_bCyclic(cyclic)
	{
		// snapshot the edited data through the same serialization (deep copy),
		// so later editor changes can't race the playing effector
		CMemoryWriter writer;
		PPEEditor_SavePPE_Writer(source, writer);

		u32 file_version = 0;
		IReader reader(writer.pointer(), writer.size());
		PPEEditor_LoadPPE_Reader(m_data, file_version, reader);

		m_length = m_data.GetLength();

		if (m_bCyclic == false)
		{
			fLifeTime = m_length;
		}
	}

	virtual bool Valid() override
	{
		return m_bCyclic ? true : CEffectorPP::Valid();
	}

	virtual void Stop(float speed) override
	{
		if (m_bStop)
		{
			return;
		}

		m_bStop = true;
		m_factor_speed = speed;
	}

	// applied live from the editor UI (checkbox / texture selector)
	void SetCyclic(bool cyclic)
	{
		m_bCyclic = cyclic;
		fLifeTime = cyclic ? 100000.0f : m_length;
	}

	void SetCmTexture(const char* name)
	{
		m_data.cm_tex1 = name;
	}

	virtual bool IsCyclic() const override { return m_bCyclic; }
	virtual float GetRealLifeTime() const override { return m_length; }
	virtual float GetLifeTimeRemaining() const override { return m_length - (Device.fTimeGlobal - m_start_time); }

	virtual bool Process(SPPInfo& PPInfo) override
	{
		if (m_bCyclic)
		{
			fLifeTime = 100000.0f;
		}

		CEffectorPP::Process(PPInfo);

		if (m_start_time < 0.0f)
		{
			m_start_time = Device.fTimeGlobal;
		}

		if (m_bCyclic && m_length > 0.0f && (Device.fTimeGlobal - m_start_time) > m_length)
		{
			m_start_time += m_length;
		}

		const float time = Device.fTimeGlobal - m_start_time;

		// evaluate the edited envelopes; empty channels keep SPPInfo
		// defaults (unlike CPostprocessAnimator, empty is safe here)
		SPPInfo params;

		auto evaluate = [&time](CEnvelope& envelope, float& out)
		{
			if (envelope.keys.empty() == false)
			{
				out = envelope.Evaluate(time);
			}
		};

		evaluate(m_data.colors[0].r, params.color_base.r);
		evaluate(m_data.colors[0].g, params.color_base.g);
		evaluate(m_data.colors[0].b, params.color_base.b);
		evaluate(m_data.colors[1].r, params.color_add.r);
		evaluate(m_data.colors[1].g, params.color_add.g);
		evaluate(m_data.colors[1].b, params.color_add.b);
		evaluate(m_data.colors[2].r, params.color_gray.r);
		evaluate(m_data.colors[2].g, params.color_gray.g);
		evaluate(m_data.colors[2].b, params.color_gray.b);
		evaluate(m_data.values[0].v, params.gray);
		evaluate(m_data.values[1].v, params.blur);
		evaluate(m_data.values[2].v, params.duality.h);
		evaluate(m_data.values[3].v, params.duality.v);
		evaluate(m_data.values[4].v, params.noise.intensity);
		evaluate(m_data.values[5].v, params.noise.grain);
		evaluate(m_data.values[6].v, params.noise.fps);
		evaluate(m_data.values[7].v, params.cm_influence);

		params.cm_tex1 = m_data.cm_tex1.c_str();

		// factor dynamics, mirroring CPostprocessAnimator::Process
		if (m_bStop)
		{
			m_factor -= Device.fTimeDelta * m_factor_speed;
		}
		else
		{
			m_factor += m_factor_speed * Device.fTimeDelta * (m_dest_factor - m_factor);
		}

		clamp(m_factor, 0.0001f, 1.0f);

		params.color_base += pp_identity.color_base;
		params.color_gray += pp_identity.color_gray;
		params.color_add += pp_identity.color_add;

		if (m_data.values[4].v.keys.empty())
		{
			params.noise.intensity = pp_identity.noise.intensity;
		}

		if (m_data.values[5].v.keys.empty())
		{
			params.noise.grain = pp_identity.noise.grain;
		}

		if (m_data.values[6].v.keys.empty())
		{
			params.noise.fps = pp_identity.noise.fps;
		}
		else
		{
			params.noise.fps *= 100.0f;
		}

		PPInfo.lerp(pp_identity, params, m_factor);

		if (fsimilar(m_factor, 0.0001f, EPS_S))
		{
			return false;
		}

		return true;
	}

private:
	SPPEffectData m_data; // owned snapshot
	float m_length = 0.0f;
	float m_factor = 1.0f;
	float m_dest_factor = 1.0f;
	float m_factor_speed = 1.0f;
	float m_start_time = -1.0f;
	bool m_bStop = false;
	bool m_bCyclic = false;
};

void PPEEditor_PlayInGame(CImGuiPPEGameState* pState)
{
	if (pState == nullptr || g_actor == nullptr)
	{
		return;
	}

	PPEEditor_SortKeys(pState->data);

	// AddPPEffector replaces an existing effector with the same id, so replay just works
	Actor()->Cameras().AddPPEffector(new CPPEditorPreviewEffector(pState->data, pState->is_cyclic_playback));
}

void PPEEditor_StopInGame()
{
	if (g_actor == nullptr)
	{
		return;
	}

	CEffectorPP* pEffector = Actor()->Cameras().GetPPEffector((EEffectorPPType)_kPPEEditorPreviewEffectorID);

	if (pEffector)
	{
		// graceful fade-out; the camera manager removes it when done
		pEffector->Stop(1.0f);
	}
}

bool PPEEditor_IsPlayingInGame()
{
	return g_actor && Actor()->Cameras().GetPPEffector((EEffectorPPType)_kPPEEditorPreviewEffectorID) != nullptr;
}

void PPEEditor_SetCyclicInGame(bool cyclic)
{
	if (g_actor == nullptr)
	{
		return;
	}

	CEffectorPP* pEffector = Actor()->Cameras().GetPPEffector((EEffectorPPType)_kPPEEditorPreviewEffectorID);
	CPPEditorPreviewEffector* pPreview = smart_cast<CPPEditorPreviewEffector*>(pEffector);

	if (pPreview)
	{
		pPreview->SetCyclic(cyclic);
	}
}

void PPEEditor_SetCmTextureInGame(const char* name)
{
	if (g_actor == nullptr)
	{
		return;
	}

	CEffectorPP* pEffector = Actor()->Cameras().GetPPEffector((EEffectorPPType)_kPPEEditorPreviewEffectorID);
	CPPEditorPreviewEffector* pPreview = smart_cast<CPPEditorPreviewEffector*>(pEffector);

	if (pPreview)
	{
		pPreview->SetCmTexture(name);
	}
}

// the preview effector owns a data snapshot, but remove it explicitly
// when the editor is shut down
void PPEEditor_DestroyPreviewEffector()
{
	if (g_actor)
	{
		g_actor->Cameras().RemovePPEffector((EEffectorPPType)_kPPEEditorPreviewEffectorID);
	}
}
#endif

// ==============================================================
// Shared UI rendering (editor tab and game tab use the same code)
// ==============================================================

void RenderPPEEditorUI_MenuBar(SPPEditorUIState& state, SPPEffectData& data)
{
	if (ImGui::BeginMenuBar())
	{
		if (ImGui::BeginMenu("File##PPEEditor"))
		{
			if (ImGui::MenuItem("New"))
			{
				data.Reset();
				state.Reset();
				state.path.clear();
				state.selected_file.clear();
				state.current_selected_file = 0;
				state.is_file_loaded = true;
			}

			if (ImGui::MenuItem("Load from disk..."))
			{
				PPEEditor_LoadFileInteractive(state, data);
			}

			if (state.is_file_loaded)
			{
				if (ImGui::MenuItem("Save"))
				{
					PPEEditor_SaveFileInteractive(state, data, false);
				}

				if (ImGui::MenuItem("Save As"))
				{
					PPEEditor_SaveFileInteractive(state, data, true);
				}

				if (ImGui::MenuItem("Close"))
				{
					state.is_file_loaded = false;
					state.path.clear();
					state.selected_file.clear();
					state.current_selected_file = 0;
				}
			}

			ImGui::EndMenu();
		}

		ImGui::EndMenuBar();
	}
}

// combo of .ppe files known to the virtual file system ($game_anims$);
// selecting one loads it implicitly
void RenderPPEEditorUI_FileSelector(SPPEditorUIState& state, SPPEffectData& data)
{
	if (state.combo_files.empty())
	{
		PPEEditor_RefreshFileList(state);
	}

	ImGui::Text("effect ($game_anims$):");
	ImGui::SameLine();
	ImGui::SetNextItemWidth(300.0f);

	if (ImGui::Combo("##ToolsInGameImGui_PPEditor_VfsCombo", &state.current_selected_file, state.combo_files.data(), (int)state.combo_files.size()))
	{
		if (state.current_selected_file > 0)
		{
			PPEEditor_LoadFromVFSInteractive(state, data, state.combo_files[state.current_selected_file]);
		}
	}

	ImGui::SameLine();

	if (ImGui::Button("Refresh##ToolsInGameImGui_PPEditor_VfsRefresh"))
	{
		PPEEditor_RefreshFileList(state);
	}

	ImGui::SetItemTooltip("Re-read the .ppe file list from the virtual file system");
}

void RenderPPEEditorUI_SourceInfo(SPPEditorUIState& state)
{
	if (state.selected_file.empty() == false)
	{
		ImGui::TextWrapped("Loaded file: [$game_anims$\\%s] (version: %u)", state.selected_file.c_str(), state.file_version);
		ImGui::Separator();
	}
	else if (state.path.empty() == false)
	{
		ImGui::TextWrapped("Loaded file: [%s] (version: %u)", state.path.c_str(), state.file_version);
		ImGui::Separator();
	}
}

// one envelope (keys list + add/delete/clear)
void RenderPPEEditorUI_Channel(CEnvelope& channel, float& add_time, float& add_value, const char* label)
{
	ImGui::PushID(label);

	float mn = 0.0f, mx = 0.0f;
	float length = channel.keys.empty() ? 0.0f : channel.GetLength(&mn, &mx);

	ImGui::Text("%s | keys: %d | length: %.3f sec", label, (int)channel.keys.size(), length);

	bool needs_sort = false;
	int delete_index = -1;

	if (ImGui::BeginTable("##PPEKeysTable", 3, ImGuiTableFlags_SizingStretchProp))
	{
		for (int i = 0; i < (int)channel.keys.size(); ++i)
		{
			st_Key& key = *channel.keys[i];

			ImGui::PushID(i);

			ImGui::TableNextRow();

			ImGui::TableSetColumnIndex(0);
			ImGui::SetNextItemWidth(-FLT_MIN);
			ImGui::DragFloat("##time", &key.time, 0.005f, 0.0f, 0.0f, "t=%.3f");

			if (ImGui::IsItemDeactivatedAfterEdit())
			{
				needs_sort = true;
			}

			ImGui::TableSetColumnIndex(1);
			ImGui::SetNextItemWidth(-FLT_MIN);
			ImGui::DragFloat("##value", &key.value, 0.005f, 0.0f, 0.0f, "v=%.3f");

			ImGui::TableSetColumnIndex(2);

			if (ImGui::SmallButton("X##delete"))
			{
				delete_index = i;
			}

			ImGui::PopID();
		}

		ImGui::EndTable();
	}

	if (delete_index >= 0)
	{
		xr_delete(channel.keys[delete_index]);
		channel.keys.erase(channel.keys.begin() + delete_index);
	}

	if (needs_sort)
	{
		std::stable_sort(
			channel.keys.begin(),
			channel.keys.end(),
			[](const st_Key* left, const st_Key* right)
			{
				return left->time < right->time;
			}
		);
	}

	ImGui::SetNextItemWidth(100.0f);
	ImGui::DragFloat("##addtime", &add_time, 0.005f, 0.0f, 0.0f, "t=%.3f");
	ImGui::SameLine();
	ImGui::SetNextItemWidth(100.0f);
	ImGui::DragFloat("##addvalue", &add_value, 0.005f, 0.0f, 0.0f, "v=%.3f");
	ImGui::SameLine();

	if (ImGui::SmallButton("Add##PPEKeyAdd"))
	{
		channel.InsertKey(add_time, add_value);
	}

	ImGui::SameLine();

	if (ImGui::SmallButton("Clear all##PPEKeysClear"))
	{
		channel.ClearAndFree();
	}

	ImGui::PopID();
}

void RenderPPEEditorUI_TexturePreview(IRHISurface* pTexture, IRHIShaderResourceView* pView)
{
	if (pTexture == nullptr)
	{
		return;
	}

	constexpr float _kMaxPreviewSize = 256.0f;

	float width = (float)pTexture->GetWidth();
	float height = (float)pTexture->GetHeight();
	float scale = _kMaxPreviewSize / std::max(width, height);

	ImVec2 preview_size(width * scale, height * scale);

	if (GRHI->APILevel == D3D9)
	{
		if (pTexture->GetRawTexture())
		{
			ImGui::Image(pTexture->GetRawTexture(), preview_size);
		}
	}
	else
	{
		if (pView && pView->GetRawSRV())
		{
			ImGui::Image(pView->GetRawSRV(), preview_size);
		}
	}
}

void PPEEditor_RequestTextureListLoad()
{
	if (g_ppe_tex_browser.is_loaded == false && g_ppe_tex_browser.is_load_started == false)
	{
		g_ppe_tex_browser.is_load_started = true;

		SRequestData req;
		req.editor_type = (u32)eImGuiEditorType::kPPEEditor;
		req.request_type = (u32)eRequestType_PPEditor::kLoadFile;

		AllEditors_SendRequest(req);
	}
}

void RenderPPEEditorUI_TextureSelectorModal()
{
	if (g_ppe_tex_browser.is_open)
	{
		ImGui::OpenPopup(_kPPEEditorModalWindow_TextureSelector);
	}

	if (ImGui::BeginPopupModal(_kPPEEditorModalWindow_TextureSelector, &g_ppe_tex_browser.is_open, ImGuiWindowFlags_AlwaysAutoResize))
	{
		if (ImGui::InputText("filter##ToolsInGameImGui_PPEEditor_TextureFilter", g_ppe_tex_browser.filter.data(), g_ppe_tex_browser.filter.max_size()))
		{
			g_ppe_tex_browser.is_filter_dirty = true;
		}

		if (g_ppe_tex_browser.is_loaded == false)
		{
			// safe-guard: re-send the request if it got lost somewhere
			PPEEditor_RequestTextureListLoad();

			ImGui::Text("Loading textures...");
		}

		ImGui::BeginDisabled(g_ppe_tex_browser.is_loaded == false);

		if (g_ppe_tex_browser.is_loaded)
		{
			if (g_ppe_tex_browser.is_filter_dirty)
			{
				g_ppe_tex_browser.filtered_indices.clear();

				if (g_ppe_tex_browser.filter.empty())
				{
					for (int i = 0; i < (int)g_ppe_tex_browser.names.size(); ++i)
					{
						g_ppe_tex_browser.filtered_indices.push_back(i);
					}
				}
				else
				{
					xr_string filter_lower = g_ppe_tex_browser.filter.c_str();
					xr_strlwr(filter_lower);

					for (int i = 0; i < (int)g_ppe_tex_browser.names_storage.size(); ++i)
					{
						xr_string name_lower = g_ppe_tex_browser.names_storage[i];
						xr_strlwr(name_lower);

						if (name_lower.find(filter_lower) != xr_string::npos)
						{
							g_ppe_tex_browser.filtered_indices.push_back(i);
						}
					}
				}

				g_ppe_tex_browser.is_filter_dirty = false;
			}

			if (ImGui::BeginListBox("##ToolsInGameImGui_PPEEditor_TextureList", ImVec2(420.0f, 300.0f)))
			{
				for (int index : g_ppe_tex_browser.filtered_indices)
				{
					const char* name = g_ppe_tex_browser.names[index];

					ImGui::PushID(index);

					if (ImGui::Selectable(name, g_ppe_tex_browser.current_selected == index))
					{
						g_ppe_tex_browser.current_selected = index;
					}

					if (ImGui::IsItemHovered())
					{
						if (!(g_ppe_tex_browser.preview_name == name))
						{
							// hovered a different texture: drop the old preview state
							g_ppe_tex_browser.is_preview_loaded = false;
							g_ppe_tex_browser.is_preview_load_started = false;
						}

						if (g_ppe_tex_browser.is_preview_loaded == false && g_ppe_tex_browser.is_preview_load_started == false)
						{
							SRequestData req;
							req.editor_type = (u32)eImGuiEditorType::kPPEEditor;
							req.request_type = (u32)eRequestType_PPEditor::kLoadTexturePreview;
							req.payload = (u32)index;

							AllEditors_SendRequest(req);

							g_ppe_tex_browser.is_preview_load_started = true;
						}

						if (ImGui::BeginTooltip())
						{
							if (g_ppe_tex_browser.is_preview_loaded && g_ppe_tex_browser.p_preview)
							{
								RenderPPEEditorUI_TexturePreview(g_ppe_tex_browser.p_preview, g_ppe_tex_browser.p_preview_srv);
							}
							else
							{
								ImGui::Text("Loading. . .");
							}

							ImGui::EndTooltip();
						}
					}

					ImGui::PopID();
				}

				ImGui::EndListBox();
			}
		}

		ImGui::EndDisabled();

		ImGui::BeginDisabled(g_ppe_tex_browser.is_loaded == false || g_ppe_tex_browser.current_selected < 0);

		if (ImGui::Button("Ok##ToolsInGameImGui_PPEEditor_TextureSelectOk"))
		{
			if (g_ppe_tex_browser.p_target_data && g_ppe_tex_browser.current_selected >= 0)
			{
				g_ppe_tex_browser.p_target_data->cm_tex1 = g_ppe_tex_browser.names[g_ppe_tex_browser.current_selected];

#if IXRAY_PPE_EDITOR_TAB_GAME == 1
				// apply to the currently playing preview effector as well
				PPEEditor_SetCmTextureInGame(g_ppe_tex_browser.names[g_ppe_tex_browser.current_selected]);
#endif
			}

			g_ppe_tex_browser.is_open = false;
		}

		ImGui::EndDisabled();

		ImGui::SameLine();

		if (ImGui::Button("Cancel##ToolsInGameImGui_PPEEditor_TextureSelectCancel"))
		{
			g_ppe_tex_browser.is_open = false;
		}

		ImGui::EndPopup();
	}

	// the modal was closed (Ok/Cancel/X/escape): free the preview resources
	if (
		g_ppe_tex_browser.is_open == false &&
		g_ppe_tex_browser.is_preview_release_sent == false &&
		(g_ppe_tex_browser.p_preview || g_ppe_tex_browser.p_preview_srv)
	)
	{
		SRequestData req;
		req.editor_type = (u32)eImGuiEditorType::kPPEEditor;
		req.request_type = (u32)eRequestType_PPEditor::kLoadTexturePreview;
		req.payload = u32(-1); // release only

		AllEditors_SendRequest(req);

		g_ppe_tex_browser.is_preview_release_sent = true;
	}

	if (g_ppe_tex_browser.is_open == false)
	{
		g_ppe_tex_browser.p_target_data = nullptr;
	}
}

void RenderPPEEditorUI_EffectBody(SPPEditorUIState& state, SPPEffectData& data)
{
	if (ImGui::Combo(
			"Param##ToolsInGameImGui_PPEditor_Combo",
			&state.current_selected_param,
			[](void* user_data, int idx) -> const char*
			{
				R_ASSERT(idx >= 0 && idx < POSTPROCESS_PARAMS_COUNT);
				return _kPPEParamMetas[idx].name;
			},
			nullptr,
			POSTPROCESS_PARAMS_COUNT
		))
	{
	}

	if (state.current_selected_param < 0 || state.current_selected_param >= POSTPROCESS_PARAMS_COUNT)
	{
		state.current_selected_param = 0;
	}

	const _SPPEParamMeta& meta = _kPPEParamMetas[state.current_selected_param];

	ImGui::Separator();

	ImGui::PushID(meta.name);

	float(&add_time)[3] = state.add_key_time[state.current_selected_param];
	float(&add_value)[3] = state.add_key_value[state.current_selected_param];

	if (meta.kind == _ePPEParamKind::kColor)
	{
		SPPEffectData::SColorParam& param = data.colors[meta.index];

		ImGui::DragFloat("base value", &param.base, 0.005f);

		if (ImGui::CollapsingHeader("R##PPEChannelR"))
		{
			RenderPPEEditorUI_Channel(param.r, add_time[0], add_value[0], "R");
		}

		if (ImGui::CollapsingHeader("G##PPEChannelG"))
		{
			RenderPPEEditorUI_Channel(param.g, add_time[1], add_value[1], "G");
		}

		if (ImGui::CollapsingHeader("B##PPEChannelB"))
		{
			RenderPPEEditorUI_Channel(param.b, add_time[2], add_value[2], "B");
		}
	}
	else
	{
		RenderPPEEditorUI_Channel(data.values[meta.index].v, add_time[0], add_value[0], meta.name);
	}

	if (ImGui::Button("Reset param##ToolsInGameImGui_PPEditor_ResetParam"))
	{
		if (meta.kind == _ePPEParamKind::kColor)
		{
			SPPEffectData::SColorParam& param = data.colors[meta.index];
			param.base = 0.0f;
			param.r.ClearAndFree();
			param.g.ClearAndFree();
			param.b.ClearAndFree();
		}
		else
		{
			data.values[meta.index].v.ClearAndFree();
		}
	}

	ImGui::PopID();

	ImGui::Separator();

	ImGui::Text("colormap texture (cm_tex1): [%s]", data.cm_tex1.empty() ? "<none>" : data.cm_tex1.c_str());
	ImGui::SameLine();

	if (ImGui::Button("Select...##ToolsInGameImGui_PPEEditor_CmTexSelect"))
	{
		g_ppe_tex_browser.p_target_data = &data;
		g_ppe_tex_browser.is_open = true;

		PPEEditor_RequestTextureListLoad();
	}

	RenderPPEEditorUI_TextureSelectorModal();

	ImGui::Separator();

	ImGui::Text("total keys: %d | total length: %.3f sec", data.GetKeysCount(), data.GetLength());
}

#if IXRAY_PPE_EDITOR_PREVIEW == 1
// todo: implement viewer of ppe (envelope curves preview)
void RenderPPEEditor_Draw_Preview(SPPEffectData& data)
{
}
#endif

// ==============================================================
// Editor tab
// ==============================================================

void RenderPPEEditor_Draw_EditorTab()
{
	RenderPPEEditorUI_MenuBar(*g_pPPEEditor, g_pPPEEditor->data);
	RenderPPEEditorUI_FileSelector(*g_pPPEEditor, g_pPPEEditor->data);

	if (g_pPPEEditor->is_file_loaded == false)
	{
		ImGui::TextWrapped("No file loaded. Select an effect in the combo, use File > Load from disk to open a .ppe file or File > New to create a new effect.");
		return;
	}

	RenderPPEEditorUI_SourceInfo(*g_pPPEEditor);

	constexpr const char* _kColumnOfMainTableNames[] = {
		"Editing",
#if IXRAY_PPE_EDITOR_PREVIEW == 1
		"Preview"
#endif
	};
	constexpr u8 _kColumnOfMainTableSize = sizeof(_kColumnOfMainTableNames) / sizeof(_kColumnOfMainTableNames[0]);

	if (ImGui::BeginTable("##TII_PPE_Main", _kColumnOfMainTableSize, ImGuiTableFlags_SizingStretchProp))
	{
		for (u8 i = 0; i < _kColumnOfMainTableSize; ++i)
		{
			ImGui::TableSetupColumn(_kColumnOfMainTableNames[i]);
		}

		ImGui::TableHeadersRow();

		ImGui::TableNextRow();

		for (u8 column = 0; column < _kColumnOfMainTableSize; ++column)
		{
			ImGui::TableSetColumnIndex(static_cast<int>(column));

			switch (column)
			{
				case 0:
				{
					RenderPPEEditorUI_EffectBody(*g_pPPEEditor, g_pPPEEditor->data);
					break;
				}
#if IXRAY_PPE_EDITOR_PREVIEW == 1
				case 1:
				{
					RenderPPEEditor_Draw_Preview(g_pPPEEditor->data);
					break;
				}
#endif
			}
		}

		ImGui::EndTable();
	}
}

// ==============================================================
// Game tab
// ==============================================================

#if IXRAY_PPE_EDITOR_TAB_GAME == 1
void RenderPPEEditor_Draw_GameTab()
{
	R_ASSERT(g_pPPEGame);

	RenderPPEEditorUI_MenuBar(*g_pPPEGame, g_pPPEGame->data);
	RenderPPEEditorUI_FileSelector(*g_pPPEGame, g_pPPEGame->data);

	if (g_pPPEGame->is_file_loaded == false)
	{
		ImGui::TextWrapped("No file loaded. Select an effect in the combo, use File > Load from disk to open a .ppe file or File > New to create a new effect, then press Play.");
		return;
	}

	RenderPPEEditorUI_SourceInfo(*g_pPPEGame);

	bool is_playing = PPEEditor_IsPlayingInGame();

	ImGui::Text("preview: [%s]", is_playing ? "playing" : "stopped");
	ImGui::SameLine();

	if (ImGui::Button("Play##ToolsInGameImGui_PPEditor_Play"))
	{
		PPEEditor_PlayInGame(g_pPPEGame);
	}

	ImGui::SameLine();

	ImGui::BeginDisabled(is_playing == false);

	if (ImGui::Button("Stop##ToolsInGameImGui_PPEditor_Stop"))
	{
		PPEEditor_StopInGame();
	}

	ImGui::EndDisabled();

	ImGui::SameLine();

	if (ImGui::Checkbox("Cyclic##ToolsInGameImGui_PPEditor_Cyclic", &g_pPPEGame->is_cyclic_playback))
	{
		// applies live to the currently playing effect
		PPEEditor_SetCyclicInGame(g_pPPEGame->is_cyclic_playback);
	}

	ImGui::SetItemTooltip("Cyclic effects play until Stop is pressed; non-cyclic effects play once for their total length");

	if (is_playing)
	{
		CEffectorPP* pEffector = Actor()->Cameras().GetPPEffector((EEffectorPPType)_kPPEEditorPreviewEffectorID);

		if (pEffector)
		{
			// works for both cyclic (position within the current cycle)
			// and non-cyclic (position of the single pass) playback
			float total = pEffector->GetRealLifeTime();
			float current = total - pEffector->GetLifeTimeRemaining();
			clamp(current, 0.0f, total);

			ImGui::Text("time: [%.2f]/[%.2f] sec", current, total);
		}
	}

	ImGui::Separator();

	RenderPPEEditorUI_EffectBody(*g_pPPEGame, g_pPPEGame->data);
}
#endif

// ==============================================================
// Requests + main window
// ==============================================================

void RequestHandler_PPEEditor(const SRequestData& req)
{
	R_ASSERT2(static_cast<eImGuiEditorType>(req.editor_type) == eImGuiEditorType::kPPEEditor, "mistaken workload calling! that means data was corrupted or some error occurred");

	eRequestType_PPEditor req_type = static_cast<eRequestType_PPEditor>(req.request_type);

	switch (req_type)
	{
		case eRequestType_PPEditor::kReadSettings:
		{
			break;
		}
		case eRequestType_PPEditor::kWriteSettings:
		{
			break;
		}
		case eRequestType_PPEditor::kLoadFile:
		{
			// loads the list of all textures known to the virtual file
			// system ($textures$), covers packed archives and loose files
			if (g_ppe_tex_browser.is_loaded == false)
			{
				g_ppe_tex_browser.names_storage.clear();
				g_ppe_tex_browser.names.clear();

				FS_FileSet files;
				FS.file_list(files, "$textures$", FS_ListFiles | FS_ClampExt, "*.dds");

				for (const FS_File& file : files)
				{
					g_ppe_tex_browser.names_storage.push_back(file.name);
				}

				g_ppe_tex_browser.names.reserve(g_ppe_tex_browser.names_storage.size());

				for (const xr_string& name : g_ppe_tex_browser.names_storage)
				{
					g_ppe_tex_browser.names.push_back(name.c_str());
				}

				g_ppe_tex_browser.is_filter_dirty = true;

				// keep last: the render thread touches the data only when this is set
				g_ppe_tex_browser.is_loaded = true;
				g_ppe_tex_browser.is_load_started = false;
			}

			break;
		}
		case eRequestType_PPEditor::kLoadTexturePreview:
		{
			// release the previous preview first (also handles the
			// "release only" case when payload is u32(-1))
			if (g_ppe_tex_browser.p_preview)
			{
				g_ppe_tex_browser.p_preview->Release();
				g_ppe_tex_browser.p_preview = nullptr;
			}

			if (g_ppe_tex_browser.p_preview_srv)
			{
				g_ppe_tex_browser.p_preview_srv->Release();
				g_ppe_tex_browser.p_preview_srv = nullptr;
			}

			if (req.payload != u32(-1) && req.payload < g_ppe_tex_browser.names_storage.size())
			{
				xr_stack_string<sizeof(string_path)> texture_path = g_ppe_tex_browser.names_storage[req.payload].c_str();
				texture_path += ".dds";

				g_ppe_tex_browser.preview_name = g_ppe_tex_browser.names_storage[req.payload].c_str();

				u32 tex_size = 0;
				IRHISurface* pSurface = Render->load_texture(texture_path.c_str(), tex_size);

				if (pSurface)
				{
					g_ppe_tex_browser.p_preview = pSurface;

					if (GRHI->APILevel != D3D9)
					{
						RHIShaderResourceViewDesc desc_srv;
						desc_srv.MipLevels = 1;
						desc_srv.Format = pSurface->GetFormat();
						desc_srv.MostDetailedMip = 0;
						desc_srv.ViewDimension = ERHI_SRV_DIMENSION::TEXTURE2D;
						desc_srv.FirstArraySlice = 0;
						desc_srv.ArraySize = 1;
						desc_srv.ElementWidth = 0;

						IRHIShaderResourceView* pView = GRHI->CreateShaderResourceView(pSurface, &desc_srv);

						if (pView)
						{
							g_ppe_tex_browser.p_preview_srv = pView;
						}
					}
				}
			}
			else
			{
				g_ppe_tex_browser.preview_name.clear();
			}

			g_ppe_tex_browser.is_preview_loaded = true;
			g_ppe_tex_browser.is_preview_load_started = false;
			g_ppe_tex_browser.is_preview_release_sent = false;

			break;
		}
		case eRequestType_PPEditor::kDeselectCurrentSelectedOrHideWindow:
		{
			// an open modal window consumes the escape first
			if (g_ppe_tex_browser.is_open)
			{
				g_ppe_tex_browser.is_open = false;
			}
			else
			{
				Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_PostProcessEffectorEditor)] = false;
			}

			break;
		}
		case eRequestType_PPEditor::kShutdown:
		{
			if (g_pPPEEditor)
			{
				delete g_pPPEEditor;
				g_pPPEEditor = nullptr;
			}

#if IXRAY_PPE_EDITOR_TAB_GAME == 1
			PPEEditor_DestroyPreviewEffector();

			if (g_pPPEGame)
			{
				delete g_pPPEGame;
				g_pPPEGame = nullptr;
			}
#endif

			if (g_ppe_tex_browser.p_preview)
			{
				g_ppe_tex_browser.p_preview->Release();
				g_ppe_tex_browser.p_preview = nullptr;
			}

			if (g_ppe_tex_browser.p_preview_srv)
			{
				g_ppe_tex_browser.p_preview_srv->Release();
				g_ppe_tex_browser.p_preview_srv = nullptr;
			}

			g_ppe_tex_browser = SPPETextureBrowser{};

			break;
		}
		default:
		{
			R_ASSERT(!"invalid request type or request type of different editor");
			break;
		}
	}
}

void RenderPPEEditor()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_PostProcessEffectorEditor)])
	{
		return;
	}

	if (g_pPPEEditor == nullptr)
	{
		g_pPPEEditor = new CImGuiPPEEditor();
	}

	if (g_pPPEEditor)
	{
		if (ImGui::Begin("Editor - [PPE]##ToolsInGameImGui", nullptr, ImGuiWindowFlags_MenuBar))
		{
			if (ImGui::BeginTabBar("##PPEToolsIII"))
			{
#if IXRAY_PPE_EDITOR_TAB_EDITOR == 1
				if (ImGui::BeginTabItem("Editor"))
				{
					RenderPPEEditor_Draw_EditorTab();

					ImGui::EndTabItem();
				}
#endif

#if IXRAY_PPE_EDITOR_TAB_GAME == 1

				bool is_in_game = false;

				if (
					g_pGameLevel &&
					ai().get_alife() &&
					g_actor
				)
				{
					CActor* pPlayer = Actor();

					if (pPlayer && pPlayer->GetfHealth() > 0.001f)
					{
						is_in_game = true;
					}
				}

				ImGui::BeginDisabled(!is_in_game);
				if (ImGui::BeginTabItem("Game"))
				{
					if (is_in_game)
					{
						if (g_pPPEGame == nullptr)
						{
							g_pPPEGame = new CImGuiPPEGameState();
						}

						RenderPPEEditor_Draw_GameTab();
					}

					ImGui::EndTabItem();
				}

				ImGui::EndDisabled();
#endif

				ImGui::EndTabBar();
			}
		}
		ImGui::End();
	}
}
