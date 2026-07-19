
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
#include "../ActorEffector.h"

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
	float v_min; // accepted value range (display expands when data exceeds it)
	float v_max;
};

// serialization order of the .ppe format (see CPostprocessAnimator::Load)
constexpr _SPPEParamMeta _kPPEParamMetas[POSTPROCESS_PARAMS_COUNT] = {
	{"Base color", _ePPEParamKind::kColor, 0, 0.0f, 1.0f},
	{"Add color", _ePPEParamKind::kColor, 1, 0.0f, 1.0f},
	{"Gray color", _ePPEParamKind::kColor, 2, 0.0f, 1.0f},
	{"Gray value", _ePPEParamKind::kValue, 0, 0.0f, 1.0f},
	{"Blur", _ePPEParamKind::kValue, 1, 0.0f, 1.0f},
	{"Duality H", _ePPEParamKind::kValue, 2, -1.0f, 1.0f},
	{"Duality V", _ePPEParamKind::kValue, 3, -1.0f, 1.0f},
	{"Noise intensity", _ePPEParamKind::kValue, 4, 0.0f, 1.0f},
	{"Noise grain", _ePPEParamKind::kValue, 5, 0.0f, 10.0f},
	{"Noise FPS", _ePPEParamKind::kValue, 6, 0.0f, 1.0f},
	{"CM influence", _ePPEParamKind::kValue, 7, 0.0f, 1.0f},
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

	// changes the total effect length: scales the time of every key
	// in every channel proportionally
	void ScaleLength(float new_length)
	{
		float old_length = GetLength();

		if (old_length <= 0.0f || new_length <= 0.0f)
		{
			return;
		}

		float scale = new_length / old_length;

		auto scale_envelope = [scale](CEnvelope& envelope)
		{
			for (st_Key* key : envelope.keys)
			{
				key->time *= scale;
			}
		};

		for (SColorParam& color : colors)
		{
			scale_envelope(color.r);
			scale_envelope(color.g);
			scale_envelope(color.b);
		}

		for (SValueParam& value : values)
		{
			scale_envelope(value.v);
		}
	}
};

// shared UI state (editor tab and game tab each carry one)
struct SPPEditorUIState
{
	bool is_file_loaded{};
	int current_selected_param{};
	int current_selected_file{}; // index into combo_files, 0 = "<not selected>"
	// timeline state, per param
	bool timeline_place_on_click{};
	float timeline_cursor[POSTPROCESS_PARAMS_COUNT]{};
	float timeline_new_key_value[POSTPROCESS_PARAMS_COUNT]{};
	int timeline_selected_channel[POSTPROCESS_PARAMS_COUNT]{};
	xr_vector<st_Key*> timeline_selected_keys[POSTPROCESS_PARAMS_COUNT];
	float timeline_drag_time[POSTPROCESS_PARAMS_COUNT]{};
	float timeline_drag_value[POSTPROCESS_PARAMS_COUNT]{};
	bool timeline_drag_started[POSTPROCESS_PARAMS_COUNT]{};
	bool timeline_is_dragging[POSTPROCESS_PARAMS_COUNT]{};
	// vertical axis range, per (param, channel): auto (fits section range
	// and data) or custom user values
	bool timeline_range_auto[POSTPROCESS_PARAMS_COUNT][3]{};
	float timeline_range_min[POSTPROCESS_PARAMS_COUNT][3]{};
	float timeline_range_max[POSTPROCESS_PARAMS_COUNT][3]{};
	float total_length_edit{};
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
			timeline_cursor[param] = 0.0f;
			timeline_new_key_value[param] = 0.0f;
			timeline_selected_channel[param] = 0;
			timeline_selected_keys[param].clear();
			timeline_drag_time[param] = 0.0f;
			timeline_drag_value[param] = 0.0f;
			timeline_drag_started[param] = false;
			timeline_is_dragging[param] = false;

			for (int channel = 0; channel < 3; ++channel)
			{
				timeline_range_auto[param][channel] = true;
				timeline_range_min[param][channel] = 0.0f;
				timeline_range_max[param][channel] = 1.0f;
			}
		}
	}
};

struct CImGuiPPEEditor : SPPEditorUIState
{
	SPPEffectData data;
	SPPEffectData data_default; // snapshot from the last load/new, for "Reset to default"
};

CImGuiPPEEditor* g_pPPEEditor = nullptr;

#if IXRAY_PPE_EDITOR_TAB_GAME == 1
struct CImGuiPPEGameState : SPPEditorUIState
{
	SPPEffectData data;
	SPPEffectData data_default; // snapshot from the last load/new, for "Reset to default"
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

// deep copy through the same serialization (SPPEffectData can't be
// copied directly: CEnvelope owns raw st_Key pointers)
void PPEEditor_CopyData(SPPEffectData& dest, SPPEffectData& source)
{
	CMemoryWriter writer;
	PPEEditor_SavePPE_Writer(source, writer);

	u32 file_version = 0;
	IReader reader(writer.pointer(), writer.size());
	PPEEditor_LoadPPE_Reader(dest, file_version, reader);
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

#if IXRAY_PPE_EDITOR_TAB_GAME == 1
// defined in the playback section below
void PPEEditor_ReplayPreviewIfPlaying(SPPEditorUIState& state);
void PPEEditor_StopPreviewIfMine(SPPEditorUIState& state);
#endif

bool PPEEditor_LoadFileInteractive(SPPEditorUIState& state, SPPEffectData& data)
{
	if (xr_EFS == nullptr)
	{
		return false;
	}

	xr_stack_tstring<sizeof(string_path)> local_path;

	if (xr_EFS->GetOpenName(local_path, XR_TEXT("PPE file\0*.ppe\0")) == false)
	{
		return false;
	}

	bool result = false;

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

		result = true;

#if IXRAY_PPE_EDITOR_TAB_GAME == 1
		PPEEditor_ReplayPreviewIfPlaying(state);
#endif
	}
	else
	{
		data.Reset();
		state.is_file_loaded = false;
		ShowMessageBox(_eMessageBoxStatus::kWarning, "Warning", "failed to load file!");
	}
#endif

	return result;
}

// implicit load when the user picks a file in the vfs combo
bool PPEEditor_LoadFromVFSInteractive(SPPEditorUIState& state, SPPEffectData& data, const char* vfs_name)
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

#if IXRAY_PPE_EDITOR_TAB_GAME == 1
		// replace the running preview with the newly selected effect
		PPEEditor_ReplayPreviewIfPlaying(state);
#endif

		return true;
	}

	ShowMessageBox(_eMessageBoxStatus::kWarning, "Warning", "failed to load file from virtual file system!");

	return false;
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
		PPEEditor_CopyData(m_data, source);

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

// restarts the preview with the current data when the game tab's
// effect is playing (call after the game tab's data was replaced)
void PPEEditor_ReplayPreviewIfPlaying(SPPEditorUIState& state)
{
	if (&state == g_pPPEGame && g_pPPEGame && PPEEditor_IsPlayingInGame())
	{
		PPEEditor_PlayInGame(g_pPPEGame);
	}
}

// stops the preview when the game tab's data was dropped (New/Close)
void PPEEditor_StopPreviewIfMine(SPPEditorUIState& state)
{
	if (&state == g_pPPEGame && g_pPPEGame)
	{
		PPEEditor_StopInGame();
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

void RenderPPEEditorUI_MenuBar(SPPEditorUIState& state, SPPEffectData& data, SPPEffectData& data_default)
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

				// the fresh defaults become the new "default" snapshot
				PPEEditor_CopyData(data_default, data);

#if IXRAY_PPE_EDITOR_TAB_GAME == 1
				PPEEditor_StopPreviewIfMine(state);
#endif
			}

			if (ImGui::MenuItem("Load from disk..."))
			{
				if (PPEEditor_LoadFileInteractive(state, data))
				{
					PPEEditor_CopyData(data_default, data);
				}
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

#if IXRAY_PPE_EDITOR_TAB_GAME == 1
					PPEEditor_StopPreviewIfMine(state);
#endif
				}
			}

			ImGui::EndMenu();
		}

		ImGui::EndMenuBar();
	}
}

// combo of .ppe files known to the virtual file system ($game_anims$);
// selecting one loads it implicitly
void RenderPPEEditorUI_FileSelector(SPPEditorUIState& state, SPPEffectData& data, SPPEffectData& data_default)
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
			if (PPEEditor_LoadFromVFSInteractive(state, data, state.combo_files[state.current_selected_file]))
			{
				PPEEditor_CopyData(data_default, data);
			}
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

// the value range of a channel: the section's accepted range from the
// meta table, expanded when the actual data exceeds it
void PPEEditor_GetChannelRange(
	const _SPPEParamMeta& meta,
	const CEnvelope& channel,
	float& out_min,
	float& out_max
)
{
	out_min = meta.v_min;
	out_max = meta.v_max;

	for (const st_Key* key : channel.keys)
	{
		out_min = std::min(out_min, key->value);
		out_max = std::max(out_max, key->value);
	}

	if (out_max - out_min < 0.0001f)
	{
		out_min -= 0.5f;
		out_max += 0.5f;
	}
}

void PPEEditor_DeleteSelectedKeys(CEnvelope& channel, xr_vector<st_Key*>& selection)
{
	for (st_Key* selected : selection)
	{
		for (auto it = channel.keys.begin(); it != channel.keys.end(); ++it)
		{
			if (*it == selected)
			{
				xr_delete(*it);
				channel.keys.erase(it);
				break;
			}
		}
	}

	selection.clear();
}

constexpr int _kPPETimelineSamples = 64;

// draws the timeline axis (0.0 .. axis_length): a sampled ramp of the
// selected channel (color params) or a value curve (value params), plus
// the marks of the channel, the time cursor and all the interaction:
// click = cursor/mark select, place-on-click = insert key at the click
// position, ctrl+a = select all keys, drag = 2d selection box
void RenderPPEEditorUI_Timeline(
	SPPEditorUIState& state,
	SPPEffectData& data,
	const _SPPEParamMeta& meta,
	float axis_length
)
{
	const int param_index = state.current_selected_param;
	float& cursor = state.timeline_cursor[param_index];
	xr_vector<st_Key*>& selection = state.timeline_selected_keys[param_index];
	const int selected_channel = state.timeline_selected_channel[param_index];

	clamp(cursor, 0.0f, axis_length);

	// channel whose keys are shown as marks
	CEnvelope* p_channel = nullptr;

	if (meta.kind == _ePPEParamKind::kColor)
	{
		SPPEffectData::SColorParam& param = data.colors[meta.index];

		CEnvelope* channels[3] = {&param.r, &param.g, &param.b};
		p_channel = channels[selected_channel];
	}
	else
	{
		p_channel = &data.values[meta.index].v;
	}

	// vertical range: auto (synced from the section range + data every
	// frame) or custom values set by the user
	const int channel_index = (meta.kind == _ePPEParamKind::kColor) ? selected_channel : 0;
	bool& range_auto = state.timeline_range_auto[param_index][channel_index];
	float& range_min = state.timeline_range_min[param_index][channel_index];
	float& range_max = state.timeline_range_max[param_index][channel_index];

	if (range_auto)
	{
		PPEEditor_GetChannelRange(meta, *p_channel, range_min, range_max);
	}

	if (range_max - range_min < 0.0001f)
	{
		range_max = range_min + 0.0001f;
	}

	static const float _kChannelHues[3][3] = {
		{1.0f, 0.12f, 0.12f},
		{0.12f, 1.0f, 0.12f},
		{0.25f, 0.45f, 1.0f},
	};

	ImVec2 canvas_size(ImGui::GetContentRegionAvail().x, 56.0f);

	if (canvas_size.x < 64.0f)
	{
		canvas_size.x = 64.0f;
	}

	const ImVec2 canvas_pos = ImGui::GetCursorScreenPos();

	ImGui::InvisibleButton("##PPETimeline", canvas_size);

	const bool is_hovered = ImGui::IsItemHovered();
	const bool is_clicked = ImGui::IsItemClicked(ImGuiMouseButton_Left);

	ImDrawList* p_draw_list = ImGui::GetWindowDrawList();

	p_draw_list->AddRectFilled(canvas_pos, ImVec2(canvas_pos.x + canvas_size.x, canvas_pos.y + canvas_size.y), IM_COL32(24, 24, 24, 255));

	auto time_to_x = [&canvas_pos, &canvas_size, axis_length](float time)
	{
		return canvas_pos.x + (time / axis_length) * canvas_size.x;
	};

	auto value_to_y = [&canvas_pos, &canvas_size, range_min, range_max](float value)
	{
		return canvas_pos.y + canvas_size.y - ((value - range_min) / (range_max - range_min)) * canvas_size.y;
	};

	auto pos_to_time = [&canvas_pos, &canvas_size, axis_length](float x)
	{
		float time = ((x - canvas_pos.x) / canvas_size.x) * axis_length;
		clamp(time, 0.0f, axis_length);
		return time;
	};

	// click y maps to the section's accepted value range (clamped):
	// top border = max value, bottom border = min value
	auto pos_to_value = [&canvas_pos, &canvas_size, range_min, range_max](float y)
	{
		float value = range_max - ((y - canvas_pos.y) / canvas_size.y) * (range_max - range_min);
		clamp(value, range_min, range_max);
		return value;
	};

	// preview: intensity ramp of the selected channel (color params)
	// or value curve (value params)
	if (meta.kind == _ePPEParamKind::kColor)
	{
		for (int i = 0; i < _kPPETimelineSamples; ++i)
		{
			float t0 = (float)i / _kPPETimelineSamples * axis_length;
			float t1 = (float)(i + 1) / _kPPETimelineSamples * axis_length;
			float t = (t0 + t1) * 0.5f;

			float value = p_channel->keys.empty() ? 0.0f : p_channel->Evaluate(t);
			value = std::min(std::max(value, 0.0f), 1.0f);

			ImU32 color = IM_COL32(
				(int)(_kChannelHues[selected_channel][0] * value * 255.0f),
				(int)(_kChannelHues[selected_channel][1] * value * 255.0f),
				(int)(_kChannelHues[selected_channel][2] * value * 255.0f),
				255
			);

			p_draw_list->AddRectFilled(
				ImVec2(time_to_x(t0), canvas_pos.y),
				ImVec2(time_to_x(t1) + 1.0f, canvas_pos.y + canvas_size.y),
				color
			);
		}
	}
	else
	{
		ImVec2 points[_kPPETimelineSamples];

		for (int i = 0; i < _kPPETimelineSamples; ++i)
		{
			float t = (float)i / (_kPPETimelineSamples - 1) * axis_length;
			float value = p_channel->keys.empty() ? 0.0f : p_channel->Evaluate(t);
			points[i] = ImVec2(time_to_x(t), value_to_y(value));
		}

		p_draw_list->AddPolyline(points, _kPPETimelineSamples, IM_COL32(90, 160, 255, 255), 0, 1.5f);

		char range_label[32];
		std::sprintf(range_label, "%.2f", range_max);
		p_draw_list->AddText(ImVec2(canvas_pos.x + 2.0f, canvas_pos.y + 2.0f), IM_COL32(140, 140, 140, 255), range_label);
		std::sprintf(range_label, "%.2f", range_min);
		p_draw_list->AddText(ImVec2(canvas_pos.x + 2.0f, canvas_pos.y + canvas_size.y - 16.0f), IM_COL32(140, 140, 140, 255), range_label);
	}

	// hover hit-test of marks
	st_Key* hovered_key = nullptr;
	const ImVec2 mouse_pos = ImGui::GetMousePos();

	if (is_hovered)
	{
		for (st_Key* key : p_channel->keys)
		{
			if (std::fabs(mouse_pos.x - time_to_x(key->time)) <= 4.0f)
			{
				hovered_key = key;
				break;
			}
		}
	}

	// interaction: mark click / empty click / selection box / place on click
	bool& is_drag_started = state.timeline_drag_started[param_index];
	bool& is_dragging = state.timeline_is_dragging[param_index];
	float& drag_time = state.timeline_drag_time[param_index];
	float& drag_value = state.timeline_drag_value[param_index];

	if (is_clicked)
	{
		if (hovered_key)
		{
			selection.clear();
			selection.push_back(hovered_key);
			is_drag_started = false;
			is_dragging = false;
		}
		else
		{
			// possible start of a click or a selection box
			drag_time = pos_to_time(mouse_pos.x);
			drag_value = pos_to_value(mouse_pos.y);
			is_drag_started = true;
			is_dragging = false;
		}
	}

	if (is_drag_started && is_dragging == false && ImGui::IsItemActive())
	{
		// start the selection box only after a small movement threshold
		if (
			std::fabs(mouse_pos.x - time_to_x(drag_time)) > 4.0f ||
			std::fabs(mouse_pos.y - value_to_y(drag_value)) > 4.0f
		)
		{
			is_dragging = true;
		}
	}

	if (is_dragging)
	{
		// the 2d selection box
		float box_x0 = std::min(time_to_x(drag_time), mouse_pos.x);
		float box_y0 = std::min(value_to_y(drag_value), mouse_pos.y);
		float box_x1 = std::max(time_to_x(drag_time), mouse_pos.x);
		float box_y1 = std::max(value_to_y(drag_value), mouse_pos.y);

		p_draw_list->AddRectFilled(ImVec2(box_x0, box_y0), ImVec2(box_x1, box_y1), IM_COL32(90, 160, 255, 40));
		p_draw_list->AddRect(ImVec2(box_x0, box_y0), ImVec2(box_x1, box_y1), IM_COL32(90, 160, 255, 200));

		if (ImGui::IsMouseReleased(ImGuiMouseButton_Left))
		{
			float time_min = std::min(drag_time, pos_to_time(mouse_pos.x));
			float time_max = std::max(drag_time, pos_to_time(mouse_pos.x));
			float value_min = std::min(drag_value, pos_to_value(mouse_pos.y));
			float value_max = std::max(drag_value, pos_to_value(mouse_pos.y));

			selection.clear();

			for (st_Key* key : p_channel->keys)
			{
				// color marks span the full height, so their box selects by
				// time only; value dots are 2d points, they select by both
				bool is_inside = (key->time >= time_min && key->time <= time_max);

				if (is_inside && meta.kind == _ePPEParamKind::kValue)
				{
					is_inside = (key->value >= value_min && key->value <= value_max);
				}

				if (is_inside)
				{
					selection.push_back(key);
				}
			}

			is_drag_started = false;
			is_dragging = false;
		}
	}
	else if (is_drag_started && ImGui::IsMouseReleased(ImGuiMouseButton_Left))
	{
		// simple click on empty space
		if (state.timeline_place_on_click)
		{
			p_channel->InsertKey(drag_time, drag_value);
		}
		else
		{
			cursor = drag_time;
			selection.clear();
		}

		is_drag_started = false;
	}

	// ctrl+a: select all keys of the channel (works regardless of the mouse
	// position, but not while typing in a text field or while a modal is open)
	if (
		ImGui::IsKeyDown(ImGuiKey_LeftCtrl) &&
		ImGui::IsKeyPressed(ImGuiKey_A) &&
		ImGui::GetIO().WantTextInput == false &&
		g_ppe_tex_browser.is_open == false
	)
	{
		selection.clear();

		for (st_Key* key : p_channel->keys)
		{
			selection.push_back(key);
		}
	}

	// delete key: delete all selected keys (same conditions as ctrl+a)
	if (
		ImGui::IsKeyPressed(ImGuiKey_Delete) &&
		selection.empty() == false &&
		ImGui::GetIO().WantTextInput == false &&
		g_ppe_tex_browser.is_open == false
	)
	{
		PPEEditor_DeleteSelectedKeys(*p_channel, selection);
	}

	for (st_Key* key : p_channel->keys)
	{
		float key_x = time_to_x(key->time);
		float intensity = 0.25f + 0.75f * std::min(std::max(key->value, 0.0f), 1.0f);
		bool is_selected = std::find(selection.begin(), selection.end(), key) != selection.end();

		if (meta.kind == _ePPEParamKind::kColor)
		{
			// mark background tinted by the channel hue and the key value,
			// with a black outline so it reads on any ramp
			ImU32 mark_color = IM_COL32(
				(int)(_kChannelHues[selected_channel][0] * intensity * 255.0f),
				(int)(_kChannelHues[selected_channel][1] * intensity * 255.0f),
				(int)(_kChannelHues[selected_channel][2] * intensity * 255.0f),
				255
			);

			p_draw_list->AddRectFilled(ImVec2(key_x - 2.5f, canvas_pos.y), ImVec2(key_x + 2.5f, canvas_pos.y + canvas_size.y), IM_COL32(0, 0, 0, 255));
			p_draw_list->AddRectFilled(ImVec2(key_x - 1.0f, canvas_pos.y), ImVec2(key_x + 1.0f, canvas_pos.y + canvas_size.y), mark_color);

			if (is_selected || key == hovered_key)
			{
				ImU32 outline_color = is_selected ? IM_COL32(255, 255, 255, 255) : IM_COL32(255, 220, 90, 255);
				p_draw_list->AddRect(ImVec2(key_x - 3.5f, canvas_pos.y), ImVec2(key_x + 3.5f, canvas_pos.y + canvas_size.y), outline_color);
			}
		}
		else
		{
			ImU32 mark_color = IM_COL32((int)(90 * intensity), (int)(160 * intensity), 255, 255);
			float key_y = value_to_y(key->value);

			p_draw_list->AddCircleFilled(ImVec2(key_x, key_y), 4.5f, IM_COL32(0, 0, 0, 255));
			p_draw_list->AddCircleFilled(ImVec2(key_x, key_y), 3.0f, mark_color);

			if (is_selected || key == hovered_key)
			{
				ImU32 outline_color = is_selected ? IM_COL32(255, 255, 255, 255) : IM_COL32(255, 220, 90, 255);
				p_draw_list->AddCircle(ImVec2(key_x, key_y), 6.0f, outline_color);
			}
		}
	}

	// time cursor
	float cursor_x = time_to_x(cursor);
	p_draw_list->AddLine(ImVec2(cursor_x, canvas_pos.y), ImVec2(cursor_x, canvas_pos.y + canvas_size.y), IM_COL32(255, 255, 255, 220), 1.5f);

	// border + axis labels
	p_draw_list->AddRect(canvas_pos, ImVec2(canvas_pos.x + canvas_size.x, canvas_pos.y + canvas_size.y), IM_COL32(90, 90, 90, 255));

	p_draw_list->AddText(ImVec2(canvas_pos.x + 2.0f, canvas_pos.y + canvas_size.y + 2.0f), IM_COL32(140, 140, 140, 255), "0.00");

	char length_label[32];
	std::sprintf(length_label, "%.2f sec", axis_length);
	ImVec2 label_size = ImGui::CalcTextSize(length_label);
	p_draw_list->AddText(ImVec2(canvas_pos.x + canvas_size.x - label_size.x - 2.0f, canvas_pos.y + canvas_size.y + 2.0f), IM_COL32(140, 140, 140, 255), length_label);

	ImGui::Dummy(ImVec2(0.0f, 16.0f));

	// vertical axis range controls
	ImGui::Checkbox("auto y##PPERangeAuto", &range_auto);
	ImGui::SetItemTooltip("Automatic vertical range (fits the section's accepted range and the actual data)");

	if (range_auto == false)
	{
		ImGui::SameLine();
		ImGui::SetNextItemWidth(80.0f);
		ImGui::DragFloat("min##PPERangeMin", &range_min, 0.005f, 0.0f, 0.0f, "min=%.3f");

		ImGui::SameLine();
		ImGui::SetNextItemWidth(80.0f);
		ImGui::DragFloat("max##PPERangeMax", &range_max, 0.005f, 0.0f, 0.0f, "max=%.3f");
	}

	// mark hover tooltip: time and channel value of the key
	if (hovered_key)
	{
		ImGui::BeginTooltip();

		if (meta.kind == _ePPEParamKind::kColor)
		{
			constexpr const char* _kChannelNames[3] = {"R", "G", "B"};
			ImGui::Text("channel: %s", _kChannelNames[selected_channel]);
		}

		ImGui::Text("t=%.3f sec", hovered_key->time);
		ImGui::Text("v=%.3f", hovered_key->value);

		ImGui::EndTooltip();
	}
}

// row under the timeline: edit the selected key(s) (time/value/delete)
// or add a new key at the cursor position, plus the place-on-click toggle
void RenderPPEEditorUI_TimelineKeyRow(
	CEnvelope& channel,
	SPPEditorUIState& state,
	int param_index
)
{
	xr_vector<st_Key*>& selection = state.timeline_selected_keys[param_index];
	float& cursor = state.timeline_cursor[param_index];
	float& new_key_value = state.timeline_new_key_value[param_index];

	if (selection.empty())
	{
		ImGui::Text("new key at cursor:");
	}
	else if (selection.size() == 1)
	{
		ImGui::Text("selected key:");
	}
	else
	{
		ImGui::Text("%d keys selected", (int)selection.size());
	}

	if (selection.size() > 1)
	{
		ImGui::SameLine();

		if (ImGui::SmallButton("Delete all##PPEKeysDeleteAll"))
		{
			PPEEditor_DeleteSelectedKeys(channel, selection);
		}

		ImGui::SameLine();
		ImGui::Checkbox("place on click", &state.timeline_place_on_click);
		ImGui::SetItemTooltip("When enabled, clicking on the timeline places a key at the click position (x = time, y = value)");

		return;
	}

	ImGui::SameLine();
	ImGui::SetNextItemWidth(100.0f);

	st_Key* selected_key = selection.empty() ? nullptr : selection[0];

	if (selected_key)
	{
		ImGui::DragFloat("t##PPEKeyTime", &selected_key->time, 0.005f, 0.0f, 0.0f, "t=%.3f");

		if (ImGui::IsItemDeactivatedAfterEdit())
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
	}
	else
	{
		ImGui::DragFloat("t##PPEKeyTime", &cursor, 0.005f, 0.0f, 0.0f, "t=%.3f");
	}

	ImGui::SameLine();
	ImGui::SetNextItemWidth(100.0f);

	if (selected_key)
	{
		ImGui::DragFloat("v##PPEKeyValue", &selected_key->value, 0.005f, 0.0f, 0.0f, "v=%.3f");
	}
	else
	{
		ImGui::DragFloat("v##PPEKeyValue", &new_key_value, 0.005f, 0.0f, 0.0f, "v=%.3f");
	}

	ImGui::SameLine();

	if (selected_key == nullptr)
	{
		if (ImGui::SmallButton("Add##PPEKeyAdd"))
		{
			channel.InsertKey(cursor, new_key_value);
		}
	}
	else
	{
		if (ImGui::SmallButton("Delete##PPEKeyDelete"))
		{
			PPEEditor_DeleteSelectedKeys(channel, selection);
		}
	}

	ImGui::SameLine();
	ImGui::Checkbox("place on click", &state.timeline_place_on_click);
	ImGui::SetItemTooltip("When enabled, clicking on the timeline places a key at the click position (x = time, y = value)");
}

// color param: channel tabs (tinted by channel hue and its value at the
// cursor) + timeline in the left column, color picker in the right one
void RenderPPEEditorUI_ColorParamEditor(
	SPPEditorUIState& state,
	SPPEffectData& data,
	const _SPPEParamMeta& meta,
	float axis_length
)
{
	const int param_index = state.current_selected_param;
	SPPEffectData::SColorParam& param = data.colors[meta.index];
	float& cursor = state.timeline_cursor[param_index];
	int& selected_channel = state.timeline_selected_channel[param_index];

	CEnvelope* channels[3] = {&param.r, &param.g, &param.b};
	CEnvelope& channel = *channels[selected_channel];

	// evaluated color at the cursor: drives the picker and the read-only text,
	// so it is always in sync with the data (also right after loading)
	float color_at_cursor[3] = {};

	for (int i = 0; i < 3; ++i)
	{
		if (channels[i]->keys.empty() == false)
		{
			color_at_cursor[i] = channels[i]->Evaluate(cursor);
		}
	}

	if (ImGui::BeginTable("##PPEColorParamTable", 2))
	{
		ImGui::TableNextRow();

		ImGui::TableSetColumnIndex(0);

		ImGui::SetNextItemWidth(120.0f);
		ImGui::DragFloat("base value", &param.base, 0.005f);

		if (ImGui::BeginTabBar("##PPEChannelTabs"))
		{
			static const ImVec4 _kChannelHues[3] = {
				ImVec4(1.0f, 0.12f, 0.12f, 1.0f),
				ImVec4(0.12f, 1.0f, 0.12f, 1.0f),
				ImVec4(0.25f, 0.45f, 1.0f, 1.0f),
			};
			constexpr const char* _kChannelNames[3] = {"R", "G", "B"};

			for (int i = 0; i < 3; ++i)
			{
				float intensity = 0.25f + 0.75f * std::min(std::max(color_at_cursor[i], 0.0f), 1.0f);

				ImVec4 tab_color = _kChannelHues[i];
				tab_color.x *= intensity;
				tab_color.y *= intensity;
				tab_color.z *= intensity;

				ImVec4 tab_color_selected = _kChannelHues[i];

				// the selected tab is drawn with the full channel color
				ImGui::PushStyleColor(ImGuiCol_Tab, i == selected_channel ? tab_color_selected : tab_color);
				ImGui::PushStyleColor(ImGuiCol_TabHovered, tab_color_selected);
				ImGui::PushStyleColor(ImGuiCol_TabSelected, tab_color_selected);

				if (ImGui::TabItemButton(_kChannelNames[i]))
				{
					if (selected_channel != i)
					{
						selected_channel = i;
						state.timeline_selected_keys[param_index].clear();
					}
				}

				ImGui::PopStyleColor(3);
			}

			ImGui::EndTabBar();
		}

		ImGui::Text("t=%.3f  r=%.3f g=%.3f b=%.3f", cursor, color_at_cursor[0], color_at_cursor[1], color_at_cursor[2]);

		RenderPPEEditorUI_Timeline(state, data, meta, axis_length);
		RenderPPEEditorUI_TimelineKeyRow(channel, state, param_index);

		ImGui::TableSetColumnIndex(1);

		// picker edits all three channels at the cursor time:
		// updates the existing key when there is one, otherwise inserts it
		if (ImGui::ColorEdit3("##PPEColorPicker", color_at_cursor, ImGuiColorEditFlags_None))
		{
			for (int i = 0; i < 3; ++i)
			{
				KeyIt key_it = channels[i]->FindKey(cursor, 0.01f);

				if (key_it != channels[i]->keys.end())
				{
					(*key_it)->value = color_at_cursor[i];
				}
				else
				{
					channels[i]->InsertKey(cursor, color_at_cursor[i]);
				}
			}
		}

		ImGui::EndTable();
	}
}

// value param: read-only value at the cursor + curve timeline
void RenderPPEEditorUI_ValueParamEditor(
	SPPEditorUIState& state,
	SPPEffectData& data,
	const _SPPEParamMeta& meta,
	float axis_length
)
{
	const int param_index = state.current_selected_param;
	CEnvelope& channel = data.values[meta.index].v;
	float& cursor = state.timeline_cursor[param_index];

	float value_at_cursor = channel.keys.empty() ? 0.0f : channel.Evaluate(cursor);

	ImGui::Text("t=%.3f  v=%.3f", cursor, value_at_cursor);

	RenderPPEEditorUI_Timeline(state, data, meta, axis_length);
	RenderPPEEditorUI_TimelineKeyRow(channel, state, param_index);
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

void RenderPPEEditorUI_EffectBody(SPPEditorUIState& state, SPPEffectData& data, SPPEffectData& data_default)
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

	// effect-global length: scales the time of every key in every channel.
	// the edit value is re-synced from the data only while the drag is
	// not active, otherwise it would fight the user's input every frame
	ImGui::SetNextItemWidth(120.0f);
	ImGui::DragFloat("total length##ToolsInGameImGui_PPEEditor_TotalLength", &state.total_length_edit, 0.01f, 0.0f, 1000.0f, "%.2f sec");

	if (ImGui::IsItemDeactivatedAfterEdit() && state.total_length_edit > 0.0f && data.GetLength() > 0.0f)
	{
		data.ScaleLength(state.total_length_edit);

#if IXRAY_PPE_EDITOR_TAB_GAME == 1
		// restart the preview with the new length when it is playing
		if (&state == g_pPPEGame && g_pPPEGame && PPEEditor_IsPlayingInGame())
		{
			PPEEditor_PlayInGame(g_pPPEGame);
		}
#endif
	}

	if (ImGui::IsItemActive() == false)
	{
		state.total_length_edit = data.GetLength();
	}

	ImGui::SetItemTooltip("Total effect length: scales the time of every key in every channel");

	ImGui::Separator();

	ImGui::PushID(meta.name);

	float axis_length = data.GetLength() > 0.0f ? data.GetLength() : 1.0f;

	if (meta.kind == _ePPEParamKind::kColor)
	{
		RenderPPEEditorUI_ColorParamEditor(state, data, meta, axis_length);
	}
	else
	{
		RenderPPEEditorUI_ValueParamEditor(state, data, meta, axis_length);
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

		state.timeline_selected_keys[state.current_selected_param].clear();
	}

	ImGui::SameLine();

	if (ImGui::Button("Reset to default##ToolsInGameImGui_PPEEditor_ResetToDefault"))
	{
		// restore the state the effect had right after it was loaded/created
		PPEEditor_CopyData(data, data_default);
		state.Reset();

#if IXRAY_PPE_EDITOR_TAB_GAME == 1
		PPEEditor_ReplayPreviewIfPlaying(state);
#endif
	}

	ImGui::SetItemTooltip("Restores all parameters of the effect to the state they had right after the file was loaded or created");

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
	RenderPPEEditorUI_MenuBar(*g_pPPEEditor, g_pPPEEditor->data, g_pPPEEditor->data_default);
	RenderPPEEditorUI_FileSelector(*g_pPPEEditor, g_pPPEEditor->data, g_pPPEEditor->data_default);

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
					RenderPPEEditorUI_EffectBody(*g_pPPEEditor, g_pPPEEditor->data, g_pPPEEditor->data_default);
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

	RenderPPEEditorUI_MenuBar(*g_pPPEGame, g_pPPEGame->data, g_pPPEGame->data_default);
	RenderPPEEditorUI_FileSelector(*g_pPPEGame, g_pPPEGame->data, g_pPPEGame->data_default);

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

	RenderPPEEditorUI_EffectBody(*g_pPPEGame, g_pPPEGame->data, g_pPPEGame->data_default);
}
#endif

// ==============================================================
// Help tab: user manual
// ==============================================================

#if IXRAY_PPE_EDITOR_TAB_HELP == 1
void RenderPPEEditor_Draw_HelpTab()
{
	ImGui::SeparatorText("About this tool");

	ImGui::TextWrapped("This editor creates and edits postprocess effects (.ppe files). A postprocess effect is a small animation for the whole screen: it changes colors, blur, noise and other screen parameters over time. The game plays it when something happens: a hit, radiation, an anomaly, alcohol.");

	ImGui::SeparatorText("Basic theory (60 seconds)");

	ImGuiEditorUI_HelpBullet("An effect is a set of curves. There is one curve per screen parameter (blur, red channel, noise and so on).");
	ImGuiEditorUI_HelpBullet("A curve is built from keys. A key is one point: a time and a value.");
	ImGuiEditorUI_HelpBullet("Time is in seconds. The game draws a smooth line through your keys and reads the value from that line every frame.");
	ImGuiEditorUI_HelpBullet("The total length is the duration of the effect. Changing it stretches all keys at once, the shape stays the same.");
	ImGuiEditorUI_HelpBullet("A cyclic effect repeats until you stop it. A one-shot effect plays once and ends by itself.");

	ImGui::SeparatorText("How to create your own effect");

	ImGuiEditorUI_HelpBullet("1. Open File > New, or pick an existing effect in the combo at the top.");
	ImGuiEditorUI_HelpBullet("2. Choose a section in the Param combo, for example Blur.");
	ImGuiEditorUI_HelpBullet("3. Click on the timeline to place the white cursor where you want.");
	ImGuiEditorUI_HelpBullet("4. Press Add to create a key there, or drag the value / pick a color.");
	ImGuiEditorUI_HelpBullet("5. Add more keys at other times. The curve is drawn between them.");
	ImGuiEditorUI_HelpBullet("6. File > Save writes the .ppe file.");
	ImGuiEditorUI_HelpBullet("7. In the Game tab press Play to see the effect on screen, Stop to end it.");

	ImGui::SeparatorText("Reading the timeline");

	ImGuiEditorUI_HelpBullet("The axis is time: 0.0 on the left, the end of the effect on the right.");
	ImGuiEditorUI_HelpBullet("Vertical marks are your keys. Hover a mark to see its time and value.");
	ImGuiEditorUI_HelpBullet("Click a mark to edit or delete it. Click empty space to move the cursor.");
	ImGuiEditorUI_HelpBullet("With 'place on click' enabled, a click on the timeline places a key at the click position: x is the time, y is the value.");
	ImGuiEditorUI_HelpBullet("Color sections show the channel intensity as a colored ramp. Value sections show the curve as a line with the value range on the side.");

	ImGui::SeparatorText("Shortcuts and selection");

	ImGuiEditorUI_HelpBullet("Click on empty space: moves the time cursor (or places a key when 'place on click' is on).");
	ImGuiEditorUI_HelpBullet("Click on a mark: selects it. Its time and value can be edited in the row below, hovering shows the exact numbers.");
	ImGuiEditorUI_HelpBullet("Drag on empty space: draws a selection box. Color marks are full-height, so the box selects them by time; value dots are selected by time and value.");
	ImGuiEditorUI_HelpBullet("Ctrl + A: selects all keys of the current channel (not while typing in a text field).");
	ImGuiEditorUI_HelpBullet("Delete: deletes all selected keys (not while typing in a text field).");
	ImGuiEditorUI_HelpBullet("Escape: closes an open window (like the texture selector), a second press closes the editor.");

	ImGui::SeparatorText("Sections: colors");

	ImGuiEditorUI_HelpSection("Base color", "The main color tint of the screen. Values around 0.5 are neutral. Raise a channel to make the picture more red, green or blue, lower it to darken that channel. Example: more red for a warm scene.");
	ImGuiEditorUI_HelpSection("Add color", "Color added on top of the picture, it makes the screen brighter. 0 means nothing is added. Small values (0.05 - 0.3) make the screen glow. Good for flashes and explosions.");
	ImGuiEditorUI_HelpSection("Gray color", "The color the picture fades toward. It works together with Gray value: Gray value says how strong, Gray color says into which color. The default 0.333 is neutral gray.");
	ImGuiEditorUI_HelpSection("Gray value", "How much the picture turns into the Gray color. 0 = normal colors, 1 = fully tinted. Think of it as the mix knob for Gray color.");

	ImGui::SeparatorText("Sections: picture");

	ImGuiEditorUI_HelpSection("Blur", "How much the picture is blurred. 0 = sharp, higher values = stronger blur. Good for hits and dizziness.");
	ImGuiEditorUI_HelpSection("Duality H", "Double vision, shifted left-right. 0 = off. Small values give a slight ghost image next to the real one.");
	ImGuiEditorUI_HelpSection("Duality V", "The same as Duality H, but the ghost image is shifted up-down.");

	ImGui::SeparatorText("Sections: noise");

	ImGuiEditorUI_HelpSection("Noise intensity", "How strong the film grain (static noise) on the screen is. 0 = clean picture.");
	ImGuiEditorUI_HelpSection("Noise grain", "The size of the noise dots. Small values = fine grain, big values = large dots.");
	ImGuiEditorUI_HelpSection("Noise FPS", "How fast the noise flickers. Low values = slow flicker, high values = fast flicker. The game multiplies this value by 100 when playing.");

	ImGui::SeparatorText("Sections: colormap");

	ImGuiEditorUI_HelpSection("CM influence", "How strongly the colormap affects the picture. 0 = off, 1 = full power. It works only when a texture is set in cm_tex1.");
	ImGuiEditorUI_HelpSection("cm_tex1 (colormap texture)", "A colormap is a lookup table that repaints all screen colors (color grading), for example a cold blue world or an old photo look. Press Select... to pick a texture from the list, hover an entry to preview it.");

	ImGui::SeparatorText("Playback (Game tab)");

	ImGuiEditorUI_HelpBullet("Play starts the effect on your character. Pressing Play again replaces the running preview with your latest changes.");
	ImGuiEditorUI_HelpBullet("Stop fades the effect out.");
	ImGuiEditorUI_HelpBullet("The line under the buttons shows the current position and the total length, for cyclic and one-shot playback.");
	ImGuiEditorUI_HelpBullet("Cyclic can be toggled while the effect is playing, it applies immediately.");

	ImGui::SeparatorText("How sections combine");

	ImGuiEditorUI_HelpBullet("The game adds your effect on top of the normal picture. Empty channels do nothing at all.");
	ImGuiEditorUI_HelpBullet("Color sections repaint the picture: Base color multiplies it, Add color brightens it, Gray fades it into the Gray color by the Gray value.");
	ImGuiEditorUI_HelpBullet("Blur, Duality and Noise are applied on top of the colored picture. They all stack together.");
	ImGuiEditorUI_HelpBullet("The colormap (cm_tex1 + CM influence) repaints the final colors through a texture. It works only when both are set.");
	ImGuiEditorUI_HelpBullet("The factor of the effect fades everything in and out smoothly by itself, you do not need keys for that.");

	ImGui::SeparatorText("Examples");

	ImGuiEditorUI_HelpSection("Example: red hit (2 seconds)", "Base color: keys (0.0: r=0.5) -> (0.2: r=0.9) -> (2.0: r=0.5), keep g and b at 0.5. Blur: keys (0.0: 0) -> (0.2: 0.6) -> (2.0: 0). Total length: 2 sec. What you get: for two seconds the screen turns red and blurry, strongest at 0.2 sec, then everything fades back to normal.");
	ImGuiEditorUI_HelpSection("Example: white flash", "Add color: keys (0.0: r=g=b=0.6) -> (0.3: r=g=b=0). Total length: 0.3 sec. What you get: a short white flash. Add color brightens the picture, so the screen goes white and quickly returns to normal.");
	ImGuiEditorUI_HelpSection("Example: radioactive dirt", "Gray value: constant 0.2. Noise intensity: 0.3. Noise grain: 1.0. Noise FPS: 0.3. Blur: 0.1. Cyclic: on. What you get: a dirty, grainy, slightly blurred picture all the time, like standing in a radioactive area.");
	ImGuiEditorUI_HelpSection("Example: cold color grading", "cm_tex1: pick a cold or blue colormap with the Select... button. CM influence: constant 1.0. What you get: all colors are repainted through the texture, the world looks cold. Tip: lower CM influence to mix the grading with the normal colors.");
	ImGuiEditorUI_HelpSection("Example: ghost vision", "Duality H: keys (0.0: 0) -> (0.5: 0.03) -> (1.0: 0). Duality V: keep 0. Blur: 0.2 constant. Total length: 1 sec. What you get: the picture doubles sideways for a moment, like seeing double after a strong hit.");
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

#if IXRAY_PPE_EDITOR_TAB_HELP == 1
				if (ImGui::BeginTabItem("Help"))
				{
					RenderPPEEditor_Draw_HelpTab();

					ImGui::EndTabItem();
				}
#endif

				ImGui::EndTabBar();
			}
		}
		ImGui::End();
	}
}
