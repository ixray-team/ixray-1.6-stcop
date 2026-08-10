#include "stdafx.h"
#include "UIBatchTools.h"

#include "../../../plugins/PCore/xr_ogf_format.h"

#include <algorithm>
#include <filesystem>

namespace
{
enum EBatchTarget
{
	btTexture,
	btShader,
	btUserData,
	btMotionRefs,
	btLOD
};

enum EBatchAction
{
	baReplace,
	baAdd,
	baDelete
};

struct SBatchRequest
{
	int target;
	int action;
	const char* find;
	const char* value;
	bool substrings;
	bool createMissing;
	u32 changes = 0;
};

xr_string Normalize(xr_string value)
{
	value.erase(std::remove_if(value.begin(), value.end(), [](char c)
	{
		return c == ' ' || c == '\r' || c == '\n';
	}), value.end());
	return value;
}

xr_vector<xr_string> Split(const xr_string& value, char separator)
{
	xr_vector<xr_string> result;
	size_t begin = 0;
	for (size_t end = 0; end <= value.size(); ++end)
	{
		if (end != value.size() && value[end] != separator)
			continue;

		xr_string item = value.substr(begin, end - begin);
		if (!item.empty() && item.back() == '\r')
			item.pop_back();
		if (!item.empty())
			result.push_back(std::move(item));
		begin = end + 1;
	}
	return result;
}

xr_string Join(const xr_vector<xr_string>& values, const char* separator)
{
	xr_string result;
	for (const xr_string& value : values)
	{
		if (!result.empty())
			result += separator;
		result += value;
	}
	return result;
}

bool Replace(xr_string& value, const SBatchRequest& request, u32& changes, bool normalized)
{
	if (!request.substrings)
	{
		if (normalized ? Normalize(value) != Normalize(request.find) : value != request.find)
			return false;

		value = request.value;
		++changes;
		return true;
	}

	bool changed = false;
	const size_t findSize = xr_strlen(request.find);
	for (size_t pos = 0; (pos = value.find(request.find, pos)) != xr_string::npos;)
	{
		value.replace(pos, findSize, request.value);
		pos += xr_strlen(request.value);
		++changes;
		changed = true;
	}
	return changed;
}

bool EditList(xr_vector<xr_string>& values, SBatchRequest& request, char valueSeparator)
{
	bool changed = false;
	if (request.action == baReplace)
	{
		for (xr_string& value : values)
			changed |= Replace(value, request, request.changes, true);
	}
	else if (request.action == baAdd)
	{
		for (const xr_string& addition : Split(request.value, valueSeparator))
		{
			const xr_string normalized = Normalize(addition);
			const bool exists = std::any_of(values.begin(), values.end(), [&](const xr_string& value)
			{
				return Normalize(value) == normalized;
			});
			if (!exists)
			{
				values.push_back(addition);
				++request.changes;
				changed = true;
			}
		}
	}
	else if (0 == xr_strcmp(request.find, "$all"))
	{
		const u32 count = static_cast<u32>(values.size());
		values.clear();
		request.changes += count;
		changed = count != 0;
	}
	else
	{
		const xr_string normalized = Normalize(request.find);
		for (auto it = values.begin(); it != values.end();)
		{
			if (Normalize(*it) == normalized)
			{
				it = values.erase(it);
				++request.changes;
				changed = true;
			}
			else
				++it;
		}
	}
	return changed;
}

bool EditScalar(xr_string& value, SBatchRequest& request)
{
	if (request.action == baReplace)
		return Replace(value, request, request.changes, true);
	if (request.action == baAdd)
	{
		if (!value.empty())
			return false;
		value = request.value;
		++request.changes;
		return true;
	}
	if (0 != xr_strcmp(request.find, "$all") && Normalize(value) != Normalize(request.find))
		return false;

	if (value.empty())
		return false;
	value.clear();
	++request.changes;
	return true;
}

void CopyChunk(CMemoryWriter& output, u32 id, IReader& chunk)
{
	output.w_chunk(id, chunk.pointer(), static_cast<u32>(chunk.length()));
}

u8 OGFVersion(IReader& source)
{
	IReader* header = source.open_chunk(xray_re::OGF_HEADER);
	if (!header)
		return 0;
	const u8 version = header->r_u8();
	header->close();
	return version;
}

bool RewriteOGF(IReader& source, CMemoryWriter& output, SBatchRequest& request, bool root)
{
	const u8 version = OGFVersion(source);
	if (version != xray_re::OGF3_VERSION && version != xray_re::OGF4_VERSION)
		return false;

	const bool v4 = version == xray_re::OGF4_VERSION;
	const u32 childrenId = v4 ? xray_re::OGF4_CHILDREN : xray_re::OGF3_CHILDREN;
	const u32 userDataId = v4 ? xray_re::OGF4_S_USERDATA : xray_re::OGF3_S_USERDATA;
	const u32 motionRefsId = v4 ? xray_re::OGF4_S_MOTION_REFS_0 : xray_re::OGF3_S_MOTION_REFS;
	const u32 motionRefs2Id = v4 ? xray_re::OGF4_S_MOTION_REFS_1 : u32(-1);
	const u32 lodId = v4 ? xray_re::OGF4_S_LODS : u32(-1);
	bool targetSeen = false;

	u32 id = 0;
	for (IReader* chunk = source.open_chunk_iterator(id); chunk; chunk = source.open_chunk_iterator(id, chunk))
	{
		const u32 chunkId = id & ~CFS_CompressMark;
		if (chunkId == childrenId && request.target <= btShader)
		{
			CMemoryWriter children;
			u32 childId = 0;
			for (IReader* child = chunk->open_chunk_iterator(childId); child;
				child = chunk->open_chunk_iterator(childId, child))
			{
				CMemoryWriter visual;
				if (!RewriteOGF(*child, visual, request, false))
				{
					child->close();
					chunk->close();
					return false;
				}
				children.w_chunk(childId & ~CFS_CompressMark, visual.pointer(), visual.size());
			}
			output.w_chunk(chunkId, children.pointer(), children.size());
		}
		else if (chunkId == (v4 ? xray_re::OGF4_TEXTURE : xray_re::OGF3_TEXTURE) &&
			request.target <= btShader)
		{
			xr_string texture;
			xr_string shader;
			chunk->r_stringZ(texture);
			chunk->r_stringZ(shader);
			Replace(request.target == btTexture ? texture : shader, request, request.changes, false);

			output.open_chunk(chunkId);
			output.w_stringZ(texture);
			output.w_stringZ(shader);
			output.close_chunk();
		}
		else if (root && chunkId == userDataId && request.target == btUserData)
		{
			targetSeen = true;
			xr_string value;
			chunk->r_stringZ(value);
			xr_vector<xr_string> lines = Split(value, '\n');
			EditList(lines, request, '\n');
			value = Join(lines, "\r\n");
			output.open_chunk(chunkId);
			output.w_stringZ(value);
			output.close_chunk();
		}
		else if (root && (chunkId == motionRefsId || chunkId == motionRefs2Id) &&
			request.target == btMotionRefs)
		{
			targetSeen = true;
			xr_vector<xr_string> refs;
			if (chunkId == motionRefs2Id)
			{
				refs.resize(chunk->r_u32());
				for (xr_string& ref : refs)
					chunk->r_stringZ(ref);
			}
			else
			{
				xr_string value;
				chunk->r_stringZ(value);
				refs = Split(value, ',');
			}

			EditList(refs, request, '\n');
			output.open_chunk(chunkId);
			if (chunkId == motionRefs2Id)
			{
				output.w_u32(static_cast<u32>(refs.size()));
				for (const xr_string& ref : refs)
					output.w_stringZ(ref);
			}
			else
				output.w_stringZ(Join(refs, ","));
			output.close_chunk();
		}
		else if (root && chunkId == lodId && request.target == btLOD)
		{
			targetSeen = true;
			if (chunk->length() > 0 && chunk->length() <= MAX_PATH &&
				static_cast<const char*>(chunk->pointer())[chunk->length() - 1] == '\0')
			{
				xr_string value;
				chunk->r_stringZ(value);
				EditScalar(value, request);
				output.open_chunk(chunkId);
				output.w_stringZ(value);
				output.close_chunk();
			}
			else
				CopyChunk(output, chunkId, *chunk);
		}
		else
			CopyChunk(output, chunkId, *chunk);
	}

	if (root && request.action == baAdd && request.createMissing && !targetSeen)
	{
		if (request.target == btUserData)
		{
			output.open_chunk(userDataId);
			output.w_stringZ(request.value);
			output.close_chunk();
			request.changes += static_cast<u32>(Split(request.value, '\n').size());
		}
		else if (request.target == btMotionRefs)
		{
			const xr_vector<xr_string> refs = Split(request.value, '\n');
			output.open_chunk(v4 ? motionRefs2Id : motionRefsId);
			if (v4)
			{
				output.w_u32(static_cast<u32>(refs.size()));
				for (const xr_string& ref : refs)
					output.w_stringZ(ref);
			}
			else
				output.w_stringZ(Join(refs, ","));
			output.close_chunk();
			request.changes += static_cast<u32>(refs.size());
		}
		else if (request.target == btLOD && v4)
		{
			output.open_chunk(lodId);
			output.w_stringZ(request.value);
			output.close_chunk();
			++request.changes;
		}
	}
	return true;
}
}

CUIBatchTools::CUIBatchTools()
{
	bOpen = false;
}

CUIBatchTools& CUIBatchTools::Instance()
{
	static CUIBatchTools form;
	static bool registered = false;
	if (!registered)
	{
		UI->Push(&form, false);
		registered = true;
	}
	return form;
}

void CUIBatchTools::Browse()
{
	xr_string file = m_Folder;
	if (EFS.GetOpenName("$fs_root$", file, false, m_Folder, -1, "*.ogf"))
		xr_strcpy(m_Folder, sizeof(m_Folder), EFS.ExtractFilePath(file.c_str()).c_str());
}

void CUIBatchTools::Run()
{
	namespace fs = std::filesystem;
	std::error_code error;
	if (!fs::is_directory(m_Folder, error))
	{
		m_Status = "Folder does not exist.";
		return;
	}
	if ((m_Action == baReplace || m_Action == baDelete) && !m_Find[0])
	{
		m_Status = "Fill in the search/delete field.";
		return;
	}
	if (m_Action != baDelete && !m_Value[0])
	{
		m_Status = "Fill in the value field.";
		return;
	}

	SBatchRequest request{m_Target, m_Action, m_Find, m_Value, m_Substrings, m_CreateMissing};
	u32 scanned = 0;
	u32 changedFiles = 0;
	u32 failed = 0;
	const auto process = [&](const fs::path& path)
	{
		++scanned;
		IReader* source = FS.r_open(path.string().c_str());
		if (!source)
		{
			++failed;
			return;
		}

		const u32 oldChanges = request.changes;
		CMemoryWriter output;
		const bool valid = RewriteOGF(*source, output, request, true);
		FS.r_close(source);
		if (!valid)
		{
			request.changes = oldChanges;
			++failed;
		}
		else if (request.changes != oldChanges)
		{
			if (output.save_to(path.string().c_str()))
				++changedFiles;
			else
			{
				request.changes = oldChanges;
				++failed;
			}
		}
	};

	if (m_Recursive)
	{
		for (fs::recursive_directory_iterator it(m_Folder, fs::directory_options::skip_permission_denied, error), end;
			it != end; it.increment(error))
		{
			if (error)
			{
				error.clear();
				continue;
			}
			if (it->is_regular_file(error) && 0 == xr_stricmp(it->path().extension().string().c_str(), ".ogf"))
				process(it->path());
		}
	}
	else
	{
		for (fs::directory_iterator it(m_Folder, fs::directory_options::skip_permission_denied, error), end;
			it != end; it.increment(error))
		{
			if (error)
			{
				error.clear();
				continue;
			}
			if (it->is_regular_file(error) && 0 == xr_stricmp(it->path().extension().string().c_str(), ".ogf"))
				process(it->path());
		}
	}

	m_Status = make_string<xr_string>("Scanned: %u, changed files: %u, changed values: %u, failed: %u",
		scanned, changedFiles, request.changes, failed);
	ELog.Msg(failed ? mtError : mtInformation, "%s", m_Status.c_str());
}

void CUIBatchTools::Draw()
{
	if (!bOpen)
		return;

	ImGui::SetNextWindowSize(ImVec2(620, 390), ImGuiCond_FirstUseEver);
	if (ImGui::Begin("Batch Tools", &bOpen))
	{
		ImGui::InputText("Folder", m_Folder, sizeof(m_Folder));
		ImGui::SameLine();
		if (ImGui::Button("Browse..."))
			Browse();
		ImGui::Checkbox("Recursive", &m_Recursive);

		const char* targets[] = {"Texture", "Shader", "User Data", "Motion Refs", "LOD"};
		ImGui::Combo("Target", &m_Target, targets, IM_ARRAYSIZE(targets));

		const char* actions[] = {"Replace", "Add", "Delete"};
		if (m_Target <= btShader)
			m_Action = baReplace;
		ImGui::BeginDisabled(m_Target <= btShader);
		ImGui::Combo("Action", &m_Action, actions, IM_ARRAYSIZE(actions));
		ImGui::EndDisabled();

		if (m_Action != baAdd)
		{
			ImGui::InputText(m_Action == baDelete ? "Delete" : "Find", m_Find, sizeof(m_Find));
			if (m_Action == baDelete && m_Target >= btUserData)
				ImGui::TextDisabled("Use $all to clear all values.");
		}
		if (m_Action != baDelete)
		{
			if (m_Action == baAdd && m_Target >= btUserData && m_Target != btLOD)
				ImGui::InputTextMultiline("Value", m_Value, sizeof(m_Value), ImVec2(-1, 100));
			else
				ImGui::InputText("Value", m_Value, sizeof(m_Value));
		}
		if (m_Action == baReplace)
			ImGui::Checkbox("Replace substrings", &m_Substrings);
		if (m_Action == baAdd && m_Target >= btUserData)
			ImGui::Checkbox("Create missing chunk", &m_CreateMissing);

		ImGui::Separator();
		if (ImGui::Button("Apply to OGF files"))
			Run();
		if (!m_Status.empty())
			ImGui::TextWrapped("%s", m_Status.c_str());
	}
	ImGui::End();
}
