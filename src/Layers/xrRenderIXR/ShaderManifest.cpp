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
#include "stdafx.h"
#include "ShaderManifest.h"
#include "ResourceManager.h"
#include "dxRenderDeviceRender.h"

namespace
{
constexpr u32 ManifestVersion = 2;
// A stage is learned for the frame it was made in: a surface shader of the forward level frame does not compile with
// the defines of the frame that shades a pixel once, and the other way round
const xr_string PresetKey = ".preset";
constexpr u32 ManifestLimit = 1u << 16;

void ManifestPath(string_path& Path)
{
	FS.update_path(Path, "$app_data_root$", "learned_shaders.bin");
}
} // namespace

bool ShaderManifest::Add(Record&& Item)
{
	xr_vector<xr_string> Parts;

	for (const auto& [Name, Value] : Item.Defines)
	{
		Parts.push_back(Name + '=' + Value);
	}

	std::sort(Parts.begin(), Parts.end());
	xr_string Key;
	Key += Item.Stage;
	Key += Item.Name;

	for (const xr_string& Part : Parts)
	{
		Key += '\n';
		Key += Part;
	}

	if (Records.size() >= ManifestLimit || !Keys.insert(Key).second)
	{
		return false;
	}

	Records.push_back(std::move(Item));
	return true;
}

void ShaderManifest::Load()
{
	if (IsLoaded)
	{
		return;
	}

	IsLoaded = true;
	string_path Path;
	ManifestPath(Path);
	IReader* Reader = FS.exist(Path) ? FS.r_open(Path) : nullptr;

	if (!Reader)
	{
		return;
	}

	if (Reader->length() >= 2 * sizeof(u32) && Reader->r_u32() == ManifestVersion)
	{
		const u32 Count = Reader->r_u32();

		for (u32 Index = 0; Index < Count && !Reader->eof(); ++Index)
		{
			Record Item;
			Item.Stage = char(Reader->r_u8());
			Reader->r_stringZ(Item.Name);
			const u32 Defines = Reader->r_u32();

			for (u32 Define = 0; Define < Defines && !Reader->eof(); ++Define)
			{
				xr_string Name, Value;
				Reader->r_stringZ(Name);
				Reader->r_stringZ(Value);
				Item.Defines[Name] = Value;
			}

			Add(std::move(Item));
		}
	}

	FS.r_close(Reader);
}

void ShaderManifest::Remember(char Stage, const char* Name, const ShaderExternalMap& Defines)
{
	xrCriticalSectionGuard Lock(Guard);
	Load();
	ShaderExternalMap Tagged = Defines;
	Tagged[PresetKey] = "forward";
	IsChanged |= Add({ Stage, Name, Tagged });
}

void ShaderManifest::Save()
{
	xrCriticalSectionGuard Lock(Guard);

	if (!IsChanged)
	{
		return;
	}

	string_path Path;
	ManifestPath(Path);
	IWriter* Writer = FS.w_open(Path);

	if (!Writer)
	{
		return;
	}

	Writer->w_u32(ManifestVersion);
	Writer->w_u32(u32(Records.size()));

	for (const Record& Item : Records)
	{
		Writer->w_u8(u8(Item.Stage));
		Writer->w_stringZ(Item.Name.c_str());
		Writer->w_u32(u32(Item.Defines.size()));

		for (const auto& [Name, Value] : Item.Defines)
		{
			Writer->w_stringZ(Name.c_str());
			Writer->w_stringZ(Value.c_str());
		}
	}

	FS.w_close(Writer);
	IsChanged = false;
	Msg("* shader manifest: %u stages saved", u32(Records.size()));
}

void ShaderManifest::OnAppStart()
{
	xr_vector<Record> Made;
	{
		xrCriticalSectionGuard Lock(Guard);
		Load();
		Made = Records;
	}

	const xr_string Preset = "forward";
	std::erase_if(Made, [&](const Record& Item)
	{
		const auto Found = Item.Defines.find(PresetKey);
		return Found == Item.Defines.end() || Found->second != Preset;
	});

	if (Made.empty())
	{
		return;
	}

	CTimer Timer;
	Timer.Start();
	RImplementation.LogShaderTimes("the start screen");
	CResourceManager& Resources = *dxRenderDeviceRender::Instance().Resources;
	std::atomic<u32> Done{ 0 };

	// One loop over all of them: every worker takes the next stage as it finishes one. In batches the workers
	// waited for the slowest stage of each batch, and most of them stood idle
	CRenderDevice::StartupFrameWaitTask([&]()
	{
		xr_parallel_for(size_t(0), Made.size(), [&](size_t Index)
		{
			ShaderExternalMap Defines = Made[Index].Defines;
			Defines.erase(PresetKey);
			Resources.KeepStage(Made[Index].Stage, Made[Index].Name.c_str(), Defines);
			++Done;
		});
	}, 78, "Precompiling shaders", [&]()
	{
		string128 Status;
		xr_sprintf(Status, "Precompiling shaders %u of %u", Done.load(), u32(Made.size()));
		CRenderDevice::SetStartupStatus(78 + int(8 * size_t(Done.load()) / Made.size()), Status);
	});

	Msg("* shader manifest: %u stages made behind the start screen in %u ms", u32(Made.size()), Timer.GetElapsed_ms());
	RImplementation.LogShaderTimes("the end of the precompile");

	// The pipelines of these stages, as the draws of earlier runs asked for them
	const u32 Pipelines = GRHI->DevicePtr->LearnedPipelineCount();
	std::atomic<u32> Created{ 0 };
	Timer.Start();

	Done = 0;

	if (Pipelines)
	{
		CRenderDevice::StartupFrameWaitTask([&]()
		{
			xr_parallel_for(size_t(0), size_t(Pipelines), [&](size_t Index)
			{
				Created += GRHI->DevicePtr->PrecreatePipeline(u32(Index)) ? 1u : 0u;
				++Done;
			});
		}, 86, "Precompiling pipelines", [&]()
		{
			string128 Status;
			xr_sprintf(Status, "Precompiling pipelines %u of %u", Done.load(), Pipelines);
			CRenderDevice::SetStartupStatus(86 + int(6 * Done.load() / Pipelines), Status);
		});
	}

	if (Pipelines)
	{
		Msg("* shader manifest: %u of %u pipelines made behind the start screen in %u ms", Created.load(), Pipelines, Timer.GetElapsed_ms());
	}
}
