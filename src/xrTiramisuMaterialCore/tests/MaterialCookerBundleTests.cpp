#include "MaterialAsset.h"
#include "MaterialBundle.h"
#include "MaterialTestHarness.h"

#include <algorithm>
#include <array>
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <map>
#include <set>
#include <tuple>
#include <vector>

namespace
{
bool HasMagic(const FMaterialShaderBlob& Blob)
{
	if (Blob.Format == EMaterialShaderBlobFormat::Dxil)
	{
		constexpr xr_array<u8, 4> Magic = {'D', 'X', 'B', 'C'};
		return Blob.Bytecode.size() >= Magic.size() && std::equal(Magic.begin(), Magic.end(), Blob.Bytecode.begin());
	}
	constexpr xr_array<u8, 4> Magic = {0x03, 0x02, 0x23, 0x07};
	return Blob.Bytecode.size() >= Magic.size() && std::equal(Magic.begin(), Magic.end(), Blob.Bytecode.begin());
}

size_t CountSourceAssets(const std::filesystem::path& Directory)
{
	size_t Count = 0;
	for (const std::filesystem::directory_entry& Entry :
		 std::filesystem::recursive_directory_iterator(Directory))
	{
		if (!Entry.is_regular_file())
		{
			continue;
		}
		const xr_string Name = Entry.path().filename().string();
		if (Name.ends_with(".material.json") || Name.ends_with(".material-instance.json"))
		{
			++Count;
		}
	}
	return Count;
}
} // namespace

int main(const int ArgumentCount, char** ArgumentValues)
{
	TiramisuMaterialTestRunner Runner("xrMaterialCookerBundleTests");
	MATERIAL_CHECK(Runner, ArgumentCount == 3);
	MATERIAL_CHECK(Runner, ArgumentCount >= 3 && xr_string_view(ArgumentValues[2]) == "-rdbg");
	if (ArgumentCount != 3 || xr_string_view(ArgumentValues[2]) != "-rdbg")
	{
		return Runner.Finish();
	}

	std::ifstream Input(std::filesystem::path(ArgumentValues[1]), std::ios::binary);
	const xr_vector<u8> Data((std::istreambuf_iterator<char>(Input)), std::istreambuf_iterator<char>());
	MATERIAL_CHECK(Runner, !Data.empty());
	const FMaterialBundleReadResult Read = DeserializeMaterialBundle(Data);
	MATERIAL_CHECK(Runner, Read.Succeeded());
	if (!Read.Succeeded())
	{
		return Runner.Finish();
	}

	MATERIAL_CHECK(Runner, !Read.Value.CompleteShaderSet);
	MATERIAL_CHECK(Runner, Read.Value.Records.size() == CountSourceAssets("gamedata/render_materials"));
	MATERIAL_CHECK(Runner, !Read.Value.ShaderBlobs.empty());
	// Each pass/permutation produces vertex and pixel shaders for both backends.
	// The total is not necessarily divisible by the old opaque-only 16-blob
	// stride once forward/UI/post-process masters are present.
	MATERIAL_CHECK(Runner, Read.Value.ShaderBlobs.size() % 4 == 0);

	xr_map<xr_string, xr_vector<EMaterialPass>> ExpectedPassesByMaterial;
	for (const FMaterialBundleRecord& Record : Read.Value.Records)
	{
		if (Record.Type != EMaterialBundleRecordType::Master)
		{
			continue;
		}

		const FMaterialAssetParseResult Parsed = ParseMaterialAssetJson(Record.AssetPayload, Record.SourcePath);
		MATERIAL_CHECK(Runner, Parsed.Succeeded());
		if (!Parsed.Succeeded())
		{
			continue;
		}

		xr_vector<EMaterialPass> Passes = GetRequiredMaterialPasses(Parsed.Value);
		Passes.push_back(EMaterialPass::Validation);
		MATERIAL_CHECK(Runner, ExpectedPassesByMaterial.emplace(Record.Id.Value, std::move(Passes)).second);
	}

	size_t DxilCount = 0;
	size_t SpirVCount = 0;
	xr_set<std::tuple<xr_string, u64, EMaterialShaderBlobFormat, EMaterialShaderStage>> UniqueBlobs;
	xr_set<u64> StandardPipelineKeys;
	xr_map<EMaterialPass, size_t> PassCounts;
	xr_map<xr_string, xr_map<EMaterialPass, size_t>> MaterialPassCounts;
	xr_map<EMaterialShaderStage, size_t> StageCounts;
	for (const FMaterialShaderBlob& Blob : Read.Value.ShaderBlobs)
	{
		MATERIAL_CHECK(Runner, Blob.EntryPoint == "Main");
		MATERIAL_CHECK(Runner, Blob.Bytecode.size() > 128);
		MATERIAL_CHECK(Runner, HasMagic(Blob));
		MATERIAL_CHECK(Runner, UniqueBlobs.emplace(Blob.MaterialId.Value, Blob.PipelineKey, Blob.Format, Blob.Stage).second);
		const FMaterialPassDefinition* Definition = FindMaterialPassDefinition(Blob.Pass);
		MATERIAL_CHECK(Runner, Definition != nullptr);
		if (Definition)
		{
			MATERIAL_CHECK(Runner, Blob.VertexFactory == Definition->VertexFactory);
			MATERIAL_CHECK(Runner, Blob.RenderPassSignature == Definition->RenderPassSignature);
			if (Blob.Stage == EMaterialShaderStage::Pixel)
			{
				MATERIAL_CHECK(Runner, Blob.EntryPoint == Definition->EntryPoint);
			}
			else
			{
				const FMaterialVertexFactoryDefinition* VertexFactory =
					FindMaterialVertexFactoryDefinition(Blob.VertexFactory);
				MATERIAL_CHECK(Runner, VertexFactory != nullptr);
				if (VertexFactory)
				{
					MATERIAL_CHECK(Runner, Blob.EntryPoint == VertexFactory->EntryPoint);
				}
			}
		}
		++PassCounts[Blob.Pass];
		++MaterialPassCounts[Blob.MaterialId.Value][Blob.Pass];
		const auto ExpectedMaterial = ExpectedPassesByMaterial.find(Blob.MaterialId.Value);
		MATERIAL_CHECK(Runner, ExpectedMaterial != ExpectedPassesByMaterial.end());
		if (ExpectedMaterial != ExpectedPassesByMaterial.end())
		{
			MATERIAL_CHECK(Runner, std::ranges::find(ExpectedMaterial->second, Blob.Pass) != ExpectedMaterial->second.end());
		}
		++StageCounts[Blob.Stage];
		if (Blob.Format == EMaterialShaderBlobFormat::Dxil)
		{
			++DxilCount;
		}
		else
		{
			++SpirVCount;
		}
		if (Blob.MaterialId.Value == "67e3bc21-9df5-4fc2-ab60-1ad7d02ad6e3")
		{
			StandardPipelineKeys.insert(Blob.PipelineKey);
		}
	}
	MATERIAL_CHECK(Runner, DxilCount == SpirVCount);
	MATERIAL_CHECK(Runner, StageCounts[EMaterialShaderStage::Vertex] == StageCounts[EMaterialShaderStage::Pixel]);
	MATERIAL_CHECK(Runner, DxilCount + SpirVCount == Read.Value.ShaderBlobs.size());
	MATERIAL_CHECK(Runner, PassCounts.contains(EMaterialPass::Forward));
	for (const auto& [MaterialId, ExpectedPasses] : ExpectedPassesByMaterial)
	{
		const auto MaterialCounts = MaterialPassCounts.find(MaterialId);
		MATERIAL_CHECK(Runner, MaterialCounts != MaterialPassCounts.end());
		if (MaterialCounts == MaterialPassCounts.end())
		{
			continue;
		}

		MATERIAL_CHECK(Runner, MaterialCounts->second.size() == ExpectedPasses.size());
		const size_t ExpectedPerPass = MaterialCounts->second.contains(EMaterialPass::Validation) ? MaterialCounts->second.at(EMaterialPass::Validation) : 0;
		MATERIAL_CHECK(Runner, ExpectedPerPass > 0);
		MATERIAL_CHECK(Runner, ExpectedPerPass % 4 == 0);
		for (const EMaterialPass Pass : ExpectedPasses)
		{
			const auto Count = MaterialCounts->second.find(Pass);
			MATERIAL_CHECK(Runner, Count != MaterialCounts->second.end());
			if (Count != MaterialCounts->second.end())
			{
				MATERIAL_CHECK(Runner, Count->second == ExpectedPerPass);
			}
		}
	}
	// Two static permutations, four passes and two backends produce sixteen cache keys.
	MATERIAL_CHECK(Runner, StandardPipelineKeys.size() == 16);
	return Runner.Finish();
}
