#include "MaterialBundle.h"
#include "MaterialTestHarness.h"

#include <algorithm>
#include <array>
#include <cstdint>
#include <vector>

namespace
{
FMaterialBundle MakeCompleteBundle()
{
	FMaterialBundle Bundle;
	Bundle.CompleteShaderSet = true;
	Bundle.Records = {
		{EMaterialBundleRecordType::FlattenedInstance, {"instance-b"}, {"master-a"}, "b.json", "payload-b", "", {"z", "a", "a"}},
		{EMaterialBundleRecordType::Master, {"master-a"}, {"master-a"}, "a.json", "payload-a", "hlsl-a", {"template"}},
	};
	Bundle.ShaderBlobs = {
		{{"master-a"}, 20, EMaterialShaderBlobFormat::SpirV, "main", {4, 5, 6}, EMaterialPass::Shadow, EMaterialShaderStage::Pixel, "level_static", "shadow:d32"},
		{{"master-a"}, 10, EMaterialShaderBlobFormat::Dxil, "main", {1, 2, 3}, EMaterialPass::GBuffer, EMaterialShaderStage::Pixel, "level_static", "gbuffer:rgba8+rgba16f+rgba16f+rg16f:d24s8"},
	};
	return Bundle;
}

void TestDeterministicSerialization(TiramisuMaterialTestRunner& Runner)
{
	FMaterialBundle FirstBundle = MakeCompleteBundle();
	const FMaterialBundleWriteResult First = SerializeMaterialBundle(FirstBundle);
	MATERIAL_CHECK(Runner, First.Succeeded());

	std::ranges::reverse(FirstBundle.Records);
	std::ranges::reverse(FirstBundle.ShaderBlobs);
	const FMaterialBundleWriteResult Second = SerializeMaterialBundle(FirstBundle);
	MATERIAL_CHECK(Runner, Second.Succeeded());
	MATERIAL_CHECK(Runner, First.Data == Second.Data);

	const FMaterialBundleReadResult Read = DeserializeMaterialBundle(First.Data);
	MATERIAL_CHECK(Runner, Read.Succeeded());
	MATERIAL_CHECK(Runner, Read.Value.CompleteShaderSet);
	MATERIAL_CHECK(Runner, Read.Value.Records.size() == 2);
	MATERIAL_CHECK(Runner, Read.Value.ShaderBlobs.size() == 2);
	MATERIAL_CHECK(Runner, Read.Value.Records[0].Dependencies == xr_vector<xr_string>({"a", "z"}));
	MATERIAL_CHECK(Runner, Read.Value.ShaderBlobs[0].Format == EMaterialShaderBlobFormat::Dxil);
	MATERIAL_CHECK(Runner, Read.Value.ShaderBlobs[0].Bytecode == xr_vector<u8>({1, 2, 3}));
	MATERIAL_CHECK(Runner, Read.Value.ShaderBlobs[0].Pass == EMaterialPass::GBuffer);
	MATERIAL_CHECK(Runner, Read.Value.ShaderBlobs[0].Stage == EMaterialShaderStage::Pixel);
	MATERIAL_CHECK(Runner, Read.Value.ShaderBlobs[0].VertexFactory == "level_static");
	MATERIAL_CHECK(Runner, Read.Value.ShaderBlobs[0].RenderPassSignature == "gbuffer:rgba8+rgba16f+rgba16f+rg16f:d24s8");
}

void TestIncompleteDevelopmentBundle(TiramisuMaterialTestRunner& Runner)
{
	FMaterialBundle Development;
	Development.CompleteShaderSet = false;
	Development.Records.push_back(
		{EMaterialBundleRecordType::Master, {"development-master"}, {"development-master"}, "master.json", "payload", "source-hlsl", {}}
	);
	const FMaterialBundleWriteResult Serialized = SerializeMaterialBundle(Development);
	MATERIAL_CHECK(Runner, Serialized.Succeeded());
	const FMaterialBundleReadResult Read = DeserializeMaterialBundle(Serialized.Data);
	MATERIAL_CHECK(Runner, Read.Succeeded());
	MATERIAL_CHECK(Runner, !Read.Value.CompleteShaderSet);
	MATERIAL_CHECK(Runner, Read.Value.ShaderBlobs.empty());

	FMaterialBundle InvalidComplete;
	InvalidComplete.CompleteShaderSet = true;
	const FMaterialBundleWriteResult Invalid = SerializeMaterialBundle(InvalidComplete);
	MATERIAL_CHECK(Runner, !Invalid.Succeeded());
	MATERIAL_CHECK(Runner, HasDiagnostic(Invalid.Diagnostics, "bundle.missing_shader_blobs"));
}

void TestCorruptionAndTruncation(TiramisuMaterialTestRunner& Runner)
{
	const FMaterialBundleWriteResult Serialized = SerializeMaterialBundle(MakeCompleteBundle());
	MATERIAL_CHECK(Runner, Serialized.Succeeded());

	xr_vector<u8> Corrupted = Serialized.Data;
	Corrupted[12] ^= 0x1u;
	const FMaterialBundleReadResult CorruptResult = DeserializeMaterialBundle(Corrupted);
	MATERIAL_CHECK(Runner, !CorruptResult.Succeeded());
	MATERIAL_CHECK(Runner, HasDiagnostic(CorruptResult.Diagnostics, "bundle.read_failed"));

	const xr_array<size_t, 7> TruncatedSizes = {
		0,
		1,
		7,
		8,
		20,
		Serialized.Data.size() / 2,
		Serialized.Data.size() - 1,
	};
	for (const size_t Size : TruncatedSizes)
	{
		const FMaterialBundleReadResult Truncated = DeserializeMaterialBundle(
			xr_span<const u8>(Serialized.Data.data(), Size)
		);
		MATERIAL_CHECK(Runner, !Truncated.Succeeded());
	}
}

void TestVersionRejection(TiramisuMaterialTestRunner& Runner)
{
	FMaterialBundle Unsupported = MakeCompleteBundle();
	Unsupported.Version = MaterialBundleVersion + 1;
	const FMaterialBundleWriteResult Result = SerializeMaterialBundle(Unsupported);
	MATERIAL_CHECK(Runner, !Result.Succeeded());
	MATERIAL_CHECK(Runner, HasDiagnostic(Result.Diagnostics, "bundle.unsupported_version"));
}

void TestInvalidPassMetadataRejected(TiramisuMaterialTestRunner& Runner)
{
	FMaterialBundle InvalidPass = MakeCompleteBundle();
	InvalidPass.ShaderBlobs[0].Pass = static_cast<EMaterialPass>(255);
	const FMaterialBundleWriteResult PassResult = SerializeMaterialBundle(InvalidPass);
	MATERIAL_CHECK(Runner, !PassResult.Succeeded());
	MATERIAL_CHECK(Runner, HasDiagnostic(PassResult.Diagnostics, "bundle.write_failed"));

	FMaterialBundle MissingVertexFactory = MakeCompleteBundle();
	MissingVertexFactory.ShaderBlobs[0].VertexFactory.clear();
	const FMaterialBundleWriteResult MetadataResult = SerializeMaterialBundle(MissingVertexFactory);
	MATERIAL_CHECK(Runner, !MetadataResult.Succeeded());
	MATERIAL_CHECK(Runner, HasDiagnostic(MetadataResult.Diagnostics, "bundle.write_failed"));
}
} // namespace

int main()
{
	TiramisuMaterialTestRunner Runner("xrMaterialBundleTests");
	TestDeterministicSerialization(Runner);
	TestIncompleteDevelopmentBundle(Runner);
	TestCorruptionAndTruncation(Runner);
	TestVersionRejection(Runner);
	TestInvalidPassMetadataRejected(Runner);
	return Runner.Finish();
}
