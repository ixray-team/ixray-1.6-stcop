#pragma once

#include "TiramisuEditorOgfMotionLoader.h"
#include "../../../Include/xrRender/EditorRenderer.h"
#include "../../../xrCore/FS.h"

#include <limits>

// CPU-результат разбора одного draw-part standalone OGF. NRI-ресурсы
// создаются позже renderer thread из обычного editor static-mesh payload.
struct FTiramisuEditorOgfSkinBinding
{
	xr_array<u16, 4> BoneIndices = {};
	xr_array<float, 4> Weights = {1.0f, 0.0f, 0.0f, 0.0f};
};

// Renderer-owned описание skeleton. Матрицы соответствуют legacy X-Ray:
// BindLocal переводит bone в parent, BindModel — в model space, а
// ModelToBone является inverse bind для построения skinning palette.
struct FTiramisuEditorOgfBoneSource
{
	xr_string Name;
	u16 ParentIndex = std::numeric_limits<u16>::max();
	Fmatrix BindLocal = {};
	Fmatrix BindModel = {};
	Fmatrix ModelToBone = {};
};

struct FTiramisuEditorOgfMeshSource
{
	xr_string ShaderName;
	xr_string TextureName;
	xr_vector<FEditorStaticMeshVertex> Vertices;
	xr_vector<u32> Indices;
	xr_vector<FTiramisuEditorOgfSkinBinding> SkinBindings;
};

enum class ETiramisuEditorOgfLoadStatus : u8
{
	Success,
	InvalidData,
	UnsupportedVisualType,
	UnsupportedVertexFormat
};

struct FTiramisuEditorOgfModelSource
{
	xr_vector<FTiramisuEditorOgfMeshSource> Meshes;
	xr_vector<FTiramisuEditorOgfBoneSource> Bones;
	FTiramisuEditorOgfMotionSet EmbeddedMotions;
	xr_vector<FTiramisuEditorOgfMotionSet> ExternalMotions;
	xr_vector<xr_string> MotionReferences;
	xr_string MotionDiagnostic;
	u16 RootBoneIndex = std::numeric_limits<u16>::max();
	ETiramisuEditorOgfLoadStatus Status =
		ETiramisuEditorOgfLoadStatus::InvalidData;
	xr_string Diagnostic;

	[[nodiscard]] bool IsValid() const noexcept
	{
		return Status == ETiramisuEditorOgfLoadStatus::Success &&
			!Meshes.empty();
	}
};

// Разбирает standalone OGF без GPU и legacy renderer. Поддерживаются
// MT_NORMAL, MT_PROGRESSIVE и embedded hierarchy из этих типов.
[[nodiscard]] bool LoadTiramisuEditorOgfModel(
	IReader& Reader,
	FTiramisuEditorOgfModelSource& OutModel
);

// Строит current-model × inverse-bind palette. Пустой LocalPose использует
// bind pose модели; иначе массив обязан содержать local transform каждой
// кости в том же порядке, что Model.Bones.
[[nodiscard]] bool BuildTiramisuEditorOgfSkinningPalette(
	const FTiramisuEditorOgfModelSource& Model,
	xr_span<const Fmatrix> LocalPose,
	xr_vector<Fmatrix>& OutPalette,
	xr_string* OutDiagnostic = nullptr
);

// Разрешает startup animation с legacy-приоритетом последнего external OMF,
// затем embedded motions, семплирует local pose и сразу строит GPU palette.
[[nodiscard]] bool SampleTiramisuEditorOgfModelMotion(
	const FTiramisuEditorOgfModelSource& Model,
	xr_string_view AnimationName,
	float TimeSeconds,
	xr_vector<Fmatrix>& OutPalette,
	xr_string* OutDiagnostic = nullptr
);
