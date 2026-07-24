#pragma once

#include "../../../xrCore/xrCore.h"

#include "../../../Include/xrRender/EditorRenderer.h"

#include <mutex>
#include <unordered_map>
#include <vector>

// CPU picking cache для renderer-neutral snapshots новой editor scene.
// Это основной picking path нового формата; legacy editor временно может
// использовать существующие object ray queries во время миграции.
class TiramisuEditorViewportScenePicker
{
public:
	// Submit атомарно заменяет scene snapshot; Pick читает согласованную CPU-копию.
	void Submit(const FEditorViewportSceneSnapshot& Snapshot);
	[[nodiscard]] FEditorViewportPickResult Pick(
		const FEditorViewportPickRequest& Request
	) const;

private:
	// CPU-представление mesh для editor picking.
	struct FMesh
	{
		xr_vector<FEditorStaticMeshVertex> Vertices;
		xr_vector<u32> Indices;
		xr_vector<FEditorStaticMeshSection> Sections;
		u64 Revision = 0;
	};

	mutable std::mutex Mutex;
	xr_hash_map<u64, FMesh> Meshes;
	xr_vector<FEditorStaticMeshInstance> Instances;
	u64 SceneRevision = 0;
};
