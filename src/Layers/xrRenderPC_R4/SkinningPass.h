#pragma once
#include "RenderAPI.h"
#include "SH_Atomic.h"

class CSkeletonX;
class CKinematics;
class GfxCmdList;

class RENDER_API SkinningPass
{
public:
	void Destroy();
	void BeginFrame();
	void Bind(GfxCmdList& Cmd);
	void Request(CSkeletonX& Mesh, const Fmatrix& World);
	void Flush(GfxCmdList& Cmd);
	void Snapshot(CKinematics& Kinematics, const Fmatrix& World);
	u32 GetFrame() const { return Frame; }

private:
	struct Item
	{
		CSkeletonX* Mesh;
		Fmatrix World;
	};

	xr_vector<Item> Pending;
	ref_cs Shader;
	IRHIBuffer* Output = nullptr;
	IRHIShaderResourceView* OutputView = nullptr;
	IRHIUnorderedAccessView* OutputAccess = nullptr;
	IRHIBuffer* Data = nullptr;
	IRHIShaderResourceView* DataView = nullptr;
	u32 OutputCapacity = 1u << 20;
	u32 DataCapacity = 1u << 14;
	u32 Cursor = 0;
	u32 Frame = 1;
};
