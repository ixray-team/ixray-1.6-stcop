#pragma once

#include "FBasicVisual.h"

class FTreeVisual_Prototype final : public dxRender_Visual
{
	GeomData* Slot = nullptr;
	xr_vector<u32> VertsCount = {};
	xr_vector<u32> IndicesCount = {};
	CDB::MODEL Collision = {};
	
public:
	void Load(const char* N, IReader *data, u32 dwFlags) override;
	void Release() override;
	
	IRHIBuffer* GetVB(int ID);
	IRHIBuffer* GetIB(int ID);
	u32 GetVBCount(int ID);
	u32 GetIBCount(int ID);
	RHIInputElementDesc* GetDecl(int ID, size_t& Size);
	
	CDB::MODEL* GetCollisionModel(){return &Collision;}
};
