#pragma once

class xrMU_Model;
struct FaceDataEmbree;

namespace CDB { class CollectorPacked; }
  
class XRLC_LIGHT_API xrMU_InstancedGroup
{
public:
	struct InstanceData
	{
		xr_vector<Fmatrix> Instances; // global, locals are not implemented yet
		xrMU_Model* Model;
		u32 ModelID;
	};
	Fmatrix Transform; // global of whole group
	xr_vector<InstanceData> Slots;
	u16 Sector;

	void export_cform_game(CDB::CollectorPacked& cl);
};