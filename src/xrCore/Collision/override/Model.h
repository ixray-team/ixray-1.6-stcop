//////////////////////////////////////////////////////////
// Desc   : Collision Detection Model + Cache System
// Author : ForserX
// Update : 20.04.2020 - Support for Hybrid Trees System 
//////////////////////////////////////////////////////////
#pragma once
#include "xrCore/Collision/xrCDB.h"

namespace CDB
{
	class BVHNode;

	class BVHNode
	{
	public:
		struct Child
		{
			Fbox AABB;
			ElementID ID;
		};
	private:
		BVHNode* parent = nullptr;
		size_t Size = 0;
		Child* childrenPtr = nullptr;
		Child children[];
	public:
		void SetChildrenPtr(Child* newChildren) { childrenPtr = newChildren; }
		
		const Fbox& GetAABB(size_t ID) const { return children[ID].AABB; }
		Fbox& GetAABB(size_t ID) { return children[ID].AABB; }
		void SetParent(BVHNode* NewParent) { parent = NewParent; }
		BVHNode* GetParent() const { return parent; }
		size_t GetSize() const { return Size; }
		void SetSize(size_t newSize) { Size = newSize; }
		ElementID& GetElement(size_t ID) { return children[ID].ID; }
		ElementID GetElement(size_t ID) const { return children[ID].ID; }
		bool HasNode(size_t ID) const { return children[ID].ID.p && !children[ID].ID.IsNotPointer; }
		const BVHNode& GetNode(size_t ID) const { return *children[ID].ID.p; }
		BVHNode& GetNode(size_t ID) { return *children[ID].ID.p; }
	};

	class XRCORE_API BVHModel
	{
		xr_vector<BVHNode> Nodes;
		Fbox AABB;
		
	public:		
		void Store(IWriter& pWriter);
		bool Restore(IReader& pReader);
		
		const Fbox& GetAABB() const { return AABB; }
		Fbox& GetAABB() { return AABB; }
		const xr_vector<BVHNode>& GetNodes() const { return Nodes; }
		xr_vector<BVHNode>& GetNodes() { return Nodes; }
	};

	struct BuilderConfig
	{
		struct Data
		{
			BVHNode** Ptr;
			size_t FacesCount;
		};
		xr_vector<Fvector>* Vertices = nullptr;
		xr_vector<TRI>* Faces = nullptr;
		xr_vector<InstanceData>* Instances = nullptr;
		Data* UserData = nullptr;
	};

	RTCBVH BuildModel(const BuilderConfig& config);
}