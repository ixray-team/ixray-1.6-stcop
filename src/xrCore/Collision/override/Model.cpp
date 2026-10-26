#include "stdafx.h"
#include "Model.h"

namespace CDB::Internal
{
	constexpr size_t PrimitiveFinalInvalidID = (size_t(-1) >> 2);

	/* Callback to create a node */
	void* EmbreeCreateNodeFunction(RTCThreadLocalAllocator allocator, unsigned int childCount, void* userPtr)
	{
		auto Node = (BVHNode*)rtcThreadLocalAlloc(allocator, sizeof(BVHNode) + sizeof(BVHNode::Child)*childCount, 16);
		Node->SetSize(childCount);
		Node->SetChildrenPtr((BVHNode::Child*)(((u8*)Node)+sizeof(BVHNode)));
		Node->SetParent(nullptr);
		for (size_t i = 0; i < childCount; i++)
		{
			Node->GetAABB(i).invalidate();
			Node->GetElement(i).p = nullptr;
		}
		return Node;
	}

	/* Callback to set the pointer to all children */
	void EmbreeSetNodeChildrenFunction(void* nodePtr, void** children, unsigned int childCount, void* userPtr)
	{
		auto Node = (BVHNode*)nodePtr;
		for (size_t i = 0; i < childCount; i++)
		{
			auto ChildNode = (BVHNode*)children[i];
			Node->GetElement(i).p = ChildNode;
			ChildNode->SetParent(Node);
		}
		*((BuilderConfig::Data*)userPtr)->Ptr = Node;
	}

	/* Callback to set the bounds of all children */
	void EmbreeSetNodeBoundsFunction(void* nodePtr, const struct RTCBounds** bounds, unsigned int childCount, void* userPtr)
	{
		auto Node = (BVHNode*)nodePtr;
		for (size_t i = 0; i < childCount; i++)
		{
			Fbox& AABB = Node->GetAABB(i);
			AABB.min.x = bounds[i]->lower_x;
			AABB.min.y = bounds[i]->lower_y;
			AABB.min.z = bounds[i]->lower_z;
			AABB.max.x = bounds[i]->upper_x;
			AABB.max.y = bounds[i]->upper_y;
			AABB.max.z = bounds[i]->upper_z;
		}
	}

	/* Callback to create a leaf node */
	void* EmbreeCreateLeafFunction(RTCThreadLocalAllocator allocator, const struct RTCBuildPrimitive* primitives, size_t primitiveCount, void* userPtr)
	{
		auto Node = (BVHNode*)EmbreeCreateNodeFunction(allocator, primitiveCount, userPtr);
		for (size_t i = 0; i < primitiveCount; i++)
		{
			Node->GetElement(i).IsNotPointer = true;
			Node->GetElement(i).IsInstance = primitives[i].geomID != u32(-1);
			Node->GetElement(i).Index = primitives[i].primID;
			if (Node->GetElement(i).IsInstance)
			{
				Node->GetElement(i).Index -= ((BuilderConfig::Data*)userPtr)->FacesCount;
			}
		}
		return Node;
	}

}

RTCBVH CDB::BuildModel(const BuilderConfig& config)
{
	using namespace CDB::Internal;
	auto model = rtcNewBVH(GetEmbreeDevice());
	
	size_t TotalPrimitives = 0;
	if (config.Faces)
	{
		TotalPrimitives += config.Faces->size();
	}
	if (config.Instances)
	{
		TotalPrimitives += config.Instances->size();
	}
	if (!TotalPrimitives)
	{
		return model;
	}
	xr_vector<RTCBuildPrimitive> Primitives;
	
	size_t i = 0;
	if (config.Faces)
	{
		for (auto& Face : *config.Faces)
		{
			Fbox AABB;
			AABB.invalidate();
			AABB.modify(config.Vertices->at(Face.verts[0]));
			AABB.modify(config.Vertices->at(Face.verts[1]));
			AABB.modify(config.Vertices->at(Face.verts[2]));
			Primitives.emplace_back(
				AABB.min.x, AABB.min.y, AABB.min.z,
				u32(-1),
				AABB.max.x, AABB.max.y, AABB.max.z,
				i++
			);
		}
	}
	if (config.Instances)
	{
		for (int j = 0; j < config.Instances->size(); j++)
		{
			auto& Inst = config.Instances->at(j);
			Primitives.emplace_back(
				Inst.GlobalAABB.min.x, Inst.GlobalAABB.min.y, Inst.GlobalAABB.min.z,
				j,
				Inst.GlobalAABB.max.x, Inst.GlobalAABB.max.y, Inst.GlobalAABB.max.z,
				i++
			);
		}
	}
	
	auto args = rtcDefaultBuildArguments();
	args.byteSize = 8;
	args.buildQuality = RTC_BUILD_QUALITY_HIGH;
	args.maxBranchingFactor = 4;
	args.maxLeafSize = 4;
	args.maxDepth = 64;
	args.bvh = model;
	args.primitives = Primitives.data();
	args.primitiveCount = Primitives.size();
	args.primitiveArrayCapacity = Primitives.capacity();
	args.createNode = &EmbreeCreateNodeFunction;
	args.setNodeChildren = &EmbreeSetNodeChildrenFunction;
	args.setNodeBounds = &EmbreeSetNodeBoundsFunction;
	args.createLeaf = &EmbreeCreateLeafFunction;
	args.userPtr = config.UserData;
	rtcBuildBVH(&args);
	return model;
}