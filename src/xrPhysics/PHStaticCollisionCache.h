#pragma once

#include "PhysicsExternalCommon.h"
#include <memory>

struct dxGeom;
struct SPHCollisionQuery;

// Only effect callbacks explicitly registered by the game may be replayed later.
XRPHYSICS_API void ConfigureParallelStaticCallbacks(ContactCallbackFun* Shell, ContactCallbackFun* Character);

bool IsParallelStaticCollision();
void MarkParallelStaticCollisionOverflow();
bool AcceptParallelStaticTriangleCount(size_t Count);

class CPHStaticCollisionCache final
{
	class Implementation;
	struct ImplementationDeleter
	{
		void operator()(Implementation* Pointer) const;
	};
	// xr_unique_ptr inspects the type immediately, so an incomplete type needs a deferred deleter.
	std::unique_ptr<Implementation, ImplementationDeleter> Data;

public:
	explicit CPHStaticCollisionCache(dxGeom* Mesh);
	~CPHStaticCollisionCache();
	void SetCallbacks(ContactCallbackFun* Shell, ContactCallbackFun* Character);
	u32 Prepare(const xr_vector<SPHCollisionQuery>& Queries, u32 Workers);
	void RunWorker(u32 Worker);
	void EndBatch(bool Release = false);
	bool TryCollide(dxGeom* Mesh, dxGeom* Primitive, int Flags, dContactGeom* Contacts, int Stride, int& Count);
};
