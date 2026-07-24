#include "../TStaticMeshTypes.h"

#include <cstdint>
#include <iostream>
#include <limits>

namespace
{
int Fail(const char* Message)
{
	std::cerr << Message << '\n';
	return 1;
}
} // namespace

int main()
{
	FStaticMeshSection Section;
	Section.FirstIndex = 27;
	Section.NumTriangles = 12;
	Section.BaseVertexIndex = -4;
	Section.MinVertexIndex = 8;
	Section.MaxVertexIndex = 31;
	Section.MaterialSlot = 3;

	FMeshBatchElement Element;
	if (!BuildStaticMeshBatchElement(Section, Element))
	{
		return Fail("A valid static mesh section was rejected");
	}
	if (Element.OffsetIndex != 27 || Element.CountIndex != 36 ||
		Element.OffsetVertex != -4 || Element.CountVertex != 24)
	{
		return Fail("Static mesh section offsets/counts were converted incorrectly");
	}

	FStaticMeshSection Empty = Section;
	Empty.NumTriangles = 0;
	if (BuildStaticMeshBatchElement(Empty, Element))
	{
		return Fail("A section without triangles must be rejected");
	}

	FStaticMeshSection InvalidRange = Section;
	InvalidRange.MinVertexIndex = 10;
	InvalidRange.MaxVertexIndex = 9;
	if (BuildStaticMeshBatchElement(InvalidRange, Element))
	{
		return Fail("An inverted vertex range must be rejected");
	}

	FStaticMeshSection Overflow = Section;
	Overflow.NumTriangles = std::numeric_limits<u32>::max() / 3u + 1u;
	if (BuildStaticMeshBatchElement(Overflow, Element))
	{
		return Fail("An overflowing index count must be rejected");
	}

	return 0;
}
