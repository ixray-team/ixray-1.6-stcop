#pragma once

#include <algorithm>

namespace RestrictionVertexRange
{
template <typename Vertex, typename Predicate>
void Visit(const Vertex* Begin, const Vertex* End, u32 RowLength, u32 FirstZ, u32 LastZ, const Predicate& Process)
{
	if (Begin >= End || RowLength == 0 || FirstZ > LastZ)
	{
		return;
	}
	const u32 FirstRow = Begin->position().xz() / RowLength;
	const u32 LastRow = (End - 1)->position().xz() / RowLength;
	for (u32 Row = FirstRow; Row <= LastRow; ++Row)
	{
		const u32 FirstKey = Row * RowLength + FirstZ;
		const u32 LastKey = Row * RowLength + LastZ;
		Begin = std::lower_bound(Begin, End, FirstKey, [](const Vertex& Node, u32 Key)
								 { return Node.position().xz() < Key; });
		while (Begin != End && Begin->position().xz() <= LastKey)
		{
			Process(*Begin);
			++Begin;
		}
	}
}
}  
