#include "FFxExtendedEntityesLogic/stdafx.h"
#include <cmath>
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELSharedFlyingSpaceCache.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "xrGame/entity_alive.h"

namespace ExtendedEntityesSpaceCache
{
struct SCell
{
	s32 Horizontal = 0;
	s32 Vertical = 0;
	s32 Lateral = 0;
	// Упорядочивает координатные ключи для поиска в общем реестре.
	bool operator<(const SCell& Other) const
	{
		if (Horizontal != Other.Horizontal)
		{
			return Horizontal < Other.Horizontal;
		}
		if (Vertical != Other.Vertical)
		{
			return Vertical < Other.Vertical;
		}
		return Lateral < Other.Lateral;
	}
};
// Ключ содержит точные концы и габарит: округление лучей могло бы пропустить тонкую стену.
struct SEdgeKey
{
	float Values[8];
	bool operator<(const SEdgeKey& Other) const
	{
		for (u32 Index = 0; Index < 8; ++Index)
		{
			if (Values[Index] != Other.Values[Index])
			{
				return Values[Index] < Other.Values[Index];
			}
		}
		return false;
	}
};
struct STile
{
	xr_map<SEdgeKey, EELGeometryResult> Edges;
	u32 LastUsedAt = 0;
};
constexpr float TileSize = 16.f;

// Использует мировую ячейку, общую для разных владельцев и повторных планов.
SCell TileAt(const Fvector& Position)
{
	return {s32(std::floor(Position.x / TileSize)), s32(std::floor(Position.y / TileSize)), s32(std::floor(Position.z / TileSize))};
}
// Не объединяет близкие, но геометрически разные отрезки и размеры тела.
SEdgeKey EdgeKey(const Fvector& Start, const Fvector& End, float Radius, float CenterHeight)
{
	return {{Start.x, Start.y, Start.z, End.x, End.y, End.z, Radius, CenterHeight}};
}

} // namespace ExtendedEntityesSpaceCache

struct CELSharedFlyingSpaceCache::SState
{
	xr_map<ExtendedEntityesSpaceCache::SCell, ExtendedEntityesSpaceCache::STile> Tiles;
	u32 NextTile = 0;
	u32 CleanupFrame = u32(-1);
	u32 Hits = 0;
	u32 Misses = 0;
};

// Хранит только собственные данные кэша; ни модель уровня, ни сущности не удерживаются.
CELSharedFlyingSpaceCache::SState& CELSharedFlyingSpaceCache::State()
{
	static SState Runtime;
	return Runtime;
}

// Кэширует только завершённые проверки статических связей, без указателей на сущности.
bool CELSharedFlyingSpaceCache::Find(const Fvector& Start, const Fvector& End, float Radius, float CenterHeight, EELGeometryResult& Result)
{
	using namespace ExtendedEntityesSpaceCache;
	const auto Tile = State().Tiles.find(TileAt(Start));
	if (Tile == State().Tiles.end())
	{
		++State().Misses;
		return false;
	}
	const auto Edge = Tile->second.Edges.find(EdgeKey(Start, End, Radius, CenterHeight));
	if (Edge == Tile->second.Edges.end())
	{
		++State().Misses;
		return false;
	}
	Tile->second.LastUsedAt = Device.dwTimeGlobal;
	++State().Hits;
	Result = Edge->second;
	return true;
}
// Сохраняет завершённую проверку в пределах жёстких лимитов памяти.
void CELSharedFlyingSpaceCache::Remember(const Fvector& Start, const Fvector& End, float Radius, float CenterHeight, EELGeometryResult Result)
{
	using namespace ExtendedEntityesSpaceCache;
	if (Result == EELGeometryResult::Pending)
	{
		return;
	}
	const SCell Key = TileAt(Start);
	auto Tile = State().Tiles.find(Key);
	if (Tile == State().Tiles.end())
	{
		if (State().Tiles.size() >= 64)
		{
			return;
		}
		Tile = State().Tiles.emplace(Key, STile()).first;
	}
	Tile->second.LastUsedAt = Device.dwTimeGlobal;
	if (Tile->second.Edges.size() < 128)
	{
		Tile->second.Edges.emplace(EdgeKey(Start, End, Radius, CenterHeight), Result);
	}
}

// Очищает по одной неиспользуемой области за кадр; занятая живым владельцем область сохраняется.
void CELSharedFlyingSpaceCache::BeginFrame()
{
	using namespace ExtendedEntityesSpaceCache;
	if (State().CleanupFrame == Device.dwFrame || State().Tiles.empty())
	{
		return;
	}
	State().CleanupFrame = Device.dwFrame;
	const u32 TileCount = u32(State().Tiles.size());
	const u32 SelectedTile = State().NextTile % TileCount;
	State().NextTile = (SelectedTile + 1u) % TileCount;
	auto Tile = State().Tiles.begin();
	// The traversal bound must stay fixed while advancing the iterator.
	for (u32 Index = 0; Index < SelectedTile && Tile != State().Tiles.end(); ++Index)
	{
		++Tile;
	}
	if (Tile == State().Tiles.end())
	{
		return;
	}
	if (Device.dwTimeGlobal - Tile->second.LastUsedAt < 10000)
	{
		return;
	}
	for (const u16 OwnerIdentifier : ExtendedEntityes::Owners())
	{
		const CEntityAlive* Owner = ExtendedEntityes::FindEntity(OwnerIdentifier);
		if (!Owner || !Owner->g_Alive())
		{
			continue;
		}
		const SCell Occupied = TileAt(Owner->Position());
		if (!(Occupied < Tile->first) && !(Tile->first < Occupied))
		{
			return;
		}
	}
	State().Tiles.erase(Tile);
}


// Сбрасывает сведения при смене уровня или ревизии правил геометрии.
void CELSharedFlyingSpaceCache::Clear()
{
	State() = SState();
}
// Возвращает число повторно использованных статических проверок.
u32 CELSharedFlyingSpaceCache::Hits()
{
	return State().Hits;
}
// Возвращает число запросов без подходящей записи.
u32 CELSharedFlyingSpaceCache::Misses()
{
	return State().Misses;
}
// Возвращает число занятых областей ограниченного кэша.
u32 CELSharedFlyingSpaceCache::TileCount()
{
	return u32(State().Tiles.size());
}
