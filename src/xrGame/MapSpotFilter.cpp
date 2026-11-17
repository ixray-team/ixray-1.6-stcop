#include "StdAfx.h"
#include "MapSpotFilter.h"
#include "map_location.h"

namespace MapSpotFilterSave
{
constexpr u32 Tag = 0x3146534D;
}

void CMapSpotFilter::LoadCategories()
{
	if (!pSettings || !pSettings->section_exist(CategoriesSection))
	{
		return;
	}

	const CInifile::Sect& Section = pSettings->r_section(CategoriesSection);
	for (const CInifile::Item& Item : Section.Data)
	{
		RegisterCategory(Item.first, Item.second.c_str());
	}
}

bool CMapSpotFilter::RegisterCategory(const shared_str& Id, const char* Patterns)
{
	if (!Id.size() || FindCategory(Id))
	{
		return false;
	}

	SMapSpotCategory Category;
	Category.Id = Id;

	const int Count = Patterns ? _GetItemCount(Patterns) : 0;
	for (int i = 0; i < Count; ++i)
	{
		string256 Pattern;
		_GetItem(Patterns, i, Pattern);
		if (!Pattern[0])
		{
			continue;
		}

		if (!xr_strcmp(Pattern, UserSpotsToken))
		{
			Category.UserSpots = true;
		}
		else
		{
			Category.Patterns.emplace_back(Pattern);
		}
	}

	const auto Pending = std::find(PendingHidden.begin(), PendingHidden.end(), Id);
	if (Pending != PendingHidden.end())
	{
		Category.Hidden = true;
		PendingHidden.erase(Pending);
	}

	Categories.push_back(std::move(Category));
	return Categories.back().Hidden;
}

bool CMapSpotFilter::HasCategory(const shared_str& Id) const
{
	return FindCategory(Id) != nullptr;
}

bool CMapSpotFilter::IsCategoryHidden(const shared_str& Id) const
{
	const SMapSpotCategory* Category = FindCategory(Id);
	return Category && Category->Hidden;
}

bool CMapSpotFilter::SetCategoryHidden(const shared_str& Id, bool Hidden)
{
	SMapSpotCategory* Category = FindCategory(Id);
	if (!Category || Category->Hidden == Hidden)
	{
		return false;
	}

	Category->Hidden = Hidden;
	return true;
}

EMapSpotFilter CMapSpotFilter::Evaluate(const CMapLocation& Location) const
{
	EMapSpotFilter Result = EMapSpotFilter::Unmanaged;
	for (const SMapSpotCategory& Category : Categories)
	{
		if (!Matches(Category, Location))
		{
			continue;
		}

		if (Category.Hidden)
		{
			return EMapSpotFilter::Hidden;
		}
		Result = EMapSpotFilter::Visible;
	}
	return Result;
}

void CMapSpotFilter::Save(NET_Packet& Packet) const
{
	xr_vector<const SMapSpotCategory*> HiddenCategories;
	for (const SMapSpotCategory& Category : Categories)
	{
		if (Category.Hidden)
		{
			HiddenCategories.push_back(&Category);
		}
	}

	Packet.w_u32(MapSpotFilterSave::Tag);
	Packet.w_u16(static_cast<u16>(HiddenCategories.size()));
	for (const SMapSpotCategory* Category : HiddenCategories)
	{
		Packet.w_stringZ(Category->Id);
	}
}

bool CMapSpotFilter::Load(IReader& Reader)
{
	if (Reader.elapsed() < static_cast<intptr_t>(sizeof(u32) + sizeof(u16)))
	{
		return false;
	}

	const intptr_t Start = Reader.tell();
	if (Reader.r_u32() != MapSpotFilterSave::Tag)
	{
		Reader.seek(Start);
		return false;
	}

	xr_vector<shared_str> HiddenIds(Reader.r_u16());
	for (shared_str& HiddenId : HiddenIds)
	{
		Reader.r_stringZ(HiddenId);
	}

	PendingHidden.clear();
	for (SMapSpotCategory& Category : Categories)
	{
		Category.Hidden = std::find(HiddenIds.begin(), HiddenIds.end(), Category.Id) != HiddenIds.end();
	}
	for (const shared_str& HiddenId : HiddenIds)
	{
		if (!FindCategory(HiddenId))
		{
			PendingHidden.push_back(HiddenId);
		}
	}
	return true;
}

SMapSpotCategory* CMapSpotFilter::FindCategory(const shared_str& Id)
{
	for (SMapSpotCategory& Category : Categories)
	{
		if (Category.Id == Id)
		{
			return &Category;
		}
	}
	return nullptr;
}

const SMapSpotCategory* CMapSpotFilter::FindCategory(const shared_str& Id) const
{
	for (const SMapSpotCategory& Category : Categories)
	{
		if (Category.Id == Id)
		{
			return &Category;
		}
	}
	return nullptr;
}

bool CMapSpotFilter::Matches(const SMapSpotCategory& Category, const CMapLocation& Location)
{
	if (Location.IsUserDefined())
	{
		return Category.UserSpots;
	}

	const char* SpotType = Location.GetType();
	if (!SpotType || !SpotType[0])
	{
		return false;
	}

	const xr_string_view TypeView = SpotType;
	for (const xr_string& Pattern : Category.Patterns)
	{
		if (!Pattern.empty() && Pattern.back() == '*')
		{
			if (TypeView.starts_with(xr_string_view(Pattern.data(), Pattern.size() - 1)))
			{
				return true;
			}
		}
		else if (TypeView == Pattern.c_str())
		{
			return true;
		}
	}
	return false;
}
