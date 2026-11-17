#pragma once

class CMapLocation;
class NET_Packet;
class IReader;

enum class EMapSpotFilter : u8
{
	Unmanaged = 0,
	Visible,
	Hidden
};

struct SMapSpotCategory
{
	shared_str Id;
	xr_vector<xr_string> Patterns;
	bool UserSpots = false;
	bool Hidden = false;
};

class CMapSpotFilter
{
public:
	static constexpr const char* CategoriesSection = "map_spot_categories";
	static constexpr const char* UserSpotsToken = "$user";

	void LoadCategories();
	bool RegisterCategory(const shared_str& Id, const char* Patterns);
	bool HasCategory(const shared_str& Id) const;
	bool IsCategoryHidden(const shared_str& Id) const;
	bool SetCategoryHidden(const shared_str& Id, bool Hidden);
	EMapSpotFilter Evaluate(const CMapLocation& Location) const;

	void Save(NET_Packet& Packet) const;
	bool Load(IReader& Reader);

	const xr_vector<SMapSpotCategory>& GetCategories() const { return Categories; }

private:
	SMapSpotCategory* FindCategory(const shared_str& Id);
	const SMapSpotCategory* FindCategory(const shared_str& Id) const;
	static bool Matches(const SMapSpotCategory& Category, const CMapLocation& Location);

	xr_vector<SMapSpotCategory> Categories;
	xr_vector<shared_str> PendingHidden;
};
