#pragma once

struct SEncyclopediaLine
{
	shared_str Style;
	xr_string Text;
};

struct SEncyclopediaEntry
{
	shared_str Key;
	xr_string Title;
	xr_string Subtitle;
	shared_str ItemSection;
	shared_str Icon;
	shared_str Image;
	xr_vector<SEncyclopediaLine> Lines;
	bool IsNew = false;
	bool Vanilla = false;
};

struct SEncyclopediaGroup
{
	shared_str Id;
	xr_string Caption;
	xr_vector<SEncyclopediaEntry> Entries;
};

namespace EncyclopediaCatalog
{
bool IsConfigured();
void MarkKnown(const char* Key, bool Read = false);
bool IsKnown(const char* Key);
void MarkRead(const SEncyclopediaEntry& Entry);
void OnItemTake(const char* Section);
void OnClassEncountered(const char* Section);
void Collect(xr_vector<SEncyclopediaGroup>& Groups);
}
