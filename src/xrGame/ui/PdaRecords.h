#pragma once

#include "../../xrScripts/script_export_space.h"

struct SPdaRecordLine
{
	shared_str Style;
	xr_string Text;
};

class CPdaRecordEntry
{
public:
	shared_str Id;
	xr_string Title;
	xr_string Subtitle;
	shared_str Icon;
	Frect IconRect = {};
	bool HasIconRect = false;
	shared_str Image;
	shared_str State;
	shared_str GroupKey;
	xr_string GroupCaption;
	shared_str LevelName;
	shared_str MapLevel;
	Fvector MapPosition = {};
	bool HasMapTarget = false;
	bool Trackable = false;
	bool IsNew = false;
	bool ScrollToEnd = false;
	u32 Time = 0;
	shared_str ActionId;
	xr_string ActionCaption;
	xr_vector<SPdaRecordLine> Lines;

	void SetTitle(const char* Value) { Title = Value ? Value : ""; }
	void SetSubtitle(const char* Value) { Subtitle = Value ? Value : ""; }
	void SetIcon(const char* Value) { Icon = Value; }
	void SetIconRect(float X, float Y, float Width, float Height);
	void SetItemIcon(const char* Section);
	void SetTime(u32 Value) { Time = Value; }
	u32 GetTime() const { return Time; }
	const char* GetId() const { return Id.c_str(); }
	void SetScrollToEnd(bool Value) { ScrollToEnd = Value; }
	void SetImage(const char* Value) { Image = Value; }
	void SetState(const char* Value) { State = Value; }
	void SetNew(bool Value) { IsNew = Value; }
	void SetLevel(const char* Value) { LevelName = Value; }
	void SetTrackable(bool Value) { Trackable = Value; }
	void SetGroup(const char* Key, const char* Caption);
	void SetMap(const char* Level, const Fvector& Position);
	void AddLine(const char* Style, const char* Text);
	void SetAction(const char* ActionIdValue, const char* Caption);

	u32 Hash() const;
};

class CPdaRecordList
{
public:
	CPdaRecordEntry* Add(const char* Id);
	CPdaRecordEntry* Get(u32 Index) { return Index < Items.size() ? &Items[Index] : nullptr; }
	void Clear() { Items.clear(); }
	u32 Count() const { return u32(Items.size()); }
	u32 NewCount() const;
	u32 Hash() const;
	const CPdaRecordEntry* Find(const shared_str& Id) const;
	const xr_deque<CPdaRecordEntry>& Entries() const { return Items; }

private:
	xr_deque<CPdaRecordEntry> Items;
};

namespace PdaRecordsContacts
{
void Fill(CPdaRecordList* List, int MaxLines);
}

struct pda_records_registrator
{
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
