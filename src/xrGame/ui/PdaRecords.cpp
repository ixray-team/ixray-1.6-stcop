#include "StdAfx.h"
#include "pch_script.h"
#include "PdaRecords.h"

#include "../Actor.h"
#include "../InventoryOwner.h"
#include "../Level.h"
#include "../game_news.h"
#include "../alife_registry_wrappers.h"
#include "../../xrEngine/string_table.h"
#include "UIInventoryUtilities.h"

using namespace luabind;

namespace PdaRecordsHelpers
{
u32 MixHash(u32 Hash, const char* Text)
{
	if (!Text)
	{
		return Hash;
	}
	for (const char* Char = Text; *Char; ++Char)
	{
		Hash = (Hash ^ u32(u8(*Char))) * 16777619u;
	}
	return (Hash ^ 0xFFu) * 16777619u;
}

ALife::_OBJECT_ID TalkPartnerId()
{
	CActor* ActorPtr = Actor();
	if (!ActorPtr || !ActorPtr->IsTalking())
	{
		return ALife::INVALID_OBJECT_ID;
	}
	CGameObject* Partner = dynamic_cast<CGameObject*>(ActorPtr->GetTalkPartner());
	return Partner ? Partner->ID() : ALife::INVALID_OBJECT_ID;
}
}

void CPdaRecordEntry::SetIconRect(float X, float Y, float Width, float Height)
{
	HasIconRect = Width > 0.f && Height > 0.f;
	IconRect.set(X, Y, Width, Height);
}

void CPdaRecordEntry::SetItemIcon(const char* Section)
{
	if (!Section || !Section[0] || !pSettings->section_exist(Section))
	{
		return;
	}
	const InventoryUtilities::InventoryIconParams Params = InventoryUtilities::GetInventoryIconParams(Section);
	Icon = Params.icons_texture && Params.icons_texture[0] ? Params.icons_texture : "ui\\ui_icon_equipment";
	const float CellWidth = INV_GRID_WIDTH(Params.scaleIcon);
	const float CellHeight = INV_GRID_HEIGHT(Params.scaleIcon);
	SetIconRect(Params.inv_grid_x * CellWidth, Params.inv_grid_y * CellHeight, Params.inv_grid_width * CellWidth, Params.inv_grid_height * CellHeight);
}

void CPdaRecordEntry::SetGroup(const char* Key, const char* Caption)
{
	GroupKey = Key;
	GroupCaption = Caption ? Caption : "";
}

void CPdaRecordEntry::SetMap(const char* Level, const Fvector& Position)
{
	HasMapTarget = Level && Level[0];
	MapLevel = Level;
	MapPosition = Position;
	if (!LevelName.size())
	{
		LevelName = Level;
	}
}

void CPdaRecordEntry::AddLine(const char* Style, const char* Text)
{
	Lines.push_back({ shared_str(Style && Style[0] ? Style : "text"), xr_string(Text ? Text : "") });
}

void CPdaRecordEntry::SetAction(const char* ActionIdValue, const char* Caption)
{
	ActionId = ActionIdValue;
	ActionCaption = Caption ? Caption : "";
}

u32 CPdaRecordEntry::Hash() const
{
	using PdaRecordsHelpers::MixHash;
	u32 Value = 2166136261u;
	Value = MixHash(Value, Id.c_str());
	Value = MixHash(Value, Title.c_str());
	Value = MixHash(Value, Subtitle.c_str());
	Value = MixHash(Value, Icon.c_str());
	Value = MixHash(Value, Image.c_str());
	Value = MixHash(Value, State.c_str());
	Value = MixHash(Value, GroupKey.c_str());
	Value = MixHash(Value, GroupCaption.c_str());
	Value = MixHash(Value, ActionCaption.c_str());
	Value = (Value ^ (IsNew ? 1u : 0u) ^ (HasMapTarget ? 2u : 0u) ^ (Trackable ? 4u : 0u) ^ (ScrollToEnd ? 8u : 0u)) * 16777619u;
	Value = (Value ^ Time) * 16777619u;
	for (const SPdaRecordLine& Line : Lines)
	{
		Value = MixHash(Value, Line.Style.c_str());
		Value = MixHash(Value, Line.Text.c_str());
	}
	return Value;
}

CPdaRecordEntry* CPdaRecordList::Add(const char* Id)
{
	Items.emplace_back();
	CPdaRecordEntry* Entry = &Items.back();
	Entry->Id = Id;
	return Entry;
}

u32 CPdaRecordList::NewCount() const
{
	u32 Count = 0;
	for (const CPdaRecordEntry& Entry : Items)
	{
		if (Entry.IsNew)
		{
			++Count;
		}
	}
	return Count;
}

u32 CPdaRecordList::Hash() const
{
	u32 Value = 2166136261u;
	for (const CPdaRecordEntry& Entry : Items)
	{
		Value = (Value ^ Entry.Hash()) * 16777619u;
	}
	return Value;
}

const CPdaRecordEntry* CPdaRecordList::Find(const shared_str& Id) const
{
	for (const CPdaRecordEntry& Entry : Items)
	{
		if (Entry.Id == Id)
		{
			return &Entry;
		}
	}
	return nullptr;
}

namespace PdaRecordsContacts
{
constexpr const char* TalkLinePrefix = "%c[250,255,232,208]";
constexpr ALife::_TIME_ID SessionGapMs = ALife::_TIME_ID(5) * 60 * 1000;
constexpr ALife::_TIME_ID DayMs = ALife::_TIME_ID(24) * 60 * 60 * 1000;

struct SLine
{
	ALife::_TIME_ID Time = 0;
	bool Actor = false;
	xr_string Text;
};

struct SContact
{
	xr_string Key;
	xr_string Name;
	shared_str Icon;
	Frect IconRect = {};
	ALife::_TIME_ID LastTime = 0;
	xr_vector<SLine> Lines;
};

xr_string StripTalkText(const char* Text)
{
	const size_t PrefixLength = xr_strlen(TalkLinePrefix);
	return xr_string(Text + PrefixLength);
}

bool IsDialogLine(const GAME_NEWS_DATA& News)
{
	const char* Text = News.news_text.c_str();
	return News.m_type == GAME_NEWS_DATA::eTalk && Text && !strncmp(Text, TalkLinePrefix, xr_strlen(TalkLinePrefix));
}

u32 DayNumber(ALife::_TIME_ID Time)
{
	const ALife::_TIME_ID StartDay = Level().GetStartGameTime() / DayMs;
	const ALife::_TIME_ID Day = Time / DayMs;
	return Day >= StartDay ? u32(Day - StartDay) + 1 : 1;
}

u32 GameMinutes(ALife::_TIME_ID Time)
{
	const ALife::_TIME_ID Start = Level().GetStartGameTime();
	return Time > Start ? u32((Time - Start) / 60000) : 0;
}

xr_string FormatNumber(const char* FormatId, u32 Value)
{
	string256 Text;
	xr_sprintf(Text, g_pStringTable->translate(FormatId).c_str(), Value);
	return Text;
}

xr_string FormatStamp(ALife::_TIME_ID Time)
{
	const shared_str Clock = InventoryUtilities::GetTimeAsString(Time, InventoryUtilities::etpTimeToMinutes);
	string256 Text;
	xr_sprintf(Text, "%s, %s", FormatNumber("pda_rec_contacts_day_short", DayNumber(Time)).c_str(), Clock.c_str());
	return Text;
}

void Commit(SContact& Contact, xr_vector<SLine>& Pending)
{
	for (SLine& Line : Pending)
	{
		Contact.LastTime = std::max(Contact.LastTime, Line.Time);
		Contact.Lines.push_back(std::move(Line));
	}
	Pending.clear();
}

void Fill(CPdaRecordList* List, int MaxLines)
{
	CActor* ActorPtr = Actor();
	if (!List || !ActorPtr || !g_pGameLevel)
	{
		return;
	}

	const char* ActorName = ActorPtr->NameReal();
	const GAME_NEWS_VECTOR& News = ActorPtr->game_news_registry->registry().objects();

	xr_vector<SContact> Contacts;
	xr_map<xr_string, u32> ContactIndex;
	xr_vector<SLine> Pending;
	int Current = -1;
	ALife::_TIME_ID LastTime = 0;

	for (const GAME_NEWS_DATA& Item : News)
	{
		if (!IsDialogLine(Item))
		{
			continue;
		}

		if (LastTime && Item.receive_time > LastTime + SessionGapMs)
		{
			if (Current >= 0)
			{
				Commit(Contacts[Current], Pending);
			}
			Pending.clear();
			Current = -1;
		}
		LastTime = Item.receive_time;

		SLine Line;
		Line.Time = Item.receive_time;
		Line.Text = StripTalkText(Item.news_text.c_str());

		const char* Caption = Item.news_caption.c_str();
		if (ActorName && Caption && !xr_strcmp(Caption, ActorName))
		{
			Line.Actor = true;
			Pending.push_back(std::move(Line));
			continue;
		}

		xr_string Key = Caption ? Caption : "";
		Key += "|";
		Key += Item.texture_name.c_str() ? Item.texture_name.c_str() : "";

		u32 Index = 0;
		const auto Found = ContactIndex.find(Key);
		if (Found == ContactIndex.end())
		{
			SContact Contact;
			Contact.Key = Key;
			Contact.Name = Caption && strstr(Caption, ":lname_") ? TranslateName(Caption) : xr_string(Caption ? Caption : "");
			Contact.Icon = Item.texture_name;
			Contact.IconRect = Item.tex_rect;
			Index = u32(Contacts.size());
			Contacts.push_back(std::move(Contact));
			ContactIndex[Key] = Index;
		}
		else
		{
			Index = Found->second;
		}

		Current = int(Index);
		Commit(Contacts[Index], Pending);
		Contacts[Index].LastTime = std::max(Contacts[Index].LastTime, Line.Time);
		Contacts[Index].Lines.push_back(std::move(Line));
	}
	if (Current >= 0)
	{
		Commit(Contacts[Current], Pending);
	}

	std::sort(Contacts.begin(), Contacts.end(), [](const SContact& Left, const SContact& Right)
	{
		return Left.LastTime > Right.LastTime;
	});

	const shared_str YouLabel = g_pStringTable->translate("pda_rec_contacts_you");
	const u32 Today = DayNumber(Level().GetGameTime());
	const size_t LineLimit = MaxLines > 0 ? size_t(MaxLines) : size_t(-1);

	for (const SContact& Contact : Contacts)
	{
		xr_string Id = "contact:";
		Id += Contact.Key;
		CPdaRecordEntry* Entry = List->Add(Id.c_str());
		Entry->SetTitle(Contact.Name.c_str());
		Entry->Icon = Contact.Icon;
		Entry->SetIconRect(Contact.IconRect.x1, Contact.IconRect.y1, Contact.IconRect.x2, Contact.IconRect.y2);
		Entry->SetImage("none");
		Entry->SetState("contact");
		Entry->SetTime(GameMinutes(Contact.LastTime));
		Entry->SetScrollToEnd(true);

		const u32 LastDay = DayNumber(Contact.LastTime);
		if (LastDay == Today)
		{
			Entry->SetSubtitle(InventoryUtilities::GetTimeAsString(Contact.LastTime, InventoryUtilities::etpTimeToMinutes).c_str());
		}
		else
		{
			Entry->SetSubtitle(FormatNumber("pda_rec_contacts_day_short", LastDay).c_str());
		}

		string32 GroupKey;
		xr_sprintf(GroupKey, "day_%u", LastDay);
		Entry->SetGroup(GroupKey, FormatNumber("pda_rec_contacts_group", LastDay).c_str());

		string256 LastText;
		xr_sprintf(LastText, g_pStringTable->translate("pda_rec_contacts_last").c_str(), FormatStamp(Contact.LastTime).c_str());
		Entry->AddLine("info", LastText);

		const size_t First = Contact.Lines.size() > LineLimit ? Contact.Lines.size() - LineLimit : 0;
		u32 ShownDay = 0;
		for (size_t Index = First; Index < Contact.Lines.size(); ++Index)
		{
			const SLine& Line = Contact.Lines[Index];
			const u32 Day = DayNumber(Line.Time);
			if (Day != ShownDay)
			{
				Entry->AddLine("day", FormatNumber("pda_rec_contacts_day", Day).c_str());
				ShownDay = Day;
			}

			xr_string Text = Line.Actor ? YouLabel.c_str() : Contact.Name.c_str();
			Text += ": ";
			Text += Line.Text;
			Entry->AddLine(Line.Actor ? "actor" : "npc", Text.c_str());
		}
	}
}
}

void pda_records_registrator::script_register(lua_State* L)
{
	module(L)
	[
		class_<CPdaRecordEntry>("pda_record_entry")
			.def("set_title", &CPdaRecordEntry::SetTitle)
			.def("set_subtitle", &CPdaRecordEntry::SetSubtitle)
			.def("set_icon", &CPdaRecordEntry::SetIcon)
			.def("set_icon_rect", &CPdaRecordEntry::SetIconRect)
			.def("set_item_icon", &CPdaRecordEntry::SetItemIcon)
			.def("set_time", &CPdaRecordEntry::SetTime)
			.def("time", &CPdaRecordEntry::GetTime)
			.def("id", &CPdaRecordEntry::GetId)
			.def("set_scroll_end", &CPdaRecordEntry::SetScrollToEnd)
			.def("set_image", &CPdaRecordEntry::SetImage)
			.def("set_state", &CPdaRecordEntry::SetState)
			.def("set_new", &CPdaRecordEntry::SetNew)
			.def("set_level", &CPdaRecordEntry::SetLevel)
			.def("set_trackable", &CPdaRecordEntry::SetTrackable)
			.def("set_group", &CPdaRecordEntry::SetGroup)
			.def("set_map", &CPdaRecordEntry::SetMap)
			.def("add_line", &CPdaRecordEntry::AddLine)
			.def("set_action", &CPdaRecordEntry::SetAction),

		class_<CPdaRecordList>("pda_record_list")
			.def("add", &CPdaRecordList::Add)
			.def("get", &CPdaRecordList::Get)
			.def("count", &CPdaRecordList::Count),

		def("pda_records_talk_partner", &PdaRecordsHelpers::TalkPartnerId),
		def("pda_records_fill_contacts", &PdaRecordsContacts::Fill)
	];
}
