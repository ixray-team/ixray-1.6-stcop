#include "StdAfx.h"
#include "EncyclopediaCatalog.h"
#include "encyclopedia_article.h"
#include "alife_registry_wrappers.h"
#include "Actor.h"
#include "Inventory.h"
#include "inventory_item.h"
#include "Level.h"
#include "../xrEngine/string_table.h"

namespace EncyclopediaCatalogConfig
{
constexpr const char* Root = "encyclopedia_catalog";
constexpr const char* SectionPrefix = "encyclopedia_section.";
constexpr const char* SchemePrefix = "encyclopedia_scheme.";
constexpr const char* OverridePrefix = "encyclopedia.";
constexpr const char* KnowledgePrefix = "enc:";
constexpr const char* InitKey = "init";
constexpr const char* ItemKey = "item.";
constexpr const char* ClassKey = "class.";
constexpr const char* ListKey = "list.";
}

namespace EncyclopediaCatalogHelpers
{
struct SColors
{
	xr_string Label;
	xr_string Value;
	xr_string Good;
	xr_string Bad;
};

const char* ReadString(const char* Section, const char* Key)
{
	if (!Section || !Section[0] || !Key || !pSettings->section_exist(Section) || !pSettings->line_exist(Section, Key))
	{
		return nullptr;
	}
	return pSettings->r_string(Section, Key);
}

xr_string Translate(const char* Id)
{
	if (!Id || !Id[0])
	{
		return {};
	}
	return g_pStringTable->translate(Id).c_str();
}

xr_vector<xr_string> SplitList(const char* Text, char Separator = ',')
{
	xr_vector<xr_string> Result;
	if (!Text || !Text[0])
	{
		return Result;
	}
	const int Count = _GetItemCount(Text, Separator);
	for (int Index = 0; Index < Count; ++Index)
	{
		xr_string Item;
		_GetItem(Text, Index, Item, Separator);
		if (!Item.empty())
		{
			Result.push_back(Item);
		}
	}
	return Result;
}

xr_string FirstItem(const char* Text)
{
	xr_string Item;
	if (Text && Text[0])
	{
		_GetItem(Text, 0, Item);
	}
	return Item;
}

bool MatchClass(const char* Class, const xr_vector<xr_string>& Patterns)
{
	if (!Class || !Class[0])
	{
		return false;
	}
	for (const xr_string& Pattern : Patterns)
	{
		if (!Pattern.empty() && Pattern.back() == '*')
		{
			if (!strncmp(Class, Pattern.c_str(), Pattern.size() - 1))
			{
				return true;
			}
		}
		else if (!xr_strcmp(Class, Pattern.c_str()))
		{
			return true;
		}
	}
	return false;
}

bool ResolvePath(const char* Section, const char* Path, xr_string& Value)
{
	xr_vector<xr_string> Steps = SplitList(Path, '>');
	if (Steps.empty())
	{
		return false;
	}
	xr_string Current = Section ? Section : "";
	size_t First = 0;
	if (Steps.size() > 1 && pSettings->section_exist(Steps[0].c_str()) && !ReadString(Current.c_str(), Steps[0].c_str()))
	{
		Current = Steps[0];
		First = 1;
	}
	for (size_t Index = First; Index + 1 < Steps.size(); ++Index)
	{
		const xr_string Next = FirstItem(ReadString(Current.c_str(), Steps[Index].c_str()));
		if (Next.empty() || !pSettings->section_exist(Next.c_str()))
		{
			return false;
		}
		Current = Next;
	}
	const char* Raw = ReadString(Current.c_str(), Steps.back().c_str());
	if (!Raw)
	{
		return false;
	}
	Value = Raw;
	return true;
}

const SColors& Colors()
{
	static SColors Cached;
	static bool Loaded = false;
	if (!Loaded)
	{
		const char* Root = EncyclopediaCatalogConfig::Root;
		const char* Label = ReadString(Root, "color_label");
		const char* Value = ReadString(Root, "color_value");
		const char* Good = ReadString(Root, "color_good");
		const char* Bad = ReadString(Root, "color_bad");
		Cached.Label = xr_string("%c[") + (Label ? Label : "255,140,140,140") + "]";
		Cached.Value = xr_string("%c[") + (Value ? Value : "255,220,220,220") + "]";
		Cached.Good = xr_string("%c[") + (Good ? Good : "255,110,210,110") + "]";
		Cached.Bad = xr_string("%c[") + (Bad ? Bad : "255,225,95,85") + "]";
		Loaded = true;
	}
	return Cached;
}

xr_string FormatNumber(float Value, const xr_string& Format)
{
	string64 Buffer;
	if (Format == "int")
	{
		xr_sprintf(Buffer, "%d", iFloor(Value + (Value >= 0.f ? 0.5f : -0.5f)));
	}
	else if (Format == "f1")
	{
		xr_sprintf(Buffer, "%.1f", Value);
	}
	else if (Format == "f2")
	{
		xr_sprintf(Buffer, "%.2f", Value);
	}
	else if (std::fabs(Value) >= 10.f)
	{
		xr_sprintf(Buffer, "%d", iFloor(Value + (Value >= 0.f ? 0.5f : -0.5f)));
	}
	else
	{
		xr_sprintf(Buffer, "%.1f", Value);
		const size_t Length = xr_strlen(Buffer);
		if (Length > 2 && Buffer[Length - 2] == '.' && Buffer[Length - 1] == '0')
		{
			Buffer[Length - 2] = 0;
		}
	}
	return Buffer;
}

xr_string FormatUnit(const xr_string& Unit)
{
	if (Unit.empty())
	{
		return {};
	}
	const xr_string Translated = Translate(Unit.c_str());
	if (Translated != Unit)
	{
		return " " + Translated;
	}
	return Unit;
}

xr_string StatLine(const char* Caption, const xr_string& Value, const xr_string& Color)
{
	const SColors& Palette = Colors();
	return Palette.Label + Translate(Caption) + ":  " + Color + Value;
}

void AddSchemeLines(const char* Scheme, const char* Section, xr_vector<SEncyclopediaLine>& Lines)
{
	if (!Scheme || !Scheme[0])
	{
		return;
	}
	string256 SchemeSection;
	xr_strconcat(SchemeSection, EncyclopediaCatalogConfig::SchemePrefix, Scheme);
	if (!pSettings->section_exist(SchemeSection))
	{
		return;
	}

	const SColors& Palette = Colors();
	xr_vector<SEncyclopediaLine> Stats;
	for (const xr_string& Key : SplitList(ReadString(SchemeSection, "order")))
	{
		const xr_vector<xr_string> Parts = SplitList(ReadString(SchemeSection, Key.c_str()));
		if (Parts.size() < 2)
		{
			continue;
		}

		xr_string Raw;
		if (!ResolvePath(Section, Parts[0].c_str(), Raw))
		{
			continue;
		}

		bool AsName = false;
		bool Signed = false;
		bool Inverse = false;
		bool KeepZero = false;
		float Multiplier = 1.f;
		xr_string Format;
		xr_string Unit;
		xr_string Divider;
		xr_string Color = Palette.Value;
		for (size_t Index = 2; Index < Parts.size(); ++Index)
		{
			const xr_string& Option = Parts[Index];
			const size_t Equal = Option.find('=');
			const xr_string Name = Option.substr(0, Equal);
			const xr_string Argument = Equal == xr_string::npos ? xr_string() : Option.substr(Equal + 1);
			if (Name == "name")
			{
				AsName = true;
			}
			else if (Name == "signed")
			{
				Signed = true;
			}
			else if (Name == "inverse")
			{
				Inverse = true;
			}
			else if (Name == "zero")
			{
				KeepZero = true;
			}
			else if (Name == "mul")
			{
				Multiplier = (float)atof(Argument.c_str());
			}
			else if (Name == "format")
			{
				Format = Argument;
			}
			else if (Name == "unit")
			{
				Unit = Argument;
			}
			else if (Name == "div")
			{
				Divider = Argument;
			}
			else if (Name == "good")
			{
				Color = Palette.Good;
			}
			else if (Name == "bad")
			{
				Color = Palette.Bad;
			}
		}

		if (AsName)
		{
			const xr_string Value = FirstItem(Raw.c_str());
			if (!Value.empty())
			{
				Stats.push_back({"stat", StatLine(Parts[1].c_str(), Translate(Value.c_str()), Color)});
			}
			continue;
		}

		float Value = (float)atof(FirstItem(Raw.c_str()).c_str());
		if (!Divider.empty())
		{
			xr_string DividerRaw;
			const float DividerValue = ResolvePath(Section, Divider.c_str(), DividerRaw) ? (float)atof(DividerRaw.c_str()) : 1.f;
			if (!fis_zero(DividerValue))
			{
				Value /= DividerValue;
			}
		}
		Value *= Multiplier;
		if (!KeepZero && fis_zero(Value, EPS_L))
		{
			continue;
		}

		xr_string Text = FormatNumber(Value, Format);
		if (Signed)
		{
			if (Value > 0.f)
			{
				Text = "+" + Text;
			}
			Color = (Value > 0.f) != Inverse ? Palette.Good : Palette.Bad;
		}
		Text += FormatUnit(Unit);
		Stats.push_back({"stat", StatLine(Parts[1].c_str(), Text, Color)});
	}

	if (!Stats.empty())
	{
		if (const char* Title = ReadString(SchemeSection, "title"))
		{
			Lines.push_back({"caption", Translate(Title)});
		}
		Lines.insert(Lines.end(), Stats.begin(), Stats.end());
	}

	if (const char* Include = ReadString(SchemeSection, "include"))
	{
		for (const xr_string& Other : SplitList(Include))
		{
			AddSchemeLines(Other.c_str(), Section, Lines);
		}
	}
}

struct SSectionConfig
{
	xr_string Id;
	xr_string Name;
	xr_string Caption;
	xr_string Source;
	xr_string Scheme;
	xr_vector<xr_string> Classes;
	xr_vector<xr_string> Exclude;
	xr_vector<xr_string> List;
	bool Always = false;
};

SSectionConfig ReadSectionConfig(const xr_string& Id)
{
	SSectionConfig Config;
	Config.Id = Id;
	Config.Name = xr_string(EncyclopediaCatalogConfig::SectionPrefix) + Id;
	const char* Name = Config.Name.c_str();
	const char* Caption = ReadString(Name, "caption");
	Config.Caption = Translate(Caption ? Caption : Name);
	const char* Source = ReadString(Name, "source");
	Config.Source = Source ? Source : "items";
	const char* Scheme = ReadString(Name, "scheme");
	Config.Scheme = Scheme ? Scheme : "";
	Config.Classes = SplitList(ReadString(Name, "classes"));
	Config.Exclude = SplitList(ReadString(Name, "exclude"));
	Config.List = SplitList(ReadString(Name, "list"));
	const char* Unlock = ReadString(Name, "unlock");
	Config.Always = Unlock && !xr_strcmp(Unlock, "always");
	return Config;
}

xr_string OverrideSection(const xr_string& Key)
{
	xr_string Name = xr_string(EncyclopediaCatalogConfig::OverridePrefix) + Key;
	return pSettings->section_exist(Name.c_str()) ? Name : xr_string();
}

bool InfoAllowed(const char* Override)
{
	const char* Info = ReadString(Override, "info");
	if (!Info || !Info[0])
	{
		return true;
	}
	return Actor() && Actor()->HasInfo(Info);
}

ARTICLE_VECTOR* Registry()
{
	if (!Actor() || !Actor()->encyclopedia_registry)
	{
		return nullptr;
	}
	return &Actor()->encyclopedia_registry->registry().objects();
}

ARTICLE_DATA* FindKnowledge(const char* Key)
{
	ARTICLE_VECTOR* Articles = Registry();
	if (!Articles)
	{
		return nullptr;
	}
	string256 Id;
	xr_strconcat(Id, EncyclopediaCatalogConfig::KnowledgePrefix, Key);
	const shared_str IdValue = Id;
	for (ARTICLE_DATA& Article : *Articles)
	{
		if (Article.article_type == ARTICLE_DATA::eKnowledgeArticle && Article.article_id == IdValue)
		{
			return &Article;
		}
	}
	return nullptr;
}

const char* SectionClass(const char* Section)
{
	return ReadString(Section, "class");
}

const char* FirstSectionOfClass(const char* Class)
{
	static xr_hash_map<xr_string, xr_string> Cache;
	const auto Found = Cache.find(Class);
	if (Found != Cache.end())
	{
		return Found->second.empty() ? nullptr : Found->second.c_str();
	}
	xr_string Result;
	for (const CInifile::Sect& Sect : pSettings->sections())
	{
		const char* Value = nullptr;
		if (pSettings->line_exist(Sect.Name.c_str(), "class"))
		{
			Value = pSettings->r_string(Sect.Name.c_str(), "class");
		}
		if (Value && !xr_strcmp(Value, Class))
		{
			Result = Sect.Name.c_str();
			break;
		}
	}
	Cache[Class] = Result;
	return Result.empty() ? nullptr : Cache[Class].c_str();
}

xr_string PathLine(const SSectionConfig& Config, const xr_string& Title)
{
	return "#/" + Config.Caption + "/" + Title;
}

void FinishEntry(SEncyclopediaEntry& Entry, const SSectionConfig& Config, const char* DataSection, const xr_string& Override, const char* Text)
{
	xr_vector<SEncyclopediaLine> Body;
	Body.push_back({"path", PathLine(Config, Entry.Title)});
	Body.push_back({"heading", Entry.Title});
	if (Text && Text[0])
	{
		Body.push_back({"text", Translate(Text)});
	}
	AddSchemeLines(Config.Scheme.c_str(), DataSection, Body);
	if (const char* Extra = ReadString(Override.c_str(), "text_extra"))
	{
		Body.push_back({"text", Translate(Extra)});
	}
	Entry.Lines.swap(Body);

	if (const char* Image = ReadString(Override.c_str(), "image"))
	{
		Entry.Image = Image;
	}
	if (const char* Icon = ReadString(Override.c_str(), "icon"))
	{
		Entry.Icon = Icon;
	}

	const float Cost = DataSection ? pSettings->read_if_exists<float>(DataSection, "cost", 0.f) : 0.f;
	if (Cost > 0.f && pSettings->line_exist(DataSection, "inv_name"))
	{
		const SColors& Palette = Colors();
		string64 Price;
		xr_sprintf(Price, "%d", iFloor(Cost + 0.5f));
		Entry.Subtitle = Palette.Label + Translate("pda_ref_price") + ":  " + Palette.Value + Price + FormatUnit("pda_ref_unit_money");
	}
}

void AddItemEntry(SEncyclopediaGroup& Group, const SSectionConfig& Config, const char* Section, const ARTICLE_DATA* Knowledge)
{
	const xr_string Key = xr_string(EncyclopediaCatalogConfig::ItemKey) + Section;
	const xr_string Override = OverrideSection(Key);
	if (!InfoAllowed(Override.c_str()))
	{
		return;
	}
	SEncyclopediaEntry Entry;
	Entry.Key = Key.c_str();
	const char* Name = ReadString(Override.c_str(), "name");
	Entry.Title = Translate(Name ? Name : ReadString(Section, "inv_name"));
	Entry.ItemSection = Section;
	Entry.IsNew = Knowledge && !Knowledge->readed;
	const char* Text = ReadString(Override.c_str(), "text");
	FinishEntry(Entry, Config, Section, Override, Text ? Text : ReadString(Section, "description"));
	Group.Entries.push_back(std::move(Entry));
}

bool ItemMatches(const SSectionConfig& Config, const char* Section)
{
	if (!pSettings->section_exist(Section) || !pSettings->line_exist(Section, "inv_name"))
	{
		return false;
	}
	const char* Class = SectionClass(Section);
	for (const xr_string& Excluded : Config.Exclude)
	{
		if (Excluded == Section || MatchClass(Class, {Excluded}))
		{
			return false;
		}
	}
	return MatchClass(Class, Config.Classes);
}

void CollectItems(SEncyclopediaGroup& Group, const SSectionConfig& Config)
{
	if (Config.Always)
	{
		for (const CInifile::Sect& Sect : pSettings->sections())
		{
			const char* Section = Sect.Name.c_str();
			if (ItemMatches(Config, Section))
			{
				const xr_string Key = xr_string(EncyclopediaCatalogConfig::ItemKey) + Section;
				AddItemEntry(Group, Config, Section, FindKnowledge(Key.c_str()));
			}
		}
		return;
	}

	ARTICLE_VECTOR* Articles = Registry();
	if (!Articles)
	{
		return;
	}
	const size_t PrefixLength = xr_strlen(EncyclopediaCatalogConfig::KnowledgePrefix) + xr_strlen(EncyclopediaCatalogConfig::ItemKey);
	for (const ARTICLE_DATA& Article : *Articles)
	{
		if (Article.article_type != ARTICLE_DATA::eKnowledgeArticle || Article.article_id.size() <= PrefixLength)
		{
			continue;
		}
		const char* Id = Article.article_id.c_str();
		if (strncmp(Id, EncyclopediaCatalogConfig::KnowledgePrefix, xr_strlen(EncyclopediaCatalogConfig::KnowledgePrefix)) != 0)
		{
			continue;
		}
		const char* Key = Id + xr_strlen(EncyclopediaCatalogConfig::KnowledgePrefix);
		if (strncmp(Key, EncyclopediaCatalogConfig::ItemKey, xr_strlen(EncyclopediaCatalogConfig::ItemKey)) != 0)
		{
			continue;
		}
		const char* Section = Id + PrefixLength;
		if (ItemMatches(Config, Section))
		{
			AddItemEntry(Group, Config, Section, &Article);
		}
	}
}

void CollectClasses(SEncyclopediaGroup& Group, const SSectionConfig& Config)
{
	for (const xr_string& Class : Config.Classes)
	{
		const xr_string Key = xr_string(EncyclopediaCatalogConfig::ClassKey) + Class;
		const ARTICLE_DATA* Knowledge = FindKnowledge(Key.c_str());
		if (!Config.Always && !Knowledge)
		{
			continue;
		}
		const xr_string Override = OverrideSection(Key);
		if (!InfoAllowed(Override.c_str()))
		{
			continue;
		}
		const char* DataSection = ReadString(Override.c_str(), "section");
		if (!DataSection || !pSettings->section_exist(DataSection))
		{
			DataSection = FirstSectionOfClass(Class.c_str());
		}

		SEncyclopediaEntry Entry;
		Entry.Key = Key.c_str();
		const char* Name = ReadString(Override.c_str(), "name");
		const char* InvName = ReadString(DataSection, "inv_name");
		Entry.Title = Name ? Translate(Name) : (InvName ? Translate(InvName) : Class);
		Entry.IsNew = Knowledge && !Knowledge->readed;
		const char* Text = ReadString(Override.c_str(), "text");
		FinishEntry(Entry, Config, DataSection, Override, Text ? Text : ReadString(DataSection, "description"));
		Group.Entries.push_back(std::move(Entry));
	}
}

void CollectList(SEncyclopediaGroup& Group, const SSectionConfig& Config)
{
	for (const xr_string& Id : Config.List)
	{
		const xr_string Key = xr_string(EncyclopediaCatalogConfig::ListKey) + Id;
		const xr_string Override = OverrideSection(Id);
		if (!InfoAllowed(Override.c_str()))
		{
			continue;
		}
		const ARTICLE_DATA* Knowledge = FindKnowledge(Key.c_str());
		if (!Config.Always && !Knowledge)
		{
			continue;
		}
		const char* DataSection = ReadString(Override.c_str(), "section");
		if (!DataSection && pSettings->section_exist(Id.c_str()))
		{
			DataSection = Id.c_str();
		}

		SEncyclopediaEntry Entry;
		Entry.Key = Key.c_str();
		const char* Name = ReadString(Override.c_str(), "name");
		Entry.Title = Translate(Name ? Name : Id.c_str());
		Entry.IsNew = Knowledge && !Knowledge->readed;
		if (DataSection && pSettings->line_exist(DataSection, "inv_name"))
		{
			Entry.ItemSection = DataSection;
		}
		FinishEntry(Entry, Config, DataSection, Override, ReadString(Override.c_str(), "text"));
		Group.Entries.push_back(std::move(Entry));
	}
}

void CollectArticles(SEncyclopediaGroup& Group, const SSectionConfig& Config)
{
	ARTICLE_VECTOR* Articles = Registry();
	if (!Articles)
	{
		return;
	}
	for (const ARTICLE_DATA& Article : *Articles)
	{
		if (Article.article_type != ARTICLE_DATA::eEncyclopediaArticle)
		{
			continue;
		}
		CEncyclopediaArticle Source;
		Source.Load(Article.article_id);
		SArticleData* Data = Source.data();
		if (!Data)
		{
			continue;
		}

		SEncyclopediaEntry Entry;
		Entry.Key = Article.article_id;
		Entry.Vanilla = true;
		Entry.Title = Translate(Data->name.c_str());
		Entry.IsNew = !Article.readed;

		Entry.Lines.push_back({"path", PathLine(Config, Translate(Data->group.c_str()) + "/" + Entry.Title)});
		Entry.Lines.push_back({"heading", Entry.Title});
		if (!Data->text.empty())
		{
			Entry.Lines.push_back({"text", Translate(Data->text.c_str())});
		}
		Group.Entries.push_back(std::move(Entry));
	}
}

void ScanInventoryOnce()
{
	if (!Actor() || EncyclopediaCatalog::IsKnown(EncyclopediaCatalogConfig::InitKey))
	{
		return;
	}
	for (PIItem Item : Actor()->inventory().m_all)
	{
		if (Item)
		{
			const xr_string Key = xr_string(EncyclopediaCatalogConfig::ItemKey) + Item->object().cNameSect().c_str();
			EncyclopediaCatalog::MarkKnown(Key.c_str(), true);
		}
	}
	EncyclopediaCatalog::MarkKnown(EncyclopediaCatalogConfig::InitKey, true);
}
}

namespace EncyclopediaCatalog
{
bool IsConfigured()
{
	return pSettings->section_exist(EncyclopediaCatalogConfig::Root);
}

void MarkKnown(const char* Key, bool Read)
{
	if (!Key || !Key[0] || !IsConfigured())
	{
		return;
	}
	ARTICLE_VECTOR* Articles = EncyclopediaCatalogHelpers::Registry();
	if (!Articles || EncyclopediaCatalogHelpers::FindKnowledge(Key))
	{
		return;
	}
	string256 Id;
	xr_strconcat(Id, EncyclopediaCatalogConfig::KnowledgePrefix, Key);
	Articles->emplace_back(shared_str(Id), Level().GetGameTime(), ARTICLE_DATA::eKnowledgeArticle);
	Articles->back().readed = Read;
}

bool IsKnown(const char* Key)
{
	return EncyclopediaCatalogHelpers::FindKnowledge(Key) != nullptr;
}

void MarkRead(const SEncyclopediaEntry& Entry)
{
	ARTICLE_VECTOR* Articles = EncyclopediaCatalogHelpers::Registry();
	if (!Articles)
	{
		return;
	}
	if (Entry.Vanilla)
	{
		for (ARTICLE_DATA& Article : *Articles)
		{
			if (Article.article_type == ARTICLE_DATA::eEncyclopediaArticle && Article.article_id == Entry.Key)
			{
				Article.readed = true;
			}
		}
		return;
	}
	if (ARTICLE_DATA* Knowledge = EncyclopediaCatalogHelpers::FindKnowledge(Entry.Key.c_str()))
	{
		Knowledge->readed = true;
	}
}

void OnItemTake(const char* Section)
{
	if (!Section || !Section[0])
	{
		return;
	}
	const xr_string Key = xr_string(EncyclopediaCatalogConfig::ItemKey) + Section;
	MarkKnown(Key.c_str());
}

void OnClassEncountered(const char* Section)
{
	const char* Class = EncyclopediaCatalogHelpers::SectionClass(Section);
	if (!Class || !Class[0])
	{
		return;
	}
	const xr_string Key = xr_string(EncyclopediaCatalogConfig::ClassKey) + Class;
	MarkKnown(Key.c_str());
}

void Collect(xr_vector<SEncyclopediaGroup>& Groups)
{
	Groups.clear();
	if (!IsConfigured())
	{
		return;
	}
	EncyclopediaCatalogHelpers::ScanInventoryOnce();

	for (const xr_string& Id : EncyclopediaCatalogHelpers::SplitList(EncyclopediaCatalogHelpers::ReadString(EncyclopediaCatalogConfig::Root, "sections")))
	{
		const EncyclopediaCatalogHelpers::SSectionConfig Config = EncyclopediaCatalogHelpers::ReadSectionConfig(Id);
		SEncyclopediaGroup Group;
		Group.Id = Id.c_str();
		Group.Caption = Config.Caption;
		if (Config.Source == "classes")
		{
			EncyclopediaCatalogHelpers::CollectClasses(Group, Config);
		}
		else if (Config.Source == "list")
		{
			EncyclopediaCatalogHelpers::CollectList(Group, Config);
		}
		else if (Config.Source == "articles")
		{
			EncyclopediaCatalogHelpers::CollectArticles(Group, Config);
			EncyclopediaCatalogHelpers::CollectList(Group, Config);
		}
		else
		{
			EncyclopediaCatalogHelpers::CollectItems(Group, Config);
		}
		std::stable_sort(Group.Entries.begin(), Group.Entries.end(), [](const SEncyclopediaEntry& Left, const SEncyclopediaEntry& Right)
		{
			return xr_strcmp(Left.Title.c_str(), Right.Title.c_str()) < 0;
		});
		if (!Group.Entries.empty())
		{
			Groups.push_back(std::move(Group));
		}
	}
}
}
