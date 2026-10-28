#include "stdafx.h"
#include "LogicDocument.h"

static xr_string Trim(const xr_string& Text)
{
	size_t Begin = 0;
	size_t End = Text.size();
	while (Begin < End && isspace((u8)Text[Begin]))
	{
		++Begin;
	}
	while (End > Begin && isspace((u8)Text[End - 1]))
	{
		--End;
	}
	return Text.substr(Begin, End - Begin);
}

// ';' starts a comment outside of quotes
static size_t FindComment(const xr_string& Line)
{
	bool Quoted = false;
	for (size_t Index = 0; Index < Line.size(); ++Index)
	{
		if (Line[Index] == '"')
		{
			Quoted = !Quoted;
		}
		else if (!Quoted && (Line[Index] == ';' || (Line[Index] == '/' && Index + 1 < Line.size() && Line[Index + 1] == '/')))
		{
			return Index;
		}
	}
	return xr_string::npos;
}

xr_string LogicCondlist::SToken::Build() const
{
	xr_string Result;
	Result += Kind;
	Result += Name;
	if (!Args.empty())
	{
		Result += "(" + Args + ")";
	}
	return Result;
}

bool LogicCondlist::SBranch::HasTarget() const
{
	return CLogicDocument::IsSectionReference(Target);
}

void LogicCondlist::ParseTokens(const xr_string& Text, xr_vector<SToken>& Tokens)
{
	Tokens.clear();
	size_t Index = 0;
	while (Index < Text.size())
	{
		while (Index < Text.size() && isspace((u8)Text[Index]))
		{
			++Index;
		}
		if (Index >= Text.size())
		{
			break;
		}

		SToken Token;
		if (strchr("+-=!~", Text[Index]) != nullptr)
		{
			Token.Kind = Text[Index++];
		}

		while (Index < Text.size() && !isspace((u8)Text[Index]) && Text[Index] != '(' && strchr("+-=!~", Text[Index]) == nullptr)
		{
			Token.Name += Text[Index++];
		}

		if (Index < Text.size() && Text[Index] == '(')
		{
			const size_t Close = Text.find(')', Index);
			const size_t End = Close == xr_string::npos ? Text.size() : Close;
			Token.Args = Text.substr(Index + 1, End - Index - 1);
			Index = Close == xr_string::npos ? Text.size() : Close + 1;
		}

		if (!Token.Name.empty())
		{
			Tokens.push_back(Token);
		}
		else if (Token.Args.empty())
		{
			// A lone sign
			++Index;
		}
	}
}

xr_string LogicCondlist::BuildTokens(const xr_vector<SToken>& Tokens)
{
	xr_string Result;
	for (const SToken& Token : Tokens)
	{
		if (!Result.empty())
		{
			Result += ' ';
		}
		Result += Token.Build();
	}
	return Result;
}

void LogicCondlist::Parse(const xr_string& Value, xr_string& Prefix, xr_vector<SBranch>& Branches)
{
	Prefix.clear();
	Branches.clear();

	// The parameter of on_timer, on_signal, on_actor_dist_le: before '|' outside of the brackets
	xr_string Body = Value;
	{
		int Depth = 0;
		bool Effect = false;
		for (size_t Index = 0; Index < Value.size(); ++Index)
		{
			const char Char = Value[Index];
			if (Char == '{')
			{
				++Depth;
			}
			else if (Char == '}')
			{
				--Depth;
			}
			else if (Char == '%')
			{
				Effect = !Effect;
			}
			else if (Char == '|' && Depth == 0 && !Effect)
			{
				// "actor|MST_step|10000|0| mob_home": the condlist is after the last one
				Prefix = Trim(Value.substr(0, Index));
				Body = Value.substr(Index + 1);
			}
		}
	}

	SBranch Branch;
	bool HasData = false;
	size_t Index = 0;
	auto Flush = [&]()
	{
		Branch.Target = Trim(Branch.Target);
		if (HasData || !Branch.Target.empty())
		{
			Branches.push_back(Branch);
		}
		Branch = SBranch();
		HasData = false;
	};

	while (Index < Body.size())
	{
		const char Char = Body[Index];
		if (Char == '{' || Char == '%')
		{
			const char CloseChar = Char == '{' ? '}' : '%';
			const size_t Close = Body.find(CloseChar, Index + 1);
			const size_t End = Close == xr_string::npos ? Body.size() : Close;
			xr_vector<SToken> Tokens;
			ParseTokens(Body.substr(Index + 1, End - Index - 1), Tokens);
			xr_vector<SToken>& Into = Char == '{' ? Branch.Conditions : Branch.Effects;
			Into.insert(Into.end(), Tokens.begin(), Tokens.end());
			HasData = true;
			Index = Close == xr_string::npos ? Body.size() : Close + 1;
		}
		else if (Char == ',')
		{
			Flush();
			++Index;
		}
		else
		{
			Branch.Target += Char;
			++Index;
		}
	}
	Flush();
}

xr_string LogicCondlist::Build(const xr_string& Prefix, const xr_vector<SBranch>& Branches)
{
	xr_string Result;
	if (!Prefix.empty())
	{
		Result = Prefix + " | ";
	}

	for (size_t Index = 0; Index < Branches.size(); ++Index)
	{
		const SBranch& Branch = Branches[Index];
		xr_string Part;
		if (!Branch.Conditions.empty())
		{
			Part += "{" + BuildTokens(Branch.Conditions) + "}";
		}
		if (!Branch.Target.empty())
		{
			if (!Part.empty())
			{
				Part += ' ';
			}
			Part += Branch.Target;
		}
		if (!Branch.Effects.empty())
		{
			if (!Part.empty())
			{
				Part += ' ';
			}
			Part += "%" + BuildTokens(Branch.Effects) + "%";
		}

		if (Index > 0)
		{
			Result += ", ";
		}
		Result += Part;
	}
	return Result;
}

void SLogicKey::Parse()
{
	LogicCondlist::Parse(Value, Prefix, Branches);
}

void SLogicKey::Rebuild()
{
	if (IsCondlist)
	{
		Value = LogicCondlist::Build(Prefix, Branches);
	}
}

xr_string SLogicSection::Scheme() const
{
	const size_t At = Name.find('@');
	return At == xr_string::npos ? Name : xr_string(Name.substr(0, At));
}

SLogicKey* SLogicSection::FindKey(const char* KeyName)
{
	for (SLogicLine& Line : Lines)
	{
		if (Line.IsKey && Line.Key.Name == KeyName)
		{
			return &Line.Key;
		}
	}
	return nullptr;
}

SLogicKey& SLogicSection::AddKey(const char* KeyName, const char* Value)
{
	// Before the empty lines that separate the sections
	size_t Insert = Lines.size();
	while (Insert > 0 && !Lines[Insert - 1].IsKey && Trim(Lines[Insert - 1].Raw).empty())
	{
		--Insert;
	}

	SLogicLine Line;
	Line.IsKey = true;
	Line.Key.Name = KeyName;
	Line.Key.Value = Value;
	Line.Key.IsCondlist = CLogicDocument::IsCondlistKey(Line.Key.Name, Line.Key.Value);
	Line.Key.Parse();
	return Lines.insert(Lines.begin() + Insert, Line)->Key;
}

void SLogicSection::RemoveKey(size_t LineIndex)
{
	if (LineIndex < Lines.size())
	{
		Lines.erase(Lines.begin() + LineIndex);
	}
}

bool CLogicDocument::IsCondlistKey(const xr_string& KeyName, const xr_string& Value)
{
	return KeyName.starts_with("on_") || KeyName == "active" || Value.find('{') != xr_string::npos || Value.find('%') != xr_string::npos;
}

bool CLogicDocument::IsSectionReference(const xr_string& Target)
{
	if (Target.empty() || Target == "nil" || Target == "true" || Target == "false")
	{
		return false;
	}
	return !isdigit((u8)Target[0]) && Target[0] != '-';
}

bool CLogicDocument::Load(const xr_string& Text)
{
	Header.clear();
	Sections.clear();

	SLogicSection* Current = nullptr;
	size_t Begin = 0;
	while (Begin <= Text.size())
	{
		size_t End = Text.find('\n', Begin);
		if (End == xr_string::npos)
		{
			End = Text.size();
		}
		xr_string Line = Text.substr(Begin, End - Begin);
		if (!Line.empty() && Line.back() == '\r')
		{
			Line.pop_back();
		}
		const bool Last = End >= Text.size();
		Begin = End + 1;

		// The last empty line is the end of the file
		if (Last && Line.empty())
		{
			break;
		}

		const xr_string Trimmed = Trim(Line);
		if (Trimmed.starts_with("["))
		{
			const size_t Close = Trimmed.find(']');
			if (Close != xr_string::npos)
			{
				xr_unique_ptr<SLogicSection> Section = xr_make_unique<SLogicSection>();
				Section->Id = NextId++;
				Section->Name = Trim(Trimmed.substr(1, Close - 1));
				Section->LoadedName = Section->Name;
				Section->HeaderRaw = Line;

				xr_string Rest = Trimmed.substr(Close + 1);
				const size_t Comment = FindComment(Rest);
				if (Comment != xr_string::npos)
				{
					Section->HeaderComment = Rest.substr(Comment);
					Rest = Rest.substr(0, Comment);
				}
				Rest = Trim(Rest);
				if (Rest.starts_with(":"))
				{
					Section->Parents = Trim(Rest.substr(1));
				}

				Current = Section.get();
				Sections.push_back(std::move(Section));
				continue;
			}
		}

		if (Current == nullptr)
		{
			Header.push_back(Line);
			continue;
		}

		SLogicLine Entry;
		Entry.Raw = Line;
		const size_t Comment = FindComment(Line);
		const xr_string Content = Trim(Comment == xr_string::npos ? Line : xr_string(Line.substr(0, Comment)));
		if (!Content.empty() && !Content.starts_with("#"))
		{
			Entry.IsKey = true;
			const size_t Equal = Content.find('=');
			Entry.Key.Name = Trim(Equal == xr_string::npos ? Content : xr_string(Content.substr(0, Equal)));
			Entry.Key.Value = Equal == xr_string::npos ? "" : Trim(Content.substr(Equal + 1));
			if (Comment != xr_string::npos)
			{
				Entry.Key.Comment = Line.substr(Comment);
			}
			Entry.LoadedName = Entry.Key.Name;
			Entry.LoadedValue = Entry.Key.Value;
			Entry.LoadedComment = Entry.Key.Comment;
			Entry.Key.IsCondlist = IsCondlistKey(Entry.Key.Name, Entry.Key.Value);
			if (Entry.Key.IsCondlist)
			{
				Entry.Key.Parse();
			}
		}
		Current->Lines.push_back(Entry);
	}

	// "meet = meet@camp": a plain value naming a section of the file is a link as well
	for (xr_unique_ptr<SLogicSection>& Section : Sections)
	{
		for (SLogicLine& Line : Section->Lines)
		{
			if (Line.IsKey && !Line.Key.IsCondlist && Find(Line.Key.Value) != nullptr)
			{
				Line.Key.IsCondlist = true;
				Line.Key.Parse();
			}
		}
	}
	return true;
}

xr_string CLogicDocument::Save() const
{
	xr_string Result;
	for (const xr_string& Line : Header)
	{
		Result += Line + "\n";
	}

	for (const xr_unique_ptr<SLogicSection>& Section : Sections)
	{
		if (!Section->HeaderRaw.empty() && Section->Name == Section->LoadedName)
		{
			Result += Section->HeaderRaw;
		}
		else
		{
			Result += "[" + Section->Name + "]";
		}
		if (Section->Name != Section->LoadedName && !Section->Parents.empty())
		{
			Result += ":" + Section->Parents;
		}
		if (Section->Name != Section->LoadedName && !Section->HeaderComment.empty())
		{
			Result += " " + Section->HeaderComment;
		}
		Result += "\n";

		for (const SLogicLine& Line : Section->Lines)
		{
			if (!Line.IsKey)
			{
				Result += Line.Raw + "\n";
				continue;
			}

			if (!Line.Raw.empty() && Line.Key.Name == Line.LoadedName && Line.Key.Value == Line.LoadedValue && Line.Key.Comment == Line.LoadedComment)
			{
				Result += Line.Raw + "\n";
				continue;
			}

			xr_string Text = Line.Key.Name;
			if (!Line.Key.Value.empty() || Line.Raw.find('=') != xr_string::npos || Line.Raw.empty())
			{
				Text += " = " + Line.Key.Value;
			}
			if (!Line.Key.Comment.empty())
			{
				Text += " " + Line.Key.Comment;
			}
			Result += Text + "\n";
		}
	}
	return Result;
}

SLogicSection* CLogicDocument::Find(const xr_string& SectionName)
{
	const int Index = IndexOf(SectionName);
	return Index < 0 ? nullptr : Sections[Index].get();
}

int CLogicDocument::IndexOf(const xr_string& SectionName) const
{
	for (size_t Index = 0; Index < Sections.size(); ++Index)
	{
		if (Sections[Index]->Name == SectionName)
		{
			return (int)Index;
		}
	}
	return -1;
}

SLogicSection& CLogicDocument::AddSection(const xr_string& SectionName)
{
	// An empty line between the sections
	if (!Sections.empty())
	{
		SLogicSection& Previous = *Sections.back();
		if (Previous.Lines.empty() || !Trim(Previous.Lines.back().Raw).empty() || Previous.Lines.back().IsKey)
		{
			Previous.Lines.push_back(SLogicLine());
		}
	}

	xr_unique_ptr<SLogicSection> Section = xr_make_unique<SLogicSection>();
	Section->Id = NextId++;
	Section->Name = SectionName;
	Sections.push_back(std::move(Section));
	return *Sections.back();
}

void CLogicDocument::RemoveSection(const xr_string& SectionName)
{
	const int Index = IndexOf(SectionName);
	if (Index >= 0)
	{
		Sections.erase(Sections.begin() + Index);
	}

	// The switches to it are cut
	for (xr_unique_ptr<SLogicSection>& Section : Sections)
	{
		for (SLogicLine& Line : Section->Lines)
		{
			if (!Line.IsKey || !Line.Key.IsCondlist)
			{
				continue;
			}
			bool Changed = false;
			for (LogicCondlist::SBranch& Branch : Line.Key.Branches)
			{
				if (Branch.Target == SectionName)
				{
					Branch.Target.clear();
					Changed = true;
				}
			}
			std::erase_if(Line.Key.Branches, [](const LogicCondlist::SBranch& Branch) { return Branch.Target.empty() && Branch.Conditions.empty() && Branch.Effects.empty(); });
			if (Changed)
			{
				Line.Key.Rebuild();
			}
		}
	}
}

void CLogicDocument::RenameSection(const xr_string& OldName, const xr_string& NewName)
{
	SLogicSection* Renamed = Find(OldName);
	if (Renamed == nullptr || OldName == NewName)
	{
		return;
	}
	Renamed->Name = NewName;

	for (xr_unique_ptr<SLogicSection>& Section : Sections)
	{
		for (SLogicLine& Line : Section->Lines)
		{
			if (!Line.IsKey || !Line.Key.IsCondlist)
			{
				continue;
			}
			bool Changed = false;
			for (LogicCondlist::SBranch& Branch : Line.Key.Branches)
			{
				if (Branch.Target == OldName)
				{
					Branch.Target = NewName;
					Changed = true;
				}
			}
			if (Changed)
			{
				Line.Key.Rebuild();
			}
		}
	}
}

xr_string CLogicDocument::UniqueName(const xr_string& Base) const
{
	if (IndexOf(Base) < 0)
	{
		return Base;
	}
	for (int Index = 2;; ++Index)
	{
		xr_string Name = Base + std::to_string(Index).c_str();
		if (IndexOf(Name) < 0)
		{
			return Name;
		}
	}
}
