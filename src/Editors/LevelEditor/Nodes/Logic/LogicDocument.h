#pragma once

// Scripted logic of xr_logic: gamedata/configs/scripts/*.ltx
// Sections are schemes ("walker@camp"), condlist keys ("on_info = {+info} walker@next %=effect%") switch them
namespace LogicCondlist
{
	// '+' has info, '-' has no info, '=' function, '!' negated function, '~' probability
	struct SToken
	{
		char Kind = '=';
		xr_string Name;
		xr_string Args;

		xr_string Build() const;
	};

	struct SBranch
	{
		xr_vector<SToken> Conditions;
		xr_string Target;
		xr_vector<SToken> Effects;

		bool HasTarget() const;
	};

	// "5000 | {+a} sect %=b%, sect2": the prefix is "5000"
	void Parse(const xr_string& Value, xr_string& Prefix, xr_vector<SBranch>& Branches);
	xr_string Build(const xr_string& Prefix, const xr_vector<SBranch>& Branches);

	void ParseTokens(const xr_string& Text, xr_vector<SToken>& Tokens);
	xr_string BuildTokens(const xr_vector<SToken>& Tokens);
}

struct SLogicKey
{
	xr_string Name;
	xr_string Value;
	// With its ';'
	xr_string Comment;

	bool IsCondlist = false;
	xr_string Prefix;
	xr_vector<LogicCondlist::SBranch> Branches;

	void Parse();
	// Value from the branches
	void Rebuild();
};

struct SLogicLine
{
	// A comment or an empty line otherwise
	bool IsKey = false;
	xr_string Raw;
	SLogicKey Key;
	// An unchanged key is written back as read
	xr_string LoadedName;
	xr_string LoadedValue;
	xr_string LoadedComment;
};

struct SLogicSection
{
	xr_string Name;
	// "[name]:parent"
	xr_string Parents;
	xr_string HeaderComment;
	// Written back as read while the name is the same
	xr_string HeaderRaw;
	xr_string LoadedName;
	xr_vector<SLogicLine> Lines;

	// Editor data
	int Id = 0;
	ImVec2 Position = { 0, 0 };
	// The position is to be set to the node
	bool HasPosition = false;

	xr_string Scheme() const;
	SLogicKey* FindKey(const char* KeyName);
	SLogicKey& AddKey(const char* KeyName, const char* Value);
	void RemoveKey(size_t LineIndex);
};

class CLogicDocument
{
public:
	bool Load(const xr_string& Text);
	xr_string Save() const;

	SLogicSection* Find(const xr_string& SectionName);
	int IndexOf(const xr_string& SectionName) const;

	SLogicSection& AddSection(const xr_string& SectionName);
	void RemoveSection(const xr_string& SectionName);
	void RenameSection(const xr_string& OldName, const xr_string& NewName);
	xr_string UniqueName(const xr_string& Base) const;

	// Values that switch to a section, not plain data
	static bool IsCondlistKey(const xr_string& KeyName, const xr_string& Value);
	// "nil", "true", numbers are not sections
	static bool IsSectionReference(const xr_string& Target);

	// Before the first section
	xr_vector<xr_string> Header;
	xr_vector<xr_unique_ptr<SLogicSection>> Sections;

private:
	int NextId = 1;
};
