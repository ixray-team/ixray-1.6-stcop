#pragma once
#include "plugins/TextEditor.h"

// ltx completion from the schema of the XREFLECT(IniRead) classes, baked into $app_data_root$\ltx_schema.txt.
// The class of a section comes from its "class" key, through its parents in every configs file
class CLtxCompletion
{
public:
	static xr_string GetSchemaPath();

	bool Load();
	bool IsLoaded() const { return !Schemas.empty(); }
	u32 GetGeneration() const { return LoadedGeneration; }
	// Counts of the schema and the index, shown in the Coder status
	xr_string GetStatus() const;
	// Ini with the hover declarations of the schema keys
	void BuildLanguage(TextEditor::LanguageDefinition& Out) const;

	// Text is the whole file: its sections override the indexed ones of the same file
	void BeforeRender(TextEditor& View);
	void AfterRender(TextEditor& View, const xr_string& Path, const xr_string& Text);
	// Marks the sections without their required keys
	void UpdateMarkers(TextEditor& View, const xr_string& Path, const xr_string& Text);

private:
	struct SField
	{
		xr_string Key;
		xr_string Type;
		bool Required = false;
		xr_string If;
		bool Nested = false;
	};

	struct SSchema
	{
		xr_vector<xr_string> Bases;
		xr_vector<SField> Fields;
	};

	struct SSection
	{
		xr_vector<xr_string> Parents;
		xr_hash_map<xr_string, xr_string> Keys;
		int Line = 0;
	};

	using SectionMap = xr_hash_map<xr_string, SSection>;

	// A key of the schemas of a section, the prefix of the nested schemas included
	struct SKey
	{
		xr_string Key;
		xr_string Type;
		bool Required = false;
		xr_string If;
		xr_string Schema;
	};

	struct SItem
	{
		xr_string Name;
		xr_string Detail;
		bool IsKey = true;
	};

	// Lines, when set, gets the section of every line
	static void ParseText(const xr_string& Text, SectionMap& Out, xr_vector<xr_string>* Lines = nullptr);
	void BuildIndex();

	const SSection* FindSection(const xr_string& Name) const;
	xr_string GetClassId(const xr_string& Section, int Depth = 0) const;
	bool HasKey(const xr_string& Section, const xr_string& Key, int Depth = 0) const;
	void ExpandSchema(const xr_string& Name, const xr_string& Prefix, xr_vector<SKey>& Out, int Depth = 0) const;
	xr_vector<SKey> GetKeys(const xr_string& Section) const;
	void Accept(TextEditor& View);

	u32 LoadedGeneration = 0;
	xr_hash_map<xr_string, SSchema> Schemas;
	// Class id of a section -> its schemas
	xr_hash_map<xr_string, xr_vector<xr_string>> ClassIds;

	// Every section of configs, the sections of the edited file come first
	bool Indexed = false;
	SectionMap Sections;
	SectionMap Local;
	xr_vector<xr_string> LocalLines;
	xr_string LocalPath;

	bool Open = false;
	bool ForceOpen = false;
	xr_vector<SItem> Items;
	int Current = 0;
	xr_string Word;
	int Line = -1;
	bool AppendAssign = false;

	int Move = 0;
	bool AcceptRequest = false;
	bool CloseRequest = false;
	bool KeysTaken = false;
	// The text view that had the focus: a click on the list gives it back
	struct ImGuiWindow* EditorWindow = nullptr;
};
