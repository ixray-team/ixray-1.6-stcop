#pragma once

#include "xr_ini.h"

// An authoring copy. Evaluation uses the engine parser, saving patches original text.
class XRCORE_API LtxDocument final
{
public:
	LtxDocument() = default;
	~LtxDocument() = default;
	bool Load(const char* FileName, xr_string& Error);
	bool Set(const char* Section, const char* Key, const char* Value, xr_string& Error);
	bool SetParents(const char* Section, const char* Parents, xr_string& Error);
	bool AddSection(const char* File, const char* Section, const char* Parents, xr_string& Error);
	bool Save(xr_string& Error);
	bool CanSave(xr_string& Error) const;
	bool IsDirty() const;
	const CInifile* GetConfig() const { return Config.get(); }
	const CInifile::SourceTextMap& GetTexts() const { return Texts; }
	xr_vector<xr_string> GetChangedFiles() const;
	xr_string ExportChanges() const;
	bool Restore(const CInifile::SourceTextMap& NewTexts, xr_string& Error);
	xr_unique_ptr<CInifile> CreateConfig() const;
	static bool IsIdentifier(xr_string_view Text);

private:
	bool Replace(const CInifile::SourceLocation& Source, const char* Key, const char* Value, bool IsParents, xr_string& Error);
	xr_string RootFile;
	CInifile::SourceTextMap SavedTexts;
	CInifile::SourceTextMap Texts;
	CInifile::SourceLists Lists;
	xr_unique_ptr<CInifile> Config;
};
