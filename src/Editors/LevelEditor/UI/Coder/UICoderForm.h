#pragma once
#include "CodeEditorUtils.h"
#include "../LayoutEditor/LuaCompletion.h"
#include "LtxCompletion.h"

// Text files of the game: CP1251 on disk, UTF-8 in the editor
class UICoderForm :
	public IEditorWnd,
	public pureFrame
{
public:
	UICoderForm();
	virtual ~UICoderForm();

	virtual void Draw() override;
	virtual void OnFrame() override;

	static void Create(int Tab);
	static void Destroy();
	static bool IsActive();
	static bool IsModified();

	static bool CanOpen(const xr_string& FullPath);
	static bool OpenFile(const xr_string& FullPath);
	static bool Save();
	static bool Undo(bool Redo);
	static void DrawTopBar(float ButtonSize);

private:
	enum class EFileKind : u8
	{
		Lua,
		Xml,
		Ini,
		Text
	};

	struct SDocument
	{
		xr_string Path;
		xr_string Name;
		EFileKind Kind = EFileKind::Text;
		xr_unique_ptr<TextEditor> View;
		// Written back as read
		bool UseCRLF = true;
		bool Utf8 = false;
		bool Utf8Bom = false;
		bool Modified = false;
		// Cached: the completion reads it every frame
		xr_string Text;
	};

	static EFileKind GetKind(const xr_string& FullPath);

	bool Load(SDocument& Document);
	bool SaveDocument(SDocument& Document);
	bool CloseDocument(size_t Index);

	void DrawToolbar(SDocument* Document, float ButtonSize);
	void DrawStatus(float ButtonSize);
	void DrawDocument(SDocument& Document);

	static UICoderForm* Form;

	xr_vector<xr_unique_ptr<SDocument>> Documents;
	size_t Active = 0;
	int SelectRequest = -1;

	// Lua with the names of the baked API, rebuilt with the database
	void UpdateLanguage();
	TextEditor::LanguageDefinition LuaLanguage;
	TextEditor::LanguageDefinition IniLanguage;
	u32 LanguageGeneration = 0;

	CCodeSearch Search;
	CLuaCompletion Completion;
	CLtxCompletion LtxCompletion;
	bool BakeRequest = false;
	xr_string StatusText;
};
