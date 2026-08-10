#pragma once

class CUIBatchTools final : 
	public IEditorWnd
{
public:
	CUIBatchTools();

	static CUIBatchTools& Instance();
	void Draw() override;
	void Open() { bOpen = true; }

private:
	void Browse();
	void Run();

	char m_Folder[MAX_PATH]{};
	char m_Find[1024]{};
	char m_Value[4096]{};
	xr_string m_Status;
	int m_Target = 0;
	int m_Action = 0;
	bool m_Recursive = true;
	bool m_Substrings = false;
	bool m_CreateMissing = true;
};
