#pragma once

#ifdef SCRIPT_FILE

class CLuaEditor;

class CScriptFile
{

public:
	void		UpdateRelPathName		();
//	void		DeleteIntermediateFiles	();
//	bool		Compile					();
//	bool		IsModified				();
//	bool		Save					(CArchive& ar);
//	bool		Load					(CArchive& ar);
//	bool		HasFile					(CString strPathName);
	void		RemoveBreakPoint		(int nLine);
	void		SetBreakPointsIn		(CLuaEditor* pEditor);
	bool		HasBreakPoint			(int nLine);
	CScriptFile							();
	~CScriptFile						();

	void		AddDebugLine			(int nLine);
	void		RemoveAllDebugLines		();
	void		AddBreakPoint			(int nLine);
	void		RemoveAllBreakPoints	();

	bool		PositionBreakPoints		();
	int			GetNearestDebugLine		(int nLine);
	int			GetPreviousDebugLine	(int nLine);
	int			GetNextDebugLine		(int nLine);

	const char* GetName();

/*	void SetPathName(CString strPathName) { m_strPathName=strPathName; UpdateRelPathName(); };
	CString GetPathName() { return m_strPathName; };
	CString GetName();
	CString GetNameExt();
	CString GetOutputNameExt() { return GetName()+".out"; }
	CString GetOutputPathNameExt();
*/
protected:
//	CString m_strPathName, m_strRelPathName;
	string_path						m_strPathName;
	string_path						m_strRelPathName;
//	CMap<int, int, bool, bool> m_breakPoints;
	xr_map<int,bool>				m_breakPoints;
	int								m_nMinBreakPoint;
	int								m_nMaxBreakPoint;
//	CMap<int, int, bool, bool> m_debugLines;
	xr_map<int,bool>				m_debugLines;
	int								m_nMinDebugLine;
	int								m_nMaxDebugLine;
//	SYSTEMTIME	m_timeCompiled;

	typedef xr_map<int,bool>::iterator	uniIt;
};

#endif
