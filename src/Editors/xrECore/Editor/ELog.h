//----------------------------------------------------
// file: Log.h
//----------------------------------------------------

#ifndef ELogH
#define ELogH
enum TMsgDlgType
{
	mtCustom =0,
	mtError =1,
	mtInformation = 2,
	mtConfirmation = 4,
	mtSkip = 8,
	mtWarning = 16
};

enum TMsgDlgButtons
{
	mbYes		= 1 << 0,
	mbNo		= 1 << 1,
	mbCancel	= 1 << 2,
	mbOK		= 1 << 3,
	mbSkip		= 1 << 4,

	// Values
	mrNone		= 0,
	mrYes,
	mrNo,
	mrCancel,
	mrOK,
	mrSkip,
};

class ECORE_API CLog
{
public:
	bool 		in_use;
public:
				CLog	(){in_use=false;}
	void 		Msg   	(TMsgDlgType mt, const char* _Format, ...);
	int 		DlgMsg 	(TMsgDlgType mt, const char* _Format, ...);
	int 		DlgMsg 	(TMsgDlgType mt, int btn, const char* _Format, ...);
	void Close();
};

inline void Log(const char*Text,const char*Text2){ Msg("%s %s",Text,Text2); }

void ECORE_API ELogCallback(const char* txt);

extern ECORE_API CLog ELog;

#endif /*_INCDEF_NETDEVICELOG_H_*/

