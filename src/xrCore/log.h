#pragma once

void XRCORE_API CorrectLog(const char* s);

template<typename ...Args>
void EngineLog(std::string_view Format, Args&&... ArgList)
{
	auto FormattedText = std::vformat(Format, std::make_format_args(ArgList...));
	CorrectLog(FormattedText.c_str());
}

template<typename ...Args>
void EngineLogW(std::wstring_view Format, Args&&... ArgList)
{
	std::wstring FormattedText = std::vformat(Format, std::make_wformat_args(ArgList...));
	CorrectLog(std::string(FormattedText.begin(), FormattedText.end()).c_str());
}

template<>
inline void EngineLog(std::string_view Message)
{
	CorrectLog(Message.data());
}

void 	XRCORE_API		LogWinErr	(LPCSTR msg, long 			err_code);

typedef void	( * LogCallback)	(LPCSTR string);
LogCallback	XRCORE_API			SetLogCB	(LogCallback cb);
void 	XRCORE_API				CreateLog	(BOOL no_log=FALSE);
void 							InitLog		();
void 							CloseLog	();
void	XRCORE_API				FlushLog	();

extern 	XRCORE_API	xr_vector<shared_str>*		LogFile;
extern 	XRCORE_API	BOOL						LogExecCB;
