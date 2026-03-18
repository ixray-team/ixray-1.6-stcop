#include "stdafx.h"

#ifdef IXR_WINDOWS
#include <Windows.h>
#endif

namespace
{
	xr_string pathToXrString(const std::filesystem::path& p)
	{
#ifdef IXR_WINDOWS
		try
		{
			return p.string().c_str();
		}
		catch (const std::exception&)
		{
			const std::wstring wide = p.wstring();
			if (wide.empty())
				return xr_string();
			int size = WideCharToMultiByte(CP_UTF8, 0, wide.c_str(), (int)wide.size(), nullptr, 0, nullptr, nullptr);
			if (size <= 0)
				return xr_string();
			xr_vector<char> buf(size + 1);
			WideCharToMultiByte(CP_UTF8, 0, wide.c_str(), (int)wide.size(), buf.data(), size, nullptr, nullptr);
			buf[size] = '\0';
			return xr_string(buf.data());
		}
#else
		return p.string().c_str();
#endif
	}
}

xr_string CFilePath::xstring() const
{
	return pathToXrString(*this);
}

xr_string CFilePath::xfilename() const
{
	return pathToXrString(inherited::filename());
}

CFilePath::operator xr_string() const
{
	return pathToXrString(*this);
}

CFilePath& CFilePath::operator=(const xr_string& Right)
{
	// FX: std moment...
	// private:
	//	string_type _Text;

	inherited::operator=(Right.c_str());
	return *this;
}

CFilePath& CFilePath::operator=(const char* Right)
{
	inherited::operator=(Right);
	return *this;
}

bool CFilePath::exists(const CFilePath& Path)
{
	return std::filesystem::exists(Path);
}

XRCORE_API CFilePath operator/(const CFilePath& _Left, const CFilePath& _Right)
{
	CFilePath Path = _Left;
	Path += "\\";
	Path += _Right;

	return std::move(Path);
}

CFilePath operator/(const xr_string& _Left, const xr_string& _Right)
{
	return CFilePath(_Left) / CFilePath(_Right);
}
