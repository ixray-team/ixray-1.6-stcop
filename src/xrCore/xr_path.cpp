#include "stdafx.h"

xr_string CFilePath::xstring() const
{
	return inherited::string().c_str();
}

xr_string CFilePath::xfilename() const
{
	return inherited::filename().string().c_str();
}

CFilePath::operator xr_string() const
{
	return inherited::string().c_str();
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
	return _Left._parent_class() / _Right._parent_class();
}

CFilePath operator/(const xr_string& _Left, const xr_string& _Right)
{
	return CFilePath(_Left) / CFilePath(_Right);
}
