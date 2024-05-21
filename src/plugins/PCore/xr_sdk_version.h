#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SDK_VERSION_H__
#define __XR_SDK_VERSION_H__

namespace xray_re {

enum sdk_version
{
	SDK_VER_756,
	SDK_VER_1098,
	SDK_VER_1850,
	SDK_VER_0_4,
	SDK_VER_0_5,
	SDK_VER_0_6,
	SDK_VER_0_7,
	SDK_VER_UNKNOWN = -1,
	SDK_VER_DEFAULT = SDK_VER_0_7
};

inline sdk_version sdk_version_from_string(const std::string& str)
{
	if(str == "756")
		return SDK_VER_756;
	if(str == "1098")
		return SDK_VER_1098;
	if(str == "1850")
		return SDK_VER_1850;
	if(str == "0.4")
		return SDK_VER_0_4;
	if(str == "0.5")
		return SDK_VER_0_5;
	if(str == "0.6")
		return SDK_VER_0_6;
	if(str == "0.7")
		return SDK_VER_0_7;

	return SDK_VER_UNKNOWN;
}

inline std::string sdk_version_to_string(sdk_version ver)
{
	switch(ver)
	{
		case SDK_VER_756:		return "756"; break;
		case SDK_VER_1098:		return "1098"; break;
		case SDK_VER_1850:		return "1850"; break;
		case SDK_VER_0_4:		return "0.4"; break;
		case SDK_VER_0_5:		return "0.5"; break;
		case SDK_VER_0_6:		return "0.6"; break;
		case SDK_VER_0_7:		return "0.7"; break;
		default:				return "<unknown>";
	}
}

} // end of namespace xray_re

#endif
