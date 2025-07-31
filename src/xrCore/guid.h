#pragma once

struct xrGUID
{
	u64 g[2];

	ICF bool operator==(const xrGUID& o) const
	{
		return ((g[0] == o.g[0]) && (g[1] == o.g[1]));
	}

	ICF bool operator!=(const xrGUID& o) const
	{
		return !(*this == o);
	}

	ICF void LoadLTX(CInifile& ini, LPCSTR section, LPCSTR name)
	{
		string128 buff;

		g[0] = ini.r_u64(section, xr_strconcat(buff, name, "_g0"));
		g[1] = ini.r_u64(section, xr_strconcat(buff, name, "_g1"));
	}

	ICF void SaveLTX(CInifile& ini, LPCSTR section, LPCSTR name)
	{
		string128 buff;

		ini.w_u64(section, xr_strconcat(buff, name, "_g0"), g[0]);
		ini.w_u64(section, xr_strconcat(buff, name, "_g1"), g[1]);
	}
};

XRCORE_API xrGUID generate_guid();
XRCORE_API LPCSTR generate_guid(const xrGUID& guid, LPSTR buffer, const u32& buffer_size);