#include "stdafx.h"

#include "dxgi.h"

struct _uniq_mode
{
	_uniq_mode(LPCSTR v) :_val(v) {}
	LPCSTR _val;
	bool operator() (LPCSTR _other) { return !_stricmp(_val, _other); }
};

bool sort_vid_mode(DXGI_MODE_DESC& left, DXGI_MODE_DESC& right) {
	auto leftString = xr_string::ToString(left.Width) + xr_string::ToString(left.Height);
	auto rightString = xr_string::ToString(right.Width) + xr_string::ToString(right.Height);

	if (leftString.length() == rightString.length()) 
	{
		if (left.Width > right.Width) {
			return true;
		}
		else if (left.Width == right.Width) 
		{
			return left.Height > right.Height;
		}

		return false;
	}

	return leftString.length() > rightString.length();
}

RHI_API void fill_vid_mode_list()
{
	if (vid_mode_token != nullptr)		
		return;

	xr_vector<LPCSTR>	_tmp;
	xr_vector<DXGI_MODE_DESC>	modes;

	IDXGIOutput* pOutput = nullptr;
	IDXGIAdapter* pAdapter = nullptr;
	IDXGIFactory* pFactory = nullptr;
	R_CHK(CreateDXGIFactory(IID_PPV_ARGS(&pFactory)));
	pFactory->EnumAdapters(0, &pAdapter);
	pAdapter->EnumOutputs(0, &pOutput);
	pAdapter->Release();
	pFactory->Release();
	VERIFY(pOutput);

	UINT num = 0;
	DXGI_FORMAT format = DXGI_FORMAT_R8G8B8A8_UNORM;
	UINT flags = 0;

	// Get the number of display modes available
	pOutput->GetDisplayModeList(format, flags, &num, 0);

	// Get the list of display modes
	modes.resize(num);
	pOutput->GetDisplayModeList(format, flags, &num, &modes.front());

	_RELEASE(pOutput);

	std::sort(modes.begin(), modes.end(), sort_vid_mode);

	for (u32 i = 0; i < num; ++i)
	{
		DXGI_MODE_DESC& desc = modes[i];
		string32 str;

		if (desc.Width < 800)
			continue;

		xr_sprintf(str, sizeof(str), "%dx%d", desc.Width, desc.Height);

		if (_tmp.end() != std::find_if(_tmp.begin(), _tmp.end(), _uniq_mode(str)))
			continue;

		_tmp.push_back(nullptr);
		_tmp.back() = xr_strdup(str);
	}

	u32 _cnt = (u32)_tmp.size() + 1;

	vid_mode_token = xr_alloc<xr_token>(_cnt);

	vid_mode_token[_cnt - 1].id = -1;
	vid_mode_token[_cnt - 1].name = nullptr;

#ifdef DEBUG
	Msg("Available video modes[%d]:", _tmp.size());
#endif // DEBUG
	for (u32 i = 0; i < _tmp.size(); ++i)
	{
		vid_mode_token[i].id = i;
		vid_mode_token[i].name = _tmp[i];
#ifdef DEBUG
		Msg("[%s]", _tmp[i]);
#endif // DEBUG
	}
}