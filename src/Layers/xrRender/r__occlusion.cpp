#include "StdAfx.h"
#include "r__occlusion.h"

#include "QueryHelper.h"

R_occlusion::R_occlusion()
{
	enabled = !Core.ParamsData.test(ECoreParams::no_occq);
}

R_occlusion::~R_occlusion()
{
	occq_destroy();
}

void R_occlusion::occq_create(u32 limit)
{
	occq_size = limit;
	pool.reserve(limit);
	used.clear();
	fids.clear();
	results.clear();
	for (u32 i = 0; i < limit; ++i)
	{
		_Q q;
		q.order = i;
		q.Q = nullptr;
		if (FAILED(CreateQuery(&q.Q, D3DQUERYTYPE_OCCLUSION))) break;
		pool.push_back(q);
	}
}

void R_occlusion::occq_destroy()
{
	for (auto& q : used) _RELEASE(q.Q);
	for (auto& q : pool) _RELEASE(q.Q);
	used.clear();
	pool.clear();
	fids.clear();
	results.clear();
	for (auto& frame : frames) frame.clear();
}

void R_occlusion::occq_refresh()
{
	if (!enabled) return;
	occq_destroy();
	occq_create(occq_size);
}

void R_occlusion::occq_stats() const
{
	g_FontManager->pFontSystem->SetAligment(CGameFont::alCenter);
	g_FontManager->pFontSystem->SetColor(color_rgba(0, 255, 100, 255));
	g_FontManager->pFontSystem->Out(float(Device.Width) * 0.5f, 40,
		"pool: %d used: %d free: %d", pool.size(), used.size(), fids.size());
}

u32 R_occlusion::occq_begin(u32& ID)
{
	PROF_EVENT("R_occlusion::occq_begin");
	if (!enabled || pool.empty())
	{
		ID = iInvalidHandle;
		return 0;
	}

	_Q q = pool.back();
	pool.pop_back();

	if (!fids.empty())
	{
		ID = fids.back();
		fids.pop_back();
		used[ID] = q;
	}
	else
	{
		ID = (u32)used.size();
		used.push_back(q);
	}

	CHK_DX(BeginQuery(used[ID].Q));
	return used[ID].order;
}

void R_occlusion::occq_end(u32& ID)
{
	PROF_EVENT("R_occlusion::occq_end");
	if (!enabled || ID == iInvalidHandle) return;

	CHK_DX(EndQuery(used[ID].Q));

	u32 frameIndex = Device.dwFrame % kLatency;
	frames[frameIndex].push_back(ID);
}

R_occlusion::occq_result R_occlusion::occq_get(u32& ID, u32 timeout_ms)
{
	PROF_EVENT("R_occlusion::occq_get");
	if (!enabled || ID == iInvalidHandle) return 0xFFFFFFFF;

	_Q& q = used[ID];
	occq_result fragments = 0;
	HRESULT hr;
	CTimer T; T.Start();
	while ((hr = GetData(q.Q, &fragments, sizeof(fragments), 0x1)) == S_FALSE)
	{
		if (!SwitchToThread()) Sleep(ps_r2_wait_sleep);
		if (T.GetElapsed_ms() > timeout_ms) { fragments = 0xFFFFFFFF; break; }
	}

	if (hr == D3DERR_DEVICELOST) fragments = 0xFFFFFFFF;
	if (fragments == 0) RImplementation.stats.o_culled++;

	pool.push_back(q);
	used[ID].Q = nullptr;
	fids.push_back(ID);
	ID = 0;
	return fragments;
}

void R_occlusion::OnFrameEnd()
{
	u32 frameToCollect = (Device.dwFrame + 1) % kLatency;
	auto& bucket = frames[frameToCollect];

	for (u32 handle : bucket)
	{
		if (handle != iInvalidHandle)
		{
			occq_result frags = occq_get(handle, 0);
		}
	}

	bucket.clear();
}