#include "stdafx.h"
#include "Autotest.h"
#include "Render.h"
#include "IGame_Level.h"
#include "Stats.h"

#include "IGame_Persistent.h"
#include "IGame_Menu.h"
#include "XR_IOConsole.h"

#include "../Include/xrRender/RenderDeviceRender.h"
#include "../xrCore/appinfo.h"
#include "../xrRHI/RHI.h"

#include <SDL3/SDL.h>

extern int fps_limit, main_menu_fps_limit;
extern XRCORE_API bool ignore_error_window;

namespace
{
	struct Sample
	{
		float	calc, dump, total;
		u32		calls, verts, polys, dips;
		u32		hash;
	};

	u32					s_target	= 120;
	u32					s_warmup	= 150;
	u32					s_timeout	= 300;
	bool				s_hash		= false;
	string_path			s_out		= { 0 };
	string_path			s_cmd		= { 0 };
	string_path			s_post_cmd	= { 0 };
	string_path			s_shot		= { 0 };
	u32					s_shot_every = 0;

	bool				s_started	= false;
	bool				s_done		= false;
	u32					s_seen		= 0;
	u32					s_errors	= 0;
	CTimer				s_clock;
	xr_vector<Sample>	s_samples;
	xr_vector<u8>		s_pixels;

	void OnLog(const char* line)
	{
		if (line && strstr(line, "[error]"))
			s_errors++;
	}

	u32 ReadParam(const char* key, u32 def)
	{
		const char* p = strstr(Core.Params, key);
		if (!p)
			return def;

		u32 value = def;
		if (1 != sscanf(p + xr_strlen(key), "%u", &value))
			return def;

		return value;
	}

	bool Readback(u32& w, u32& h, u32& pitch)
	{
		IRHIRenderTargetView* rtv = GRHI->DevicePtr->SwapChainRTV;
		if (!rtv)
			return false;

		if (s_pixels.empty())
			s_pixels.resize(size_t(Device.TargetWidth + 64) * size_t(Device.TargetHeight + 64) * 4);

		w = h = pitch = 0;
		return GRHI->DevicePtr->ReadRenderTargetPixels(rtv, s_pixels.data(), (u32)s_pixels.size(), w, h, pitch);
	}

	u32 Hash(u32 w, u32 h, u32 pitch)
	{
		u32 crc = 0;
		for (u32 y = 0; y < h; ++y)
			crc = crc32(s_pixels.data() + size_t(y) * pitch, size_t(w) * 4, crc);

		return crc;
	}

	// Swapchain is DXGI_FORMAT_B8G8R8A8_UNORM, which is TGA's native byte order.
	void WriteTGA(u32 frame, u32 w, u32 h, u32 pitch)
	{
		string_path name;
		xr_sprintf(name, "%s_%04u.tga", s_shot, frame);

		IWriter* W = FS.w_open(name);
		if (!W)
			return;

		u8 hdr[18] = { 0 };
		hdr[2]	= 2;
		hdr[12]	= u8(w & 0xff);		hdr[13] = u8(w >> 8);
		hdr[14]	= u8(h & 0xff);		hdr[15] = u8(h >> 8);
		hdr[16]	= 32;
		hdr[17]	= 8 | 0x20;

		W->w(hdr, sizeof(hdr));

		// Nothing writes swapchain alpha, so it reads back as 0 and every viewer shows the
		// dump as fully transparent. Force it opaque.
		xr_vector<u8> row(size_t(w) * 4);
		for (u32 y = 0; y < h; ++y)
		{
			CopyMemory(row.data(), s_pixels.data() + size_t(y) * pitch, row.size());
			for (size_t x = 3; x < row.size(); x += 4)
				row[x] = 0xff;

			W->w(row.data(), (u32)row.size());
		}

		FS.w_close(W);
		Msg("~ [autotest] %s", name);
	}

	float TakeTimer(CStatTimer& T)
	{
		float ms = T.GetElapsed_ms_f();
		T.FrameStart();
		return ms;
	}

	float Median(xr_vector<float>& v)
	{
		if (v.empty())
			return 0.f;

		std::sort(v.begin(), v.end());
		return v[v.size() / 2];
	}

	void Report()
	{
		xr_vector<float> calc, dump, total;
		calc.reserve(s_samples.size());
		dump.reserve(s_samples.size());
		total.reserve(s_samples.size());

		for (const Sample& S : s_samples)
		{
			calc.push_back(S.calc);
			dump.push_back(S.dump);
			total.push_back(S.total);
		}

		if (IWriter* W = FS.w_open(s_out))
		{
			W->w_string("frame,calc_ms,dump_ms,total_ms,calls,verts,polys,static_dips,hash");
			for (u32 i = 0; i < s_samples.size(); ++i)
			{
				const Sample& S = s_samples[i];
				string512 line;
				xr_sprintf(line, "%u,%.4f,%.4f,%.4f,%u,%u,%u,%u,%08x",
					i, S.calc, S.dump, S.total, S.calls, S.verts, S.polys, S.dips, S.hash);
				W->w_string(line);
			}
			FS.w_close(W);
		}

		Msg("~ [autotest] frames=%u errors=%u out=%s", (u32)s_samples.size(), s_errors, s_out);
		Msg("~ [autotest] median calc=%.3fms dump=%.3fms total=%.3fms",
			Median(calc), Median(dump), Median(total));


		if (!s_samples.empty())
		{
			const Sample& S = s_samples.back();
			Msg("~ [autotest] last calls=%u verts=%u polys=%u static_dips=%u hash=%08x",
				S.calls, S.verts, S.polys, S.dips, S.hash);
		}

		Msg("~ [autotest] RESULT: %s", Autotest::Verdict() ? "FAIL" : "PASS");
	}

	// Engine shutdown with a level still loaded null-derefs g_btnHint in CUIGameCustom::UnLoad,
	// which would replace the verdict with an access violation. The report is already written.
	[[noreturn]] void Finish()
	{
		Report();
		int code = Autotest::Verdict();
		xrLogger::FlushLog();
		xrLogger::CloseLog();
		exit(code);
	}
}

namespace Autotest
{
	bool Active()
	{
		return Core.ParamsData.test(ECoreParams::autotest);
	}

	void FrameBegin()
	{
		if (!s_started)
		{
			s_started	= true;
			s_target	= ReadParam("-autotest_frames ", 120);
			s_warmup	= ReadParam("-autotest_warmup ", 150);
			s_timeout	= ReadParam("-autotest_timeout ", 300);
			s_hash		= !!strstr(Core.Params, "-autotest_hash");

			string_path leaf;
			const char* name = strstr(Core.Params, "-autotest_out ");
			if (name && 1 == sscanf(name + xr_strlen("-autotest_out "), "%259[^ ]", leaf))
				xr_strcpy(s_out, leaf);
			else
				FS.update_path(s_out, "$logs$", "autotest.csv");

			s_shot_every = ReadParam("-autotest_shot ", strstr(Core.Params, "-autotest_shot") ? 1 : 0);
			if (s_shot_every)
				FS.update_path(s_shot, "$logs$", "autotest");

			if (const char* cmd = strstr(Core.Params, "-autotest_cmd "))
				xr_strcpy(s_cmd, cmd + xr_strlen("-autotest_cmd "));

			if (const char* pc = strstr(Core.Params, "-autotest_post_cmd "))
				xr_strcpy(s_post_cmd, pc + xr_strlen("-autotest_post_cmd "));

			if (char* tail = strstr(s_post_cmd, " -autotest"))
				*tail = 0;

			ignore_error_window = true;

			psDeviceFlags.set(rsFullscreen, FALSE);

			Msg("~ [autotest] frames=%u warmup=%u", s_target, s_warmup);

			s_samples.reserve(s_target);
			xrLogger::AddLogCallback(OnLog);
			psDeviceFlags.set(rsDeviceActive, TRUE);
			fps_limit = main_menu_fps_limit = 0;
			s_clock.Start();
		}

		SDL_HideWindow(g_AppInfo.Window);

		g_bEnableStatGather = true;

		// CStats::Show() owns the timer FrameEnd/FrameStart cycle and would consume the
		// accumulators before FrameEnd() reads them; keep all three of its triggers off.
		psDeviceFlags.set(rsStatistic, FALSE);
		psDeviceFlags.set(rsCameraPos, FALSE);
		if (Device.Statistic)
			Device.Statistic->errors.clear();

		if (s_cmd[0] && g_pGamePersistent && g_pGamePersistent->m_pMainMenu &&
			g_pGamePersistent->m_pMainMenu->IsActive() && g_loading_events.empty())
		{
			Console->Execute("keypress_on_start 0");

			// ';' separates commands so a run can toggle settings before starting the level
			for (char* p = s_cmd; *p; )
			{
				char* sep = strchr(p, ';');
				if (sep)
					*sep = 0;

				while (' ' == *p)
					++p;

				Msg("~ [autotest] %s", p);
				Console->Execute(p);

				p = sep ? sep + 1 : p + xr_strlen(p);
			}

			s_cmd[0] = 0;
		}
	}

	void FrameEnd()
	{
		if (!Device.Statistic || !Device.m_pRender)
			return;

		CStats& St = *Device.Statistic;

		Sample S = { 0 };
		S.calc	= TakeTimer(St.RenderCALC);
		S.dump	= TakeTimer(St.RenderDUMP);
		S.total	= TakeTimer(St.RenderTOTAL_Real);

		if (s_done)
			return;

		if (s_clock.GetElapsed_sec() > float(s_timeout))
		{
			Msg("~ [autotest] timed out after %us with %u/%u frames", s_timeout, (u32)s_samples.size(), s_target);
			s_done = true;
			Finish();
		}

		if (!g_pGameLevel || Device.dwPrecacheFrame || !g_loading_events.empty())
			return;

		if (s_seen == s_warmup && s_post_cmd[0])
		{
			Msg("~ [autotest] post: %s", s_post_cmd);
			Console->Execute(s_post_cmd);
		}

		if (s_seen++ < s_warmup)
			return;

		Device.m_pRender->GetCacheStats(S.calls, S.verts, S.polys, S.dips);

		const u32 index = (u32)s_samples.size();
		const bool shot = s_shot_every && 0 == (index % s_shot_every);

		if (s_hash || shot)
		{
			u32 w, h, pitch;
			if (Readback(w, h, pitch))
			{
				if (s_hash)
					S.hash = Hash(w, h, pitch);
				if (shot)
					WriteTGA(index, w, h, pitch);
			}
		}

		s_samples.push_back(S);

		if (s_samples.size() >= s_target)
		{
			s_done = true;
			Finish();
		}
	}

	int Verdict()
	{
		if (!Active())
			return 0;

		if (s_errors || s_samples.size() < s_target)
			return 1;

		return 0;
	}
}
