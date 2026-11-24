#include "stdafx.h"
#include "Autotest.h"
#include "Render.h"
#include "IGame_Level.h"
#include "Stats.h"

#include "IGame_Persistent.h"
#include "IGame_Menu.h"
#include "XR_IOConsole.h"
#include "xr_input.h"
#include "IInputReceiver.h"

#include "../Include/xrRender/RenderDeviceRender.h"
#include "../xrRHI/RHI.h"
#include "../3rd-party/renderdoc/renderdoc/api/app/renderdoc_app.h"

#ifdef IXR_WINDOWS
#include <psapi.h>
#else
#include <sys/resource.h>
#endif

#ifdef DEBUG_DRAW
void ExportProfilerOverlay();
void BeginProfilerCapture();
void EndProfilerCapture();
#endif


extern int fps_limit, main_menu_fps_limit;
extern XRCORE_API bool ignore_error_window;

namespace
{
	struct Sample
	{
		float	calc, dump, total;
		u32		calls, verts, polys, dips;
		u32		hash;
		float	frame;
		u32		counted;
		u32		step;
	};

	u32					s_target	= 120;
	u32					s_warmup	= 150;
	u32					s_timeout	= 300;
	bool				s_hash		= false;
	string_path			s_out		= { 0 };
	string_path			s_cmd		= { 0 };
	string_path			s_post_cmd	= { 0 };
	bool				s_post_pending = false;
	string_path			s_shot		= { 0 };
	u32					s_shot_every = 0;

	// -autotest_capture, -autotest_profile: what is taken once the measured frames are done
	bool				s_capture	= false;
	bool				s_profile	= false;
	string_path			s_capture_path = { 0 };
	RENDERDOC_API_1_6_0*	s_renderdoc = nullptr;
	u32					s_capture_count = 0;
	u32					s_capture_wait = 0;
	constexpr u32		CaptureFrames = 600;

	// -autotest_script: the steps of one run, a line each
	xr_vector<xr_string>	s_steps;
	u32					s_step		= 0;
	u32					s_step_wait	= 0;
	bool				s_step_loading = false;
	bool				s_step_unloaded = false;
	bool				s_step_menu = false;

	// -autotest_dt, settle: the clocks move only on the frames a run counts
	float				s_step_dt	= 0.0f;
	u32					s_counted	= 0;
	u32					s_settle	= 0;
	u32					s_starts	= 0;
	u32					s_anchored	= 0;
	constexpr u32		SettleAlign = 256;

	// profile N: the frame profiler over N counted frames; its capture is restarted once it runs, two frames before them
	u32					s_profile_lead = 0;
	u32					s_profile_wait = 0;
	bool				s_profiling = false;
	constexpr u32		ProfileLead = 4;

	// A sample a drawn level frame, its time from the start of its frame to the start of the next
	CTimer				s_frame_clock;
	bool				s_frame_drawn = false;
	bool				s_frame_counted = false;
	bool				s_frame_step = false;
	bool				s_sample_open = false;

	bool				s_started	= false;
	bool				s_done		= false;
	u32					s_seen		= 0;
	u32					s_errors	= 0;
	CTimer				s_clock;
	xr_vector<Sample>	s_samples;
	xr_vector<u8>		s_pixels;
	string_path			s_frame_path = {};
	float				s_frame_aspect = 0.0f;
	float				s_video_quality = 0.9f;
	s32					s_frame_status = 0;
	xr_vector<u8>		s_jpeg;

	class MjpegAvi
	{
	public:
		bool Open(const char* Path, double Rate)
		{
			xr_strcpy(Base, Path);
			Parts = 0;
			FrameRate = Rate;
			return Start();
		}

		bool Add(const xr_vector<u8>& Jpeg, u32 FrameWidth, u32 FrameHeight)
		{
			const u32 Size = static_cast<u32>(Jpeg.size());
			if (!File || Jpeg.size() > MaxMovie || (Frames && (FrameWidth != Width || FrameHeight != Height)))
			{
				return false;
			}
			if (Moved + 8u + Size + 1u > MaxMovie && (!Finish() || !Start()))
			{
				return false;
			}
			Width = FrameWidth;
			Height = FrameHeight;
			Index.push_back({ Moved + 4, Size });
			File->w("00dc", 4);
			File->w_u32(Size);
			File->w(Jpeg.data(), Size);
			(Size & 1) ? File->w_u8(0) : (void)0;
			Moved += 8 + Size + (Size & 1);
			Largest = std::max(Largest, Size);
			++Frames;
			return true;
		}

		u32 Close()
		{
			Finish();
			return Parts;
		}

		bool IsOpen() const
		{
			return File != nullptr;
		}

	private:
		struct Entry
		{
			u32 Offset;
			u32 Size;
		};

		static constexpr u32 HeaderSize = 224, MaxMovie = 1u << 30;

		bool Start()
		{
			string_path Path;
			const size_t Stem = xr_strlen(Base) > 4 ? xr_strlen(Base) - 4 : xr_strlen(Base);
			xr_sprintf(Path, Parts ? "%.*s_%u.avi" : "%.*s", static_cast<int>(Parts ? Stem : xr_strlen(Base)), Base, Parts + 1);
			File = FS.w_open(Path);
			if (File && !File->valid())
			{
				FS.w_close(File);
			}
			if (!File)
			{
				return false;
			}
			++Parts;
			Index.clear();
			Frames = Moved = Largest = 0;
			WriteHeader();
			return true;
		}

		bool Finish()
		{
			if (!File)
			{
				return false;
			}
			File->w("idx1", 4);
			File->w_u32(16 * Frames);
			for (const Entry& Item : Index)
			{
				File->w("00dc", 4);
				File->w_u32(0x10);
				File->w_u32(Item.Offset);
				File->w_u32(Item.Size);
			}
			File->seek(0);
			WriteHeader();
			FS.w_close(File);
			return true;
		}

		void WriteHeader()
		{
			const u32 Scale = 1000, Rate = static_cast<u32>(std::lround(FrameRate * Scale));
			const auto Tag = [this](const char* Code) { File->w(Code, 4); };
			Tag("RIFF");
			File->w_u32(HeaderSize - 8 + Moved + 8 + 16 * Frames);
			Tag("AVI ");
			Tag("LIST");
			File->w_u32(192);
			Tag("hdrl");
			Tag("avih");
			File->w_u32(56);
			const u32 Main[14] = { static_cast<u32>(std::lround(1000000.0 / FrameRate)), static_cast<u32>(Largest * FrameRate), 0, 0x10, Frames, 0, 1, Largest, Width, Height, 0, 0, 0, 0 };
			File->w(Main, sizeof(Main));
			Tag("LIST");
			File->w_u32(116);
			Tag("strl");
			Tag("strh");
			File->w_u32(56);
			Tag("vids");
			Tag("MJPG");
			const u32 Stream[10] = { 0, 0, 0, Scale, Rate, 0, Frames, Largest, u32(-1), 0 };
			File->w(Stream, sizeof(Stream));
			const u16 Frame[4] = { 0, 0, static_cast<u16>(Width), static_cast<u16>(Height) };
			File->w(Frame, sizeof(Frame));
			Tag("strf");
			File->w_u32(40);
			File->w_u32(40);
			File->w_u32(Width);
			File->w_u32(Height);
			File->w_u16(1);
			File->w_u16(24);
			Tag("MJPG");
			const u32 Bitmap[5] = { Width * Height * 3, 0, 0, 0, 0 };
			File->w(Bitmap, sizeof(Bitmap));
			Tag("LIST");
			File->w_u32(4 + Moved);
			Tag("movi");
		}

		IWriter* File = nullptr;
		string_path Base = {};
		xr_vector<Entry> Index;
		double FrameRate = 30.0;
		u32 Parts = 0, Frames = 0, Moved = 0, Largest = 0, Width = 0, Height = 0;
	};

	MjpegAvi s_video;

	void OnLog(const char* line)
	{
		if (line && strstr(line, "[error]"))
			s_errors++;
	}

	bool IsLevelReady()
	{
		return g_pGameLevel && !Device.dwPrecacheFrame && g_loading_events.empty();
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

	// The game's picture, the view: its corner of the output target in the builds with the tools, where the gamma pass
	// put it in the window in the others. IsWindow takes the whole window, the tools' windows and the bars included
	bool Readback(Irect& Area, u32& Pitch, bool IsWindow = false)
	{
#if defined(DEBUG_DRAW) && defined(IXR_WINDOWS)
		IRHIRenderTargetView* rtv = IsWindow ? GRHI->DevicePtr->SwapChainRTV : GRHI->DevicePtr->RenderRTV;
#else
		IRHIRenderTargetView* rtv = GRHI->DevicePtr->SwapChainRTV;
#endif
		if (!rtv)
			return false;

		// The whole surface is read: a reset can make the targets larger between shots, and the swap chain has the
		// window's size
		const size_t Size = size_t(std::max(Device.OutputWidth, GRHI->SwapChain.Width) + 64) * size_t(std::max(Device.OutputHeight, GRHI->SwapChain.Height) + 64) * 4;
		if (s_pixels.size() < Size)
			s_pixels.resize(Size);

		u32 w = 0, h = 0;
		Pitch = 0;
		if (!GRHI->DevicePtr->ReadRenderTargetPixels(rtv, s_pixels.data(), (u32)s_pixels.size(), w, h, Pitch))
			return false;

		const Irect View = IsWindow ? Irect{ 0, 0, int(w), int(h) } : rtv == GRHI->DevicePtr->SwapChainRTV ? Device.PresentRect : Irect{ 0, 0, int(Device.TargetWidth), int(Device.TargetHeight) };
		Area.set(std::clamp(View.x1, 0, int(w)), std::clamp(View.y1, 0, int(h)), 0, 0);
		Area.x2 = std::clamp(View.x2, Area.x1, int(w));
		Area.y2 = std::clamp(View.y2, Area.y1, int(h));
		return Area.width() > 0 && Area.height() > 0;
	}

	u32 Hash(const Irect& Area, u32 pitch)
	{
		u32 crc = 0;
		for (int y = Area.y1; y < Area.y2; ++y)
			crc = crc32(s_pixels.data() + size_t(y) * pitch + size_t(Area.x1) * 4, size_t(Area.width()) * 4, crc);

		return crc;
	}

	// Swapchain is DXGI_FORMAT_B8G8R8A8_UNORM, which is TGA's native byte order.
	bool WriteTGA(const char* name, u32 x, u32 y, u32 w, u32 h, u32 pitch)
	{
		IWriter* W = FS.w_open(name);
		if (W && !W->valid())
			FS.w_close(W);
		if (!W)
			return false;

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
		for (u32 row_y = 0; row_y < h; ++row_y)
		{
			CopyMemory(row.data(), s_pixels.data() + size_t(y + row_y) * pitch + size_t(x) * 4, row.size());
			for (size_t alpha = 3; alpha < row.size(); alpha += 4)
				row[alpha] = 0xff;

			W->w(row.data(), (u32)row.size());
		}

		FS.w_close(W);
		return true;
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

	// Resident and committed bytes of the process, now and at their peak; a field the system does not give stays 0
	void LogProcessMemory()
	{
		u64 Values[4] = {};
#ifdef IXR_WINDOWS
		PROCESS_MEMORY_COUNTERS Counters = {};

		if (GetProcessMemoryInfo(GetCurrentProcess(), &Counters, sizeof(Counters)))
		{
			Values[0] = Counters.WorkingSetSize;
			Values[1] = Counters.PeakWorkingSetSize;
			Values[2] = Counters.PagefileUsage;
			Values[3] = Counters.PeakPagefileUsage;
		}
#else
		const char* const Keys[4] = { "VmRSS:", "VmHWM:", "VmSize:", "VmPeak:" };

		if (FILE* File = fopen("/proc/self/status", "r"))
		{
			char Line[256];

			while (fgets(Line, sizeof(Line), File))
			{
				for (u32 Key = 0; Key < 4; ++Key)
				{
					Values[Key] = strncmp(Line, Keys[Key], xr_strlen(Keys[Key])) ? Values[Key] : strtoull(Line + xr_strlen(Keys[Key]), nullptr, 10) << 10;
				}
			}

			fclose(File);
		}

		rusage Usage = {};

		if (!Values[1] && !getrusage(RUSAGE_SELF, &Usage))
		{
#ifdef IXR_APPLE
			Values[1] = u64(Usage.ru_maxrss);
#else
			Values[1] = u64(Usage.ru_maxrss) << 10;
#endif
		}
#endif
		Msg("~ [autotest] process memory: working set %llu MB (peak %llu MB), commit %llu MB (peak %llu MB)", Values[0] >> 20, Values[1] >> 20, Values[2] >> 20, Values[3] >> 20);
	}

	void Report()
	{
		xr_vector<float> calc, dump, total, calls, packets;
		u32 MostCalls = 0;
		calc.reserve(s_samples.size());
		dump.reserve(s_samples.size());
		total.reserve(s_samples.size());
		calls.reserve(s_samples.size());
		packets.reserve(s_samples.size());

		for (const Sample& S : s_samples)
		{
			if (!S.counted)
			{
				continue;
			}

			calc.push_back(S.calc);
			dump.push_back(S.dump);
			total.push_back(S.total);
			calls.push_back(float(S.calls));
			packets.push_back(float(S.dips));
			MostCalls = std::max(MostCalls, S.calls);
		}

		if (IWriter* W = FS.w_open(s_out))
		{
			W->w_string("frame,calc_ms,dump_ms,total_ms,calls,verts,polys,static_dips,hash,frame_ms,counted,step");
			for (u32 i = 0; i < s_samples.size(); ++i)
			{
				const Sample& S = s_samples[i];
				string512 line;
				xr_sprintf(line, "%u,%.4f,%.4f,%.4f,%u,%u,%u,%u,%08x,%.4f,%u,%u",
					i, S.calc, S.dump, S.total, S.calls, S.verts, S.polys, S.dips, S.hash, S.frame, S.counted, S.step);
				W->w_string(line);
			}
			FS.w_close(W);
		}

		Msg("~ [autotest] frames=%u errors=%u out=%s", (u32)s_samples.size(), s_errors, s_out);
		Msg("~ [autotest] median calc=%.3fms dump=%.3fms total=%.3fms",
			Median(calc), Median(dump), Median(total));
		Msg("~ [autotest] old path: DIP/DP %u median, %u most over %u counted frames, graph packets %u median", u32(Median(calls)), MostCalls, u32(calls.size()), u32(Median(packets)));
		LogProcessMemory();


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

	// One step of -autotest_script, or the wait for the one before it. A line each, in order:
	//   start <save>   loads the save: from the main menu, or over the level that is up
	//   cmd <command>  runs a console command
	//   menu           leaves the level for the main menu
	//   wait <frames>  lets that many frames of the loaded level pass
	//   mouse_down <button>, mouse_up <button>  presses or releases a mouse button (0 left, 1 right) for the game: held until released
	//   key_down <code>, key_up <code>  the same for a key, by its SDL scancode (12 is I)
	//   mouse_move <dx> <dy>  moves the mouse by so many counts, once: positive dy looks down
	//   shot <name>    writes the frame as $logs$\<name>.tga
	//   capture        takes a RenderDoc capture (with -autotest_capture) and ends the run once it is written
	//   settle [N]     holds N level frames or more, until the frame number is a multiple of SettleAlign; the next frame is a camera cut
	//   profile <N>    holds ProfileLead frames, then waits N + 1 frames; the frame profiler's capture holds the last N of them
	// The run ends after the last step
	// A script's key and button steps, given to the game where its own input arrives: at the start of the next frame,
	// before the game thread runs. Given at the frame's end they ran the game's scripts beside that thread's
	struct ScriptInput
	{
		int Code;
		bool IsMouse;
		bool IsDown;
		int MoveY = 0;
		bool IsMove = false;
	};
	xr_vector<ScriptInput> s_inputs;

	void RunScript()
	{
		const bool IsReady = IsLevelReady();

		if (s_step_loading)
		{
			// The level that was up goes away before the new one comes
			s_step_unloaded |= !IsReady;

			if (!IsReady || !s_step_unloaded)
			{
				return;
			}

			s_step_loading = false;
		}

		if (s_step_wait)
		{
			s_step_wait -= IsReady ? 1 : 0;
			return;
		}

		// The profiler is on by now: a restarted capture begins two frames on, with the second frame of the wait
		if (s_profile_lead)
		{
			s_profile_lead -= IsReady ? 1 : 0;

			if (!s_profile_lead)
			{
#ifdef DEBUG_DRAW
				BeginProfilerCapture();
#endif
				s_step_wait = s_profile_wait + 1;
				s_profiling = true;
			}

			return;
		}

		if (s_settle)
		{
			s_settle -= IsReady && s_settle > 1 ? 1 : 0;

			if (s_settle > 1 || !IsReady || (Device.dwFrame + 1) % SettleAlign)
			{
				return;
			}

			s_settle = 0;
			Device.CutFrame = Device.dwFrame + 1;
			return;
		}

		const bool IsMenu = g_pGamePersistent && g_pGamePersistent->m_pMainMenu && g_pGamePersistent->m_pMainMenu->IsActive() && g_loading_events.empty();

		if (s_step_menu)
		{
			if (g_pGameLevel || !IsMenu)
			{
				return;
			}

			s_step_menu = false;
		}

		if (s_post_pending || (!IsReady && !IsMenu))
		{
			return;
		}

		if (s_step == s_steps.size())
		{
			Finish();
		}

		const char* Line = s_steps[s_step++].c_str();
		string_path Name = {};
		Msg("~ [autotest] step: %s", Line);

		if (1 == sscanf(Line, "start %259s", Name))
		{
			Console->Execute("keypress_on_start 0");
			xr_sprintf(s_post_cmd, g_pGameLevel ? "load %s" : "start server(%s/single/alife/load) client(localhost)", Name);
			s_post_pending = s_step_loading = true;
			s_step_unloaded = !g_pGameLevel;

			++s_starts;
		}
		else if (!strcmp(Line, "menu"))
		{
			xr_strcpy(s_post_cmd, "disconnect");
			s_post_pending = s_step_menu = true;
		}
		else if (!strncmp(Line, "cmd ", 4))
		{
			xr_strcpy(s_post_cmd, Line + 4);
			s_post_pending = true;
		}
		else if (const bool IsWindow = 1 == sscanf(Line, "shot_window %259s", Name); IsWindow || 1 == sscanf(Line, "shot %259s", Name))
		{
			string_path Leaf, File;
			Irect Area;
			u32 Pitch;
			FS.update_path(File, "$logs$", xr_strconcat(Leaf, Name, ".tga"));

			if (!Readback(Area, Pitch, IsWindow) || !WriteTGA(File, Area.x1, Area.y1, Area.width(), Area.height(), Pitch))
			{
				Msg("! [autotest] shot: %s is not written", File);
			}
		}
		else if (!strncmp(Line, "settle", 6) && (!Line[6] || Line[6] == ' '))
		{
			s_settle = u32(atoi(Line + 6)) + 1;
		}
		else if (1 == sscanf(Line, "profile %u", &s_profile_wait) && s_profile_wait)
		{
			s_profile_lead = ProfileLead;
#ifdef DEBUG_DRAW
			BeginProfilerCapture();
#else
			Msg("! [autotest] profile: this build has no frame profiler");
#endif
		}
		else if (!strcmp(Line, "capture"))
		{
			if (s_renderdoc)
			{
				s_capture_count = s_renderdoc->GetNumCaptures();
				s_capture_wait = CaptureFrames;
				s_renderdoc->TriggerCapture();
			}
		}
		else if (int Button = 0; 1 == sscanf(Line, "mouse_down %d", &Button) || 1 == sscanf(Line, "mouse_up %d", &Button))
		{
			s_inputs.push_back({ Button, true, Line[6] == 'd' });
		}
		else if (int X = 0, Y = 0; 2 == sscanf(Line, "mouse_move %d %d", &X, &Y))
		{
			s_inputs.push_back({ X, true, false, Y, true });
		}
		else if (int Key = 0; 1 == sscanf(Line, "key_down %d", &Key) || 1 == sscanf(Line, "key_up %d", &Key))
		{
			s_inputs.push_back({ Key, false, Line[4] == 'd' });
		}
		else if (1 != sscanf(Line, "wait %u", &s_step_wait))
		{
			Msg("! [autotest] step is not understood");
		}
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
		for (const ScriptInput& Input : s_inputs)
		{
			IInputReceiver* Receiver = pInput ? pInput->CurrentIR() : nullptr;

			if (!Receiver)
			{
				continue;
			}

			if (Input.IsMove)
			{
				Receiver->IR_OnMouseMove(Input.Code, Input.MoveY);
			}
			else if (Input.IsMouse)
			{
				Input.IsDown ? Receiver->IR_OnMousePress(Input.Code) : Receiver->IR_OnMouseRelease(Input.Code);
			}
			else
			{
				Input.IsDown ? Receiver->IR_OnKeyboardPress(Input.Code) : Receiver->IR_OnKeyboardRelease(Input.Code);
			}
		}

		s_inputs.clear();

		const float FrameMs = s_frame_clock.GetElapsed_ms_f();
		s_frame_clock.Start();

		if (s_sample_open)
		{
			s_samples.back().frame = FrameMs;
			s_sample_open = false;
		}

		// The wait of a profile step is over: the frame before this one was its last
		if (s_profiling && !s_step_wait)
		{
			s_profiling = false;
#ifdef DEBUG_DRAW
			EndProfilerCapture();
#endif
		}

		// FrameEnd runs inside Device.End(), before the game thread of that frame is waited for.
		// Commands such as save call scripts and must not overlap with it.
		// Every save starts at the same engine time, an hour per start: forward only, and not while the game thread runs
		if (const u64 Anchor = u64(s_starts) * 3600 * 1000000; s_step_dt > 0.0f && s_starts != s_anchored)
		{
			s_anchored = s_starts;
			s_counted = 0;

			if (u64(Device.dwTimeGlobal) * 1000 < Anchor)
			{
				Device.AnchorTime(Anchor);
			}
			else
			{
				Msg("! [autotest] the engine time is past %u s: this start is not anchored", u32(Anchor / 1000000));
			}
		}

		s_frame_step = s_post_pending;

		if (s_post_pending)
		{
			s_post_pending = false;
			Msg("~ [autotest] post: %s", s_post_cmd);
			Console->Execute(s_post_cmd);
		}

		if (!s_started)
		{
			s_started	= true;
			s_target	= ReadParam("-autotest_frames ", 120);
			s_warmup	= ReadParam("-autotest_warmup ", 150);
			s_timeout	= ReadParam("-autotest_timeout ", 300);
			s_hash		= !!strstr(Core.Params, "-autotest_hash");

			if (Core.ParamsData.test(ECoreParams::autotest_dt))
			{
				const char* Step = strstr(Core.Params, "-autotest_dt");
				const float Ms = float(atof(Step + xr_strlen("-autotest_dt")));
				s_step_dt = std::clamp(Ms > 0.0f ? Ms : 1000.0f / 60.0f, 1.0f, 100.0f) / 1000.0f;
				Msg("~ [autotest] fixed step %.3f ms", s_step_dt * 1000.0f);
			}

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

			s_profile = !!strstr(Core.Params, "-autotest_profile");
			s_capture = !!strstr(Core.Params, "-autotest_capture");

			if (s_capture)
			{
				// RenderDoc hooks the graphics API when the process starts: only a library that is
				// loaded already can capture
				typedef int (RENDERDOC_CC* GetApi)(RENDERDOC_Version, void**);
#ifdef IXR_WINDOWS
				const char* Library = "renderdoc.dll";
#else
				const char* Library = "librenderdoc.so";
#endif
				SDL_SharedObject* Module = SDL_LoadObject(Library);
				GetApi Get = Module ? reinterpret_cast<GetApi>(SDL_LoadFunction(Module, "RENDERDOC_GetAPI")) : nullptr;

				if (!Get || Get(eRENDERDOC_API_Version_1_5_0, reinterpret_cast<void**>(&s_renderdoc)) != 1)
				{
					s_renderdoc = nullptr;
					Msg("! [autotest] capture: RenderDoc is not in the process, start the game from RenderDoc");
				}

				string_path leaf;
				const char* path = strstr(Core.Params, "-autotest_capture ");
				if (path && 1 == sscanf(path + xr_strlen("-autotest_capture "), "%259[^ ]", leaf) && leaf[0] != '-')
					xr_strcpy(s_capture_path, leaf);
				else
					FS.update_path(s_capture_path, "$logs$", "autotest");

				if (s_renderdoc)
				{
					s_renderdoc->SetCaptureFilePathTemplate(s_capture_path);
				}
			}

			if (const char* Script = strstr(Core.Params, "-autotest_script "); Script && 1 == sscanf(Script + xr_strlen("-autotest_script "), "%259[^ ]", leaf))
			{
				// A path of the system, not of the game's file system
				if (FILE* File = fopen(leaf, "r"))
				{
					string1024 Line;

					while (fgets(Line, sizeof(Line), File))
					{
						Line[strcspn(Line, "\r\n")] = 0;

						if (Line[0] && Line[0] != ';')
						{
							s_steps.emplace_back(Line);
						}
					}

					fclose(File);
				}

				Msg("~ [autotest] script: %s, %u steps", leaf, u32(s_steps.size()));
			}

			ignore_error_window = true;

			psDeviceFlags.set(rsFullscreen, FALSE);

			Msg("~ [autotest] frames=%u warmup=%u", s_target, s_warmup);

			s_samples.reserve(s_target);
			xrLogger::AddLogCallback(OnLog);
			psDeviceFlags.set(rsDeviceActive, TRUE);
			fps_limit = main_menu_fps_limit = 0;
			s_clock.Start();
		}

		// Here, not in FrameEnd: no frame is drawn while the start screen is up
		if (s_clock.GetElapsed_sec() > float(s_timeout))
		{
			Msg("~ [autotest] timed out after %us at step %u of %u, %u of %u frames", s_timeout, s_step, u32(s_steps.size()), u32(s_samples.size()), s_target);
			Finish();
		}

		const bool IsCounted = IsLevelReady() && !s_settle && (s_steps.empty() || s_step_wait);
		s_frame_drawn = g_pGameLevel && g_loading_events.empty();
		s_frame_counted = IsCounted;

		if (s_step_dt > 0.0f)
		{
			// Without a level every frame takes the step: the start screen and the intro run on time. With one, only
			// the counted frames do; a step below a microsecond moves none of the clocks
			Device.FixedDelta = IsCounted || !g_pGameLevel ? s_step_dt : EPS_S;
			s_counted += IsCounted ? 1 : 0;
			::Random.seed(s32(s_counted * 0x9E3779B9u));
		}

		// A capture is taken of a window that is shown
		if (!s_capture)
		{
			SDL_HideWindow(g_AppInfo.Window);
		}

#ifdef DEBUG_DRAW
		// The profiler gathers while its window is open. Not while the level loads: loading fills
		// its table of labels and leaves no room for the render passes of the GPU
		if (s_profile && IsLevelReady())
		{
			Engine.External.EditorStates[static_cast<u8>(EditorUI::Profiler)] = true;
		}
#endif

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

		// The measured frames are done: the run ends once the capture is on disk
		if (s_capture_wait)
		{
			const bool IsWritten = s_renderdoc->GetNumCaptures() > s_capture_count;

			if (IsWritten)
			{
				string_path File;
				u32 Length = sizeof(File);
				s_renderdoc->GetCapture(s_capture_count, File, &Length, nullptr);
				Msg("~ [autotest] capture: %s", File);
			}
			else if (--s_capture_wait == 0)
			{
				Msg("! [autotest] capture: no file after %u frames", CaptureFrames);
			}

			if (IsWritten || !s_capture_wait)
			{
				Finish();
			}

			return;
		}

		if (!s_steps.empty())
		{
			const u32 Step = s_step;
			RunScript();

			// A frame that ran a step or its console command
			if (s_frame_drawn)
			{
				Device.m_pRender->GetCacheStats(S.calls, S.verts, S.polys, S.dips);
				S.counted = s_frame_counted;
				S.step = s_frame_step || s_step != Step;
				s_samples.push_back(S);
				s_sample_open = true;
			}

			return;
		}

		if (s_done)
			return;

		if (!IsLevelReady())
			return;

		if (s_seen == s_warmup && s_post_cmd[0])
		{
			s_post_pending = true;
		}

		if (s_seen++ < s_warmup)
			return;

		Device.m_pRender->GetCacheStats(S.calls, S.verts, S.polys, S.dips);
		S.counted = 1;

		const u32 index = (u32)s_samples.size();
		const bool shot = s_shot_every && 0 == (index % s_shot_every);

		if (s_hash || shot)
		{
			Irect Area;
			u32 pitch;
			if (Readback(Area, pitch))
			{
				if (s_hash)
					S.hash = Hash(Area, pitch);
				if (shot)
				{
					string_path name;
					xr_sprintf(name, "%s_%04u.tga", s_shot, index);
					WriteTGA(name, Area.x1, Area.y1, Area.width(), Area.height(), pitch);
					Msg("~ [autotest] %s", name);
				}
			}
		}

		s_samples.push_back(S);
		s_sample_open = true;

		if (s_samples.size() >= s_target)
		{
			s_done = true;
#ifdef DEBUG_DRAW
			// Before the capture: its frames take seconds
			if (s_profile)
			{
				ExportProfilerOverlay();
			}
#endif

			if (!s_renderdoc)
			{
				Finish();
			}

			s_capture_count = s_renderdoc->GetNumCaptures();
			s_capture_wait = CaptureFrames;
			s_renderdoc->TriggerCapture();
		}
	}

	int Verdict()
	{
		if (!Active())
			return 0;

		// A scripted run passes when every step was taken
		if (!s_steps.empty())
			return s_errors || s_step < s_steps.size();

		if (s_errors || s_samples.size() < s_target)
			return 1;

		return 0;
	}

	void RequestFrame(const char* Path, float Aspect)
	{
		xr_strcpy(s_frame_path, Path);
		s_frame_aspect = Aspect;
		s_frame_status = 1;
	}

	s32 FrameStatus()
	{
		return s_frame_status;
	}

	void CaptureFrame()
	{
		Irect Area;
		u32 Pitch = 0;
		if (s_frame_status != 1)
			return;

		s_frame_status = -1;
		if (!Readback(Area, Pitch))
			return;

		const u32 Width = u32(Area.width()), Height = u32(Area.height());
		const float Screen = float(Width) / float(Height);
		const u32 CropWidth = s_frame_aspect > 0.0f && s_frame_aspect < Screen ? std::min(std::max(u32(std::lround(Height * s_frame_aspect)), 1u), Width) : Width;
		const u32 CropHeight = s_frame_aspect > Screen ? std::min(std::max(u32(std::lround(Width / s_frame_aspect)), 1u), Height) : Height;
		const u32 Left = u32(Area.x1) + (Width - CropWidth) / 2, Top = u32(Area.y1) + (Height - CropHeight) / 2;
		if (!s_video.IsOpen() && strext(s_frame_path) && !xr_strcmp(strext(s_frame_path), ".png"))
		{
			IWriter* File = SUCCEEDED(GRHI->EncodeFrame(s_pixels.data() + size_t(Top) * Pitch + size_t(Left) * 4, CropWidth, CropHeight, Pitch, s_jpeg, 0.0f)) ? FS.w_open(s_frame_path) : nullptr;
			if (File)
			{
				File->w(s_jpeg.data(), static_cast<u32>(s_jpeg.size()));
				FS.w_close(File);
				s_frame_status = 0;
			}
			return;
		}
		if (!s_video.IsOpen())
		{
			s_frame_status = WriteTGA(s_frame_path, Left, Top, CropWidth, CropHeight, Pitch) ? 0 : -1;
			return;
		}
		const u32 EvenWidth = std::max(CropWidth & ~1u, 2u), EvenHeight = std::max(CropHeight & ~1u, 2u);
		const bool IsEncoded = EvenWidth <= Width && EvenHeight <= Height && SUCCEEDED(GRHI->EncodeFrame(s_pixels.data() + size_t(Top) * Pitch + size_t(Left) * 4, EvenWidth, EvenHeight, Pitch, s_jpeg, s_video_quality));
		s_frame_status = IsEncoded && s_video.Add(s_jpeg, EvenWidth, EvenHeight) ? 0 : -1;
	}

	bool OpenVideo(const char* Path, double Rate, float Quality)
	{
		s_video_quality = std::clamp(Quality, 0.05f, 1.0f);
		s_video.Close();
		return Rate > 0.0 && s_video.Open(Path, Rate);
	}

	u32 CloseVideo()
	{
		return s_video.Close();
	}
}
