/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray SDK
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "stdafx.h"
#include "EditorAutotest.h"

namespace
{
	enum class EView : u8
	{
		Lit,
		Unlit,
		Wireframe,
		Current
	};

	constexpr const char* ViewNames[] = { "lit", "unlit", "wireframe" };

	xr_vector<EView> Views;
	u32 ViewIdx = 0;
	u32 Frames = 120;
	u32 Timeout = 600;
	u32 FrameIdx = 0;
	u32 RenderFrame = 0;
	float Hours = 12.0f;
	string_path Shot = {};
	string256 Select = {};
	string256 Weather = {};
	Fvector CameraPos = {};
	Fvector CameraHpb = {};
	bool HasCamera = false;
	bool IsLoading = false;
	CTimer Clock;

	xrDebug::SendErrorPrototype* NextErrorCallback = nullptr;
	xrCriticalSection FailureLock;
	string1024 Failure = {};

	const char* Param(const char* Key)
	{
		const char* At = strstr(Core.Params, Key);
		return At ? At + xr_strlen(Key) : nullptr;
	}

	template<size_t Size>
	bool ReadToken(const char* Key, char (&Out)[Size])
	{
		static_assert(Size >= 256);
		const char* At = Param(Key);
		return At && 1 == sscanf(At, "%255[^ ]", Out);
	}

	void Fail(const char* Reason)
	{
		xrCriticalSectionGuard Guard(FailureLock);
		if (!Failure[0])
		{
			xr_sprintf(Failure, "%s", Reason[0] ? Reason : "unknown");
		}
	}

	// exit() would run the editor's shutdown: its dialogs and the destruction of a live device
	[[noreturn]] void Finish()
	{
		string1024 Reason;
		{
			xrCriticalSectionGuard Guard(FailureLock);
			xr_strcpy(Reason, Failure);
		}

		if (Reason[0])
		{
			Msg("~ [autotest] RESULT: FAIL: %s", Reason);
		}
		else
		{
			Msg("~ [autotest] RESULT: PASS");
		}

		xrLogger::FlushLog();
		xrLogger::CloseLog();
		std::quick_exit(Reason[0] ? 1 : 0);
	}

	// Every assertion fails the run. The editor's silent error mode goes on after them, and so does the run, to take its
	// shots; not after a fatal error
	void OnError(const char* Info)
	{
		if (NextErrorCallback)
		{
			NextErrorCallback(Info);
		}

		string1024 Reason = {};
		for (const char* Key : { "Expression    : ", "Description   : ", "Arguments     : " })
		{
			if (const char* At = strstr(Info, Key))
			{
				const size_t Used = xr_strlen(Reason);
				xr_sprintf(Reason + Used, sizeof(Reason) - Used, "%.*s ", int(strcspn(At, "\r\n")), At);
			}
		}

		Fail(Reason);

		if (strstr(Info, "Expression    : fatal error"))
		{
			Finish();
		}
	}

	// The unhandled exception filter calls it after the dump and the stack are written, before its message box
	void OnCrash(bool)
	{
		Fail("unhandled exception");
		Finish();
	}

	void ApplyView(EView View)
	{
		if (View == EView::Current)
		{
			return;
		}

		GEditorDraw.FillMode = View == EView::Wireframe ? D3DFILL_WIREFRAME : D3DFILL_SOLID;
		psDeviceFlags.set(rsEnvironment, View != EView::Unlit);

		if (View == EView::Unlit)
		{
			return;
		}

		// A still time of day: the shots of two builds see the same sky
		CEnvironment& Environment = g_pGamePersistent->Environment();
		Environment.ed_from_time = 0.0f;
		Environment.ed_to_time = DAY_LENGTH;
		Environment.SetGameTime(Hours * 3600.0f, 0.0f);

		shared_str Cycle = Weather[0] ? shared_str(Weather) : Environment.CurrentCycleName;
		if (!Cycle.size() && !Environment.WeatherCycles.empty())
		{
			Cycle = Environment.WeatherCycles.begin()->first;
		}

		if (Cycle.size())
		{
			Environment.SetWeather(Cycle, true);
		}
	}

	void BeginView()
	{
		FrameIdx = 0;
		RenderFrame = EDevice->dwRenderFrame;
		ApplyView(Views[ViewIdx]);
		Msg("~ [autotest] view: %s", Views[ViewIdx] == EView::Current ? "current" : ViewNames[u32(Views[ViewIdx])]);
	}

	void PrepareScene()
	{
		if (Select[0])
		{
			CCustomObject* Object = Scene->FindObjectByName(Select, OBJCLASS_DUMMY);
			if (!Object)
			{
				string1024 Reason;
				xr_sprintf(Reason, "object '%s' is not in the level", Select);
				Fail(Reason);
				Finish();
			}

			ExecCommand(COMMAND_CHANGE_TARGET, u32(Object->FClassID));
			Scene->SelectObjects(false);
			Object->Select(true);

			if (!HasCamera)
			{
				Tools->ZoomObject(true);
			}
		}

		CUI_Camera& Camera = EContext.UI->CurrentView().m_Camera;
		if (HasCamera)
		{
			Camera.Set(CameraHpb, CameraPos);
			Camera.SetStyle(Camera.GetStyle());
		}

		const Fvector& Pos = Camera.GetPosition();
		const Fvector& Hpb = Camera.GetHPB();
		Msg("~ [autotest] camera: %f,%f,%f,%f,%f,%f", Pos.x, Pos.y, Pos.z, Hpb.x, Hpb.y, Hpb.z);
	}

	void WriteShot()
	{
		if (!Shot[0])
		{
			return;
		}

		string_path Path;
		if (strchr(Shot, ':'))
		{
			xr_strcpy(Path, Shot);
		}
		else
		{
			FS.update_path(Path, "$logs$", Shot);
		}

		if (Views.size() > 1)
		{
			const char* Extension = strrchr(Path, '.');
			string_path Stem;
			xr_sprintf(Stem, "%.*s", int(Extension ? Extension - Path : xr_strlen(Path)), Path);
			xr_sprintf(Path, "%s_%s.tga", Stem, ViewNames[u32(Views[ViewIdx])]);
		}

		ref_rt& Target = EContext.UI->RT;
		U32Vec Pixels;
		if (!EDevice->ReadbackRT(Target, Pixels) || Target->dwWidth > 0xffff || Target->dwHeight > 0xffff)
		{
			Fail("the viewport is not read back");
			return;
		}

		IWriter* File = FS.w_open(Path);
		if (File && !File->valid())
		{
			FS.w_close(File);
		}

		if (!File)
		{
			string1024 Reason;
			xr_sprintf(Reason, "%s is not written", Path);
			Fail(Reason);
			return;
		}

		const u32 Width = Target->dwWidth;
		const u32 Height = Target->dwHeight;
		const u8 Header[18] = { 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, u8(Width), u8(Width >> 8), u8(Height), u8(Height >> 8), 32, 8 | 0x20 };

		// The target is B8G8R8A8_UNORM, the byte order of TGA: only its alpha is undefined
		for (u32& Pixel : Pixels)
		{
			Pixel |= 0xff000000u;
		}

		File->w(Header, sizeof(Header));
		File->w(Pixels.data(), u32(Pixels.size() * sizeof(u32)));
		FS.w_close(File);
		Msg("~ [autotest] shot: %s %ux%u", Path, Width, Height);
	}
}

namespace EditorAutotest
{
	bool Active()
	{
		return Core.ParamsData.test(ECoreParams::autotest);
	}

	void Initialize()
	{
		NextErrorCallback = Debug.SendErrorCallback;
		Debug.SendErrorCallback = OnError;
		Debug.set_on_dialog(OnCrash);
		Clock.Start();
	}

	void Start()
	{
		// Again: the input device has set its own
		Debug.set_on_dialog(OnCrash);

		if (const char* At = Param("-autotest_frames "))
		{
			sscanf(At, "%u", &Frames);
		}

		if (const char* At = Param("-autotest_timeout "))
		{
			sscanf(At, "%u", &Timeout);
		}

		if (const char* At = Param("-autotest_time "))
		{
			sscanf(At, "%f", &Hours);
		}

		if (const char* At = Param("-autotest_camera "))
		{
			HasCamera = 6 == sscanf(At, "%f,%f,%f,%f,%f,%f", &CameraPos.x, &CameraPos.y, &CameraPos.z, &CameraHpb.x, &CameraHpb.y, &CameraHpb.z);
		}

		Frames = std::max(Frames, 1u);
		ReadToken("-autotest_shot ", Shot);
		ReadToken("-autotest_select ", Select);
		ReadToken("-autotest_weather ", Weather);

		string256 List = {};
		ReadToken("-autotest_view ", List);
		for (char* Name = List; *Name;)
		{
			char* Next = Name + strcspn(Name, ",");
			const bool IsLast = !*Next;
			*Next = 0;

			const u32 View = u32(std::find_if(std::begin(ViewNames), std::end(ViewNames), [Name](const char* It) { return !xr_strcmp(It, Name); }) - std::begin(ViewNames));
			if (View < std::size(ViewNames))
			{
				Views.push_back(EView(View));
			}
			else
			{
				Fail("-autotest_view takes lit, unlit and wireframe");
			}

			Name = IsLast ? Next : Next + 1;
		}

		if (Views.empty())
		{
			Views.push_back(EView::Current);
		}

		// Every frame draws the viewport, not only the few after a change
		psDeviceFlags.set(rsRenderRealTime, true);

		if (!strstr(Core.Params, "-autotest_windowed"))
		{
			SDL_HideWindow(g_AppInfo.Window);
		}

		string_path Level = {};
		if (ReadToken("-autotest_level ", Level))
		{
			string_path Path;
			FS.update_path(Path, _maps_, Level);
			IsLoading = true;

			if (!u32(ExecCommand(COMMAND_LOAD, xr_string(FS.exist(Level) ? Level : Path))))
			{
				string1024 Reason;
				xr_sprintf(Reason, "level '%s' is not loaded", Path);
				Fail(Reason);
			}
		}

		Msg("~ [autotest] level=%s frames=%u views=%u shot=%s", Level[0] ? Level : "none", Frames, u32(Views.size()), Shot[0] ? Shot : "none");

		if (!IsLoading)
		{
			BeginView();
		}
	}

	void Frame()
	{
		if (Clock.GetElapsed_sec() > float(Timeout))
		{
			string1024 Reason;
			xr_sprintf(Reason, "timed out after %us", Timeout);
			Fail(Reason);
			Finish();
		}

		if (IsLoading)
		{
			if (LUI->LoaderEvent.IsBusy())
			{
				return;
			}

			IsLoading = false;

			if (LTools->m_LastFileName.empty())
			{
				Fail("the level is not loaded");
				Finish();
			}

			PrepareScene();
			BeginView();
			return;
		}

		if (++FrameIdx < Frames)
		{
			return;
		}

		const EView View = Views[ViewIdx];
		if (EDevice->dwRenderFrame == RenderFrame)
		{
			Fail("the viewport was not drawn");
		}
		else if (View != EView::Current && View != EView::Unlit && !RImplementation.EditorIsLit())
		{
			Fail("the lit view has no weather");
		}

		WriteShot();

		if (++ViewIdx < Views.size())
		{
			BeginView();
			return;
		}

		Finish();
	}
}
