#include "stdafx.h"
#include "DedicatedConsoleInput.h"

#include "../xrEngine/XR_IOConsole.h"

namespace DedicatedConsoleInput
{
	namespace
	{
		xr_atomic_bool g_consoleInputThreadActive{ false };
		xrCriticalSection g_consoleInputMutex;
		xr_vector<xr_string> g_consoleInputQueue;
		xr_atomic_bool g_consoleInputThreadRunning{ false };

#if defined(IXR_WINDOWS)
		ThreadID g_consoleInputThread = 0;
		xrCriticalSection g_consoleInputStateMutex;
		xrCriticalSection g_consoleOutputMutex;
		xr_string g_consoleInputBuffer;
		HANDLE g_consoleStdIn = INVALID_HANDLE_VALUE;
		HANDLE g_consoleStdOut = INVALID_HANDLE_VALUE;
		DWORD g_consoleOriginalInputMode = 0;

		class ThreadActivityGuard final
		{
		public:
			explicit ThreadActivityGuard(xr_atomic_bool& flag)
				: m_flag(flag)
			{
				m_flag.store(true, std::memory_order_release);
			}

			~ThreadActivityGuard()
			{
				m_flag.store(false, std::memory_order_release);
			}

			ThreadActivityGuard(const ThreadActivityGuard&) = delete;
			ThreadActivityGuard& operator=(const ThreadActivityGuard&) = delete;

		private:
			xr_atomic_bool& m_flag;
		};

		xr_vector<wchar_t> Utf8ToWide(const xr_string& text)
		{
			if (text.empty())
				return {};

			const wchar_t* wide = Platform::ANSI_TO_TCHAR(text.c_str());
			if (wide == nullptr)
				return {};

			const size_t length = std::wcslen(wide);
			return xr_vector<wchar_t>(wide, wide + length);
		}

		xr_string WideCharToUtf8(wchar_t symbol, WORD repeatCount)
		{
			if (symbol == 0 || repeatCount == 0)
				return {};

			const wchar_t buffer[2] = { symbol, 0 };
			const xr_string converted = Platform::CP_TCHAR_TO_ANSI_U8(buffer);
			if (converted.empty())
				return {};

			xr_string result;
			result.reserve(converted.size() * repeatCount);
			for (WORD index = 0; index < repeatCount; ++index)
				result.append(converted);

			return result;
		}

		void RenderConsoleInputLine()
		{
			if (!g_consoleInputThreadRunning.load())
				return;

			if (g_consoleStdOut == INVALID_HANDLE_VALUE)
				return;

			xr_string currentBuffer;
			{
				xrCriticalSectionGuard lock(&g_consoleInputStateMutex);
				currentBuffer = g_consoleInputBuffer;
			}

			CONSOLE_SCREEN_BUFFER_INFO info = {};
			if (!GetConsoleScreenBufferInfo(g_consoleStdOut, &info))
				return;

			const xr_string prompt = ">>> ";
			const xr_string line = prompt + currentBuffer;
			const xr_vector<wchar_t> wideLine = Utf8ToWide(line);
			const DWORD consoleWidth = info.dwSize.X;
			const int displayLength = static_cast<int>(wideLine.size());

			xrCriticalSectionGuard outputLock(&g_consoleOutputMutex);

			COORD basePosition = info.dwCursorPosition;
			basePosition.X = 0;

			DWORD written = 0;
			if (!wideLine.empty())
				WriteConsoleOutputCharacterW(g_consoleStdOut, wideLine.data(), static_cast<DWORD>(wideLine.size()), basePosition, &written);

			if (consoleWidth > static_cast<DWORD>(displayLength))
			{
				const DWORD spacesCount = consoleWidth - static_cast<DWORD>(displayLength);
				if (spacesCount > 0)
				{
					std::wstring spaces(static_cast<size_t>(spacesCount), L' ');
					COORD clearPos = basePosition;
					clearPos.X = static_cast<SHORT>(displayLength);
					WriteConsoleOutputCharacterW(g_consoleStdOut, spaces.c_str(), spacesCount, clearPos, &written);
				}
			}

			COORD cursorPosition = basePosition;
			if (consoleWidth > 0)
				cursorPosition.X = static_cast<SHORT>(std::min(displayLength, static_cast<int>(consoleWidth - 1)));
			else
				cursorPosition.X = 0;

			SetConsoleCursorPosition(g_consoleStdOut, cursorPosition);
		}

		void AppendToInputBuffer(const xr_string& text)
		{
			if (text.empty())
				return;

			{
				xrCriticalSectionGuard lock(&g_consoleInputStateMutex);
				g_consoleInputBuffer += text;
			}

			RenderConsoleInputLine();
		}

		void RemoveLastInputCharacter(WORD repeatCount)
		{
			if (repeatCount == 0)
				return;

			bool modified = false;
			{
				xrCriticalSectionGuard lock(&g_consoleInputStateMutex);
				while (repeatCount-- > 0 && !g_consoleInputBuffer.empty())
				{
					size_t erasePosition = g_consoleInputBuffer.size();
					while (erasePosition > 0)
					{
						--erasePosition;
						if ((static_cast<unsigned char>(g_consoleInputBuffer[erasePosition]) & 0xC0) != 0x80)
							break;
					}

					g_consoleInputBuffer.erase(erasePosition);
					modified = true;
					if (g_consoleInputBuffer.empty())
						break;
				}
			}

			if (modified)
				RenderConsoleInputLine();
		}
#endif // defined(IXR_WINDOWS)

		xr_string TrimConsoleCommand(const xr_string& source)
		{
			const size_t first = source.find_first_not_of(" \t\r\n");
			if (first == xr_string::npos)
				return {};

			const size_t last = source.find_last_not_of(" \t\r\n");
			return source.substr(first, last - first + 1);
		}

		class DedicatedConsoleInputProcessor final : public pureFrame
		{
		public:
			void _BCL OnFrame() override
			{
				if (!Console)
					return;

				xr_vector<xr_string> pendingCommands;
				{
					xrCriticalSectionGuard lock(&g_consoleInputMutex);
					if (g_consoleInputQueue.empty())
						return;

					pendingCommands.swap(g_consoleInputQueue);
				}

				for (xr_string& command : pendingCommands)
				{
					if (!command.empty())
						Console->Execute(command.c_str());
				}
			}
		};

		DedicatedConsoleInputProcessor g_consoleInputProcessor;

		void DedicatedConsoleInputLoop()
		{
#if defined(IXR_WINDOWS)
			ThreadActivityGuard activityGuard(g_consoleInputThreadActive);
			g_consoleStdIn = GetStdHandle(STD_INPUT_HANDLE);
			g_consoleStdOut = GetStdHandle(STD_OUTPUT_HANDLE);

			if (g_consoleStdIn != INVALID_HANDLE_VALUE &&
				g_consoleStdOut != INVALID_HANDLE_VALUE &&
				GetConsoleMode(g_consoleStdIn, &g_consoleOriginalInputMode))
			{
				DWORD consoleMode = g_consoleOriginalInputMode;
				consoleMode |= ENABLE_EXTENDED_FLAGS;
				consoleMode &= ~ENABLE_QUICK_EDIT_MODE;
				consoleMode &= ~(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT);
				consoleMode |= ENABLE_PROCESSED_INPUT;
				SetConsoleMode(g_consoleStdIn, consoleMode);
				FlushConsoleInputBuffer(g_consoleStdIn);

				RenderConsoleInputLine();

				INPUT_RECORD record = {};
				DWORD eventsRead = 0;
				while (g_consoleInputThreadRunning.load())
				{
					if (!ReadConsoleInputW(g_consoleStdIn, &record, 1, &eventsRead))
					{
						std::this_thread::sleep_for(std::chrono::milliseconds(10));
						continue;
					}

					if (!g_consoleInputThreadRunning.load())
						break;

					if (record.EventType != KEY_EVENT)
						continue;

					KEY_EVENT_RECORD& key = record.Event.KeyEvent;
					if (!key.bKeyDown)
						continue;

					switch (key.wVirtualKeyCode)
					{
					case VK_BACK:
						RemoveLastInputCharacter(key.wRepeatCount);
						break;
					case VK_ESCAPE:
					{
						bool cleared = false;
						{
							xrCriticalSectionGuard lock(&g_consoleInputStateMutex);
							cleared = !g_consoleInputBuffer.empty();
							g_consoleInputBuffer.clear();
						}
						if (cleared)
							RenderConsoleInputLine();
						break;
					}
					case VK_RETURN:
					{
						xr_string utf8Command;
						{
							xrCriticalSectionGuard lock(&g_consoleInputStateMutex);
							utf8Command = g_consoleInputBuffer;
							g_consoleInputBuffer.clear();
						}

						{
							xrCriticalSectionGuard outputLock(&g_consoleOutputMutex);
							DWORD written = 0;
							WriteConsoleW(g_consoleStdOut, L"\n", 1, &written, nullptr);
						}

						RenderConsoleInputLine();

						const xr_string trimmed = TrimConsoleCommand(utf8Command);
						if (trimmed.empty())
							break;

						xr_string command = Platform::UTF8_to_CP1251(trimmed);
						if (command.empty())
							break;

						xrCriticalSectionGuard lock(&g_consoleInputMutex);
						g_consoleInputQueue.emplace_back(std::move(command));
						break;
					}
					default:
					{
						const wchar_t unicodeChar = key.uChar.UnicodeChar;
						if (unicodeChar >= 32 && unicodeChar != 127)
						{
							const xr_string utf8 = WideCharToUtf8(unicodeChar, key.wRepeatCount);
							AppendToInputBuffer(utf8);
						}
						break;
					}
					}
				}

				return;
			}

			g_consoleStdIn = INVALID_HANDLE_VALUE;
			g_consoleStdOut = INVALID_HANDLE_VALUE;
#endif // defined(IXR_WINDOWS)

			xr_string line;
			while (g_consoleInputThreadRunning.load())
			{
				if (!std::getline(std::cin, line))
				{
					if (!g_consoleInputThreadRunning.load())
						break;

					if (std::cin.eof())
						break;

					std::cin.clear();
					std::this_thread::sleep_for(std::chrono::milliseconds(50));
					continue;
				}

				const xr_string trimmed = TrimConsoleCommand(line);
				if (trimmed.empty())
					continue;

				const xr_string command = Platform::UTF8_to_CP1251(trimmed);
				if (command.empty())
					continue;

				xrCriticalSectionGuard lock(&g_consoleInputMutex);
				g_consoleInputQueue.emplace_back(std::move(command));
		}
		}
	} // namespace

#if defined(IXR_WINDOWS)
	void DedicatedConsoleInputThread(void*)
	{
		DedicatedConsoleInputLoop();
	}
#endif

	void Start()
	{
		if (g_consoleInputThreadRunning.exchange(true))
			return;

		Device.seqFrame.Add(&g_consoleInputProcessor, REG_PRIORITY_LOW);

#if defined(IXR_WINDOWS)
		while (g_consoleInputThreadActive.load(std::memory_order_acquire))
			std::this_thread::sleep_for(std::chrono::milliseconds(1));

		g_consoleInputThread = thread_spawn(DedicatedConsoleInputThread, "dedicated-console-input", 0, nullptr);
#else
		std::thread(DedicatedConsoleInputLoop).detach();
#endif
	}

	void Stop()
	{
		if (!g_consoleInputThreadRunning.exchange(false))
			return;

#if defined(IXR_WINDOWS)
		if (g_consoleStdIn != INVALID_HANDLE_VALUE)
		{
			INPUT_RECORD record = {};
			record.EventType = KEY_EVENT;
			record.Event.KeyEvent.bKeyDown = TRUE;
			record.Event.KeyEvent.wVirtualKeyCode = VK_RETURN;
			record.Event.KeyEvent.uChar.UnicodeChar = L'\r';
			DWORD written = 0;
			WriteConsoleInputW(g_consoleStdIn, &record, 1, &written);
		}

		while (g_consoleInputThreadActive.load(std::memory_order_acquire))
			std::this_thread::sleep_for(std::chrono::milliseconds(1));

		if (g_consoleStdIn != INVALID_HANDLE_VALUE)
		{
			SetConsoleMode(g_consoleStdIn, g_consoleOriginalInputMode);
			g_consoleStdIn = INVALID_HANDLE_VALUE;
		}

		g_consoleStdOut = INVALID_HANDLE_VALUE;
		{
			xrCriticalSectionGuard lock(&g_consoleInputStateMutex);
			g_consoleInputBuffer.clear();
		}

		g_consoleInputThread = 0;
#endif

		Device.seqFrame.Remove(&g_consoleInputProcessor);
	}

	void HandleLogLine(const xr_string& utf8Text, u32 originalLength)
	{
#if defined(IXR_WINDOWS)
		if (g_consoleStdOut != INVALID_HANDLE_VALUE)
		{
			{
				xrCriticalSectionGuard outputLock(&g_consoleOutputMutex);
				CONSOLE_SCREEN_BUFFER_INFO info = {};
				if (GetConsoleScreenBufferInfo(g_consoleStdOut, &info))
				{
					COORD lineStart = info.dwCursorPosition;
					lineStart.X = 0;
					SetConsoleCursorPosition(g_consoleStdOut, lineStart);

					DWORD consoleWidth = info.dwSize.X;
					if (consoleWidth > 0)
					{
						xr_vector<wchar_t> blank(static_cast<size_t>(consoleWidth), L' ');
						if (!blank.empty())
						{
							DWORD cleared = 0;
							WriteConsoleW(g_consoleStdOut, blank.data(), consoleWidth, &cleared, nullptr);
						}
					}

					SetConsoleCursorPosition(g_consoleStdOut, lineStart);
				}

				DWORD written = 0;
				if (!utf8Text.empty())
				{
					const xr_vector<wchar_t> wideText = Utf8ToWide(utf8Text);
					if (!wideText.empty())
						WriteConsoleW(g_consoleStdOut, wideText.data(), static_cast<DWORD>(wideText.size()), &written, nullptr);
				}

				if (originalLength == 0 || utf8Text.empty() || utf8Text.back() != '\n')
					WriteConsoleW(g_consoleStdOut, L"\n", 1, &written, nullptr);
			}

			RenderConsoleInputLine();
			return;
		}
#endif

		if (!utf8Text.empty())
			std::fputs(utf8Text.c_str(), stdout);

		if (originalLength == 0 || utf8Text.empty() || utf8Text.back() != '\n')
			std::fputc('\n', stdout);

		std::fflush(stdout);

#if defined(IXR_WINDOWS)
		RenderConsoleInputLine();
#endif
	}
} // namespace DedicatedConsoleInput
