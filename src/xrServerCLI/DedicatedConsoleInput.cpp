#include "stdafx.h"
#include "DedicatedConsoleInput.h"

#include "../xrEngine/XR_IOConsole.h"

#include <limits>

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
		int g_consoleWheelDelta = 0;

		COORD g_consoleOutputPosition = {};
		bool g_consoleOutputPositionPending = false;

		bool GetConsoleOutputInfo(CONSOLE_SCREEN_BUFFER_INFO& info)
		{
			if (!GetConsoleScreenBufferInfo(g_consoleStdOut, &info))
				return false;
			if (g_consoleOutputPositionPending)
			{
				g_consoleOutputPosition.X = std::clamp<SHORT>(g_consoleOutputPosition.X, 0, info.dwSize.X - 1);
				g_consoleOutputPosition.Y = std::clamp<SHORT>(g_consoleOutputPosition.Y, 0, info.dwSize.Y - 1);
				info.dwCursorPosition = g_consoleOutputPosition;
			}
			return true;
		}

		// Stream output and cursor movement make conhost snap to the bottom.
		// While reading history, write cells directly and track the output cursor separately.
		class ConsoleHistoryWriter final
		{
		public:
			explicit ConsoleHistoryWriter(const CONSOLE_SCREEN_BUFFER_INFO& info)
				: m_width(info.dwSize.X), m_height(info.dwSize.Y),
				m_row(info.dwCursorPosition.Y), m_attributes(info.wAttributes)
			{
				GetConsoleMode(g_consoleStdOut, &m_mode);
			}

			~ConsoleHistoryWriter()
			{
				Flush();
				g_consoleOutputPosition = { static_cast<SHORT>(m_column), static_cast<SHORT>(m_row) };
				g_consoleOutputPositionPending = true;
			}

			void Write(const wchar_t* text, DWORD length)
			{
				// Engine logs originate in CP1251: printable characters occupy one cell.
				for (DWORD index = 0; index < length; ++index)
				{
					const wchar_t symbol = text[index];
					if (m_mode & ENABLE_PROCESSED_OUTPUT)
					{
						if (symbol < 32)
							Flush();
						switch (symbol)
						{
						case L'\r': m_column = 0; m_wrapPending = false; continue;
						case L'\n':
							if (!(m_mode & DISABLE_NEWLINE_AUTO_RETURN))
								m_column = 0;
							m_wrapPending = false;
							AdvanceRow();
							continue;
						case L'\b': m_column = std::max(0, m_column - 1); m_wrapPending = false; continue;
						case L'\t':
							if (IsVirtualTerminalOutput())
							{
								m_column = std::min(m_width - 1, (m_column / 8 + 1) * 8);
								m_wrapPending = false;
							}
							else
							{
								const int spaces = std::min(m_width - m_column, 8 - (m_column & 7));
								for (int space = 0; space < spaces; ++space)
									PutSymbol(L' ');
							}
							continue;
						default:
							if (symbol < 32 && symbol != 0)
								continue;
						}
					}
					PutSymbol(symbol);
				}
				Flush();
			}

			ConsoleHistoryWriter(const ConsoleHistoryWriter&) = delete;
			ConsoleHistoryWriter& operator=(const ConsoleHistoryWriter&) = delete;

		private:
			void Flush()
			{
				if (m_text.empty())
					return;
				const COORD position = { static_cast<SHORT>(m_textColumn), static_cast<SHORT>(m_row) };
				DWORD written = 0;
				WriteConsoleOutputCharacterW(g_consoleStdOut, m_text.data(), static_cast<DWORD>(m_text.size()), position, &written);
				FillConsoleOutputAttribute(g_consoleStdOut, m_attributes, static_cast<DWORD>(m_text.size()), position, &written);
				m_text.clear();
			}

			bool IsVirtualTerminalOutput() const
			{
				constexpr DWORD flags = ENABLE_PROCESSED_OUTPUT | ENABLE_VIRTUAL_TERMINAL_PROCESSING;
				return (m_mode & flags) == flags;
			}

			void AdvanceRow()
			{
				Flush();
				if (++m_row >= m_height)
				{
					m_row = m_height - 1;
					const SMALL_RECT source = { 0, 1, static_cast<SHORT>(m_width - 1), static_cast<SHORT>(m_height - 1) };
					CHAR_INFO fill = {};
					fill.Char.UnicodeChar = L' ';
					fill.Attributes = m_attributes;
					ScrollConsoleScreenBufferW(g_consoleStdOut, &source, nullptr, { 0, 0 }, &fill);

					CONSOLE_SCREEN_BUFFER_INFO info = {};
					if (GetConsoleScreenBufferInfo(g_consoleStdOut, &info) && info.srWindow.Top > 0)
					{
						--info.srWindow.Top;
						--info.srWindow.Bottom;
						SetConsoleWindowInfo(g_consoleStdOut, TRUE, &info.srWindow);
					}
				}
			}

			void PutSymbol(wchar_t symbol)
			{
				if (m_wrapPending)
				{
					Flush();
					m_column = 0;
					m_wrapPending = false;
					AdvanceRow();
				}
				if (m_text.empty())
					m_textColumn = m_column;
				m_text.push_back(symbol);
				if (m_column + 1 >= m_width)
				{
					Flush();
					if (m_mode & ENABLE_WRAP_AT_EOL_OUTPUT)
					{
						if (IsVirtualTerminalOutput())
							m_wrapPending = true;
						else
						{
							m_column = 0;
							AdvanceRow();
						}
					}
				}
				else
					++m_column;
			}

			int m_width;
			int m_height;
			int m_row;
			int m_column = 0;
			int m_textColumn = 0;
			xr_vector<wchar_t> m_text;
			WORD m_attributes;
			DWORD m_mode = 0;
			bool m_wrapPending = false;
		};

		void RenderConsoleInputLineLocked(bool preserveViewport = false);

		void ScrollConsoleWindow(int steps, UINT linesPerStep)
		{
			if (steps == 0 || linesPerStep == 0)
				return;

			xrCriticalSectionGuard outputLock(&g_consoleOutputMutex);
			CONSOLE_SCREEN_BUFFER_INFO info = {};
			if (!GetConsoleOutputInfo(info))
				return;

			SMALL_RECT window = info.srWindow;
			const int height = window.Bottom - window.Top + 1;
			const int lines = linesPerStep == WHEEL_PAGESCROLL ? std::max(1, height - 1) :
				static_cast<int>(std::min(linesPerStep, static_cast<UINT>(info.dwSize.Y)));
			const int maxTop = std::max(0, info.dwCursorPosition.Y - height + 1);
			const int top = std::clamp(window.Top + steps * lines, 0, maxTop);
			window.Top = static_cast<SHORT>(top);
			window.Bottom = static_cast<SHORT>(top + height - 1);
			SetConsoleWindowInfo(g_consoleStdOut, TRUE, &window);
			if (top == maxTop)
				RenderConsoleInputLineLocked();
		}

		void HandleConsoleMouseWheel(const MOUSE_EVENT_RECORD& mouse)
		{
			g_consoleWheelDelta += static_cast<SHORT>(HIWORD(mouse.dwButtonState));
			const int steps = g_consoleWheelDelta / WHEEL_DELTA;
			g_consoleWheelDelta %= WHEEL_DELTA;
			if (steps == 0)
				return;

			UINT lines = 3;
			SystemParametersInfoW(SPI_GETWHEELSCROLLLINES, 0, &lines, 0);
			ScrollConsoleWindow(-steps, lines);
		}

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
			if (text.empty() || text.size() > static_cast<size_t>((std::numeric_limits<int>::max)()))
				return {};

			const int textLength = static_cast<int>(text.size());
			const int wideLength = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, text.data(), textLength, nullptr, 0);
			if (wideLength == 0)
				return {};

			xr_vector<wchar_t> wide(static_cast<size_t>(wideLength));
			if (MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, text.data(), textLength, wide.data(), wideLength) != wideLength)
				return {};

			return wide;
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

		// The caller must hold g_consoleOutputMutex for the entire redraw.
		void RenderConsoleInputLineLocked(bool preserveViewport)
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
			if (!GetConsoleOutputInfo(info))
				return;

			const int consoleWidth = info.srWindow.Right - info.srWindow.Left + 1;
			if (consoleWidth <= 0)
				return;

			// Reserve one cell for the cursor; never wrap the editable line.
			const size_t maxDisplayLength = static_cast<size_t>(consoleWidth - 1);
			xr_vector<wchar_t> wideLine = { L'>', L'>', L'>', L' ' };
			wideLine.resize(std::min(wideLine.size(), maxDisplayLength));
			const xr_vector<wchar_t> wideBuffer = Utf8ToWide(currentBuffer);
			const size_t inputWidth = maxDisplayLength - wideLine.size();
			if (inputWidth > 0)
			{
				size_t inputOffset = 0;
				if (wideBuffer.size() > inputWidth)
				{
					wideLine.push_back(L'<');
					inputOffset = wideBuffer.size() - (inputWidth - 1);
					// Do not start the visible suffix in the middle of a UTF-16 pair.
					if (inputOffset < wideBuffer.size() &&
						wideBuffer[inputOffset] >= 0xDC00 && wideBuffer[inputOffset] <= 0xDFFF)
						++inputOffset;
				}
				wideLine.insert(wideLine.end(), wideBuffer.begin() + inputOffset, wideBuffer.end());
			}

			COORD basePosition = info.dwCursorPosition;
			basePosition.X = info.srWindow.Left;

			DWORD written = 0;
			COORD clearPosition = basePosition;
			clearPosition.X = 0;
			FillConsoleOutputCharacterW(g_consoleStdOut, L' ', info.dwSize.X, clearPosition, &written);
			if (!wideLine.empty())
				WriteConsoleOutputCharacterW(g_consoleStdOut, wideLine.data(), static_cast<DWORD>(wideLine.size()), basePosition, &written);

			COORD cursorPosition = basePosition;
			cursorPosition.X += static_cast<SHORT>(wideLine.size());

			if (!preserveViewport || cursorPosition.Y <= info.srWindow.Bottom)
			{
				if (SetConsoleCursorPosition(g_consoleStdOut, cursorPosition))
					g_consoleOutputPositionPending = false;
			}
		}

		void RenderConsoleInputLine(bool preserveViewport = false)
		{
			xrCriticalSectionGuard outputLock(&g_consoleOutputMutex);
			RenderConsoleInputLineLocked(preserveViewport);
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
				consoleMode |= ENABLE_WINDOW_INPUT;
				consoleMode |= ENABLE_MOUSE_INPUT;
				SetConsoleMode(g_consoleStdIn, consoleMode);
				FlushConsoleInputBuffer(g_consoleStdIn);
				g_consoleWheelDelta = 0;

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

					if (record.EventType == WINDOW_BUFFER_SIZE_EVENT)
					{
						RenderConsoleInputLine(true);
						continue;
					}

					if (record.EventType == MOUSE_EVENT)
					{
						const MOUSE_EVENT_RECORD& mouse = record.Event.MouseEvent;
						if (mouse.dwEventFlags & MOUSE_WHEELED)
							HandleConsoleMouseWheel(mouse);
						continue;
					}

					if (record.EventType != KEY_EVENT)
						continue;

					KEY_EVENT_RECORD& key = record.Event.KeyEvent;
					if (!key.bKeyDown)
						continue;

					switch (key.wVirtualKeyCode)
					{
					case VK_PRIOR:
						ScrollConsoleWindow(-static_cast<int>(key.wRepeatCount), WHEEL_PAGESCROLL);
						break;
					case VK_NEXT:
						ScrollConsoleWindow(key.wRepeatCount, WHEEL_PAGESCROLL);
						break;
					case VK_END:
						if (key.dwControlKeyState & (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED))
							ScrollConsoleWindow(1, static_cast<UINT>((std::numeric_limits<SHORT>::max)()));
						break;
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
							xrCriticalSectionGuard outputLock(&g_consoleOutputMutex);
							{
								xrCriticalSectionGuard lock(&g_consoleInputStateMutex);
								utf8Command = g_consoleInputBuffer;
								g_consoleInputBuffer.clear();
							}

							DWORD written = 0;
							CONSOLE_SCREEN_BUFFER_INFO info = {};
							if (GetConsoleOutputInfo(info))
							{
								COORD lineStart = info.dwCursorPosition;
								lineStart.X = 0;
								FillConsoleOutputCharacterW(g_consoleStdOut, L' ', info.dwSize.X, lineStart, &written);
								SetConsoleCursorPosition(g_consoleStdOut, lineStart);
								g_consoleOutputPositionPending = false;
							}
							const xr_vector<wchar_t> wideCommand = Utf8ToWide(">>> " + utf8Command);
							if (!wideCommand.empty())
								WriteConsoleW(g_consoleStdOut, wideCommand.data(), static_cast<DWORD>(wideCommand.size()), &written, nullptr);
							WriteConsoleW(g_consoleStdOut, L"\r\n", 2, &written, nullptr);
							RenderConsoleInputLineLocked();
						}

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
			record.Event.KeyEvent.bKeyDown = true;
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
		g_consoleOutputPositionPending = false;
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
				const bool hasScreenInfo = GetConsoleOutputInfo(info);
				const bool readingHistory = hasScreenInfo && info.dwCursorPosition.Y > info.srWindow.Bottom;
				if (hasScreenInfo)
				{
					COORD lineStart = info.dwCursorPosition;
					lineStart.X = 0;
					if (!readingHistory)
					{
						SetConsoleCursorPosition(g_consoleStdOut, lineStart);
						g_consoleOutputPositionPending = false;
					}

					DWORD consoleWidth = info.dwSize.X;
					if (consoleWidth > 0)
					{
						DWORD cleared = 0;
						FillConsoleOutputCharacterW(g_consoleStdOut, L' ', consoleWidth, lineStart, &cleared);
					}
				}

				const xr_vector<wchar_t> wideText = Utf8ToWide(utf8Text);
				const bool appendNewline = originalLength == 0 || utf8Text.empty() || utf8Text.back() != '\n';
				if (readingHistory)
				{
					ConsoleHistoryWriter history(info);
					if (!wideText.empty())
						history.Write(wideText.data(), static_cast<DWORD>(wideText.size()));
					if (appendNewline)
						history.Write(L"\n", 1);
				}
				else
				{
					DWORD written = 0;
					if (!wideText.empty())
						WriteConsoleW(g_consoleStdOut, wideText.data(), static_cast<DWORD>(wideText.size()), &written, nullptr);
					if (appendNewline)
						WriteConsoleW(g_consoleStdOut, L"\n", 1, &written, nullptr);
				}

				RenderConsoleInputLineLocked(readingHistory);
			}

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
