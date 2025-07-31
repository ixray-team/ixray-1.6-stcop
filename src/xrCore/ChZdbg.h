#pragma once

#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <chrono>
#include <mutex>
#include <string>
#include <map>

	class Logger {
	public:
		enum class Level {
			L_DEBUG, L_INFO, L_WARN, L_ERROR
		};

		explicit Logger(std::ostream& stream = std::cout, bool enable_colors = true)
			: out(stream), use_colors(enable_colors && (&stream == &std::cout)) {
		}

		template<typename T>
		Logger& operator<<(const T& val) {
			std::lock_guard<std::mutex> lock(mutex);

			buffer << val;
			return *this;
		}

		Logger& operator<<(std::ostream& (*manip)(std::ostream&)) {
			std::lock_guard<std::mutex> lock(mutex);
			buffer << manip;
			if (manip == static_cast<std::ostream & (*)(std::ostream&)>(std::endl)) {
				flush();
			}
			return *this;
		}

		Logger& log(Level lvl, const char* file, int line) {
			std::lock_guard<std::mutex> lock(mutex);
			current_level = lvl;
			meta_file = file;
			meta_line = line;
			return *this;
		}

	private:
		friend class ScopedLog;

		std::mutex& getMutex() {
			return mutex;
		}

		std::ostringstream& getStream() {
			return buffer;
		}

		void beginLog(Level lvl, const char* file, int line) {
			current_level = lvl;
			meta_file = file;
			meta_line = line;
		}


		std::ostream& out;
		std::ostringstream buffer;
		std::mutex mutex;
		Level current_level = Level::L_INFO;
		const char* meta_file = "";
		int meta_line = 0;
		bool use_colors;

		void flush()
		{
#if 0
			out << timestamp()
				<< " " << level_tag(current_level)
				<< " [" << short_file(meta_file) << ":" << meta_line << "] "
				<< buffer.str();
#endif
			{
				OutputDebugStringA(buffer.str().c_str());
				OutputDebugStringA("\n");
			}

			 xr_string s_level("  ");
			 s_level[0] = level_sym(current_level);

			Log(std::format("{}{}", (s_level[0] == ' ' ? "" : s_level.c_str()), buffer.str()).c_str());

			buffer.str("");
			buffer.clear();
		}

		// Snizy std::string functions but oni ne used
		//		V flush() pod #if 0 ubran code
		std::string timestamp() {
			using namespace std::chrono;
			auto now = system_clock::now();
			auto time_t_now = system_clock::to_time_t(now);
			std::tm tm_now;
#if defined(_WIN32)
			localtime_s(&tm_now, &time_t_now);
#else
			localtime_r(&time_t_now, &tm_now);
#endif
			std::ostringstream ss;
			ss << "[" << std::put_time(&tm_now, "%Y-%m-%d %H:%M:%S") << "]";
			return ss.str();
		}

		std::string level_tag(Level lvl) {
			static const std::map<Level, std::string> tags = {
				{Level::L_DEBUG, "DEBUG"},
				{Level::L_INFO,  "INFO "},
				{Level::L_WARN,  "WARN "},
				{Level::L_ERROR, "ERROR"}
			};
			static const std::map<Level, std::string> colors = {
				{Level::L_DEBUG, "\033[36m"},
				{Level::L_INFO,  "\033[32m"},
				{Level::L_WARN,  "\033[33m"},
				{Level::L_ERROR, "\033[31m"},
			};

			std::string tag = tags.at(lvl);
			if (use_colors)
				return colors.at(lvl) + "[" + tag + "]" + "\033[0m";
			else
				return "[" + tag + "]";
		}

		char level_sym(Level lvl)
		{
			static const std::map<Level, char> tags = {
				{Level::L_DEBUG, '/'},
				{Level::L_INFO,  ' '},
				{Level::L_WARN,  '!'},
				{Level::L_ERROR, '~'}
			};

			char sym = tags.at(lvl);

			return sym;
		}

		std::string short_file(const std::string& full_path) {
			size_t pos = full_path.find_last_of("/\\");
			return (pos != std::string::npos) ? full_path.substr(pos + 1) : full_path;
		}
	};