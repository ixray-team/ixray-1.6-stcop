#pragma once

#include "ChZdbg.h"
#include <memory>
#include <codecvt>

class LoggerManager {
public:
	static Logger& Instance() {
		static std::unique_ptr<Logger> logger = std::make_unique<Logger>(std::cout);
		return *logger;
	}

	static void SetOutput(std::ostream& stream, bool use_color = true) {
		auto& logger = internal();
		logger = std::make_unique<Logger>(stream, use_color);
	}

private:
	static std::unique_ptr<Logger>& internal() {
		static std::unique_ptr<Logger> logger = std::make_unique<Logger>(std::cout);
		return logger;
	}
};

// Удобный доступ
inline Logger& Log() {
	return LoggerManager::Instance();
}
class LoggerManager;

class ScopedLog {
public:
	ScopedLog(Logger::Level lvl, const char* file, int line)
		: level(lvl), file(file), line(line) {
	}

	~ScopedLog() {
		Logger& logger = LoggerManager::Instance();
		std::lock_guard<std::mutex> lock(logger.getMutex());

		logger.beginLog(level, file, line);
		logger.getStream() << buffer.str();
		logger.flush();
	}

	template <typename T>
	ScopedLog& operator<<(const T& val) {
		buffer << val;
		return *this;
	}
	ScopedLog& operator<<(const std::wstring& wstr) {
		std::wstring_convert<std::codecvt_utf8<wchar_t>> conv;
		std::string utf8 = conv.to_bytes(wstr);
		buffer << utf8;
		return *this;
	}

	ScopedLog& operator<<(std::ostream& (*manip)(std::ostream&)) {
		buffer << manip;
		return *this;
	}

private:
	Logger::Level level;
	const char* file;
	int line;
	std::ostringstream buffer;
};
// Макросы
#define LOG_DEBUG ScopedLog(Logger::Level::L_DEBUG, __FILE__, __LINE__)
#define LOG_INFO  ScopedLog(Logger::Level::L_INFO,  __FILE__, __LINE__)
#define LOG_WARN  ScopedLog(Logger::Level::L_WARN,  __FILE__, __LINE__)
#define LOG_ERROR ScopedLog(Logger::Level::L_ERROR, __FILE__, __LINE__)
