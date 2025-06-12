#include "stdafx.h"
#include "PluginPython.h"

#include <fstream>

bool CPluginPython::IsPythonInstalled() const
{
	int exitCode = std::system("python --version >nul 2>&1");
	if (exitCode == 0) return true;

	exitCode = std::system("python3 --version >nul 2>&1");
	return exitCode == 0;
}

xr_string CPluginPython::RunCommand(const xr_string& command)
{
	HANDLE hRead, hWrite;
	SECURITY_ATTRIBUTES sa = { sizeof(SECURITY_ATTRIBUTES), nullptr, TRUE };

	// Создаем анонимный канал (pipe)
	if (!CreatePipe(&hRead, &hWrite, &sa, 0))
	{
		throw std::runtime_error("Ошибка создания pipe");
	}

	STARTUPINFOA si = { sizeof(STARTUPINFOA) };
	si.dwFlags = STARTF_USESTDHANDLES;
	si.hStdOutput = hWrite;
	si.hStdError = hWrite;

	PROCESS_INFORMATION pi;
	xr_string cmd = "cmd /C " + command; // Запускаем через cmd.exe

	// Создаем процесс
	if (!CreateProcessA(nullptr, const_cast<char*>(cmd.c_str()), nullptr, nullptr, TRUE, CREATE_NO_WINDOW, nullptr, nullptr, &si, &pi))
	{
		CloseHandle(hRead);
		CloseHandle(hWrite);
		throw std::runtime_error("Ошибка запуска команды");
	}

	CloseHandle(hWrite); // Закрываем ненужную сторону канала

	// Читаем вывод
	string128 buffer;
	DWORD bytesRead;
	xr_string output;

	while (ReadFile(hRead, buffer, sizeof(buffer) - 1, &bytesRead, nullptr) && bytesRead > 0)
	{
		buffer[bytesRead] = '\0';
		output += buffer;
	}

	CloseHandle(hRead);
	CloseHandle(pi.hProcess);
	CloseHandle(pi.hThread);

	return output;
}

CPluginPython::CPluginPython()
{
	Type = EPluginType::Python;
}

void CPluginPython::Run()
{
	if (!IsPythonInstalled())
	{
		Msg("! Not found Python SDK!");
		return;
	}

	xr_string Command = "python \"";
	Command += Path;
	Command += "\" -level=\"";
	Command += Scene->full_name;
	Command += "\"";

	Msg(RunCommand(Command).c_str());
}

xr_string CPluginPython::ReadDesc() const
{
	std::ifstream file(Path.data());

	xr_string line;
	const std::string_view prefix = "# desc: ";

	while (std::getline(file, line))
	{
		if (line.rfind(prefix, 0) == 0)
		{
			size_t pos = line.find(':');
			if (pos != std::string::npos && pos + 1 < line.size())
			{
				return line.substr(pos + 1);
			}
		}
	}

	return "Not found description!";
}