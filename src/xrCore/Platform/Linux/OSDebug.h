#pragma once
#include <sys/ptrace.h>
#include <sys/wait.h>
#include <iostream>

IC const char* GetCommandLineA()
{
    return "";
}

IC bool IsDebuggerPresent()
{
    /*
    int res = 0;
    int pid = fork();

    if (pid == -1)
    {
        perror("fork");
        return -1;
    }

    if (pid == 0)
    {
        int ppid = getppid();

        if (ptrace(PTRACE_ATTACH, ppid, NULL, NULL) == 0)
        {
            waitpid(ppid, NULL, 0);
            ptrace(PTRACE_CONT, NULL, NULL);

            ptrace(PTRACE_DETACH, getppid(), NULL, NULL);
        }
        else
        {
            res = 1;
        }
        exit(res);
    }
    else
    {
        int status;
        waitpid(pid, &status, 0);
        res = WEXITSTATUS(status);
    }
        */

    return true;
    //return !!res;
}

inline void DebugBreak()
{
    __builtin_trap();
}

inline void OutputDebugStringA(const char* data)
{
    std::cout << "IXR Engine: " << data << std::endl;
}