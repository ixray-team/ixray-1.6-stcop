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

        /* Child */
        if (ptrace(PTRACE_ATTACH, ppid, NULL, NULL) == 0)
        {
            /* Wait for the parent to stop and continue it */
            waitpid(ppid, NULL, 0);
            ptrace(PTRACE_CONT, NULL, NULL);

            /* Detach */
            ptrace(PTRACE_DETACH, getppid(), NULL, NULL);
        }
        else
        {
            /* Trace failed so GDB is present */
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

    return !!res;
}

inline void DebugBreak()
{
    using BYTE = unsigned char;
    BYTE bCrash = *(BYTE*)(nullptr);
}

inline void OutputDebugStringA(const char* data)
{
    std::cout << "IXR Engine: " << data << std::endl;
}