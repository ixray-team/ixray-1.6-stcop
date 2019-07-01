
/*******************************************************************************
 * Copyright 1993-2008 NVIDIA Corporation.  All rights reserved.
 *
 * NOTICE TO USER:   
 *
 * This source code is subject to NVIDIA ownership rights under U.S. and 
 * international Copyright laws.  
 * 
 * This software and the information contained herein is PROPRIETARY and 
 * CONFIDENTIAL to NVIDIA and is being provided under the terms and conditions 
 * of a Non-Disclosure Agreement.  Any reproduction or disclosure to any third 
 * party without the express written consent of NVIDIA is prohibited.     
 *
 * NVIDIA MAKES NO REPRESENTATION ABOUT THE SUITABILITY OF THIS SOURCE CODE FOR 
 * ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY OF 
 * ANY KIND.  NVIDIA DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOURCE CODE, 
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY, NONINFRINGEMENT, AND 
 * FITNESS FOR A PARTICULAR PURPOSE.   IN NO EVENT SHALL NVIDIA BE LIABLE FOR 
 * ANY SPECIAL, INDIRECT, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, OR ANY DAMAGES 
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,  WHETHER IN AN 
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,  ARISING OUT OF OR 
 * IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOURCE CODE.  
 *
 * U.S. Government End Users.   This source code is a "commercial item" as that 
 * term is defined at  48 C.F.R. 2.101 (OCT 1995), consisting  of "commercial 
 * computer  software"  and "commercial computer software documentation" as such 
 * terms are  used in 48 C.F.R. 12.212 (SEPT 1995) and is provided to the U.S. 
 * Government only as a commercial end item.  Consistent with 48 C.F.R.12.212 
 * and 48 C.F.R. 227.7202-1 through 227.7202-4 (JUNE 1995), all U.S. Government 
 * End Users acquire the source code with only those rights set forth herein. 
 *
 ******************************************************************************/


#include <assert.h>


#ifdef WIN32
# include <windows.h>
#endif

#include <oac/Process.h>
#include <oac/Log.h>

using namespace std;

#ifdef WIN32


struct oaProcess_InfoType
{
  STARTUPINFOA ChildStartupInfo;
  PROCESS_INFORMATION ProcInfo;
}; 

static string ConstructCmd(const vector<string> &cmd)
{
  string Ret;

  assert(cmd.size() > 0);

  Ret = cmd[0];

  vector<string>::const_iterator Iter = cmd.begin();
  Iter++;
  for(; Iter != cmd.end(); Iter++)
  {
    Ret += " \"";
    Ret += *Iter;
    Ret += "\"";
  }

  OAC_MSG("Starting process: " << Ret)

  return(Ret);
}

bool oacProcess::RunAsync(void)
{
  assert(!pIsRunning);

  oaProcess_InfoType *Info = (oaProcess_InfoType *)pProcInfo;

  BOOL Ret;
  string Cmd = ConstructCmd(pCmd);

  ZeroMemory(&Info->ChildStartupInfo, sizeof(Info->ChildStartupInfo));
  Info->ChildStartupInfo.cb = sizeof(Info->ChildStartupInfo);
  Info->ChildStartupInfo.dwFlags = STARTF_USESTDHANDLES;
  Info->ChildStartupInfo.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
  Info->ChildStartupInfo.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
  Info->ChildStartupInfo.hStdError = GetStdHandle(STD_ERROR_HANDLE);

  ZeroMemory(&Info->ProcInfo, sizeof(Info->ProcInfo));

  LPSTR TmpCmd = (LPSTR)Cmd.data();

  Ret = CreateProcessA(NULL,
                       TmpCmd,
                       NULL,
                       NULL,
                       FALSE,
                       0, 
                       NULL,
                       NULL,
                       &Info->ChildStartupInfo,
                       &Info->ProcInfo);

  if(!Ret)
    return(false);

  pIsRunning = true;
  return(true);
}

int oacProcess::Id(void)
{
  oaProcess_InfoType *Info = (oaProcess_InfoType *)pProcInfo;
  return((int)Info->ProcInfo.dwProcessId);
}

bool oacProcess::Wait(int &exit_code)
{
  if(!pIsRunning)
    return(false);

  oaProcess_InfoType *Info = (oaProcess_InfoType *)pProcInfo;

  if(!pIsRunning)
    return(false);

  DWORD Ret;
  DWORD ExitCode;
 
  CloseHandle(Info->ProcInfo.hThread);
  Ret = WaitForSingleObject(Info->ProcInfo.hProcess, INFINITE);

  pIsRunning = false;

  if(Ret == WAIT_FAILED)
    return(false);

  if(GetExitCodeProcess(Info->ProcInfo.hProcess, &ExitCode))
  {
    exit_code = ExitCode;
    return(true);
  }

  return(false);
}

int oacProcess::Kill(void)
{
  oaProcess_InfoType *Info = (oaProcess_InfoType *)pProcInfo;

  if(!pIsRunning)
    return(-1);

  return(0);
}


#else

# error "Only Win32 is implemented currently."

#endif


void oacProcess::Init(void)
{
  pProcInfo = malloc(sizeof(oaProcess_InfoType));
  memset(pProcInfo, 0, sizeof(oaProcess_InfoType));

  pIsRunning = false;
}

oacProcess::oacProcess(int argc, char *argv[])
{
  Init();

  assert(argc > 0);
  assert(argv != NULL);

  for(int i=0; i < argc; ++i)
    pCmd.push_back(string(argv[1]));
}

oacProcess::oacProcess(const std::vector<std::string> &cmd)
{
  Init();

  assert(cmd.size() > 0);

  pCmd = cmd;
}

oacProcess::~oacProcess()
{
  int ExitCode;
  Wait(ExitCode);

  free(pProcInfo);
}

int oacProcess::Run(void)
{
  if(RunAsync())
    return(-1);

  int ExitCode;
  if(!Wait(ExitCode))
    return(-1);
  
  return(ExitCode);
}

