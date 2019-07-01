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

#include <stdio.h>
#include <iostream>
#include <sstream>

#include <oaRPCSocketTransport.h>
#include <oac/Context.h>
#include <oac/TestBase.h>
#include <oac/Process.h>
#include <oac/Log.h>
#include <oac/FileLogger.h>

#define OATEST_ERROR(msg) \
         { \
           ostringstream OStrStream; \
           OStrStream << msg; \
           Error(__FILE__, __LINE__, (OStrStream.str().data())); \
         }

using namespace std;

//******************************************************************************
//*** Prototypes
//******************************************************************************

static void Error(const char *file, int line, const char *msg);
static void ParseArgs(int argc, char *argv[]);
static void RegisterTestModules(void);
static string GetOAPluginPath(void);
static bool RunApp(void *data = NULL);

//******************************************************************************
//*** Globals
//******************************************************************************

static const char *CommandExe;
static const char *TraceFilename = NULL;
static const char *LogFilename = NULL;
static int Port = 6969;
static int NumColumns = 78;
static bool RunAppFlag = true;
static bool NoStdoutFlag = false;
static vector<string> AppCmd;
static bool RPCInitialized = false;
static oacContext *Context = NULL;
static oacProcess *AppProcess = NULL;

//******************************************************************************
//*** class MyClientApp
//******************************************************************************

class MyClientApp : public oacContext::ClientApp
{
public :

  virtual ~MyClientApp();
  virtual bool Run(void);
  virtual long WaitForExit(void);

  bool IsRunning(void);

private:

  bool pIsRunning;
};

//******************************************************************************
//*** Main
//******************************************************************************

int main(int argc, char *argv[])
{
  char ErrorMsg[4096];
  oaRPCTransport Transport;
  void *Data = NULL;
  oaBool Ret;

  ParseArgs(argc, argv);

  oacFileLogger *StdoutLogger = new oacFileLogger(NumColumns);
  StdoutLogger->SetFile(stdout);
  oacLog::AddLogger(StdoutLogger);

 
  if(NoStdoutFlag)
    StdoutLogger->SetEnabled(false);

  if(LogFilename != NULL)
  {
    oacFileLogger *FileLogger = new oacFileLogger(NumColumns);
    if(!FileLogger->SetFile(LogFilename))
      OATEST_ERROR("Could not open log file \"" << LogFilename << 
                   "for write.")
    oacLog::AddLogger(FileLogger);
  }

  string OAPluginPath = GetOAPluginPath();


  Ret = oaRPCInitSocketServerTransport(&Transport, 
                                       &Data, 
                                       Port);
  if(!Ret)
  {
    sprintf(ErrorMsg, "Couldn't start server on tcp port %d", Port); 
    OATEST_ERROR(ErrorMsg)
  }



  oaRPCServer *Server = oaRPCCreateServer(Context->GetOAFuncTable(), 
                                          &Transport, 
                                          Data, 
                                          TraceFilename);

  MyClientApp ClientApp;
  Context = new oacContext(Server, &ClientApp);
  if(!Context->Run())
    OATEST_ERROR("Could not run application.");


  RegisterTestModules();

  Ret = oaRPCRunServer(Server);

  oaRPCDestroyServer(Server);
  RPCInitialized = true;

  if(AppProcess)
  {
    int ExitCode = 69;

    if(!AppProcess->Wait(ExitCode))
      OATEST_ERROR("Wait for OA application failed.");

    if(ExitCode != 0)
      OATEST_ERROR("OA application exited with non-zero exit code " << 
                   ExitCode);

    delete AppProcess;
    AppProcess = NULL;
  }
  
  oaRPCCleanup();

  if(oacLog::TotalGroupTestsFailed() > 0)
    return(1);

  return(0);
}

//******************************************************************************
//*** Functions
//******************************************************************************


static const char *Basename(const char *cmd)
{
  int Len = (int)strlen(cmd);

  if(Len <= 0)
    return(cmd);

  for(int i=Len-1; i > 0; --i)
    if(cmd[i] == '/' || cmd [i] == '\\')
    {
      if(i + 1 < Len)
        return(&cmd[i+1]);
      break;
    }

  return(cmd);
}

void Usage(bool verbose = false)
{
  cerr << "Usage: " << Basename(CommandExe) << " [-h|-help] [-port <num>]\n"
          "              [-log <file>] [-col <num>] [-nostdout] [-trace <file>]\n"
          "              [-noapp] <app.exe> [app_opt1] [app_opt2] [...]" << endl;
  
 #define INDENT "         "

  if(verbose)
  {
    cerr << 
      INDENT "-port <num>   : Same as what's passed to -openautomate for the\n" 
      INDENT "                OA enabled application. Default is 6969.\n"
      INDENT "-log <file>   : Writes out all messages, results, etc... to a\n"
      INDENT "                a log file in addition to the default stdout.\n"
      INDENT "-col <num>    : Maximum number of columns for log output.  \n"
      INDENT "                Default is 78.\n"
      INDENT "-nostdout     : Disables writing the result data to stdout.\n"
      INDENT "-trace <file> : Writes out a readable trace of requests and \n"
      INDENT "                response to the\n"
      INDENT "                given file.\n"
      INDENT "-noapp        : If this flag is given, the application is not\n"
      INDENT "                launched.\n"
      ;
  }

  exit(1);
}

void ParseArgs(int argc, char *argv[])
{
  CommandExe = argv[0];
  int i;

  for(i=1;  i < argc; ++i)
  {
    if(argv[i][0] != '-' || !strcmp(argv[i], "--"))
      break;

    if(!strcmp(argv[i], "-help") || !strcmp(argv[i], "-h"))
      Usage(true);
    else if(!strcmp(argv[i], "-port"))
    {
      i++;
      if(i >= argc)
      {
        OATEST_ERROR("-port must have argument <num>" << argv[i])
        Usage();
      }

      Port = atoi(argv[i]);
      if(Port <= 1)
        OATEST_ERROR("port must be greater than 1")
    }
    else if(!strcmp(argv[i], "-trace"))
    {
      i++;
      if(i >= argc)
      {
        OATEST_ERROR("trace must have argument <file>" << argv[i]);
        Usage();
      }

      TraceFilename = argv[i];
    }
    else if(!strcmp(argv[i], "-log"))
    {
      i++;
      if(i >= argc)
      {
        OATEST_ERROR("-log must have argument <file>" << argv[i]);
        Usage();
      }

      LogFilename = argv[i];
    }
    else if(!strcmp(argv[i], "-col"))
    {
      i++;
      if(i >= argc)
      {
        OATEST_ERROR("-col must have argument <num>" << argv[i])
        Usage();
      }

      NumColumns = atoi(argv[i]);
      if(NumColumns <= 1)
        OATEST_ERROR("number of columns must be greater than 1")
    }
    else if(!strcmp(argv[i], "-nostdout"))
    {
      NoStdoutFlag = true;
    }
    else if(!strcmp(argv[i], "-noapp"))
    {
      RunAppFlag = false;
    }
    else
    {
      Usage();
      OATEST_ERROR("Unknown option " << argv[i]);
    }
  }

  if(RunAppFlag)
  {
    if(i == argc)
    {
      Usage();
      OATEST_ERROR("Must have at least one argument for the OA application "
                   "exe.");
    }

    AppCmd.push_back(string(argv[i++]));
    AppCmd.push_back(string("-openautomate"));

    char Str[1024];
    sprintf(Str, "%s;localhost:%d", GetOAPluginPath().data(), Port);
    AppCmd.push_back(string(Str));

    for(; i < argc; ++i)
      AppCmd.push_back(string(argv[i]));
  }
}

void Error(const char *file, int line, const char *msg)
{
  fprintf(stderr, "ERROR: %s\n", msg);
  fflush(stderr);

  if(RPCInitialized)
    oaRPCCleanup();

  exit(1);
}

#ifdef WIN32

#include <windows.h>

string ThisExePath(void)
{
  char Path[4096];

  HMODULE Module = GetModuleHandle(NULL);

  GetModuleFileNameA(Module, Path, sizeof(Path));

  size_t End = strlen(Path) - 1;
  assert(End > 0);

  while(End > 0 && Path[End] != '\\')
    End--;

  Path[End] = 0;

  return(string(Path)); 
}

string GetOAPluginPath(void)
{
  string FullPath = ThisExePath() + string("\\oaremote_plugin.dll");

  FILE *FP = fopen(FullPath.data(), "rb");
  if(!FP)
  {
    cerr << "ERROR: couldn't open \"" << FullPath << "\" for read." 
            "  The plugin must be in the same directory as " << 
            Basename(CommandExe) << "." << endl;
    exit(-1);
  }

  fclose(FP);
  return(FullPath);
}

#else

# error "Only Win32 is currently supported"

#endif


static bool RunApp(void *data)
{
  int ExitCode;

  if(!RunAppFlag)
  {
    cerr << "Warning: Please launch the application with oaremote_plugin.dll "
            "manually." << endl;
    return(true);
  }

  if(AppProcess != NULL)
  {
    if(!AppProcess->Wait(ExitCode))
      OATEST_ERROR("Wait for OA application to exit failed.");
      
    if(ExitCode != 0)
      OATEST_ERROR("OA application exited with non-zero exit code " << 
                   ExitCode);

    delete AppProcess;
    AppProcess = NULL;
  }

  AppProcess = new oacProcess(AppCmd);
  if(!AppProcess->RunAsync())
      OATEST_ERROR("Couldn't launch OA application.");

  return(true);
}

#define OAC_TEST_MODULE(name) \
  extern oacTestBase * OAC_TEST_CONSTRUCTOR_NAME(name) (void);

#include "../tests/test_list.h"

static void RegisterTestModules(void)
{
#  define OAC_TEST_MODULE(name) \
     Context->AddTest( OAC_TEST_CONSTRUCTOR_NAME(name) () );

#include "../tests/test_list.h"
}

//******************************************************************************
//*** MyClientApp methods
//******************************************************************************

MyClientApp::~MyClientApp()
{
  if(AppProcess != NULL)
    WaitForExit();
}

bool MyClientApp::Run(void)
{
  if(!RunAppFlag)
  {
    cerr << "Note: Please launch the application with oaremote_plugin.dll "
            "manually." << endl;
    return(true);
  }

  if(AppProcess != NULL)
    WaitForExit();

  AppProcess = new oacProcess(AppCmd);
  if(!AppProcess->RunAsync())
    OATEST_ERROR("Couldn't launch OA application.");

  pIsRunning = true;
  return(true);
}

long MyClientApp::WaitForExit(void)
{
  int ExitCode;

  if(!RunAppFlag)
  {
    cerr << "Note: Client application should exit now." << endl;
    return(0);
  }

  assert(IsRunning());
  assert(AppProcess != NULL);

  if(!IsRunning())
    return(-1);

  if(!AppProcess->Wait(ExitCode))
    OATEST_ERROR("Wait for OA application to exit failed.");
    
  if(ExitCode != 0)
    OATEST_ERROR("OA application exited with non-zero exit code " << 
                 ExitCode);

  delete AppProcess;
  AppProcess = NULL;

  pIsRunning = false;
  return(ExitCode);
}

bool MyClientApp::IsRunning(void)
{
  if(RunAppFlag)
    return(AppProcess != NULL);
  
  return(pIsRunning);
}
