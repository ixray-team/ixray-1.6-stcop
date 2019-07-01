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

#include "../inc/OpenAutomate.h"
#include "../inc/OpenAutomate_Internal.h"
#include <string.h>
#include <stdlib.h>

#define MIN_REQUIRED_VERSION_MINOR 6

#ifdef WIN32
# include <windows.h>
typedef int __w64 OA_NATIVE_INT;
#elif _WIN64
typedef __int64 OA_NATIVE_INT;
#else
# include <dlfcn.h>
typedef oaInt OA_NATIVE_INT;
#endif

#define OA_MAX_PATH_LENGTH  2048
#define OA_MIN(a, b) ((a) < (b) ? (a) : (b))

/*******************************************************************************
* Prototypes
******************************************************************************/

static void Cleanup(void);
static oaBool ParseInitStr(const oaChar *init_str, 
                           oaChar *plugin_path, 
                           oaChar *opt);

static oaChar* SearchAndReplace(const oaChar* src, 
                                oaChar* search, 
                                oaChar* replace);
static oaBool LoadPlugin(void);

/*******************************************************************************
* Global Variables
******************************************************************************/

static oaiPluginInitFunc PluginInitFunc = NULL;
static oaiFunctionTable FuncTable;
static oaBool InitFlag = OA_FALSE;
static oaChar *PluginPath = NULL;
static oaChar *Opt = NULL;
static oaVersion PluginVersion, OAVersion;

#ifdef WIN32
static HMODULE PluginHandle = NULL;
#else
static void *PluginHandle = NULL;
#endif

/*******************************************************************************
* Public Functions
******************************************************************************/

#ifdef __cplusplus
extern "C"
{
#endif

oaBool oaInit(const oaChar *init_str, oaVersion *version)
{
  size_t InitStrLen;
  oaiFunctionTable *PluginFuncTable;

  if(InitFlag)
  {
    OA_ERROR("oaInit() called more than once.");
    Cleanup();
    return(OA_FALSE);
  }

  InitStrLen = OA_STRLEN(init_str);
  PluginPath = (oaChar *)malloc((InitStrLen+1)*sizeof(oaChar));
  Opt = (oaChar *)malloc((InitStrLen+1)*sizeof(oaChar));

  if(!ParseInitStr(init_str, PluginPath, Opt))
  {
    OA_ERROR("Couldn't parse initialization string.");
    Cleanup();
    return(OA_FALSE);
  }

  PluginInitFunc = NULL;
  memset(&FuncTable, 0, sizeof(FuncTable));

  if(!LoadPlugin())
  {
    OA_ERROR("Couldn't load the plugin.");
    Cleanup();
    return(OA_FALSE);
  }

  OA_ASSERT(PluginInitFunc);
  OA_INIT_VERSION_STRUCT(OAVersion)

  PluginFuncTable = PluginInitFunc(Opt, &PluginVersion, OAVersion);

  if(PluginVersion.Major != OA_VERSION_MAJOR)
  {
    OA_ERROR("Plugin version mismatch.");
    Cleanup();
    return(OA_FALSE);
  }

  if(PluginVersion.Minor < MIN_REQUIRED_VERSION_MINOR)
  {
    oaChar ErrorStr[1024];
    sprintf(ErrorStr, 
            "Minimum required version is %d.%d.  "
            "Plugin version is %d.%d.", 
            OA_VERSION_MAJOR, 
            MIN_REQUIRED_VERSION_MINOR,
            PluginVersion.Major,
            PluginVersion.Minor);

    OA_ERROR(ErrorStr);
    Cleanup();
    return(OA_FALSE);
  }

  if(PluginVersion.Major != OA_VERSION_MAJOR)
  {
    OA_ERROR("Plugin version mismatch.");
    Cleanup();
    return(OA_FALSE);
  }

  if(!PluginFuncTable || !PluginFuncTable->GetNextCommand)
  {
    OA_ERROR("Plugin is misconfigured.");
    Cleanup();
    return(OA_FALSE);
  }

  memcpy(&FuncTable, 
         PluginFuncTable, 
         OA_MIN(sizeof(FuncTable), PluginFuncTable->TableSize));

  InitFlag = OA_TRUE;

  *version = OAVersion;

  return(OA_TRUE);
}

#define INIT_CHECK_RET(ret) \
  if(!InitFlag) \
  { \
    OA_ERROR("OA not initialized."); \
    return ret; \
  }

#define INIT_CHECK() \
  if(!InitFlag) \
  { \
    OA_ERROR("OA not initialized."); \
    return; \
  }

void oaInitCommand(oaCommand *command)
{
  OA_ASSERT(command);

  memset(command, 0, sizeof(oaCommand));
  command->StructSize = sizeof(oaCommand);
}

oaCommandType oaGetNextCommand(oaCommand *command)
{
  oaCommandType NextCommand;

  OA_ASSERT(command);

  INIT_CHECK_RET(OA_CMD_EXIT);
  OA_ASSERT(FuncTable.GetNextCommand);

  NextCommand = FuncTable.GetNextCommand(command);

  if(NextCommand == OA_CMD_EXIT) 
    Cleanup();

  command->Type = NextCommand;
  return NextCommand;
}

oaNamedOption *oaGetNextOption(void)
{
  INIT_CHECK_RET(NULL);
  
  if(FuncTable.GetNextOption)
    return(FuncTable.GetNextOption());

  return(NULL);
}

void oaInitOption(oaNamedOption *option)
{
  OA_ASSERT(option);

  memset(option, 0, sizeof(oaNamedOption));
  option->StructSize = sizeof(oaNamedOption);
  option->NumSteps = -1;

  option->Dependency.StructSize = sizeof(oaOptionDependency);
}

void oaAddOption(const oaNamedOption *option)
{
  INIT_CHECK();

  if(FuncTable.AddOption)
    FuncTable.AddOption(option);
}

void oaAddOptionValue(const oaChar *name, 
                      oaOptionDataType value_type, 
                      const oaValue *value)
{
  INIT_CHECK();

  if(FuncTable.AddOptionValue)
    FuncTable.AddOptionValue(name, value_type, value);
}

void oaAddBenchmark(const oaChar *benchmark_name)
{
  INIT_CHECK();

  if(FuncTable.AddBenchmark)
    FuncTable.AddBenchmark(benchmark_name);
}

void oaAddResultValue(const oaChar *name, 
                      oaOptionDataType value_type,
                      const oaValue *value)
{
  INIT_CHECK();

  if(FuncTable.AddResultValue)
    FuncTable.AddResultValue(name, value_type, value);
}

oaBool oaSendSignal(oaSignalType signal, void *param)
{
  INIT_CHECK_RET(OA_FALSE);

  if(FuncTable.SendSignal)
    return(FuncTable.SendSignal(signal, param));

  return(OA_FALSE);
}

void oaInitMessage(oaMessage *message)
{
  memset(message, 0, sizeof(oaMessage)); 
  message->StructSize = sizeof(oaMessage);
}

void oaStartBenchmark(void)
{
  INIT_CHECK();

  if(FuncTable.StartBenchmark)
    FuncTable.StartBenchmark();
}

void oaDisplayFrame(oaFloat t)
{
  INIT_CHECK();

  if(FuncTable.DisplayFrame)
    FuncTable.DisplayFrame(t);
}

void oaEndBenchmark(void)
{
  INIT_CHECK();

  if(FuncTable.EndBenchmark)
    FuncTable.EndBenchmark();
}

void oaiInitFuncTable(oaiFunctionTable *table)
{
  assert(table);
  memset(table, 0, sizeof(oaiFunctionTable));
  table->TableSize = sizeof(oaiFunctionTable);
}

oaiFunctionTable *oaiGetCurrentFuncTable(void)
{
  INIT_CHECK_RET(NULL);

  return(&FuncTable);
}

#ifdef __cplusplus
}
#endif

/*******************************************************************************
* Private Functions
******************************************************************************/

static void Cleanup(void)
{
  if(PluginPath)
  {
    free(PluginPath);
    PluginPath = NULL;
  }

  if(Opt)
  {
    free(Opt);
    Opt = NULL;
  }

  InitFlag = OA_FALSE;
  PluginInitFunc = NULL;
  memset(&FuncTable, 0, sizeof(FuncTable));

  if(PluginHandle)
  {
#ifdef WIN32
    FreeLibrary(PluginHandle);
#else
    dlclose(PluginHandle);
#endif
    PluginHandle = NULL;
  }
}

static oaBool ParseInitStr(const oaChar *init_str, 
                           oaChar *plugin_path, 
                           oaChar *opt)
{
  oaChar* InitStr = SearchAndReplace(init_str, "%20", " ");

  int i=0;
  for(i=0; InitStr[i]; ++i)
  {
    if(InitStr[i] == ';')
    {break;}
  }

  OA_STRNCPY(plugin_path,InitStr,i);

  plugin_path[i] = 0;

  if(InitStr[i] == ';')
  {
    OA_STRCPY(opt, InitStr + i + 1);
  }
  else
    opt[0] = 0;

  return(OA_TRUE);
}

static oaChar* SearchAndReplace(const oaChar* src, oaChar* search, oaChar* replace)
{
  const oaChar* Src1;
  const oaChar* Src2;
  oaChar* ReturnStr;
  oaChar* Dest;
  OA_NATIVE_INT Num;
  size_t SourceLen = OA_STRLEN(src);
  size_t SearchLen = OA_STRLEN(search);
  size_t ReplaceLen = OA_STRLEN(replace);
  size_t NewLen = 0;
  int SearchCnt = 0;

  Src1 = src;

  while ((Src2 = strstr(Src1, search)))
  { 
    Src1 = Src2 + ReplaceLen;
    SearchCnt++;
  }

  NewLen = SourceLen - SearchCnt*SearchLen + SearchCnt*ReplaceLen;
 
  ReturnStr = (oaChar*) malloc((NewLen+1)*sizeof(oaChar));

  Src1 = src;
  Dest = ReturnStr;

  while ((Src2 = strstr (Src1, search)))
  {
    Num = Src2 - Src1;
    memcpy (Dest, Src1, Num);

    Src1 = Src2 + SearchLen;

    Dest += Num;
    memcpy(Dest, replace, ReplaceLen);
    Dest += ReplaceLen;
  }

  strcpy_s(Dest, ((NewLen+1)*sizeof(oaChar)), Src1);

  return ReturnStr;
}

static oaBool LoadPlugin(void)
{
#ifdef WIN32
  WCHAR WidePluginPath[OA_MAX_PATH_LENGTH];

  int Length = MultiByteToWideChar(CP_UTF8, 0, 
    PluginPath, (int)OA_STRLEN(PluginPath)+1, 
    WidePluginPath, OA_MAX_PATH_LENGTH);

  if( Length==0 )
  {
    DWORD Error = GetLastError();
    if(Error == ERROR_INSUFFICIENT_BUFFER )
      fprintf(stderr, "OpenAutomate MultiByteToWideChar returned error: ERROR_INSUFFICIENT_BUFFER\n");
    else if( Error == ERROR_INVALID_FLAGS )
      fprintf(stderr, "OpenAutomate MultiByteToWideChar returned error: ERROR_INVALID_FLAGS\n");
    else if( Error == ERROR_INVALID_PARAMETER  )
      fprintf(stderr, "OpenAutomate MultiByteToWideChar returned error: ERROR_INVALID_PARAMETER\n");
    else if( Error == ERROR_NO_UNICODE_TRANSLATION )
      fprintf(stderr, "OpenAutomate MultiByteToWideChar returned error: ERROR_NO_UNICODE_TRANSLATION\n");

    if( OA_STRLEN(PluginPath)==0 )
      fprintf(stderr, "OpenAutomate PluginPath was undefined\n");

    return(OA_FALSE);
  }
  else 
    PluginHandle = LoadLibraryW(WidePluginPath);
#else
  PluginHandle = dlopen(PluginPath, RTLD_LAZY);
#endif

  if(!PluginHandle)
  {
    fprintf(stderr, "OpenAutomate Failed loading: '%s'\n", PluginPath);
    return(OA_FALSE);
  }

  PluginInitFunc = 
#ifdef WIN32
    (oaiPluginInitFunc)GetProcAddress(PluginHandle, OA_PLUGIN_INIT_FUNC);
#else
    (oaiPluginInitFunc)dlsym(PluginHandle, OA_PLUGIN_INIT_FUNC);
#endif

  if(!PluginInitFunc)
  {
    OA_ERROR("Plugin does not have the correct entry point.");
    return(OA_FALSE);
  }

  return(OA_TRUE);
}

void oaAddFrameValue(const oaChar *name, 
                     oaOptionDataType value_type,
                     const oaValue *value)
{
  INIT_CHECK();

  if(FuncTable.AddFrameValue)
    FuncTable.AddFrameValue(name, value_type, value);
}
