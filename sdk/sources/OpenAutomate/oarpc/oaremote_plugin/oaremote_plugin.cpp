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

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <windows.h>

#include <OpenAutomate_Internal.h>
#include <oaRPC.h>
#include <oaRPCSocketTransport.h>

#ifdef __cplusplus
extern "C"
{
#endif

#define DLLEXPORT __declspec(dllexport)

#define PRINT(var) \
  cerr << __FILE__ << "," << __LINE__ << ": " #var " = " << (var) << endl;


using namespace std;


BOOL APIENTRY DllMain(HANDLE hModule, 
                      DWORD  ul_reason_for_call, 
                      LPVOID lpReserved)
{
  return TRUE;
}

oaiFunctionTable FuncTable;
static oaRPCTransport Transport;
static void *Data = NULL;
static char Hostname[1024] = "localhost";
static int Port = 6969;


void Usage(bool verbose = false)
{
  cerr << "Usage: <PATH>/oaremote_plugin.dll;  [-h|-help] [hostname[:port]]" << endl;
  
 #define INDENT "         "

  if(verbose)
  {
    cerr << 
      INDENT "-h|-help        : This help message.\n" 
      INDENT "hostname[:port] : Hostname tand optional port to connect to.\n"
      INDENT "                  Default is localhost:6969.\n"
      ;
  }

  exit(1);
}

void ParseHostname(char *hostname)
{
  char *Tmp  = strtok(hostname, ":");

  strncpy(Hostname, Tmp, sizeof(Hostname));

  Tmp = strtok(NULL, "");
  if(Tmp)
  {
    Port = atoi(Tmp);
  }
}

void ParseArgs(int argc, char *argv[])
{
  int i;

  for(i=0;  i < argc; ++i)
  {
    if(argv[i][0] != '-' || !strcmp(argv[i], "--"))
      break;

    if(!strcmp(argv[i], "-help") || !strcmp(argv[i], "-h"))
      Usage(true);
    else
    {
      cerr << "ERROR: Unknown option " << argv[i] << endl;
      Usage();
    }
  }

  if(i == argc)
    Usage();

  ParseHostname(argv[i++]);
}

static bool ParseOpt(char *init_str)
{
  if(!init_str || !*init_str)
    return(true);

  int Argc = 0;
  char *Argv[256];

  Argv[Argc] = strtok(init_str, " ");
  while(1)
  {
    if(!Argv[Argc])
      break;

    Argv[++Argc] = strtok(NULL, " ");
  } 

  ParseArgs(Argc, Argv);

  return(true);
}


DLLEXPORT oaiFunctionTable *oaPluginInit(const char *init_str, 
                                         oaVersion *version)
{
  OA_INIT_VERSION_STRUCT(*version)

  char *Opt = strdup(init_str);
  bool Ret = ParseOpt(Opt);
  free(Opt);

  if(!Ret)
  {
    fprintf(stderr, "ERROR: Couldn't parse option string.\n");
    Usage();
    fflush(stderr);
    return(NULL);
  }

  Ret = (oaRPCInitSocketClientTransport(&Transport, 
                                        &Data, 
                                        Hostname, 
                                        Port) == OA_TRUE);

  if(!Ret)
  {
    fprintf(stderr, "ERROR: Couldn't establish connection to %s:%d\n", 
            Hostname, Port);
    fflush(stderr);
    return(NULL);
  }

  const oaiFunctionTable *TmpTable = oaRPCInitClient(&Transport, Data, version);

  assert(TmpTable);
  oaiInitFuncTable(&FuncTable);
  FuncTable = *TmpTable;
  FuncTable.TableSize = sizeof(FuncTable);

  return(&FuncTable);
}

#ifdef __cplusplus
}

#endif


