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

#include <oaRPC.h>
#include <oaRPCSocketTransport.h>
#include <OpenAutomate.h>
#include <OpenAutomate_Internal.h>

#define ERROR(msg) \
         Error(__FILE__, __LINE__, (msg));

using namespace std;

//******************************************************************************
//*** Prototypes
//******************************************************************************

static void Error(const char *file, int line, const char *msg);
void ParseArgs(int argc, char *argv[]);

//******************************************************************************
//*** Globals
//******************************************************************************

static const char *CommandExe;
static const char *TextLogFilename = NULL;
static int Port = 6969;
static bool RPCInitialized = false;
static bool NoExit  = false;
static char *OAOpt = NULL;

//******************************************************************************
//*** Main
//******************************************************************************

int main(int argc, char* argv[])
{
  char ErrorMsg[4096];
  oaRPCTransport Transport;
  void *Data = NULL;
  oaBool Ret;

  ParseArgs(argc, argv);

  oaVersion Version;
  if(oaInit((const oaString)OAOpt, &Version) != OA_TRUE)
    ERROR("OpenAutomate failed to initialize.")

  Ret = oaRPCInitSocketServerTransport(&Transport, 
                                       &Data, 
                                       Port);
  if(!Ret)
  {
    sprintf(ErrorMsg, "Couldn't start server on tcp port %d", Port); 
    ERROR(ErrorMsg)
  }

  RPCInitialized = true;

  oaRPCServer *Server = oaRPCCreateServer(oaiGetCurrentFuncTable(), 
                                          &Transport, 
                                          Data, 
                                          TextLogFilename);
  while(1)
  {
    Ret = oaRPCRunServer(Server);

    if(Ret == OA_FALSE || !NoExit)
      break;
  }

  oaRPCDestroyServer(Server);

  oaRPCCleanup();

  return(0);
}

void Error(const char *file, int line, const char *msg)
{
  fprintf(stderr, "ERROR: %s\n", msg);
  fflush(stderr);

  if(RPCInitialized)
    oaRPCCleanup();

  exit(1);
}

//******************************************************************************
//*** Functions
//******************************************************************************


const char *Basename(const char *cmd)
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
  cerr << "Usage: " << Basename(CommandExe) << " [-h|-help] [-port <num>] "
       << "[-noexit] [-log <file>] <oa_option>" << endl;
  
 #define INDENT "         "

  if(verbose)
  {
    cerr << 
      INDENT "oa_option   : Same as what's passed to -openautomate for the\n" 
      INDENT "              OA enabled application.\n" 
      INDENT "-port <num> : Same as what's passed to -openautomate for the\n" 
      INDENT "              OA enabled application. Default is 6969.\n"
      INDENT "-noexit     : Doesn't exit when the OA_CMD_EXIT command is\n"
      INDENT "              issued.  Instead it just keeps on accepting new\n" 
      INDENT "              TCP connections, and continues as normal.\n" 
      INDENT "-log <file> : Writes out a log of requests and response to the\n"
      INDENT "              given file.\n"
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
        cerr << "ERROR: -port must have argument <num>" << argv[i] << endl;
        Usage();
      }

      Port = atoi(argv[i]);
      if(Port < 1)
        ERROR("port must be greater than 1")
    }
    else if(!strcmp(argv[i], "-log"))
    {
      i++;
      if(i >= argc)
      {
        cerr << "ERROR: -log must have argument <file>" << argv[i] << endl;
        Usage();
      }

      TextLogFilename = argv[i];
    }
    else if(!strcmp(argv[i], "-noexit"))
    {
      NoExit = true;
    }
    else
    {
      cerr << "ERROR: Unknown option " << argv[i] << endl;
      Usage();
    }
  }

  if(i == argc)
    Usage();

  OAOpt = argv[i++];
}


