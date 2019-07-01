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

#ifndef _oaRPC_h
#define _oaRPC_h

#include <OpenAutomate.h>
#include <OpenAutomate_Internal.h>

#ifdef __cplusplus
extern "C"
{
#endif

/*******************************************************************************
 *** Macros
 ******************************************************************************/

#ifndef OARPC_ERROR 

#include <stdlib.h>

void oaRPC_Error(const char *filename, int line, const char *msg);

#define OARPC_ERROR(str) \
  oaRPC_Error(__FILE__, __LINE__, (str)); 

#endif

#define OARPC_MAX_CALL_DEPTH 128

/*******************************************************************************
 *** Types
 ******************************************************************************/

typedef unsigned char oaRPCUInt8;
typedef unsigned short oaRPCUInt16;
typedef unsigned long oaRPCUInt32;

typedef oaRPCUInt32 oaRPCSize;

typedef enum
{
  OARPC_ERROR_NONE              = 0,
  OARPC_ERROR_UNKNOWN_FUNCTION  = 1,
  OARPC_ERROR_MALFORMED_REQUEST = 2,
  OARPC_ERROR_NEGOTIATION_FAILED = 3,
} oaRPCErrorType;

typedef enum
{
#define OARPC_DECLARE_FUNC(func_name, func_id) \
   OARPC_FUNC_##func_name = func_id, 

#include "oaRPC_Functions.h"

   OARPC_FUNC_END
} oaRPCFuncType;

typedef struct oaRPCBufStruct
{
  oaRPCSize Size;
  oaRPCSize BufSize;
  char *Buf;
} oaRPCBuf;

typedef struct oaRPCTransportStruct
{
  oaBool (*Send)(void *user_data, const oaRPCBuf *buf);
  oaBool (*Recv)(void *user_data, oaRPCBuf *buf, oaBool *new_connection);
  oaBool (*Cleanup)(void *user_data);
} oaRPCTransport;

typedef enum
{
  OARPC_MODE_SERVER,
  OARPC_MODE_CLIENT,
} oaRPCMode;

typedef struct oaRPCServerStruct
{
  const oaiFunctionTable *FuncTable;
  oaRPCTransport *Transport;
  void *TransportUserData;
  char *LogFilename;
  
  struct 
  {
    oaRPCFuncType Func;
    const oaRPCBuf *PreResponseBuf;
  } CallStack[OARPC_MAX_CALL_DEPTH];

  oaInt CurDepth;

} oaRPCServer;


/*******************************************************************************
 *** RPC Client/Server Init & Cleanup
 ******************************************************************************/


const oaiFunctionTable *oaRPCInitClient(oaRPCTransport *transport, 
                                        void *user_data,
                                        const oaVersion *app_oa_version);

oaRPCServer *oaRPCCreateServer(const oaiFunctionTable *func_table, 
                               oaRPCTransport *transport, 
                               void *user_data,
                               const char *log_filename);

oaBool oaRPCRunServer(oaRPCServer *server);

/* oaRPCServerNestCommand() may be called from within GetNextCommand() 
   callback dispatched by either oaRPCRunServer() or another instance
   of oaRPCServerNestCommand().  It returns 'command' to the application
   without having to exit the current invocation of GetNextCommand().
   Control will be returned the next time the application calls 
   GetNextCommand() */
oaBool oaRPCServerNestCommand(oaRPCServer *server, const oaCommand *command);

/* Sends the exit command as a response.  It may only be called from within
   the GetNextCommand() callback.  Returns false on error. */
oaBool oaRPCServerSendExitCommand(oaRPCServer *server,
                                  oaBool (*post_exit_func)(void *),
                                  void *data);

oaBool oaRPCDestroyServer(oaRPCServer *server);

oaBool oaRPCCleanup(void);

/*******************************************************************************
 *** Low-level buffer functions
 ******************************************************************************/

oaRPCBuf *oaRPCAllocBuf(void);
void oaRPCFreeBuf(oaRPCBuf *buf);
void oaRPCPushBuf(oaRPCBuf *buf, const void *src, oaRPCSize n);
void oaRPCClearBuf(oaRPCBuf *buf);
void oaRPCSetBufSize(oaRPCBuf *buf, oaRPCSize size);
char *oaRPCGetBuf(oaRPCBuf *buf, oaRPCSize offset);
const char *oaRPCGetBufConst(const oaRPCBuf *buf, oaRPCSize offset);
oaBool oaRPCWriteBufToFile(const char *filename, oaRPCBuf *buf, oaBool append);

#ifndef NDEBUG
 
#  define OARPC_GET_BUF(buf, offset) \
     oaRPCGetBuf((buf), (offset))

#  define OARPC_GET_BUF_CONST(buf, offset) \
     oaRPCGetBufConst((buf), (offset))

#else
#  define OARPC_GET_BUF(buf, offset) \
     ((buf)->Buf + (offset))

#  define OARPC_GET_BUF_CONST OARPC_GET_BUF

#endif

#  define OARPC_BUF_SIZE(buf) ((buf)->Size)
    

/*******************************************************************************
 *** Low-level serialization functions
 ******************************************************************************/

void oaRPCSerializeString(oaRPCBuf *buf, const oaChar *str);
void oaRPCSerializeInt(oaRPCBuf *buf, oaInt val);
void oaRPCSerializeFloat(oaRPCBuf *buf, oaFloat val);
void oaRPCSerializeBool(oaRPCBuf *buf, oaBool val);
void oaRPCSerializeValue(oaRPCBuf *buf, oaOptionDataType type, oaValue value);

const oaChar *oaRPCDeserializeString(const oaRPCBuf *buf, oaRPCSize *offset);
oaInt oaRPCDeserializeInt(const oaRPCBuf *buf, oaRPCSize *offset);
oaFloat oaRPCDeserializeFloat(const oaRPCBuf *buf, oaRPCSize *offset);
oaBool oaRPCDeserializeBool(const oaRPCBuf *buf, oaRPCSize *offset);
oaValue oaRPCDeserializeValue(const oaRPCBuf *buf, 
                              oaOptionDataType *type, 
                              oaRPCSize *offset);


#ifdef __cplusplus
}
#endif


#endif

