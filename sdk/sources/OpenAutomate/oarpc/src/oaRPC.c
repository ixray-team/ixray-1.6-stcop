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

#define _USE_MATH_DEFINES 1

#include <oaRPC.h>
#include <math.h>
#include <stdlib.h>
#include <assert.h>


#define OARPC_MIN_BUF_SIZE 64

#define ERROR_MSG(msg) \
  { \
    fprintf(stderr, "ERROR: %s\n", (msg)); \
    fflush(stderr); \
  }

#ifndef NDEBUG


#  define DUMP_REQ_OR_RESP(type, req_buf, filename) \
    if(filename != NULL) \
    { \
      oaBool Ret; \
      oaRPCBuf *Tmp = oaRPCAllocBuf(); \
      oaRPCPrint##type (req_buf, Tmp); \
      Ret = oaRPCWriteBufToFile(filename, Tmp, OA_TRUE); \
      assert(Ret); \
      oaRPCFreeBuf(Tmp); \
    }
#  define DUMP_REQUEST(req_buf, filename) \
     DUMP_REQ_OR_RESP(Request, req_buf, filename)

#  define DUMP_RESPONSE(req_buf, filename) \
     DUMP_REQ_OR_RESP(Response, req_buf, filename)

#  define PRINT_INT(var) \
     fprintf(stderr, "%s, %d: " #var " = %d\n",__FILE__, __LINE__, (var)); \
     fflush(stderr);
      
#  define MSG(msg) \
     fprintf(stderr, "%s, %d: %s\n",__FILE__, __LINE__, (msg)); \
     fflush(stderr);
      
#else

#  define DUMP_REQUEST(req_buf, filename)
#  define DUMP_RESPONSE(resp_buf, filename)
#  define PRINT_INT(val) 
#  define MSG(msg) 

#endif

/*******************************************************************************
 *** Prototypes
 ******************************************************************************/

static void CommonInit(void);
static void CommonCleanup(void);
static void SerializeGetNextCommandResponse(const oaCommand *command, 
                                            oaRPCBuf *buf);
static void oaRPCPrintRequest(const oaRPCBuf *request, oaRPCBuf *ret);
static void oaRPCPrintResponse(const oaRPCBuf *response, oaRPCBuf *ret);

/*******************************************************************************
 *** Globals
 ******************************************************************************/

static oaBool ClientExited = OA_FALSE;
static char Magic[] = "OpenAutomateRPC";
static const unsigned long RPCVersionMajor = 1;
static const unsigned long RPCVersionMinor = 0;


/*******************************************************************************
 *** oaRPCBuf functions
 ******************************************************************************/

oaRPCBuf *oaRPCAllocBuf(void)
{
  oaRPCBuf *Ret;

  Ret = (oaRPCBuf *)malloc(sizeof(oaRPCBuf));
  assert(Ret);

  Ret->Size = 0;
  Ret->BufSize = OARPC_MIN_BUF_SIZE;
  Ret->Buf = (char *)malloc(OARPC_MIN_BUF_SIZE);
  assert(Ret->Buf);

  return(Ret);
}

void oaRPCFreeBuf(oaRPCBuf *buf)
{
  assert(buf);

  free(buf->Buf);
  free(buf);
}

static void ResizeBuf(oaRPCBuf *buf, oaRPCSize size)
{
  assert(buf);

  if(size > buf->BufSize)
  {
    int Exp = (int)ceil(log((double)size) / M_LN2) + 1;
    oaRPCSize NewSize = 1 << Exp;

    buf->Buf = (char *)realloc(buf->Buf, NewSize);
    assert(buf->Buf);

    buf->BufSize = NewSize;
  }

  buf->Size = size;
}

void oaRPCPushBuf(oaRPCBuf *buf, const void *src, oaRPCSize n)
{
  oaRPCSize End;
  assert(buf);
  assert(src);
  assert(n > 0);

  End = buf->Size;
  ResizeBuf(buf, n + buf->Size);

  memcpy(buf->Buf + End, src, n);
}

void oaRPCSetBufSize(oaRPCBuf *buf, oaRPCSize size)
{
  assert(buf);

  ResizeBuf(buf, size);
  buf->Size = size;
}

void oaRPCClearBuf(oaRPCBuf *buf)
{
  assert(buf);

  buf->Size = 0;
}

char *oaRPCGetBuf(oaRPCBuf *buf, oaRPCSize offset)
{
  assert(buf);
  assert(offset >= 0);
  assert(offset < buf->Size);

  return(buf->Buf + offset);
}

const char *oaRPCGetBufConst(const oaRPCBuf *buf, oaRPCSize offset)
{
  assert(buf);
  assert(offset >= 0);
  assert(offset < buf->Size);

  return(buf->Buf + offset);
}

oaBool oaRPCWriteBufToFile(const char *filename, oaRPCBuf *buf, oaBool append)
{
  FILE *FP;
  const char *OpenStr = (append == OA_TRUE) ? "ab" : "wb";
  oaRPCSize Size, NumWrote;

  assert(filename != NULL);
  assert(buf != NULL);

  if((FP = fopen(filename, OpenStr)) == NULL)
    return(OA_FALSE);

  Size = OARPC_BUF_SIZE(buf);
  NumWrote = (oaRPCSize)fwrite(OARPC_GET_BUF(buf, 0), 1, Size, FP);

  fclose(FP);

  if(NumWrote != Size)
    return(OA_FALSE);

  return(OA_TRUE);
}

/*******************************************************************************
 *** Low-level serialization functions
 ******************************************************************************/

#define SERIALIZE_VALUE_HEADER(buf, type, size) \
  { \
    oaRPCUInt8 Type = type; \
    oaRPCSize Size = size; \
    oaRPCPushBuf(buf, &Type, sizeof(Type)); \
    oaRPCPushBuf(buf, &Size, sizeof(Size)); \
  }

void oaRPCSerializeString(oaRPCBuf *buf, const oaChar *str)
{ 
  oaRPCSize StrLen;
  
  assert(buf);

  if(str == NULL)
  {
    SERIALIZE_VALUE_HEADER(buf, OA_TYPE_STRING, 0);
  }
  else
  {
    StrLen = (oaRPCSize)(OA_STRLEN(str) + 1); /* Include the trailing null */

    SERIALIZE_VALUE_HEADER(buf, OA_TYPE_STRING, StrLen);
    oaRPCPushBuf(buf, str, StrLen);
  }
}

void oaRPCSerializeInt(oaRPCBuf *buf, oaInt val)
{
  SERIALIZE_VALUE_HEADER(buf, OA_TYPE_INT, sizeof(val));
  oaRPCPushBuf(buf, &val, sizeof(val));
}

void oaRPCSerializeFloat(oaRPCBuf *buf, oaFloat val)
{
  SERIALIZE_VALUE_HEADER(buf, OA_TYPE_FLOAT, sizeof(val));
  oaRPCPushBuf(buf, &val, sizeof(val));
}

void oaRPCSerializeBool(oaRPCBuf *buf, oaBool val)
{
  oaRPCUInt8 Val = (oaRPCUInt8)val;
  SERIALIZE_VALUE_HEADER(buf, OA_TYPE_BOOL, sizeof(Val));
  oaRPCPushBuf(buf, &Val, sizeof(Val));
}

void oaRPCSerializeValue(oaRPCBuf *buf, oaOptionDataType type, oaValue value)
{
  oaRPCSerializeInt(buf, (oaInt)type);

  switch(type)
  {
    case OA_TYPE_INVALID:
      break;

    case OA_TYPE_STRING:
      oaRPCSerializeString(buf, value.String);
      break;

    case OA_TYPE_INT:
      oaRPCSerializeInt(buf, value.Int);
      break;

    case OA_TYPE_FLOAT:
      oaRPCSerializeFloat(buf, value.Float);
      break;

    case OA_TYPE_ENUM:
      oaRPCSerializeString(buf, value.Enum);
      break;

    case OA_TYPE_BOOL:
      oaRPCSerializeBool(buf, value.Bool);
      break;

    default:
      assert("Unkown type!" == NULL);
  }
}

#define DESERIALIZE_VALUE_HEADER(buf, type, size, offset) \
  { \
    (type) = *(oaRPCUInt8 *)OARPC_GET_BUF_CONST(buf, *offset); \
    *offset += sizeof(oaRPCUInt8); \
    (size) = *(oaRPCSize *)OARPC_GET_BUF_CONST(buf, *offset); \
    *offset += sizeof(oaRPCSize); \
  }

const oaChar *oaRPCDeserializeString(const oaRPCBuf *buf, oaRPCSize *offset)
{
  oaRPCUInt8 Type;
  oaRPCSize Size;
  const oaChar *Ret;

  DESERIALIZE_VALUE_HEADER(buf, Type, Size, offset);
  assert(Type == OA_TYPE_STRING);

  if(Size == 0)
    return(NULL);

  Ret = (const oaChar *)OARPC_GET_BUF_CONST(buf, *offset);
  *offset += Size;

  return(Ret);
}

oaInt oaRPCDeserializeInt(const oaRPCBuf *buf, oaRPCSize *offset)
{
  oaRPCUInt8 Type;
  oaRPCSize Size;
  oaInt Ret;

  DESERIALIZE_VALUE_HEADER(buf, Type, Size, offset);
  assert(Type == OA_TYPE_INT);
  assert(Size == sizeof(oaInt));

  Ret = *(oaInt *)OARPC_GET_BUF_CONST(buf, *offset);
  *offset += Size;

  return(Ret);
}

oaFloat oaRPCDeserializeFloat(const oaRPCBuf *buf, oaRPCSize *offset)
{
  oaRPCUInt8 Type;
  oaRPCSize Size;
  oaFloat Ret;

  DESERIALIZE_VALUE_HEADER(buf, Type, Size, offset);
  assert(Type == OA_TYPE_FLOAT);
  assert(Size == sizeof(oaFloat));

  Ret = *(oaFloat *)OARPC_GET_BUF_CONST(buf, *offset);
  *offset += Size;

  return(Ret);
}

oaBool oaRPCDeserializeBool(const oaRPCBuf *buf, oaRPCSize *offset)
{
  oaRPCUInt8 Type;
  oaRPCSize Size;
  oaRPCUInt8 Ret;

  DESERIALIZE_VALUE_HEADER(buf, Type, Size, offset);
  assert(Type == OA_TYPE_BOOL);
  assert(Size == sizeof(oaRPCUInt8));

  Ret = *(oaRPCUInt8 *)OARPC_GET_BUF_CONST(buf, *offset);
  *offset += Size;

  return(Ret ? OA_TRUE : OA_FALSE);
}

oaValue oaRPCDeserializeValue(const oaRPCBuf *buf, 
                              oaOptionDataType *type, 
                              oaRPCSize *offset)
{
  oaValue Ret;
  memset(&Ret, 0, sizeof(Ret));

  assert(buf != NULL);
  assert(type != NULL);

  *type = (oaOptionDataType)oaRPCDeserializeInt(buf, offset);

  switch(*type)
  {
    case OA_TYPE_INVALID:
      Ret.Int = 0;
      break;

    case OA_TYPE_STRING:
      Ret.String = (oaString)oaRPCDeserializeString(buf, offset);
      break;

    case OA_TYPE_INT:
      Ret.Int = oaRPCDeserializeInt(buf, offset);
      break;

    case OA_TYPE_FLOAT:
      Ret.Float = oaRPCDeserializeFloat(buf, offset);
      break;

    case OA_TYPE_ENUM:
      Ret.Enum = (oaString)oaRPCDeserializeString(buf, offset);
      break;

    case OA_TYPE_BOOL:
      Ret.Bool = oaRPCDeserializeBool(buf, offset);
      break;

    default:
      assert("Unkown type!" == NULL);
  }

  return(Ret);
}

/*******************************************************************************
 *** RPC Client/Server Init & Cleanup
 ******************************************************************************/

static oaBool Initialized = OA_FALSE;
static oaiFunctionTable DispatchTable;
static oaRPCBuf *GlobBuf = NULL;
static oaRPCBuf *BenchmarkBuf = NULL;
static oaRPCTransport *Transport = NULL;
static void *UserData = NULL;

#define CHECK_BUF(buf, type) \
  if((buf)->Size - Offset < 5 + sizeof(type)) \
    OARPC_ERROR("Message did not have enough bytes.  Expected data of " \
                "type '" #type "'."); \

#define DESERIALIZE_STRING(buf, dst) \
   if((buf)->Size - Offset < 5) \
     OARPC_ERROR("Message did not have enough bytes.  Expected data of " \
                 "type 'string'."); \
   (dst) = oaRPCDeserializeString((buf), &Offset); 

#define DESERIALIZE_INT(buf, dst) \
   CHECK_BUF(buf, oaInt) \
   (dst) = oaRPCDeserializeInt((buf), &Offset); 

#define DESERIALIZE_BOOL(buf, dst) \
   CHECK_BUF(buf, oaRPCUInt8) \
   (dst) = oaRPCDeserializeBool((buf), &Offset); 

#define DESERIALIZE_FLOAT(buf, dst) \
   CHECK_BUF(buf, oaFloat) \
   (dst) = oaRPCDeserializeFloat((buf), &Offset); 

#define DESERIALIZE_VALUE(buf, type, dst) \
   { \
     oaOptionDataType DESERIALIZE_VALUE_Type; \
     CHECK_BUF(buf, oaInt) \
     (dst) = oaRPCDeserializeValue((buf), &DESERIALIZE_VALUE_Type, &Offset);  \
     type = DESERIALIZE_VALUE_Type; \
   }


#define SHIFT_INT_PARAM(buf, param) \
  DESERIALIZE_INT(buf, param)
   
#define SHIFT_FLOAT_PARAM(buf, param) \
  DESERIALIZE_FLOAT(buf, param)
   
#define SHIFT_BOOL_PARAM(buf, param) \
  DESERIALIZE_BOOL(buf, param)
   
#define SHIFT_STRING_PARAM(buf, param) \
  DESERIALIZE_STRING(buf, param)
   
#define SHIFT_ENUM_PARAM(buf, param) \
  DESERIALIZE_STRING(buf, param)
   
#define SHIFT_VALUE_PARAM(buf, type, param) \
  DESERIALIZE_VALUE(buf, type, param)
   

#define PUSH_INT_PARAM(buf, param) \
  { \
    oaInt TmpIntParam = (param); \
    oaRPCSerializeInt((buf), TmpIntParam); \
  }
   
#define PUSH_FLOAT_PARAM(buf, param) \
  oaRPCSerializeFloat(buf, (oaFloat)(param));
   
#define PUSH_BOOL_PARAM(buf, param) \
  oaRPCSerializeBool(buf, (oaBool)(param));
   
#define PUSH_STRING_PARAM(buf, param) \
  oaRPCSerializeString(buf, param);
   
#define PUSH_ENUM_PARAM(buf, param) \
  oaRPCSerializeString(buf, param);
   
#define PUSH_VALUE_PARAM(buf, type, param) \
  oaRPCSerializeValue(buf, type, param);
   

static oaBool Dispatch(oaRPCFuncType func_id, 
                       oaRPCBuf *recv_buf,
                       oaInt offset,
                       oaRPCBuf *send_buf,
                       const oaiFunctionTable *func_table,
                       oaBool *exit_loop);

oaRPCServer *oaRPCCreateServer(const oaiFunctionTable *func_table, 
                               oaRPCTransport *transport, 
                               void *user_data,
                               const char *log_filename)
{
  oaRPCServer *Ret = (oaRPCServer *)malloc(sizeof(oaRPCServer));

  assert(func_table);
  assert(transport);

  memset(Ret, 0, sizeof(oaRPCServer));

  Ret->FuncTable = func_table;
  Ret->Transport = transport;
  Ret->TransportUserData = user_data;
  Ret->LogFilename = strdup(log_filename);

  Ret->CurDepth = 0;

  return(Ret);
}

static oaBool ServerNegotiate(const oaRPCBuf *request, oaRPCBuf *response)
{
  unsigned char Endian;
  unsigned long ClientRPCVersionMajor, ClientRPCVersionMinor;
  unsigned long AppOAVersionMajor, AppOAVersionMinor, AppOABuild, AppOACustom;
  unsigned long ClientOAVersionMajor, ClientOAVersionMinor, 
                ClientOABuild, ClientOACustom;
  oaRPCSize Offset;
  const unsigned int EndianTest = 1; 
  int SystemEndian = (*(char *)&EndianTest == 1);

  assert(sizeof(unsigned long) == 4);

  if(OARPC_BUF_SIZE(request) < 57)
  {
    ERROR_MSG("oaRPC negotiation failed.  Request size is too small.");
    return(OA_FALSE);
  }

  assert(sizeof(Magic) == 16); 

  if(memcmp((const void *)OARPC_GET_BUF_CONST(request, 0), 
            (const void *)Magic, 
            sizeof(Magic)) != 0)
  {
    ERROR_MSG("oaRPC negotiation failed.  Wrong magic.");
    return(OA_FALSE);
  }

  Endian = *(const unsigned char *)OARPC_GET_BUF_CONST(request, 16);

  if(Endian != SystemEndian)
  {
    ERROR_MSG("System endian doesn't match client's endian.");
    return(OA_FALSE);
  }


  Offset = 17;
  SHIFT_INT_PARAM(request, ClientRPCVersionMajor)
  SHIFT_INT_PARAM(request, ClientRPCVersionMinor)
  SHIFT_INT_PARAM(request, AppOAVersionMajor)
  SHIFT_INT_PARAM(request, AppOAVersionMinor)
  SHIFT_INT_PARAM(request, AppOACustom)
  SHIFT_INT_PARAM(request, AppOABuild)
  SHIFT_INT_PARAM(request, ClientOAVersionMajor)
  SHIFT_INT_PARAM(request, ClientOAVersionMinor)
  SHIFT_INT_PARAM(request, ClientOACustom)
  SHIFT_INT_PARAM(request, ClientOABuild)

  oaRPCClearBuf(response);

  if(ClientRPCVersionMajor != RPCVersionMajor)
  {
    PUSH_INT_PARAM(response, OARPC_ERROR_NEGOTIATION_FAILED)
    PUSH_STRING_PARAM(response, 
      "Negotiation failed. RPC major versions don't match.")
    return(OA_FALSE);
  }

  PUSH_INT_PARAM(response, OARPC_ERROR_NONE)
  PUSH_STRING_PARAM(response, "")
  PUSH_INT_PARAM(response, OA_VERSION_MAJOR)
  PUSH_INT_PARAM(response, OA_VERSION_MINOR)
  PUSH_INT_PARAM(response, OA_VERSION_CUSTOM)
  PUSH_INT_PARAM(response, OA_VERSION_BUILD)
  
  return(OA_TRUE);
}

static oaBool Recv(oaRPCTransport *Transport, void *user_data, oaRPCBuf *buf)
{
  oaBool NewConnection;
  oaBool Ret;

  Ret = Transport->Recv(user_data, buf, &NewConnection);

  /* Negotiate the oaRPC and OA version if it's a new connection */
  if(NewConnection == OA_TRUE)
  {
    oaRPCBuf *Response = oaRPCAllocBuf();
    if(ServerNegotiate(buf, Response))
    {
      if(!Transport->Send(user_data, Response))
        assert("transport error case not implemented yet!" == NULL);

      Ret = Transport->Recv(user_data, buf, &NewConnection);
    }
    else
      Ret = OA_FALSE;

    oaRPCFreeBuf(Response);
  }

  return(Ret);
}

oaBool oaRPCRunServer(oaRPCServer *server)
{
  oaRPCBuf *RecvBuf = oaRPCAllocBuf();
  oaRPCBuf *SendBuf = oaRPCAllocBuf();
  const oaRPCBuf *PreResponseBuf = NULL;
  oaRPCSize Offset = 0; 
  oaBool ExitLoop = OA_FALSE;
  const oaiFunctionTable *FuncTable;
  oaRPCTransport *Transport;
  void *UserData;
  const char *LogFilename;

  assert(server);
  assert(server->CurDepth < OARPC_MAX_CALL_DEPTH);

  FuncTable = server->FuncTable;
  Transport = server->Transport;
  UserData = server->TransportUserData;
  LogFilename = server->LogFilename;

  if(!Initialized)
    CommonInit();


  PreResponseBuf = server->CallStack[server->CurDepth].PreResponseBuf;

  /* if PreResponseBuf was defined, send the response before starting
     the req/response loop.  This is for the case where 
     oaRPCServerNestCommand() was called. */
  if(PreResponseBuf != NULL)
  {
    DUMP_RESPONSE(PreResponseBuf, LogFilename)

    if(!Transport->Send(UserData, PreResponseBuf))
      assert("transport error case not implemented yet!" == NULL);

    server->CallStack[server->CurDepth].PreResponseBuf = NULL;
  }

  while(Recv(Transport, UserData, RecvBuf))
  {
    oaInt FuncId;
    CHECK_BUF(RecvBuf, oaInt)

    DUMP_REQUEST(RecvBuf, LogFilename)
      
    DESERIALIZE_INT(RecvBuf, FuncId)

    /* If we were called from oaRPCServerNestCommand(), we must return control
       when a request for GetNextCommand() is made. */
    if(PreResponseBuf && FuncId == OARPC_FUNC_GET_NEXT_COMMAND)
      return(OA_TRUE);

    server->CallStack[server->CurDepth].Func = FuncId;
    server->CurDepth++;

    if(Dispatch((oaRPCFuncType)FuncId, 
                RecvBuf, 
                Offset, 
                SendBuf, 
                FuncTable, 
                &ExitLoop))
    {
      DUMP_RESPONSE(SendBuf, LogFilename)

      if(!Transport->Send(UserData, SendBuf))
      {
        assert("transport error case not implemented yet!" == NULL);
      }
    }
    else
    {
      assert("error case not implemented yet!" == NULL);
    }

    server->CurDepth--;

    if(ExitLoop)
    {
      ExitLoop = OA_FALSE;
      break;
    }
   
    oaRPCClearBuf(RecvBuf);
    oaRPCClearBuf(SendBuf);
    Offset = 0;
  }


  Transport->Cleanup(UserData);
  oaRPCFreeBuf(RecvBuf);
  oaRPCFreeBuf(SendBuf);

  return(OA_TRUE);
}

oaBool oaRPCServerNestCommand(oaRPCServer *server, const oaCommand *command)
{
  oaBool Ret;
  oaRPCBuf *SendBuf = oaRPCAllocBuf();

  assert(command != NULL);
  assert(server != NULL);
  assert(server->CurDepth > 0);
  assert(server->CallStack[server->CurDepth-1].Func == 
         OARPC_FUNC_GET_NEXT_COMMAND);

  SerializeGetNextCommandResponse(command, SendBuf);
  server->CallStack[server->CurDepth].PreResponseBuf = SendBuf;

  Ret = oaRPCRunServer(server);

  oaRPCFreeBuf(SendBuf);

  return(Ret);
}

oaBool oaRPCServerSendExitCommand(oaRPCServer *server,
                                  oaBool (*post_exit_func)(void *),
                                  void *data)
{
  const oaiFunctionTable *FuncTable;
  oaRPCTransport *Transport;
  void *UserData;
  const char *LogFilename;
  oaRPCBuf *SendBuf = oaRPCAllocBuf();
  oaRPCBuf *RecvBuf = oaRPCAllocBuf();
  oaCommand Command;
  oaRPCSize Offset = 0; 

  assert(server != NULL);
  assert(server->CurDepth > 0);
  assert(server->CallStack[server->CurDepth-1].Func == 
         OARPC_FUNC_GET_NEXT_COMMAND);


  FuncTable = server->FuncTable;
  Transport = server->Transport;
  UserData = server->TransportUserData;
  LogFilename = server->LogFilename;

  if(!Initialized)
    CommonInit();

  oaInitCommand(&Command);
  Command.Type = OA_CMD_EXIT;
  SerializeGetNextCommandResponse(&Command, SendBuf);

  DUMP_RESPONSE(SendBuf, LogFilename)

  if(!Transport->Send(UserData, SendBuf))
    assert("transport error case not implemented yet!" == NULL);

  if(post_exit_func != NULL)
    if(post_exit_func(data) != OA_TRUE)
      return(OA_FALSE);

  if(Recv(Transport, UserData, RecvBuf))
  {
    oaInt FuncId;
    CHECK_BUF(RecvBuf, oaInt)

    DUMP_REQUEST(RecvBuf, LogFilename)
      
    DESERIALIZE_INT(RecvBuf, FuncId)

    if(FuncId == OARPC_FUNC_GET_NEXT_COMMAND)
      return(OA_TRUE);
    else
      return(OA_FALSE);
  }
  else
  {
    assert("transport error case not implemented yet!" == NULL);
  }

  oaRPCFreeBuf(SendBuf);
  oaRPCFreeBuf(RecvBuf);

  return(OA_FALSE);
}

oaBool oaRPCDestroyServer(oaRPCServer *server)
{
  assert(server);
  free(server);

  return(OA_TRUE);
}

static oaCommandType oaRPC_GetNextCommand(oaCommand *command);
static oaNamedOption *oaRPC_GetNextOption(void);
static void oaRPC_AddOption(const oaNamedOption *option);
static void oaRPC_AddOptionValue(const oaChar *name, 
                                 oaOptionDataType value_type,
                                 const oaValue *value);
static void oaRPC_AddBenchmark(const oaChar *benchmark_name);
static oaBool oaRPC_SendSignal(oaSignalType signal, void *param);
static void oaRPC_StartBenchmark(void);
static void oaRPC_EndBenchmark(void);
static void oaRPC_DisplayFrame(oaFloat t);
static void oaRPC_AddResultValue(const oaChar *name, 
                                 oaOptionDataType value_type,
                                 const oaValue *value);
static void oaRPC_AddFrameValue(const oaChar *name, 
                                oaOptionDataType value_type,
                                const oaValue *value);

static oaBool ClientNegotiate(oaRPCTransport *transport, 
                              void *user_data,
                              const oaVersion *app_oa_version,
                              oaVersion *server_oa_version)
{
  const unsigned int EndianTest = 1; 
  int SystemEndian = (*(char *)&EndianTest == 1);
  oaRPCBuf *Request = oaRPCAllocBuf();
  oaRPCBuf *Response = oaRPCAllocBuf();
  oaBool Ret = OA_TRUE;
  oaBool NewConnection;
  oaInt ErrorCode; 
  const oaChar *ErrorStr; 
  oaRPCSize Offset = 0;

  oaRPCPushBuf(Request, Magic, sizeof(Magic));
  oaRPCPushBuf(Request, &EndianTest, 1);

  PUSH_INT_PARAM(Request, RPCVersionMajor)
  PUSH_INT_PARAM(Request, RPCVersionMinor)

  assert(app_oa_version != NULL);
  PUSH_INT_PARAM(Request, app_oa_version->Major)
  PUSH_INT_PARAM(Request, app_oa_version->Minor)
  PUSH_INT_PARAM(Request, app_oa_version->Custom)
  PUSH_INT_PARAM(Request, app_oa_version->Build)

  PUSH_INT_PARAM(Request, OA_VERSION_MAJOR)
  PUSH_INT_PARAM(Request, OA_VERSION_MINOR)
  PUSH_INT_PARAM(Request, OA_VERSION_CUSTOM)
  PUSH_INT_PARAM(Request, OA_VERSION_BUILD)

  if(transport->Send(UserData, Request) != OA_TRUE ||
     transport->Recv(UserData, Response, &NewConnection) != OA_TRUE)
  {
    ERROR_MSG("RPC negotiation failed.");
    Ret = OA_FALSE;
  }
    
  DESERIALIZE_INT(Response, ErrorCode)
  DESERIALIZE_STRING(Response, ErrorStr)

  if(ErrorCode == OARPC_ERROR_NONE)
  {
    oaVersion ServerOAVersion;
    DESERIALIZE_INT(Response, ServerOAVersion.Major)
    DESERIALIZE_INT(Response, ServerOAVersion.Minor)
    DESERIALIZE_INT(Response, ServerOAVersion.Custom)
    DESERIALIZE_INT(Response, ServerOAVersion.Build)
  }
  else
  {
    ERROR_MSG(ErrorStr);
    Ret = OA_FALSE;
  }

  oaRPCFreeBuf(Request);
  oaRPCFreeBuf(Response);

  return(Ret);
}

const oaiFunctionTable *oaRPCInitClient(oaRPCTransport *transport, 
                                        void *user_data,
                                        const oaVersion *app_oa_version)
{
  oaVersion ServerOAVersion;

  assert(!Initialized);
  assert(transport);

  CommonInit();

  Transport = transport;
  UserData = user_data;

  ClientExited = OA_FALSE;

  oaiInitFuncTable(&DispatchTable);
  DispatchTable.GetNextCommand = oaRPC_GetNextCommand;
  DispatchTable.GetNextOption = oaRPC_GetNextOption;
  DispatchTable.AddOption = oaRPC_AddOption;
  DispatchTable.AddOptionValue = oaRPC_AddOptionValue;
  DispatchTable.AddBenchmark = oaRPC_AddBenchmark;
  DispatchTable.SendSignal = oaRPC_SendSignal;
  DispatchTable.StartBenchmark = oaRPC_StartBenchmark;
  DispatchTable.EndBenchmark = oaRPC_EndBenchmark;
  DispatchTable.DisplayFrame = oaRPC_DisplayFrame;
  DispatchTable.AddResultValue = oaRPC_AddResultValue;
  DispatchTable.AddFrameValue = oaRPC_AddFrameValue;

  if(ClientNegotiate(Transport, 
                     UserData, 
                     app_oa_version,
                     &ServerOAVersion) != OA_TRUE)
    return(NULL);

  return(&DispatchTable);
};

oaBool oaRPCCleanup(void)
{
  assert(Initialized);

  CommonCleanup();
  Initialized = OA_FALSE;

  return(OA_TRUE);
}

void CommonInit(void)
{
  assert(Initialized == OA_FALSE);

  GlobBuf = oaRPCAllocBuf();
  BenchmarkBuf = oaRPCAllocBuf();

  Initialized = OA_TRUE;
}

static void CommonCleanup(void)
{
  assert(Initialized == OA_TRUE);

  if(GlobBuf != NULL)
  {
    oaRPCFreeBuf(GlobBuf);
    GlobBuf = NULL;
  }

  if(BenchmarkBuf != NULL)
  {
    oaRPCFreeBuf(BenchmarkBuf);
    BenchmarkBuf = NULL;
  }

  Initialized = OA_FALSE;
}

/*******************************************************************************
 *** RPC Debug functions
 ******************************************************************************/

#define PUSH_BUF_STR(buf, str) oaRPCPushBuf(buf, str, (oaRPCSize)strlen(str))

void PrintParams(const oaRPCBuf *buf, oaRPCBuf *ret, oaInt *offset)
{
  oaRPCSize Offset = *offset;
  oaRPCSize BufSize = OARPC_BUF_SIZE(buf);
  oaInt ParamCount = 0;

  while(BufSize - Offset >= 5)
  {
    oaRPCSize TmpOffset = Offset;
    oaRPCUInt8 Type;
    oaInt Size;
    char Msg[4096];

    DESERIALIZE_VALUE_HEADER(buf, Type, Size, &TmpOffset)

    switch((oaOptionDataType)Type)
    {
      case OA_TYPE_STRING :
        {
          const char *Str;
          DESERIALIZE_STRING(buf, Str)
          sprintf(Msg, "Param %d (string[%d]): \"%s\"\n", 
                  ParamCount, Size, Str);
        }
        break;

      case OA_TYPE_INT :
        {
          oaInt Int;
          DESERIALIZE_INT(buf, Int)
          sprintf(Msg, "Param %d (int[%d]): %d\n", 
                  ParamCount, Size, Int);
        }
        break;

      case OA_TYPE_FLOAT :
        {
          oaFloat Float;
          DESERIALIZE_FLOAT(buf, Float)
          sprintf(Msg, "Param %d (float[%d]): %f\n", 
                  ParamCount, Size, Float);
        }
        break;

      case OA_TYPE_BOOL :
        {
          oaFloat Bool;
          DESERIALIZE_BOOL(buf, Bool)
          sprintf(Msg, "Param %d (bool[%d]): %s\n", 
                  ParamCount, Size, (Bool == OA_TRUE) ? "TRUE" : "FALSE" );
        }
        break;

      default:
        assert("Unknown type!" == NULL);
    }

    PUSH_BUF_STR(ret, Msg);
    ParamCount++;
  }

  *offset = Offset;
}

static const char *FuncIdToStr(oaInt func_id)
{
  switch(func_id)
  {
#define OARPC_DECLARE_FUNC(func_name, func_id) \
    case func_id : \
      return(#func_name);

#include "oaRPC_Functions.h"
  };

  return("UNKNOWN FUNC");
}

void oaRPCPrintRequest(const oaRPCBuf *request, oaRPCBuf *ret)
{
  oaRPCSize Offset = 0;
  char Msg[4096];
  oaRPCSize ReqSize;
  oaInt FuncId;
  oaInt ParamCount = 0;


  assert(request != NULL);
  assert(ret != NULL);

  ReqSize = OARPC_BUF_SIZE(request);

  
  if(ReqSize < 9)
  {
    PUSH_BUF_STR(ret, "ERROR: Not enough bytes for function id.\n");
    return;
  }

  sprintf(Msg, "-- REQUEST\nRequest size: %d\n", ReqSize);
  PUSH_BUF_STR(ret, Msg);

  SHIFT_INT_PARAM(request, FuncId)
  sprintf(Msg, "Function id: %d\n", FuncId);
  PUSH_BUF_STR(ret, Msg);

  sprintf(Msg, "Function name: %s\n", FuncIdToStr(FuncId));
  PUSH_BUF_STR(ret, Msg);

  PrintParams(request, ret, &Offset);


  if(ReqSize != Offset)
  {
    sprintf(Msg, "ERROR: There are an extra %d bytes at the tail of the "
                 "request.\n", (ReqSize - Offset));
    PUSH_BUF_STR(ret, Msg);
  }
}

void oaRPCPrintResponse(const oaRPCBuf *response, oaRPCBuf *ret)
{
  oaRPCSize Offset = 0;
  char Msg[4096];
  oaRPCSize RespSize;
  oaInt ErrorCode;
  const oaChar *ErrorStr;
  oaInt ParamCount = 0;


  assert(response != NULL);
  assert(ret != NULL);

  RespSize = OARPC_BUF_SIZE(response);

  
  if(RespSize < 14)
  {
    PUSH_BUF_STR(ret, "ERROR: Not enough bytes for error code and string.\n");
    return;
  }

  sprintf(Msg, "-- RESPONSE\nResponse size: %d\n", RespSize);
  PUSH_BUF_STR(ret, Msg);

  SHIFT_INT_PARAM(response, ErrorCode)
  sprintf(Msg, "Error code: %d\n", ErrorCode);
  PUSH_BUF_STR(ret, Msg);

  SHIFT_STRING_PARAM(response, ErrorStr)
  sprintf(Msg, "Error string: \"%s\"\n", ErrorStr);
  PUSH_BUF_STR(ret, Msg);

  PrintParams(response, ret, &Offset);

  if(RespSize != Offset)
  {
    sprintf(Msg, "ERROR: There are an extra %d bytes at the tail of the "
                 "response.\n", (RespSize - Offset));
    PUSH_BUF_STR(ret, Msg);
  }
}

/*******************************************************************************
 *** RPC Server-side Functions
 ******************************************************************************/

#define SERIALIZE_RESPONSE_HEADER(error_code, error_str) \
  assert(send_buf); \
  oaRPCSerializeInt(send_buf, error_code); \
  oaRPCSerializeString(send_buf, error_str);

void SerializeGetNextCommandResponse(const oaCommand *command, 
                                     oaRPCBuf *send_buf)
{
  assert(command);
  assert(send_buf);

  SERIALIZE_RESPONSE_HEADER(OARPC_ERROR_NONE, "")

  oaRPCSerializeInt(send_buf, command->Type);

  if(command->Type == OA_CMD_RUN_BENCHMARK)
    PUSH_STRING_PARAM(send_buf, command->BenchmarkName)
  else
    PUSH_STRING_PARAM(send_buf, NULL)
}

static oaBool Dispatch_GetNextCommand(oaRPCBuf *recv_buf,
                                      oaInt offset,
                                      oaRPCBuf *send_buf,
                                      const oaiFunctionTable *func_table,
                                      oaBool *exit_loop)
{
  oaInt Offset = offset;
  oaCommand Command;
  oaCommandType Ret;

  assert(func_table);
  assert(func_table->GetNextCommand);

  oaInitCommand(&Command);
  Ret = func_table->GetNextCommand(&Command);

  Command.Type = Ret;

  SerializeGetNextCommandResponse(&Command, send_buf);

  if(Ret == OA_CMD_EXIT)
    *exit_loop = OA_TRUE;

  return(OA_TRUE);
}

static oaBool Dispatch_GetNextOption(oaRPCBuf *recv_buf,
                                     oaInt offset,
                                     oaRPCBuf *send_buf,
                                     const oaiFunctionTable *func_table,
                                     oaBool *exit_loop)
{
  oaInt Offset = offset;

  assert(func_table);

  SERIALIZE_RESPONSE_HEADER(OARPC_ERROR_NONE, "")

  if(func_table->GetNextOption)
  {
    oaNamedOption *Option = func_table->GetNextOption();

    if(Option == NULL)
      PUSH_INT_PARAM(send_buf, 0)
    else
    {
      PUSH_INT_PARAM(send_buf, Option->DataType)
      PUSH_STRING_PARAM(send_buf, Option->Name)
      PUSH_VALUE_PARAM(send_buf, Option->DataType, Option->Value)
    }
  }
  else
  {
    PUSH_INT_PARAM(send_buf, 0)
  }

  return(OA_TRUE);
}

static oaBool Dispatch_AddOption(oaRPCBuf *recv_buf,
                                 oaInt offset,
                                 oaRPCBuf *send_buf,
                                 const oaiFunctionTable *func_table,
                                 oaBool *exit_loop)
{
  oaInt Offset = offset;

  assert(func_table);

  SERIALIZE_RESPONSE_HEADER(OARPC_ERROR_NONE, "")

  if(func_table->AddOption)
  {
    oaNamedOption Option;
    oaOptionDataType TmpType;

    oaInitOption(&Option);
    SHIFT_INT_PARAM(recv_buf, Option.DataType)
    SHIFT_STRING_PARAM(recv_buf, Option.Name)
    SHIFT_VALUE_PARAM(recv_buf, TmpType, Option.Value)
    assert(TmpType == Option.DataType);
    SHIFT_VALUE_PARAM(recv_buf, TmpType, Option.MinValue)
    assert(TmpType == Option.DataType);
    SHIFT_VALUE_PARAM(recv_buf, TmpType, Option.MaxValue)
    assert(TmpType == Option.DataType);
    SHIFT_INT_PARAM(recv_buf, Option.NumSteps)
    SHIFT_STRING_PARAM(recv_buf, Option.Dependency.ParentName)
    SHIFT_INT_PARAM(recv_buf, Option.Dependency.ComparisonOp);
    SHIFT_INT_PARAM(recv_buf, Option.Dependency.ComparisonValType);
    SHIFT_VALUE_PARAM(recv_buf, TmpType, Option.Dependency.ComparisonVal)
    assert(TmpType == Option.Dependency.ComparisonValType);

    func_table->AddOption(&Option);
  }

  return(OA_TRUE);
}

static oaBool Dispatch_AddOptionValue(oaRPCBuf *recv_buf,
                                      oaInt offset,
                                      oaRPCBuf *send_buf,
                                      const oaiFunctionTable *func_table,
                                      oaBool *exit_loop)
{
  oaInt Offset = offset;

  SERIALIZE_RESPONSE_HEADER(OARPC_ERROR_NONE, "")

  assert(func_table);

  if(func_table->AddOptionValue)
  {
    const oaChar *Name;
    oaInt Type;
    oaValue Value;
    oaOptionDataType TmpType;
    
    SHIFT_STRING_PARAM(recv_buf, Name)
    SHIFT_INT_PARAM(recv_buf, Type)
    SHIFT_VALUE_PARAM(recv_buf, TmpType, Value)
    assert(Type == TmpType);

    func_table->AddOptionValue(Name, Type, &Value);
  }

  return(OA_TRUE);
}

static oaBool Dispatch_AddBenchmark(oaRPCBuf *recv_buf,
                                    oaInt offset,
                                    oaRPCBuf *send_buf,
                                    const oaiFunctionTable *func_table,
                                    oaBool *exit_loop)
{
  oaInt Offset = offset;

  SERIALIZE_RESPONSE_HEADER(OARPC_ERROR_NONE, "")

  assert(func_table);

  if(func_table->AddOptionValue)
  {
    const oaChar *Benchmark;
    
    SHIFT_STRING_PARAM(recv_buf, Benchmark)

    func_table->AddBenchmark(Benchmark);
  }

  return(OA_TRUE);
}

static oaBool Dispatch_SendSignal(oaRPCBuf *recv_buf,
                                  oaInt offset,
                                  oaRPCBuf *send_buf,
                                  const oaiFunctionTable *func_table,
                                  oaBool *exit_loop)
{
  oaInt Offset = offset;
  oaBool Ret = OA_FALSE;

  SERIALIZE_RESPONSE_HEADER(OARPC_ERROR_NONE, "")

  assert(func_table);

  if(func_table->SendSignal)
  {
    oaInt Signal;
    void *Param = NULL;
    oaMessage Message;
    
    SHIFT_INT_PARAM(recv_buf, Signal)

    switch(Signal)
    {
      case OA_SIGNAL_ERROR:
        SHIFT_INT_PARAM(recv_buf, Message.Error)
        SHIFT_STRING_PARAM(recv_buf, Message.Message)
        Param = &Message;
        break;

      case OA_SIGNAL_SYSTEM_REBOOT:
        break;
      
      default:
        assert("Unknown signal!" == NULL);
    };

    Ret = func_table->SendSignal((oaSignalType)Signal, Param);
  }

  PUSH_BOOL_PARAM(send_buf, Ret);
  return(OA_TRUE);
}

static oaBool Dispatch_StartBenchmark(oaRPCBuf *recv_buf,
                                      oaInt offset,
                                      oaRPCBuf *send_buf,
                                      const oaiFunctionTable *func_table,
                                      oaBool *exit_loop)
{
  oaInt Offset = offset;

  SERIALIZE_RESPONSE_HEADER(OARPC_ERROR_NONE, "")

  assert(func_table);

  if(func_table->StartBenchmark)
    func_table->StartBenchmark();

  return(OA_TRUE);
}

static oaBool Dispatch_EndBenchmark(oaRPCBuf *recv_buf,
                                    oaInt offset,
                                    oaRPCBuf *send_buf,
                                    const oaiFunctionTable *func_table,
                                    oaBool *exit_loop)
{
  oaInt Offset = offset;

  SERIALIZE_RESPONSE_HEADER(OARPC_ERROR_NONE, "")

  assert(func_table);

  if(func_table->EndBenchmark)
    func_table->EndBenchmark();

  return(OA_TRUE);
}


static oaBool Dispatch_DisplayFrame(oaRPCBuf *recv_buf,
                                    oaInt offset,
                                    oaRPCBuf *send_buf,
                                    const oaiFunctionTable *func_table,
                                    oaBool *exit_loop)
{
  oaInt Offset = offset;
  oaFloat T;

  SERIALIZE_RESPONSE_HEADER(OARPC_ERROR_NONE, "")

  assert(func_table);

  if(func_table->DisplayFrame)
  {
    SHIFT_FLOAT_PARAM(recv_buf, T)
    func_table->DisplayFrame(T);
  }

  return(OA_TRUE);
}

static oaBool Dispatch_AddResultValue(oaRPCBuf *recv_buf,
                                      oaInt offset,
                                      oaRPCBuf *send_buf,
                                      const oaiFunctionTable *func_table,
                                      oaBool *exit_loop)
{
  oaInt Offset = offset;

  SERIALIZE_RESPONSE_HEADER(OARPC_ERROR_NONE, "")

  assert(func_table);

  if(func_table->AddResultValue)
  {
    const oaChar *Name;
    oaInt ValueType;
    oaValue Value;

    SHIFT_STRING_PARAM(recv_buf, Name)
    SHIFT_INT_PARAM(recv_buf, ValueType)
    SHIFT_VALUE_PARAM(recv_buf, ValueType, Value)

    func_table->AddResultValue(Name, (oaOptionDataType)ValueType, &Value);
  }

  return(OA_TRUE);
}

static oaBool Dispatch_AddFrameValue(oaRPCBuf *recv_buf,
                                     oaInt offset,
                                     oaRPCBuf *send_buf,
                                     const oaiFunctionTable *func_table,
                                     oaBool *exit_loop)
{
  oaInt Offset = offset;

  SERIALIZE_RESPONSE_HEADER(OARPC_ERROR_NONE, "")

  assert(func_table);

  if(func_table->AddFrameValue)
  {
    const oaChar *Name;
    oaInt ValueType;
    oaValue Value;

    SHIFT_STRING_PARAM(recv_buf, Name)
    SHIFT_INT_PARAM(recv_buf, ValueType)
    SHIFT_VALUE_PARAM(recv_buf, ValueType, Value)

    func_table->AddFrameValue(Name, (oaOptionDataType)ValueType, &Value);
  }

  return(OA_TRUE);
}

oaBool Dispatch(oaRPCFuncType func_id, 
                oaRPCBuf *recv_buf,
                oaInt offset,
                oaRPCBuf *send_buf,
                const oaiFunctionTable *func_table,
                oaBool *exit_loop)
{
  switch(func_id)
  {
    case OARPC_FUNC_INVALID:
      OARPC_ERROR("Invalid function id.")
      break;
      
#define DISPATCH_CASE(command_enum, command_name) \
    case OARPC_FUNC_##command_enum: \
      return(Dispatch_##command_name (recv_buf, offset, send_buf, func_table, \
                                      exit_loop));

    DISPATCH_CASE(GET_NEXT_COMMAND, GetNextCommand)
    DISPATCH_CASE(GET_NEXT_OPTION, GetNextOption)
    DISPATCH_CASE(ADD_OPTION, AddOption)
    DISPATCH_CASE(ADD_OPTION_VALUE, AddOptionValue)
    DISPATCH_CASE(ADD_BENCHMARK, AddBenchmark)
    DISPATCH_CASE(SEND_SIGNAL, SendSignal)
    DISPATCH_CASE(START_BENCHMARK, StartBenchmark)
    DISPATCH_CASE(END_BENCHMARK, EndBenchmark)
    DISPATCH_CASE(DISPLAY_FRAME, DisplayFrame)
    DISPATCH_CASE(ADD_RESULT_VALUE, AddResultValue)
    DISPATCH_CASE(ADD_FRAME_VALUE, AddFrameValue)

    default:
      OARPC_ERROR("Unknown function id.")
  }

  return(OA_TRUE);
}

/*******************************************************************************
 *** RPC Client-side Functions
 ******************************************************************************/

#define SERIALIZE_FUNC_HEADER(func_id) \
  oaInt ErrorCode; \
  const oaChar *ErrorStr; \
  oaRPCSize Offset = 0; \
  { \
    assert(Initialized); \
    assert(ClientExited != OA_TRUE); \
    assert(GlobBuf); \
    oaRPCClearBuf(GlobBuf); \
    oaRPCSerializeInt(GlobBuf, OARPC_FUNC_##func_id); \
  }

#define SEND_REQUEST \
  if(!Transport->Send(UserData, GlobBuf)) \
    OARPC_ERROR("Send request failed."); 

#define CHECK_GLOB_BUF(type) \
  CHECK_BUF(GlobBuf, type)

#define DESERIALIZE_GLOB_STRING(dst) \
  DESERIALIZE_STRING(GlobBuf, dst) 

#define DESERIALIZE_GLOB_INT(dst) \
  DESERIALIZE_INT(GlobBuf, dst) 

#define DESERIALIZE_GLOB_FLOAT(dst) \
  DESERIALIZE_FLOAT(GlobBuf, dst) 

#define DESERIALIZE_GLOB_BOOL(dst) \
  DESERIALIZE_BOOL(GlobBuf, dst) 

#define DESERIALIZE_GLOB_VALUE(type, dst) \
  DESERIALIZE_VALUE(GlobBuf, type, dst) 

#define RECV_RESPONSE \
  oaRPCClearBuf(GlobBuf); \
  if(!Recv(Transport, UserData, GlobBuf)) \
    OARPC_ERROR("Recv response failed."); \
  DESERIALIZE_GLOB_INT(ErrorCode) \
  DESERIALIZE_GLOB_STRING(ErrorStr) 

  

oaCommandType oaRPC_GetNextCommand(oaCommand *command)
{
  SERIALIZE_FUNC_HEADER(GET_NEXT_COMMAND)

  SEND_REQUEST

  RECV_RESPONSE

  {
    oaInt CommandType;
    const oaChar *BenchmarkName;

    DESERIALIZE_GLOB_INT(CommandType)
    DESERIALIZE_GLOB_STRING(BenchmarkName)

    memset(command, 0, sizeof(oaCommand));
    if(command)
    {
      command->Type = (oaCommandType)CommandType;
      if(BenchmarkName != NULL)
      {
        oaRPCClearBuf(BenchmarkBuf);
        oaRPCPushBuf(BenchmarkBuf, BenchmarkName, (oaRPCSize)strlen(BenchmarkName) + 1);
        command->BenchmarkName = OARPC_GET_BUF(BenchmarkBuf, 0);
      }
    }

    /* Cleanup the low-level transport since it shouldn't be used after the exit
       command has been sent to the client. */
    if(CommandType == OA_CMD_EXIT)
    {
      ClientExited = OA_TRUE;

      if(Transport->Cleanup)
        Transport->Cleanup(UserData);
    }


    return((oaCommandType)CommandType);
  }
}

static oaNamedOption *oaRPC_GetNextOption(void)
{
  SERIALIZE_FUNC_HEADER(GET_NEXT_OPTION)

  SEND_REQUEST

  RECV_RESPONSE

  {
    static oaNamedOption Option;
    oaInt Type;

    oaInitOption(&Option);

    DESERIALIZE_GLOB_INT(Type)

    if(Type == 0)
      return(NULL);

    Option.DataType = (oaOptionDataType)Type;

    DESERIALIZE_GLOB_STRING(Option.Name)
    DESERIALIZE_GLOB_VALUE(Type, Option.Value)

    return(&Option);
  }
}

static void oaRPC_AddOption(const oaNamedOption *option)
{
  SERIALIZE_FUNC_HEADER(ADD_OPTION)

  assert(option != NULL);

  PUSH_INT_PARAM(GlobBuf, option->DataType)
  PUSH_STRING_PARAM(GlobBuf, option->Name)
  PUSH_VALUE_PARAM(GlobBuf, option->DataType, option->Value)
  PUSH_VALUE_PARAM(GlobBuf, option->DataType, option->MinValue)
  PUSH_VALUE_PARAM(GlobBuf, option->DataType, option->MaxValue)
  PUSH_INT_PARAM(GlobBuf, option->NumSteps)
  PUSH_STRING_PARAM(GlobBuf, option->Dependency.ParentName)
  PUSH_INT_PARAM(GlobBuf, option->Dependency.ComparisonOp);
  PUSH_INT_PARAM(GlobBuf, option->Dependency.ComparisonValType);
  PUSH_VALUE_PARAM(GlobBuf, option->Dependency.ComparisonValType, 
                   option->Dependency.ComparisonVal)

  /* todo:rev:need to serialize option->Dependency.ComparisonVal, but can't
              unless we keep around state for mapping option name to data
              type, since we need to know the parent options type in order
              to serialize this value */


  SEND_REQUEST

  RECV_RESPONSE
}

void oaRPC_AddOptionValue(const oaChar *name, 
                          oaOptionDataType value_type,
                          const oaValue *value)
{
  SERIALIZE_FUNC_HEADER(ADD_OPTION_VALUE)

  assert(name != NULL);
  assert(value != NULL);

  PUSH_STRING_PARAM(GlobBuf, name)
  PUSH_INT_PARAM(GlobBuf, (oaInt)value_type)
  PUSH_VALUE_PARAM(GlobBuf, value_type, *value)

  SEND_REQUEST

  RECV_RESPONSE
}

void oaRPC_AddBenchmark(const oaChar *benchmark_name)
{
  SERIALIZE_FUNC_HEADER(ADD_BENCHMARK)

  assert(benchmark_name != NULL);

  PUSH_STRING_PARAM(GlobBuf, benchmark_name)

  SEND_REQUEST

  RECV_RESPONSE
}

oaBool oaRPC_SendSignal(oaSignalType signal, void *param)
{
  oaBool Ret;

  SERIALIZE_FUNC_HEADER(SEND_SIGNAL)

  PUSH_INT_PARAM(GlobBuf, signal)

  switch(signal)
  {
    case OA_SIGNAL_ERROR:
      {
        const oaMessage *Message = (oaMessage *)param;
        assert(Message != NULL);
        PUSH_INT_PARAM(GlobBuf, Message->Error)
        PUSH_STRING_PARAM(GlobBuf, Message->Message)
      }

      break;

    case OA_SIGNAL_SYSTEM_REBOOT:
      assert(param == NULL);
      break;
    
    default:
      assert("Unknown signal!" == NULL);
  };

  SEND_REQUEST

  RECV_RESPONSE

  DESERIALIZE_GLOB_BOOL(Ret)

  return(Ret);
}

void oaRPC_StartBenchmark(void)
{
  SERIALIZE_FUNC_HEADER(START_BENCHMARK)

  SEND_REQUEST

  RECV_RESPONSE
}

void oaRPC_EndBenchmark(void)
{
  SERIALIZE_FUNC_HEADER(END_BENCHMARK)

  SEND_REQUEST

  RECV_RESPONSE
}

void oaRPC_DisplayFrame(oaFloat t)
{
  SERIALIZE_FUNC_HEADER(DISPLAY_FRAME)

  PUSH_FLOAT_PARAM(GlobBuf, t)

  SEND_REQUEST

  RECV_RESPONSE
}

void oaRPC_AddResultValue(const oaChar *name, 
                          oaOptionDataType value_type, 
                          const oaValue *value)
{
  SERIALIZE_FUNC_HEADER(ADD_RESULT_VALUE)

  PUSH_STRING_PARAM(GlobBuf, name)
  PUSH_INT_PARAM(GlobBuf, value_type)
  PUSH_VALUE_PARAM(GlobBuf, value_type, *value)

  SEND_REQUEST

  RECV_RESPONSE
}

void oaRPC_AddFrameValue(const oaChar *name, 
                         oaOptionDataType value_type, 
                         const oaValue *value)
{
  SERIALIZE_FUNC_HEADER(ADD_FRAME_VALUE)

  PUSH_STRING_PARAM(GlobBuf, name)
  PUSH_INT_PARAM(GlobBuf, value_type)
  PUSH_VALUE_PARAM(GlobBuf, value_type, *value)

  SEND_REQUEST

  RECV_RESPONSE
}

/*******************************************************************************
 *** Misc
 ******************************************************************************/

void oaRPC_Error(const char *filename, int line, const char *msg)
{
  FILE *FP = fopen("c:/tmp/debug.txt", "a");

  fprintf(stderr, "ERROR %s,%d: %s\n", filename, line, msg);
  if(FP)
  {
    fprintf(FP, "ERROR %s,%d: %s\n", filename, line, msg);
    fclose(FP);
  }

  exit(1);
}

