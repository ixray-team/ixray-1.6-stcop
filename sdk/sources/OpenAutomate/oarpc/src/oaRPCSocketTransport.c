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


#include <oaRPCSocketTransport.h>

#if defined(WIN32) || PLATFORM == OA_WIN32 || PLATFORM == OA_CYGWIN

#include <winsock2.h>
#include <ws2tcpip.h>
#include <stdlib.h>
#include <stdio.h>

#define PRINT_INT(var) \
   fprintf(stderr, "%s, %d: " #var " = %d\n", __FILE__, __LINE__, (var)); \
   fflush(stderr);

#define PRINT_STR(var) \
   fprintf(stderr, "%s, %d: " #var " = '%s'\n", __FILE__, __LINE__, (var)); \
   fflush(stderr);

typedef struct SocketDataStruct
{
  SOCKET Socket;
  SOCKET ListenSocket;
  oaBool Listening;
  oaRPCBuf *TmpBuf;
} SocketData;

static int InitCount = 0;

static SocketData *SocketInit(void)
{
  SocketData *Socket;
  int Result;
  WSADATA WSAData;
  static oaBool Initialized = OA_FALSE;

  if(InitCount == 0)
  {
    Result = WSAStartup(MAKEWORD(2,2), &WSAData);
    if(Result != 0)
    {
      OARPC_ERROR("WSAStartup failed");
      return(NULL);
    }
  }

  InitCount++;

  Socket = (SocketData *)malloc(sizeof(SocketData));
  memset(Socket, 0, sizeof(SocketData));

  Socket->Socket = INVALID_SOCKET;
  Socket->ListenSocket = INVALID_SOCKET;

  Socket->TmpBuf = oaRPCAllocBuf();

  return(Socket);
}

static oaBool SocketCleanup(SocketData *socket)
{
  SocketData *Socket = (SocketData *)socket;

  assert(Socket != NULL);

  if(Socket->Socket != INVALID_SOCKET)
    closesocket(Socket->Socket);

  Socket->Socket = INVALID_SOCKET;
  return(OA_TRUE);
}
    
static void FullSocketCleanup(SocketData *socket)
{
  SocketData *Socket = (SocketData *)socket;

  assert(Socket != NULL);

  SocketCleanup(socket);

  if(Socket->ListenSocket != INVALID_SOCKET)
    closesocket(Socket->ListenSocket);

  Socket->ListenSocket = INVALID_SOCKET;

  oaRPCFreeBuf(Socket->TmpBuf);
  free(Socket);

  assert(InitCount > 0);
  if(--InitCount == 0)
    WSACleanup();
}
    
#else

#error "Only Windows is currently supported."

#endif


static oaBool SocketSend(void *user_data, const oaRPCBuf *buf)
{
  oaRPCSize Sent, TotalSent, ToSend, BufSize;
  SocketData *Socket = (SocketData *)user_data;

  assert(Socket != NULL);
  assert(buf != NULL);

  if(!Socket->Socket)
    return(OA_FALSE);

  TotalSent = 0;
  BufSize = OARPC_BUF_SIZE(buf);


  oaRPCClearBuf(Socket->TmpBuf);
  oaRPCPushBuf(Socket->TmpBuf, &BufSize, sizeof(BufSize));
  oaRPCPushBuf(Socket->TmpBuf, 
               OARPC_GET_BUF_CONST(buf, 0), 
               OARPC_BUF_SIZE(buf));

  ToSend = OARPC_BUF_SIZE(Socket->TmpBuf);

  while(ToSend > 0)
  {
    if((Sent = send(Socket->Socket, 
                    OARPC_GET_BUF(Socket->TmpBuf, TotalSent), 
                    ToSend, 
                    0)) < 0) 
      return(OA_FALSE);

    TotalSent += Sent;
    ToSend -= Sent;
  }
  
  return(OA_TRUE);
}

static oaBool AcceptConnection(SocketData *socket)
{
  SOCKET ClientSocket; 
  int Ret;

  assert(socket != NULL);
  assert(socket->ListenSocket != INVALID_SOCKET);
  assert(socket->Socket == INVALID_SOCKET);

  if(socket->Listening != OA_TRUE)
  {
    Ret = listen(socket->ListenSocket, SOMAXCONN);
    if(Ret == SOCKET_ERROR) 
    {
      OARPC_ERROR("listen failed");
      goto failed;
    }
  }

  ClientSocket = accept(socket->ListenSocket, NULL, NULL);
  if(ClientSocket == INVALID_SOCKET)
  {
    OARPC_ERROR("accept failed");
    goto failed;
  }

  socket->Socket = ClientSocket;
  socket->Listening = OA_FALSE;

  return(OA_TRUE);

  failed:
    
    if(socket->ListenSocket != INVALID_SOCKET)
    {
      closesocket(socket->ListenSocket);
      socket->ListenSocket = INVALID_SOCKET;
    }

    if(socket->Socket != INVALID_SOCKET)
    {
      closesocket(socket->Socket);
      socket->Socket = INVALID_SOCKET;
    }

    return(OA_FALSE);
}

static oaBool SocketRecv(void *user_data, oaRPCBuf *buf, oaBool *new_connection)
{
  oaRPCSize BufSize;
  oaRPCSize NRead;
  oaRPCSize TotalRead, TotalLeft;
  SocketData *Socket = (SocketData *)user_data;

  *new_connection = OA_FALSE;

  if(Socket->Socket == INVALID_SOCKET)
  {
    if(Socket->ListenSocket == INVALID_SOCKET)
      return(OA_FALSE);

    if(!AcceptConnection(Socket))
      return(OA_FALSE);

    *new_connection = OA_TRUE;
  }

  assert(sizeof(BufSize) == 4);
  NRead = recv(Socket->Socket, (char *)&BufSize, sizeof(BufSize), 0);

  if(NRead == 0 && Socket->ListenSocket != INVALID_SOCKET)
  {
    /* Connection was closed, so accept a new one and continue as normal */
    closesocket(Socket->Socket);

	Socket->Socket = INVALID_SOCKET;
    if(!AcceptConnection(Socket))
      return(OA_FALSE);

    *new_connection = OA_TRUE;
    NRead = recv(Socket->Socket, (char *)&BufSize, sizeof(BufSize), 0);
  }

  if(NRead != 4)
    return(OA_FALSE);

  if(BufSize < 1)
    return(OA_FALSE);

  oaRPCSetBufSize(buf, BufSize);

  TotalLeft = BufSize;
  TotalRead = 0;
  while(TotalLeft > 0)
  {
    NRead = recv(Socket->Socket, 
                 OARPC_GET_BUF(buf, TotalRead), 
                 TotalLeft, 
                 0);

    if(NRead < 1)
      return(OA_FALSE);

    TotalRead += NRead;
    TotalLeft -= NRead;
  }

  return(OA_TRUE);
}

oaBool oaRPCInitSocketServerTransport(oaRPCTransport *transport,
                                      void **user_data, 
                                      int port)
{
  SocketData *Socket;
  struct addrinfo *Result = NULL, Hints;
  char PortStr[256];
  int Ret;

  assert(transport != NULL);
  assert(user_data != NULL);
  assert(port > 0);

  memset(transport, 0, sizeof(oaRPCTransport));
  *user_data = NULL;

  if((Socket = SocketInit()) == NULL)
  {
    OARPC_ERROR("Could not initialize socket library.") 
    return(OA_FALSE);
  }

  memset(&Hints, 0, sizeof(Hints));
  Hints.ai_family = AF_INET;
  Hints.ai_socktype = SOCK_STREAM;
  Hints.ai_protocol = IPPROTO_TCP;
  Hints.ai_flags = AI_PASSIVE;

  sprintf(PortStr, "%d", port);

  if((Ret = getaddrinfo(NULL, PortStr, &Hints, &Result)) != 0) 
  {
    OARPC_ERROR("getaddrinfo() failed.")
    goto failed;
  }

  Socket->ListenSocket = socket(Result->ai_family, 
                                Result->ai_socktype, 
                                Result->ai_protocol);

  if(Socket->ListenSocket == INVALID_SOCKET) 
  {
    OARPC_ERROR("Could not open listen socket.");
    goto failed;
  }

  Ret = bind(Socket->ListenSocket, Result->ai_addr, (int)Result->ai_addrlen);
  if(Ret == SOCKET_ERROR) 
  {
     OARPC_ERROR("bind failed");
     closesocket(Socket->ListenSocket);
     Socket->ListenSocket = INVALID_SOCKET;
     goto failed;
  }

  freeaddrinfo(Result);
  Result = NULL;

  Ret = listen(Socket->ListenSocket, SOMAXCONN);
  if(Ret == SOCKET_ERROR) 
  {
    OARPC_ERROR("listen failed");
    goto failed;
  }

  Socket->Listening = OA_TRUE;

  transport->Send = SocketSend;
  transport->Recv = SocketRecv;
  transport->Cleanup = SocketCleanup;
  *user_data = (void *)Socket;


  return(OA_TRUE);

  failed:
    if(Result)
      freeaddrinfo(Result);
    SocketCleanup(Socket);
    return(OA_FALSE);
}

oaBool oaRPCInitSocketClientTransport(oaRPCTransport *transport,
                                      void **user_data, 
                                      const char *hostname,
                                      int port)
{
  SocketData *Socket;
  struct addrinfo *Result = NULL, *Ptr, Hints;
  int Ret;
  char PortStr[256];

  assert(transport != NULL);
  assert(user_data != NULL);
  assert(port > 0);
  assert(hostname != NULL);

  memset(transport, 0, sizeof(oaRPCTransport));
  *user_data = NULL;

  if((Socket = SocketInit()) == NULL)
  {
    OARPC_ERROR("Could not initialize socket library.") 
    return(OA_FALSE);
  }

  memset(&Hints, 0, sizeof(Hints));
  Hints.ai_family = AF_UNSPEC;
  Hints.ai_socktype = SOCK_STREAM;
  Hints.ai_protocol = IPPROTO_TCP;

  sprintf(PortStr, "%d", port);

  Ret = getaddrinfo(hostname, PortStr, &Hints, &Result);
  if(Ret != 0) 
  {
    OARPC_ERROR("getaddrinfo failed");
    goto failed;
  }

  // Attempt to connect to an address until one succeeds
  for(Ptr=Result; Ptr != NULL ; Ptr=Ptr->ai_next) 
  {
    SOCKET ConnectSocket = socket(Ptr->ai_family, 
                                  Ptr->ai_socktype, 
                                  Ptr->ai_protocol);
    if(ConnectSocket == INVALID_SOCKET) 
    {
       OARPC_ERROR("Error at socket()");
       goto failed;
    }

    Ret = connect(ConnectSocket, Ptr->ai_addr, (int)Ptr->ai_addrlen);
    if(Ret == SOCKET_ERROR) 
    {
      closesocket(ConnectSocket);
      continue;
    }

    Socket->Socket = ConnectSocket;
    break;
  }

  if(Socket->Socket == INVALID_SOCKET)
    goto failed;

  transport->Send = SocketSend;
  transport->Recv = SocketRecv;
  transport->Cleanup = SocketCleanup;
  *user_data = (void *)Socket;

  freeaddrinfo(Result);

  return(OA_TRUE);

  failed:
    if(Result)
      freeaddrinfo(Result);
    SocketCleanup(Socket);
    return(OA_FALSE);
}

void oaRPCCleanupSocket(void *user_data)
{
  assert(user_data != NULL);

  FullSocketCleanup((SocketData *)user_data);
}


