/* 
 * Copyright (c) 2018 NVIDIA CORPORATION. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *  * Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *  * Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *  * Neither the name of NVIDIA CORPORATION nor the names of its
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#pragma once

#include <optix_prime\optix_prime.h>
#include <cuda_runtime.h>
#include <cstdlib>
#include <iostream>


//------------------------------------------------------------------------------
#define CHK_CUDA( code )                                                       \
{                                                                              \
  cudaError_t err__ = code;                                                    \
  if( err__ != cudaSuccess )                                                   \
  {                                                                            \
    std::cerr << "Error at " __FILE__ << "(" << __LINE__ << "): "              \
              << cudaGetErrorString( err__ ) << std::endl;                     \
    exit(1);                                                                   \
  }                                                                            \
}

//------------------------------------------------------------------------------
#define CHK_PRIME( code )                                                      \
{                                                                              \
  RTPresult res__ = code;                                                      \
  if( res__ != RTP_SUCCESS )                                                   \
  {                                                                            \
  const char* err_string;                                                      \
  rtpContextGetLastErrorString( context, &err_string );                        \
  std::cerr << "Error at <<" __FILE__ << "(" << __LINE__ << "): "              \
            << err_string                                                      \
  << "' (" << res__ << ")" << std::endl;                                       \
  exit(1);                                                                     \
  }                                                                            \
}

