/*
 * NVIDIA_COPYRIGHT_BEGIN
 *
 * Copyright (c) 2022-2023, NVIDIA CORPORATION.  All rights reserved.
 *
 * NVIDIA CORPORATION and its licensors retain all intellectual property
 * and proprietary rights in and to this software, related documentation
 * and any modifications thereto.  Any use, reproduction, disclosure or
 * distribution of this software and related documentation without an express
 * license agreement from NVIDIA CORPORATION is strictly prohibited.
 *
 * NVIDIA_COPYRIGHT_END
 */

#ifndef nvJitLink_INCLUDED
#define nvJitLink_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdlib.h>

/**
 *
 * \defgroup error Error codes
 *
 */

/** \ingroup error
 *
 * \brief    The enumerated type nvJitLinkResult defines API call result codes.
 *           nvJitLink APIs return nvJitLinkResult codes to indicate the result.
 */

typedef enum {
  NVJITLINK_SUCCESS = 0,
  NVJITLINK_ERROR_UNRECOGNIZED_OPTION,
  NVJITLINK_ERROR_MISSING_ARCH, // -arch=sm_NN option not specified
  NVJITLINK_ERROR_INVALID_INPUT,
  NVJITLINK_ERROR_PTX_COMPILE,
  NVJITLINK_ERROR_NVVM_COMPILE,
  NVJITLINK_ERROR_INTERNAL
} nvJitLinkResult;
  
/**
 *
 * \defgroup linking Linking
 *
 */

/** \ingroup linking
 *
 * \brief    The enumerated type nvJitLinkInputType defines the kind of inputs
 *           that can be passed to nvJitLinkAdd* APIs.
 */

typedef enum {
  NVJITLINK_INPUT_NONE = 0, // error
  NVJITLINK_INPUT_CUBIN = 1,
  NVJITLINK_INPUT_PTX,
  NVJITLINK_INPUT_LTOIR,
  NVJITLINK_INPUT_FATBIN,
  NVJITLINK_INPUT_OBJECT,
  NVJITLINK_INPUT_LIBRARY
} nvJitLinkInputType;

/**
 * \defgroup options Supported Link Options
 *
 * nvJitLink supports the link options below.
 * Option names are prefixed with a single dash (\c -).
 * Options that take a value have an assignment operator (\c =)
 * followed by the option value, with no spaces, e.g. \c "-arch=sm_90".
 *
 * The supported options are:
 * - \c -arch=sm_<N\> \n
 *   Pass SM architecture value.  See nvcc for valid values of <N\>.
 *   Can use compute_<N\> value instead if only generating PTX.
 *   This is a required option.  
 * - \c -maxrregcount=<N\> \n
 *   Maximum register count.
 * - \c -time \n
 *   Print timing information to InfoLog.
 * - \c -verbose \n
 *   Print verbose messages to InfoLog.
 * - \c -lto \n
 *   Do link time optimization.
 * - \c -ptx \n
 *   Emit ptx after linking instead of cubin; only supported with \c -lto
 * - \c -O<N\> \n
 *   Optimization level.
 * - \c -g \n
 *   Generate debug information.
 * - \c -lineinfo \n
 *   Generate line information.
 * - \c -ftz=<n\> \n
 *   Flush to zero.
 * - \c -prec-div=<n\> \n
 *   Precise divide.
 * - \c -prec-sqrt=<n\> \n
 *   Precise square root.
 * - \c -fma=<n\> \n
 *   Fast multiply add.
 * - \c -kernels-used=<name\> \n
 *   Pass list of kernels that are used; any not in the list can be removed.
 *   This option can be specified multiple times.
 * - \c -variables-used=<name\> \n
 *   Pass list of variables that are used; any not in the list can be removed.
 *   This option can be specified multiple times.
 * - \c -optimize-unused-variables \n
 *   Normally device code optimization is limited by not knowing what the
 *   host code references.  With this option it can assume that if a variable
 *   is not referenced in device code then it can be removed.
 * - \c -Xptxas=<opt\> \n
 *   Pass <opt\> to ptxas.  This option can be called multiple times.
 * - \c -Xnvvm=<opt\> \n
 *   Pass <opt\> to nvvm.  This option can be called multiple times.
 */

/**
 * \ingroup linking
 * \brief   nvJitLinkHandle is the unit of linking, and an opaque handle for
 *          a program.
 *
 * To link inputs, an instance of nvJitLinkHandle must be created first with
 * nvJitLinkCreate().
 */

typedef struct nvJitLink* nvJitLinkHandle; // opaque handle

// For versioning we will have separate API version for each library version

extern nvJitLinkResult __nvJitLinkCreate_12_2(
  nvJitLinkHandle *handle,
  uint32_t numOptions,
  const char **options);
/**
 * \ingroup linking
 * \brief   nvJitLinkCreate creates an instance of nvJitLinkHandle with the
 *          given input options, and sets the output parameter \p handle.
 *
 * \param   [out] handle       Address of nvJitLink handle.
 * \param   [in]  numOptions   Number of options passed.
 * \param   [in]  options      Array of size \p numOptions of option strings.
 * \return
 *   - \link #nvJitLinkResult NVJITLINK_SUCCESS \endlink
 *   - \link #nvJitLinkResult NVJITLINK_ERROR_UNRECOGNIZED_OPTION\endlink
 *   - \link #nvJitLinkResult NVJITLINK_ERROR_MISSING_ARCH\endlink
 *   - \link #nvJitLinkResult NVJITLINK_ERROR_INVALID_INPUT\endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INTERNAL\endlink
 *
 * It supports options listed in \ref options.
 *
 * \see nvJitLinkDestroy
 */
static inline nvJitLinkResult nvJitLinkCreate(
  nvJitLinkHandle *handle,
  uint32_t numOptions,
  const char **options)
{
  return __nvJitLinkCreate_12_2 (handle, numOptions, options);
}
 
extern nvJitLinkResult __nvJitLinkDestroy_12_2 (nvJitLinkHandle *handle);
/**
 * \ingroup linking
 * \brief   nvJitLinkDestroy frees the memory associated with the given handle
 *          and sets it to NULL.
 *
 * \param    [in] handle      Address of nvJitLink handle.
 * \return
 *   - \link #nvJitLinkResult NVJITLINK_SUCCESS \endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INVALID_INPUT\endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INTERNAL\endlink
 *
 * \see nvJitLinkCreate
 */
static inline nvJitLinkResult nvJitLinkDestroy (nvJitLinkHandle *handle)
{
  return __nvJitLinkDestroy_12_2 (handle);
}
 
extern nvJitLinkResult __nvJitLinkAddData_12_2(
  nvJitLinkHandle handle,
  nvJitLinkInputType inputType,
  const void *data,
  size_t size,
  const char *name); // name can be null
/**
 * \ingroup linking
 * \brief   nvJitLinkAddData adds data image to the link. 
 *
 * \param    [in] handle      nvJitLink handle.
 * \param    [in] inputType   kind of input.
 * \param    [in] data        pointer to data image in memory.
 * \param    [in] name        name of input object.
 * \return
 *   - \link #nvJitLinkResult NVJITLINK_SUCCESS \endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INVALID_INPUT\endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INTERNAL\endlink
 */
static inline nvJitLinkResult nvJitLinkAddData(
  nvJitLinkHandle handle,
  nvJitLinkInputType inputType,
  const void *data,
  size_t size,
  const char *name) // name can be null
{
  return __nvJitLinkAddData_12_2 (handle, inputType, data, size, name);
}
 
extern nvJitLinkResult __nvJitLinkAddFile_12_2(
  nvJitLinkHandle handle,
  nvJitLinkInputType inputType,
  const char *fileName); // includes path to file
/**
 * \ingroup linking
 * \brief   nvJitLinkAddFile reads data from file and links it in. 
 *
 * \param    [in] handle      nvJitLink handle.
 * \param    [in] inputType   kind of input.
 * \param    [in] fileName    name of file.
 * \return
 *   - \link #nvJitLinkResult NVJITLINK_SUCCESS \endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INVALID_INPUT\endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INTERNAL\endlink
 */
static inline nvJitLinkResult nvJitLinkAddFile(
  nvJitLinkHandle handle,
  nvJitLinkInputType inputType,
  const char *fileName) // includes path to file
{
  return __nvJitLinkAddFile_12_2 (handle, inputType, fileName);
}
 
extern nvJitLinkResult __nvJitLinkComplete_12_2 (nvJitLinkHandle handle);
/**
 * \ingroup linking
 * \brief   nvJitLinkComplete does the actual link. 
 *
 * \param    [in] handle      nvJitLink handle.
 * \return
 *   - \link #nvJitLinkResult NVJITLINK_SUCCESS \endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INVALID_INPUT\endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INTERNAL\endlink
 */
static inline nvJitLinkResult nvJitLinkComplete (nvJitLinkHandle handle)
{
  return __nvJitLinkComplete_12_2 (handle);
}
 
extern nvJitLinkResult __nvJitLinkGetLinkedCubinSize_12_2(
  nvJitLinkHandle handle,
  size_t *size);
/**
 * \ingroup linking
 * \brief   nvJitLinkGetLinkedCubinSize gets the size of the linked cubin.
 *
 * \param    [in] handle      nvJitLink handle.
 * \param    [out] size       Size of the linked cubin.
 * \return
 *   - \link #nvJitLinkResult NVJITLINK_SUCCESS \endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INVALID_INPUT\endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INTERNAL\endlink
 *
 * \see nvJitLinkGetLinkedCubin
 */
static inline nvJitLinkResult nvJitLinkGetLinkedCubinSize(
  nvJitLinkHandle handle,
  size_t *size)
{
  return __nvJitLinkGetLinkedCubinSize_12_2 (handle, size);
}

extern nvJitLinkResult __nvJitLinkGetLinkedCubin_12_2(
  nvJitLinkHandle handle,
  void *cubin);
/**
 * \ingroup linking
 * \brief   nvJitLinkGetLinkedCubin gets the linked cubin.
 *
 * \param    [in] handle      nvJitLink handle.
 * \param    [out] cubin      The linked cubin.
 * \return
 *   - \link #nvJitLinkResult NVJITLINK_SUCCESS \endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INVALID_INPUT\endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INTERNAL\endlink
 *
 * User is responsible for allocating enough space to hold the \p cubin.
 * \see nvJitLinkGetLinkedCubinSize
 */
static inline nvJitLinkResult nvJitLinkGetLinkedCubin(
  nvJitLinkHandle handle,
  void *cubin)
{
  return __nvJitLinkGetLinkedCubin_12_2 (handle, cubin);
}
 
extern nvJitLinkResult __nvJitLinkGetLinkedPtxSize_12_2(
  nvJitLinkHandle handle,
  size_t *size);
/**
 * \ingroup linking
 * \brief   nvJitLinkGetLinkedPtxSize gets the size of the linked ptx.
 *
 * \param    [in] handle      nvJitLink handle.
 * \param    [out] size       Size of the linked PTX.
 * \return
 *   - \link #nvJitLinkResult NVJITLINK_SUCCESS \endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INVALID_INPUT\endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INTERNAL\endlink
 *
 * Linked PTX is only available when using the \c -lto option.
 * \see nvJitLinkGetLinkedPtx
 */
static inline nvJitLinkResult nvJitLinkGetLinkedPtxSize(
  nvJitLinkHandle handle,
  size_t *size)
{
  return __nvJitLinkGetLinkedPtxSize_12_2 (handle, size);
}

extern nvJitLinkResult __nvJitLinkGetLinkedPtx_12_2(
  nvJitLinkHandle handle,
  char *ptx);
/**
 * \ingroup linking
 * \brief   nvJitLinkGetLinkedPtx gets the linked ptx.
 *
 * \param    [in] handle      nvJitLink handle.
 * \param    [out] ptx        The linked PTX.
 * \return
 *   - \link #nvJitLinkResult NVJITLINK_SUCCESS \endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INVALID_INPUT\endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INTERNAL\endlink
 *
 * Linked PTX is only available when using the \c -lto option.
 * User is responsible for allocating enough space to hold the \p ptx.
 * \see nvJitLinkGetLinkedPtxSize
 */
static inline nvJitLinkResult nvJitLinkGetLinkedPtx(
  nvJitLinkHandle handle,
  char *ptx)
{
  return __nvJitLinkGetLinkedPtx_12_2 (handle, ptx);
}
 
extern nvJitLinkResult __nvJitLinkGetErrorLogSize_12_2(
  nvJitLinkHandle handle,
  size_t *size);
/**
 * \ingroup linking
 * \brief   nvJitLinkGetErrorLogSize gets the size of the error log.
 *
 * \param    [in] handle      nvJitLink handle.
 * \param    [out] size       Size of the error log.
 * \return
 *   - \link #nvJitLinkResult NVJITLINK_SUCCESS \endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INVALID_INPUT\endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INTERNAL\endlink
 *
 * \see nvJitLinkGetErrorLog
 */
static inline nvJitLinkResult nvJitLinkGetErrorLogSize(
  nvJitLinkHandle handle,
  size_t *size)
{
  return __nvJitLinkGetErrorLogSize_12_2 (handle, size);
}

extern nvJitLinkResult __nvJitLinkGetErrorLog_12_2(
  nvJitLinkHandle handle,
  char *log);
/**
 * \ingroup linking
 * \brief   nvJitLinkGetErrorLog puts any error messages in the log.
 *
 * \param    [in] handle      nvJitLink handle.
 * \param    [out] log        The error log.
 * \return
 *   - \link #nvJitLinkResult NVJITLINK_SUCCESS \endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INVALID_INPUT\endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INTERNAL\endlink
 *
 * User is responsible for allocating enough space to hold the \p log.
 * \see nvJitLinkGetErrorLogSize
 */
static inline nvJitLinkResult nvJitLinkGetErrorLog(
  nvJitLinkHandle handle,
  char *log)
{
  return __nvJitLinkGetErrorLog_12_2 (handle, log);
}

extern nvJitLinkResult __nvJitLinkGetInfoLogSize_12_2(
  nvJitLinkHandle handle,
  size_t *size);
/**
 * \ingroup linking
 * \brief   nvJitLinkGetInfoLogSize gets the size of the info log.
 *
 * \param    [in] handle      nvJitLink handle.
 * \param    [out] size       Size of the info log.
 * \return
 *   - \link #nvJitLinkResult NVJITLINK_SUCCESS \endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INVALID_INPUT\endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INTERNAL\endlink
 *
 * \see nvJitLinkGetInfoLog
 */
static inline nvJitLinkResult nvJitLinkGetInfoLogSize(
  nvJitLinkHandle handle,
  size_t *size)
{
  return __nvJitLinkGetInfoLogSize_12_2 (handle, size);
}

extern nvJitLinkResult __nvJitLinkGetInfoLog_12_2(
  nvJitLinkHandle handle,
  char *log);
/**
 * \ingroup linking
 * \brief   nvJitLinkGetInfoLog puts any info messages in the log.
 *
 * \param    [in] handle      nvJitLink handle.
 * \param    [out] log        The info log.
 * \return
 *   - \link #nvJitLinkResult NVJITLINK_SUCCESS \endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INVALID_INPUT\endlink
 *   - \link #nvJitLinkResult NVRTC_ERROR_INTERNAL\endlink
 *
 * User is responsible for allocating enough space to hold the \p log.
 * \see nvJitLinkGetInfoLogSize
 */
static inline nvJitLinkResult nvJitLinkGetInfoLog(
  nvJitLinkHandle handle,
  char *log)
{
  return __nvJitLinkGetInfoLog_12_2 (handle, log);
}

#ifdef __cplusplus
}
#endif

#endif // nvJitLink_INCLUDED

