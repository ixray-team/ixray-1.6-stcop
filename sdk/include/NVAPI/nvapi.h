 /***************************************************************************\
|*                                                                           *|
|*      Copyright 2005-2006 NVIDIA Corporation.  All rights reserved.        *|
|*                                                                           *|     
|*   NOTICE TO USER:                                                         *|                 
|*                                                                           *|
|*   This source code is subject to NVIDIA ownership rights under U.S.       *|
|*   and international Copyright laws.  Users and possessors of this         *| 
|*   source code are hereby granted a nonexclusive, royalty-free             *|
|*   license to use this code in individual and commercial software.         *|
|*                                                                           *|
|*   NVIDIA MAKES NO REPRESENTATION ABOUT THE SUITABILITY OF THIS SOURCE     *|
|*   CODE FOR ANY PURPOSE. IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR         *|
|*   IMPLIED WARRANTY OF ANY KIND. NVIDIA DISCLAIMS ALL WARRANTIES WITH      *|
|*   REGARD TO THIS SOURCE CODE, INCLUDING ALL IMPLIED WARRANTIES OF         *|
|*   MERCHANTABILITY, NONINFRINGEMENT, AND FITNESS FOR A PARTICULAR          *|
|*   PURPOSE. IN NO EVENT SHALL NVIDIA BE LIABLE FOR ANY SPECIAL,            *|
|*   INDIRECT, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, OR ANY DAMAGES          *|
|*   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN      *|
|*   AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING     *|
|*   OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOURCE      *| 
|*   CODE.                                                                   *|
|*                                                                           *|
|*   U.S. Government End Users. This source code is a "commercial item"      *|
|*   as that term is defined at 48 C.F.R. 2.101 (OCT 1995), consisting       *|
|*   of "commercial computer  software" and "commercial computer software    *|
|*   documentation" as such terms are used in 48 C.F.R. 12.212 (SEPT 1995)   *|
|*   and is provided to the U.S. Government only as a commercial end item.   *|
|*   Consistent with 48 C.F.R.12.212 and 48 C.F.R. 227.7202-1 through        *|
|*   227.7202-4 (JUNE 1995), all U.S. Government End Users acquire the       *|
|*   source code with only those rights set forth herein.                    *|
|*                                                                           *|
|*   Any use of this source code in individual and commercial software must  *| 
|*   include, in the user documentation and internal comments to the code,   *| 
|*   the above Disclaimer and U.S. Government End Users Notice.              *|
|*                                                                           *|
|*                                                                           *|
 \***************************************************************************/
///////////////////////////////////////////////////////////////////////////////
//
//$Date$
// File: nvapi.h
//
// NvAPI provides an interface to NVIDIA devices. This file contains the 
// interface constants, structure definitions and function prototypes.
//
// Target Profile: developer
// Target OS-Arch: winxp_x86
//
///////////////////////////////////////////////////////////////////////////////
#ifndef _NVAPI_H
#define _NVAPI_H

#pragma pack(push,8) // Make sure we have consistent structure packings

#ifdef __cplusplus
extern "C" {
#endif

// ====================================================
// Universal NvAPI Definitions
// ====================================================
#ifdef _WIN32
#define NVAPI_INTERFACE extern NvAPI_Status __cdecl 
#else
#define NVAPI_INTERFACE extern NvAPI_Status __cdecl 
#endif

typedef unsigned __int64 NvU64;
typedef unsigned long    NvU32;
typedef long             NvS32;
typedef unsigned char    NvU8;

#define DECLARE_HANDLE(name) struct name##__ { int unused; }; typedef struct name##__ *name

// NVAPI Handles - These handles are retrieved from various calls and passed in to others in NvAPI
//                 These are meant to be opaque types.  Do not assume they correspond to indices, HDCs,
//                 display indexes or anything else.
//
//                 Most handles remain valid until a display re-configuration (display mode set) or GPU
//                 reconfiguration (going into or out of SLI modes) occurs.  If NVAPI_HANDLE_INVALIDATED
//                 is received by an app, it should discard all handles, and re-enumerate them.
//
DECLARE_HANDLE(NvDisplayHandle);            // Display Device driven by NVidia GPU(s) (an attached display)
DECLARE_HANDLE(NvUnAttachedDisplayHandle);  // Unattached Display Device driven by NVidia GPU(s)
DECLARE_HANDLE(NvLogicalGpuHandle);         // One or more physical GPUs acting in concert (SLI)
DECLARE_HANDLE(NvPhysicalGpuHandle);        // A single physical GPU

#define NVAPI_DEFAULT_HANDLE        0

#define NVAPI_GENERIC_STRING_MAX    4096
#define NVAPI_LONG_STRING_MAX       256
#define NVAPI_SHORT_STRING_MAX      64

typedef struct 
{
    NvS32   sX;
    NvS32   sY;
    NvS32   sWidth;
    NvS32   sHeight;
} NvSBox;

#define NVAPI_MAX_PHYSICAL_GPUS             64
#define NVAPI_MAX_LOGICAL_GPUS              64
#define NVAPI_MAX_AVAILABLE_GPU_TOPOLOGIES  256
#define NVAPI_MAX_GPU_TOPOLOGIES            NVAPI_MAX_PHYSICAL_GPUS
#define NVAPI_MAX_GPU_PER_TOPOLOGY          8
#define NVAPI_MAX_DISPLAY_HEADS             2
#define NVAPI_MAX_DISPLAYS                  NVAPI_MAX_PHYSICAL_GPUS * NVAPI_MAX_DISPLAY_HEADS

typedef char NvAPI_String[NVAPI_GENERIC_STRING_MAX];
typedef char NvAPI_LongString[NVAPI_LONG_STRING_MAX];
typedef char NvAPI_ShortString[NVAPI_SHORT_STRING_MAX];

// =========================================================================================
// NvAPI Version Definition
// Maintain per structure specific version define using the MAKE_NVAPI_VERSION macro.
// Usage: #define NV_GENLOCK_STATUS_VER  MAKE_NVAPI_VERSION(NV_GENLOCK_STATUS, 1)
// =========================================================================================
#define MAKE_NVAPI_VERSION(typeName,ver) (NvU32)(sizeof(typeName) | ((ver)<<16))
#define GET_NVAPI_VERSION(ver) (NvU32)((ver)>>16)
#define GET_NVAPI_SIZE(ver) (NvU32)((ver) & 0xffff)

// ====================================================
// NvAPI Status Values
//    All NvAPI functions return one of these codes.
// ====================================================
typedef enum 
{
    NVAPI_OK                                    =  0,      // Success
    NVAPI_ERROR                                 = -1,      // Generic error
    NVAPI_LIBRARY_NOT_FOUND                     = -2,      // nvapi.dll can not be loaded
    NVAPI_NO_IMPLEMENTATION                     = -3,      // not implemented in current driver installation
    NVAPI_API_NOT_INTIALIZED                    = -4,      // NvAPI_Initialize has not been called (successfully)
    NVAPI_INVALID_ARGUMENT                      = -5,      // invalid argument
    NVAPI_NVIDIA_DEVICE_NOT_FOUND               = -6,      // no NVIDIA display driver was found
    NVAPI_END_ENUMERATION                       = -7,      // no more to enum
    NVAPI_INVALID_HANDLE                        = -8,      // invalid handle
    NVAPI_INCOMPATIBLE_STRUCT_VERSION           = -9,      // an argument's structure version is not supported
    NVAPI_HANDLE_INVALIDATED                    = -10,     // handle is no longer valid (likely due to GPU or display re-configuration)
    NVAPI_OPENGL_CONTEXT_NOT_CURRENT            = -11,     // no NVIDIA OpenGL context is current (but needs to be)
    NVAPI_EXPECTED_LOGICAL_GPU_HANDLE           = -100,    // expected a logical GPU handle for one or more parameters
    NVAPI_EXPECTED_PHYSICAL_GPU_HANDLE          = -101,    // expected a phyiscal GPU handle for one or more parameters
    NVAPI_EXPECTED_DISPLAY_HANDLE               = -102,    // expected an NV display handle for one or more parameters
    NVAPI_INVALID_COMBINATION                   = -103,    // used in some commands to indicate that the combination of parameters is not validexpected an NV display handle for one or more parameters
    NVAPI_NOT_SUPPORTED                         = -104,    // Requested feature not supported in the selected GPU
    NVAPI_PORTID_NOT_FOUND                      = -105,    // NO port ID found for I2C transaction
    NVAPI_EXPECTED_UNATTACHED_DISPLAY_HANDLE    = -106,    // expected an unattached display handle as one of the input param
    NVAPI_INVALID_PERF_LEVEL                    = -107,    // invalid perf level 
    NVAPI_DEVICE_BUSY                           = -108,    // device is busy, request not fulfilled
    NVAPI_NV_PERSIST_FILE_NOT_FOUND             = -109,    // NV persist file is not found
    NVAPI_PERSIST_DATA_NOT_FOUND                = -110,    // NV persist data is not found
    NVAPI_EXPECTED_TV_DISPLAY                   = -111,    // expected TV output display
    NVAPI_EXPECTED_TV_DISPLAY_ON_DCONNECTOR     = -112,    // expected TV output on D Connector - HDTV_EIAJ4120.
    NVAPI_NO_ACTIVE_SLI_TOPOLOGY                = -113,    // SLI is not supported for this device
    NVAPI_SLI_RENDERING_MODE_NOTALLOWED         = -114,    // setup of SLI rendering mode is not possible right now
    NVAPI_EXPECTED_DIGITAL_FLAT_PANEL           = -115,    // expected digital flat panel
    NVAPI_ARGUMENT_EXCEED_MAX_SIZE              = -116,    // argument exceeds expected size
    NVAPI_DEVICE_SWITCHING_NOT_ALLOWED          = -117,    // inhibit ON due to one of the flags in NV_GPU_DISPLAY_CHANGE_INHIBIT or SLI Active
    NVAPI_TESTING_CLOCKS_NOT_SUPPORTED          = -118,    // testing clocks not supported
    NVAPI_UNKNOWN_UNDERSCAN_CONFIG              = -119,    // the specified underscan config is from an unknown source (e.g. INF)
    NVAPI_DATA_NOT_FOUND                        = -121,    // Requested data was not found
} NvAPI_Status;


///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_Initialize
//
//   DESCRIPTION: Initializes NVAPI library. This must be called before any 
//                other NvAPI_ function.
//
// RETURN STATUS: NVAPI_ERROR            Something is wrong during the initialization process (generic error)
//                NVAPI_LIBRARYNOTFOUND  Can not load nvapi.dll
//                NVAPI_OK                  Initialized
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_Initialize();

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GetErrorMessage
//
//   DESCRIPTION: converts an NvAPI error code into a null terminated string
//
// RETURN STATUS: null terminated string (always, never NULL)
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GetErrorMessage(NvAPI_Status nr,NvAPI_ShortString szDesc);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GetInterfaceVersionString
//
//   DESCRIPTION: Returns a string describing the version of the NvAPI library.
//                Contents of the string are human readable.  Do not assume a fixed
//                format.
//
// RETURN STATUS: User readable string giving info on NvAPI's version
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GetInterfaceVersionString(NvAPI_ShortString szDesc);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GetDisplayDriverVersion
//
//   DESCRIPTION: Returns a struct that describes aspects of the display driver
//                build.
//
// RETURN STATUS: NVAPI_ERROR or NVAPI_OK
//
///////////////////////////////////////////////////////////////////////////////
typedef struct 
{
    NvU32              version;             // Structure version
    NvU32              drvVersion;           
    NvU32              bldChangeListNum;     
    NvAPI_ShortString  szBuildBranchString; 
    NvAPI_ShortString  szAdapterString;
} NV_DISPLAY_DRIVER_VERSION;
#define NV_DISPLAY_DRIVER_VERSION_VER  MAKE_NVAPI_VERSION(NV_DISPLAY_DRIVER_VERSION,1)
NVAPI_INTERFACE NvAPI_GetDisplayDriverVersion(NvDisplayHandle hNvDisplay, NV_DISPLAY_DRIVER_VERSION *pVersion);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_EnumNvidiaDisplayHandle
//
//   DESCRIPTION: Returns the handle of the NVidia display specified by the enum 
//                index (thisEnum). The client should keep enumerating until it
//                returns NVAPI_END_ENUMERATION.
//
//                Note: Display handles can get invalidated on a modeset, so the calling applications need to 
//                renum the handles after every modeset.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: either the handle pointer is NULL or enum index too big
//                NVAPI_OK: return a valid NvDisplayHandle based on the enum index
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia device found in the system
//                NVAPI_END_ENUMERATION: no more display device to enumerate.
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_EnumNvidiaDisplayHandle(NvU32 thisEnum, NvDisplayHandle *pNvDispHandle);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_EnumNvidiaUnAttachedDisplayHandle
//
//   DESCRIPTION: Returns the handle of the NVidia UnAttached display specified by the enum 
//                index (thisEnum). The client should keep enumerating till it
//                return error.
//
//                Note: Display handles can get invalidated on a modeset, so the calling applications need to 
//                renum the handles after every modeset.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: either the handle pointer is NULL or enum index too big
//                NVAPI_OK: return a valid NvDisplayHandle based on the enum index
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia device found in the system
//                NVAPI_END_ENUMERATION: no more display device to enumerate.
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_EnumNvidiaUnAttachedDisplayHandle(NvU32 thisEnum, NvUnAttachedDisplayHandle *pNvUnAttachedDispHandle);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_EnumPhysicalGPUs
//
//   DESCRIPTION: Returns an array of physical GPU handles.
//
//                Each handle represents a physical GPU present in the system.
//                That GPU may be part of a SLI configuration, or not be visible to the OS directly.
//
//                At least 1 GPU must be present in the system and running an NV display driver.
//
//                The array nvGPUHandle will be filled with physical GPU handle values.  The returned
//                gpuCount determines how many entries in the array are valid.
//
//                Note: All GPU handles get invalidated on a modeset, so the calling applications need to 
//                renum the handles after every modeset.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: nvGPUHandle or pGpuCount is NULL
//                NVAPI_OK: one or more handles were returned
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_EnumPhysicalGPUs(NvPhysicalGpuHandle nvGPUHandle[NVAPI_MAX_PHYSICAL_GPUS], NvU32 *pGpuCount);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_EnumLogicalGPUs
//
//   DESCRIPTION: Returns an array of logical GPU handles.
//
//                Each handle represents one or more GPUs acting in concert as a single graphics device.
//
//                At least 1 GPU must be present in the system and running an NV display driver.
//
//                The array nvGPUHandle will be filled with logical GPU handle values.  The returned
//                gpuCount determines how many entries in the array are valid.
//
//                Note: All GPU handles get invalidated on a modeset, so the calling applications need to 
//                renum the handles after every modeset.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: nvGPUHandle or pGpuCount is NULL
//                NVAPI_OK: one or more handles were returned
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_EnumLogicalGPUs(NvLogicalGpuHandle nvGPUHandle[NVAPI_MAX_LOGICAL_GPUS], NvU32 *pGpuCount);
 
///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GetPhysicalGPUsFromDisplay
//
//   DESCRIPTION: Returns an array of physical GPU handles associated with the specified display.
//
//                At least 1 GPU must be present in the system and running an NV display driver.
//
//                The array nvGPUHandle will be filled with physical GPU handle values.  The returned
//                gpuCount determines how many entries in the array are valid.
//
//                If the display corresponds to more than one physical GPU, the first GPU returned
//                is the one with the attached active output.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: hNvDisp is not valid; nvGPUHandle or pGpuCount is NULL
//                NVAPI_OK: one or more handles were returned
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GetPhysicalGPUsFromDisplay(NvDisplayHandle hNvDisp, NvPhysicalGpuHandle nvGPUHandle[NVAPI_MAX_PHYSICAL_GPUS], NvU32 *pGpuCount);
 
///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GetPhysicalGPUFromUnAttachedDisplay
//
//   DESCRIPTION: Returns a physical GPU handle associated with the specified unattached display.
//
//                At least 1 GPU must be present in the system and running an NV display driver.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: hNvUnAttachedDisp is not valid or pPhysicalGpu is NULL.
//                NVAPI_OK: one or more handles were returned
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GetPhysicalGPUFromUnAttachedDisplay(NvUnAttachedDisplayHandle hNvUnAttachedDisp, NvPhysicalGpuHandle *pPhysicalGpu);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_CreateDisplayFromUnAttachedDisplay
//
//   DESCRIPTION: The unattached display handle is converted to a active attached display handle.
//
//                At least 1 GPU must be present in the system and running an NV display driver.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: hNvUnAttachedDisp is not valid or pNvDisplay is NULL.
//                NVAPI_OK: one or more handles were returned
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_CreateDisplayFromUnAttachedDisplay(NvUnAttachedDisplayHandle hNvUnAttachedDisp, NvDisplayHandle *pNvDisplay);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GetLogicalGPUFromDisplay
//
//   DESCRIPTION: Returns the logical GPU handle associated with the specified display.
//
//                At least 1 GPU must be present in the system and running an NV display driver.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: hNvDisp is not valid; pLogicalGPU is NULL
//                NVAPI_OK: one or more handles were returned
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GetLogicalGPUFromDisplay(NvDisplayHandle hNvDisp, NvLogicalGpuHandle *pLogicalGPU);
 
///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GetLogicalGPUFromPhysicalGPU
//
//   DESCRIPTION: Returns the logical GPU handle associated with specified physical GPU handle.
//
//                At least 1 GPU must be present in the system and running an NV display driver.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: hPhysicalGPU is not valid; pLogicalGPU is NULL
//                NVAPI_OK: one or more handles were returned
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GetLogicalGPUFromPhysicalGPU(NvPhysicalGpuHandle hPhysicalGPU, NvLogicalGpuHandle *pLogicalGPU);
   
///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GetPhysicalGPUsFromLogicalGPU
//
//   DESCRIPTION: Returns the physical GPU handles associated with the specified logical GPU handle.
//
//                At least 1 GPU must be present in the system and running an NV display driver.
//
//                The array hPhysicalGPU will be filled with physical GPU handle values.  The returned
//                gpuCount determines how many entries in the array are valid.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: hLogicalGPU is not valid; hPhysicalGPU is NULL
//                NVAPI_OK: one or more handles were returned
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//                NVAPI_EXPECTED_LOGICAL_GPU_HANDLE: hLogicalGPU was not a logical GPU handle
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GetPhysicalGPUsFromLogicalGPU(NvLogicalGpuHandle hLogicalGPU,NvPhysicalGpuHandle hPhysicalGPU[NVAPI_MAX_PHYSICAL_GPUS], NvU32 *pGpuCount);
   
///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GetAssociatedNvidiaDisplayHandle
//
//   DESCRIPTION: Returns the handle of the NVidia display that is associated
//                with the display name given.  Eg: "\\DISPLAY1"
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: either argument is NULL
//                NVAPI_OK: *pNvDispHandle is now valid
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia device maps to that display name
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GetAssociatedNvidiaDisplayHandle(const char *szDisplayName, NvDisplayHandle *pNvDispHandle);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GetAssociatedNvidiaDisplayName
//
//   DESCRIPTION: Returns the display name given.  Eg: "\\DISPLAY1" using the NVidia display handle
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: either argument is NULL
//                NVAPI_OK: *pNvDispHandle is now valid
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia device maps to that display name
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GetAssociatedNvidiaDisplayName(NvDisplayHandle NvDispHandle, NvAPI_ShortString szDisplayName);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GetUnAttachedAssociatedDisplayName
//
//   DESCRIPTION: Returns the display name given.  Eg: "\\DISPLAY1" using the NVidia unattached display handle
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: either argument is NULL
//                NVAPI_OK: *pNvDispHandle is now valid
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia device maps to that display name
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GetUnAttachedAssociatedDisplayName(NvUnAttachedDisplayHandle hNvUnAttachedDisp, NvAPI_ShortString szDisplayName);



///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_EnableHWCursor
//
//   DESCRIPTION: Enable hardware cursor support
//
// RETURN STATUS: NVAPI_ERROR or NVAPI_OK
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_EnableHWCursor(NvDisplayHandle hNvDisplay);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_DisableHWCursor
//
//   DESCRIPTION: Disable hardware cursor support
//
// RETURN STATUS: NVAPI_ERROR or NVAPI_OK
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_DisableHWCursor(NvDisplayHandle hNvDisplay);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GetVBlankCounter
//
//   DESCRIPTION: get vblank counter
//
// RETURN STATUS: NVAPI_ERROR or NVAPI_OK
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GetVBlankCounter(NvDisplayHandle hNvDisplay, NvU32 *pCounter);

///////////////////////////////////////////////////////////////////////////////
// FUNCTION NAME: NvAPI_OverrideRefreshRate
//   DESCRIPTION: Override the refresh rate on the given  display/outputsMask.
//                The new refresh rate can be applied right away in this API call or deferred to happen with the
//                next OS modeset. The override is only good for one modeset (doesn't matter it's deferred or immediate).
//               
//
//         INPUT: hNvDisplay - the NVidia display handle. It can be NVAPI_DEFAULT_HANDLE or a handle
//                             enumerated from NvAPI_EnumNVidiaDisplayHandle().
//                
//                outputsMask - a set of bits that identify all target outputs which are associated with the NVidia 
//                              display handle to apply the refresh rate override. Note when SLI is enabled,  the
//                              outputsMask only applies to the GPU that is driving the display output.
//
//                refreshRate - the override value. "0.0" means cancel the override.
//                              
//
//                bSetDeferred - "0": apply the refresh rate override immediately in this API call.
//                               "1":  apply refresh rate at the next OS modeset.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: hNvDisplay or outputsMask is invalid
//                NVAPI_OK: the refresh rate override is correct set
//                NVAPI_ERROR: the operation failed
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_SetRefreshRateOverride(NvDisplayHandle hNvDisplay, NvU32 outputsMask, float refreshRate, NvU32 bSetDeferred);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GetAssociatedDisplayOutputId
//
//   DESCRIPTION: Gets the active outputId associated with the display handle.
//
//    PARAMETERS: hNvDisplay(IN) - NVIDIA Display selection. It can be NVAPI_DEFAULT_HANDLE or a handle enumerated from NvAPI_EnumNVidiaDisplayHandle().
//                outputId(OUT)  - The active display output id associated with the selected display handle hNvDisplay.
//                                 The outputid will have only one bit set. In case of clone or span this  will indicate the display
//                                 outputId of the primay display that the GPU is driving.
// RETURN STATUS: NVAPI_OK: call successful.
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found.
//                NVAPI_EXPECTED_DISPLAY_HANDLE: hNvDisplay is not a valid display handle.
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GetAssociatedDisplayOutputId(NvDisplayHandle hNvDisplay, NvU32 *pOutputId);


///////////////////////////////////////////////////////////////////////////////
// FUNCTION NAME:   NvAPI_GetHDMISupportInfo
//
// DESCRIPTION:     This API returns the current infoframe data on the specified device(monitor)
//
// PARAMETERS:      hNvDisplay(IN) - NVIDIA Display selection. It can be NVAPI_DEFAULT_HANDLE or a handle enumerated from NvAPI_EnumNVidiaDisplayHandle().
//                  outputId(IN)   - The display output id. If it's "0" then the default outputId from NvAPI_GetAssociatedDisplayOutputId() will be used.
//                  pInfo(OUT)     - The monitor and GPU's HDMI support info
//
// RETURN STATUS: 
//                  NVAPI_OK - completed request
//                  NVAPI_ERROR - miscellaneous error occurred
//                  NVAPI_INVALID_ARGUMENT: Invalid input parameter.
//
///////////////////////////////////////////////////////////////////////////////
typedef struct
{
    NvU32      version;                     // structure version
    NvU32      isGpuHDMICapable       : 1;  // if the GPU can handle HDMI
    NvU32      isMonUnderscanCapable  : 1;  // if the monitor supports underscan
    NvU32      isMonBasicAudioCapable : 1;  // if the monitor supports basic audio
    NvU32      isMonYCbCr444Capable   : 1;  // if YCbCr 4:4:4 is supported
    NvU32      isMonYCbCr422Capable   : 1;  // if YCbCr 4:2:2 is supported
    NvU32      isMonHDMI              : 1;  // if the monitor is HDMI (with IEEE's HDMI registry ID)
    NvU32      EDID861ExtRev;               // the revision number of the EDID 861 extension
 } NV_HDMI_SUPPORT_INFO; 

#define NV_HDMI_SUPPORT_INFO_VER  MAKE_NVAPI_VERSION(NV_HDMI_SUPPORT_INFO,1)

NVAPI_INTERFACE NvAPI_GetHDMISupportInfo(NvDisplayHandle hNvDisplay, NvU32 outputId, NV_HDMI_SUPPORT_INFO *pInfo);

///////////////////////////////////////////////////////////////////////////////
// FUNCTION NAME:   NvAPI_GetInfoFrame
//
// DESCRIPTION:     This API returns the current infoframe data on the specified device(monitor)
//
// PARAMETERS:      hNvDisplay(IN) - NVIDIA Display selection. It can be NVAPI_DEFAULT_HANDLE or a handle enumerated from NvAPI_EnumNVidiaDisplayHandle().
//                  outputId(IN)   - The display output id. If it's "0" then the default outputId from NvAPI_GetAssociatedDisplayOutputId() will be used.
//                  type(IN)       - The type of infoframe to set
//                  pInfoFrame(IN) - The infoframe data
//
// RETURN STATUS: 
//                  NVAPI_OK - completed request
//                  NVAPI_ERROR - miscellaneous error occurred
//                  NVAPI_INVALID_ARGUMENT: Invalid input parameter.
//
///////////////////////////////////////////////////////////////////////////////
typedef enum _NV_INFOFRAME_TYPE
{
    NV_INFOFRAME_TYPE_AVI   = 2,
    NV_INFOFRAME_TYPE_SPD   = 3,
    NV_INFOFRAME_TYPE_AUDIO = 4,
    NV_INFOFRAME_TYPE_MS    = 5,
} NV_INFOFRAME_TYPE;

typedef struct
{
    NvU8 type;
    NvU8 version;
    NvU8 length;
} NV_INFOFRAME_HEADER;

// since this is for Windows OS so far, we use this bit little endian defination
// to handle the translation
typedef struct
{
    // byte 1
    NvU8 channelCount     : 3;
    NvU8 rsvd_bits_byte1  : 1;
    NvU8 codingType       : 4;

    // byte 2
    NvU8 sampleSize       : 2;
    NvU8 sampleRate       : 3;
    NvU8 rsvd_bits_byte2  : 3;

    // byte 3
    NvU8  byte3;

    // byte 4
    NvU8  speakerPlacement;

    // byte 5
    NvU8 rsvd_bits_byte5  : 3;
    NvU8 levelShift       : 4;
    NvU8 downmixInhibit   : 1;

    // byte 6~10
    NvU8 rsvd_byte6;
    NvU8 rsvd_byte7;
    NvU8 rsvd_byte8;
    NvU8 rsvd_byte9;
    NvU8 rsvd_byte10;

}NV_AUDIO_INFOFRAME;

typedef struct
{
    // byte 1
    NvU8 scanInfo                : 2;
    NvU8 barInfo                 : 2;
    NvU8 activeFormatInfoPresent : 1;
    NvU8 colorSpace              : 2;
    NvU8 rsvd_bits_byte1         : 1;

    // byte 2
    NvU8 activeFormatAspectRatio : 4;
    NvU8 picAspectRatio          : 2;
    NvU8 colorimetry             : 2;

    // byte 3
    NvU8 nonuniformScaling       : 2;
    NvU8 rsvd_bits_byte3         : 6;

    // byte 4
    NvU8 vic                     : 7;
    NvU8 rsvd_bits_byte4         : 1;

    // byte 5
    NvU8 pixelRepeat             : 4;
    NvU8 rsvd_bits_byte5         : 4;

    // byte 6~13 
    NvU8 topBarLow;
    NvU8 topBarHigh;
    NvU8 bottomBarLow;
    NvU8 bottomBarHigh;
    NvU8 leftBarLow;
    NvU8 leftBarHigh;
    NvU8 rightBarLow;
    NvU8 rightBarHigh;

} NV_VIDEO_INFOFRAME;

typedef struct
{
    NV_INFOFRAME_HEADER    header;
    union
    {
        NV_AUDIO_INFOFRAME audio;
        NV_VIDEO_INFOFRAME video;
    }u;
} NV_INFOFRAME;
NVAPI_INTERFACE NvAPI_GetInfoFrame(NvDisplayHandle hNvDisplay, NvU32 outputId, NV_INFOFRAME_TYPE type, NV_INFOFRAME *pInfoFrame);

///////////////////////////////////////////////////////////////////////////////
// FUNCTION NAME:   NvAPI_SetInfoFrame
//
// DESCRIPTION:     This API returns the current infoframe data on the specified device(monitor)
//
// PARAMETERS:      hNvDisplay(IN) - NVIDIA Display selection. It can be NVAPI_DEFAULT_HANDLE or a handle enumerated from NvAPI_EnumNVidiaDisplayHandle().
//                  outputId(IN)   - The display output id. If it's "0" then the default outputId from NvAPI_GetAssociatedDisplayOutputId() will be used.
//                  type(IN)       - The type of infoframe to set
//                  pInfoFrame(IN) - The infoframe data, NULL mean reset to the default value.
//
// RETURN STATUS: 
//                  NVAPI_OK - completed request
//                  NVAPI_ERROR - miscellaneous error occurred
//                  NVAPI_INVALID_ARGUMENT: Invalid input parameter.
//
///////////////////////////////////////////////////////////////////////////////

NVAPI_INTERFACE NvAPI_SetInfoFrame(NvDisplayHandle hNvDisplay, NvU32 outputId, NV_INFOFRAME_TYPE type, NV_INFOFRAME *pInfoFrame);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GPU_GetAllOutputs
//
//   DESCRIPTION: Returns set of all GPU-output identifiers as a bitmask.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: hPhysicalGpu or pOutputsMask is NULL
//                NVAPI_OK: *pOutputsMask contains a set of GPU-output identifiers
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//                NVAPI_EXPECTED_PHYSICAL_GPU_HANDLE: hPhysicalGpu was not a physical GPU handle
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GPU_GetAllOutputs(NvPhysicalGpuHandle hPhysicalGpu,NvU32 *pOutputsMask);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GPU_GetConnectedOutputs
//
//   DESCRIPTION: Same as NvAPI_GPU_GetAllOutputs but returns only the set of GPU-output 
//                identifiers that are connected to display devices.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: hPhysicalGpu or pOutputsMask is NULL
//                NVAPI_OK: *pOutputsMask contains a set of GPU-output identifiers
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//                NVAPI_EXPECTED_PHYSICAL_GPU_HANDLE: hPhysicalGpu was not a physical GPU handle
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GPU_GetConnectedOutputs(NvPhysicalGpuHandle hPhysicalGpu, NvU32 *pOutputsMask);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GPU_GetConnectedOutputsWithLidState
//
//   DESCRIPTION: Similar to NvAPI_GPU_GetConnectedOutputs this API returns the connected display identifiers that are connected 
//		          as a output mask but unlike NvAPI_GPU_GetConnectedOutputs this API "always" reflects the Lid State in the output mask.
//                Thus if you expect the LID close state to be available in the connection mask use this API.
//		          If LID is closed then this API will remove the LID panel from the connected display identifiers. 
//		          If LID is open then this API will reflect the LID panel in the connected display identifiers. 
//		          Note:This API should be used on laptop systems and on systems where LID state is required in the connection output mask. 
//                     On desktop systems the returned identifiers will match NvAPI_GPU_GetConnectedOutputs.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: hPhysicalGpu or pOutputsMask is NULL
//                NVAPI_OK: *pOutputsMask contains a set of GPU-output identifiers
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//                NVAPI_EXPECTED_PHYSICAL_GPU_HANDLE: hPhysicalGpu was not a physical GPU handle
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GPU_GetConnectedOutputsWithLidState(NvPhysicalGpuHandle hPhysicalGpu, NvU32 *pOutputsMask);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GPU_GetActiveOutputs
//
//   DESCRIPTION: Same as NvAPI_GPU_GetAllOutputs but returns only the set of GPU-output 
//                identifiers that are actively driving display devices.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: hPhysicalGpu or pOutputsMask is NULL
//                NVAPI_OK: *pOutputsMask contains a set of GPU-output identifiers
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//                NVAPI_EXPECTED_PHYSICAL_GPU_HANDLE: hPhysicalGpu was not a physical GPU handle
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GPU_GetActiveOutputs(NvPhysicalGpuHandle hPhysicalGpu, NvU32 *pOutputsMask);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GPU_GetEDID
//
//   DESCRIPTION: Returns the EDID data for the specified GPU handle and connection bit mask.
//                displayOutputId should have exactly 1 bit set to indicate a single display.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: pEDID is NULL; displayOutputId has 0 or > 1 bits set.
//                NVAPI_OK: *pEDID contains valid data.
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found.
//                NVAPI_EXPECTED_PHYSICAL_GPU_HANDLE: hPhysicalGpu was not a physical GPU handle.
//                NVAPI_DATA_NOT_FOUND: requested display does not contain an EDID
//
///////////////////////////////////////////////////////////////////////////////
#define NV_EDID_DATA_SIZE   256
#define NV_EDID_V1_DATA_SIZE   256
typedef struct
{
    NvU32   version;        //structure version
    NvU8    EDID_Data[NV_EDID_DATA_SIZE];
    NvU32   sizeofEDID;
} NV_EDID;

#define NV_EDID_VER         MAKE_NVAPI_VERSION(NV_EDID,2)
NVAPI_INTERFACE NvAPI_GPU_GetEDID(NvPhysicalGpuHandle hPhysicalGpu, NvU32 displayOutputId, NV_EDID *pEDID);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GPU_GetOutputType
//
//   DESCRIPTION: Give a physical GPU handle and a single outputId (exactly 1 bit set), this API
//                returns the output type.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: hPhysicalGpu, outputId or pOutputsMask is NULL; or outputId has > 1 bit set
//                NVAPI_OK: *pOutputType contains a NvGpuOutputType value
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//                NVAPI_EXPECTED_PHYSICAL_GPU_HANDLE: hPhysicalGpu was not a physical GPU handle
//
///////////////////////////////////////////////////////////////////////////////
typedef enum _NV_GPU_OUTPUT_TYPE
{
    NVAPI_GPU_OUTPUT_UNKNOWN  = 0,
    NVAPI_GPU_OUTPUT_CRT      = 1,     // CRT display device
    NVAPI_GPU_OUTPUT_DFP      = 2,     // Digital Flat Panel display device
    NVAPI_GPU_OUTPUT_TV       = 3,     // TV display device
} NV_GPU_OUTPUT_TYPE;

NVAPI_INTERFACE NvAPI_GPU_GetOutputType(NvPhysicalGpuHandle hPhysicalGpu, NvU32 outputId, NV_GPU_OUTPUT_TYPE *pOutputType);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GPU_ValidateOutputCombination
//
//   DESCRIPTION: This call is used to determine if a set of GPU outputs can be active 
//                simultaneously.  While a GPU may have <n> outputs, they can not typically
//                all be active at the same time due to internal resource sharing.
//
//                Given a physical GPU handle and a mask of candidate outputs, this call
//                will return NVAPI_OK if all of the specified outputs can be driven
//                simultaneously.  It will return NVAPI_INVALID_COMBINATION if they cannot.
//                
//                Use NvAPI_GPU_GetAllOutputs() to determine which outputs are candidates.
//
// RETURN STATUS: NVAPI_OK: combination of outputs in outputsMask are valid (can be active simultaneously)
//                NVAPI_INVALID_COMBINATION: combination of outputs in outputsMask are NOT valid
//                NVAPI_INVALID_ARGUMENT: hPhysicalGpu or outputsMask does not have at least 2 bits set
//                NVAPI_EXPECTED_PHYSICAL_GPU_HANDLE: hPhysicalGpu was not a physical GPU handle
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GPU_ValidateOutputCombination(NvPhysicalGpuHandle hPhysicalGpu, NvU32 outputsMask);

typedef enum _NV_GPU_CONNECTOR_TYPE
{
    NVAPI_GPU_CONNECTOR_VGA_15_PIN                      = 0x00000000,
    NVAPI_GPU_CONNECTOR_TV_COMPOSITE                    = 0x00000010,
    NVAPI_GPU_CONNECTOR_TV_SVIDEO                       = 0x00000011,
    NVAPI_GPU_CONNECTOR_TV_HDTV_COMPONENT               = 0x00000013,
    NVAPI_GPU_CONNECTOR_TV_SCART                        = 0x00000014,
    NVAPI_GPU_CONNECTOR_TV_COMPOSITE_SCART_ON_EIAJ4120  = 0x00000016,
    NVAPI_GPU_CONNECTOR_TV_HDTV_EIAJ4120                = 0x00000017,
    NVAPI_GPU_CONNECTOR_PC_POD_HDTV_YPRPB               = 0x00000018,
    NVAPI_GPU_CONNECTOR_PC_POD_SVIDEO                   = 0x00000019,
    NVAPI_GPU_CONNECTOR_PC_POD_COMPOSITE                = 0x0000001A,
    NVAPI_GPU_CONNECTOR_DVI_I_TV_SVIDEO                 = 0x00000020,
    NVAPI_GPU_CONNECTOR_DVI_I_TV_COMPOSITE              = 0x00000021,
    NVAPI_GPU_CONNECTOR_DVI_I                           = 0x00000030,
    NVAPI_GPU_CONNECTOR_DVI_D                           = 0x00000031,
    NVAPI_GPU_CONNECTOR_ADC                             = 0x00000032,
    NVAPI_GPU_CONNECTOR_LFH_DVI_I_1                     = 0x00000038,
    NVAPI_GPU_CONNECTOR_LFH_DVI_I_2                     = 0x00000039,
    NVAPI_GPU_CONNECTOR_SPWG                            = 0x00000040,
    NVAPI_GPU_CONNECTOR_OEM                             = 0x00000041,
    NVAPI_GPU_CONNECTOR_HDMI_A                          = 0x00000061,
    NVAPI_GPU_CONNECTOR_UNKNOWN                         = 0xFFFFFFFF,
} NV_GPU_CONNECTOR_TYPE;

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GPU_GetFullName
//
//   DESCRIPTION: Retrieves the full GPU name as an ascii string.  Eg: "Quadro FX 1400"
//
// RETURN STATUS: NVAPI_ERROR or NVAPI_OK
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GPU_GetFullName(NvPhysicalGpuHandle hPhysicalGpu, NvAPI_ShortString szName);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GPU_GetPCIIdentifiers
//
//   DESCRIPTION: Returns the PCI identifiers associated this this GPU.
//                  DeviceId - the internal PCI device identifier for the GPU.
//                  SubSystemId - the internal PCI subsystem identifier for the GPU.
//                  RevisionId - the internal PCI device-specific revision identifier for the GPU.
//                  ExtDeviceId - the external PCI device identifier for the GPU.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: hPhysicalGpu or an argument is NULL
//                NVAPI_OK: arguments are populated with PCI identifiers
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//                NVAPI_EXPECTED_PHYSICAL_GPU_HANDLE: hPhysicalGpu was not a physical GPU handle
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GPU_GetPCIIdentifiers(NvPhysicalGpuHandle hPhysicalGpu,NvU32 *pDeviceId,NvU32 *pSubSystemId,NvU32 *pRevisionId,NvU32 *pExtDeviceId);
    
typedef enum _NV_GPU_BUS_TYPE
{
    NVAPI_GPU_BUS_TYPE_UNDEFINED    = 0,
    NVAPI_GPU_BUS_TYPE_PCI          = 1,
    NVAPI_GPU_BUS_TYPE_AGP          = 2,
    NVAPI_GPU_BUS_TYPE_PCI_EXPRESS  = 3,
    NVAPI_GPU_BUS_TYPE_FPCI         = 4,
} NV_GPU_BUS_TYPE;
///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GPU_GetBusType
//
//   DESCRIPTION: Returns the type of bus associated this this GPU.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: hPhysicalGpu or pBusType is NULL
//                NVAPI_OK: *pBusType contains bus identifier
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//                NVAPI_EXPECTED_PHYSICAL_GPU_HANDLE: hPhysicalGpu was not a physical GPU handle
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GPU_GetBusType(NvPhysicalGpuHandle hPhysicalGpu,NV_GPU_BUS_TYPE *pBusType);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GPU_GetIRQ
//
//   DESCRIPTION: Returns the interrupt number associated this this GPU.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: hPhysicalGpu or pIRQ is NULL
//                NVAPI_OK: *pIRQ contains interrupt number
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//                NVAPI_EXPECTED_PHYSICAL_GPU_HANDLE: hPhysicalGpu was not a physical GPU handle
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GPU_GetIRQ(NvPhysicalGpuHandle hPhysicalGpu,NvU32 *pIRQ);
    
///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GPU_GetVbiosRevision
//
//   DESCRIPTION: Returns the revision of the video bios associated this this GPU.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: hPhysicalGpu or pBiosRevision is NULL
//                NVAPI_OK: *pBiosRevision contains revision number
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//                NVAPI_EXPECTED_PHYSICAL_GPU_HANDLE: hPhysicalGpu was not a physical GPU handle
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GPU_GetVbiosRevision(NvPhysicalGpuHandle hPhysicalGpu,NvU32 *pBiosRevision);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GPU_GetVbiosOEMRevision
//
//   DESCRIPTION: Returns the OEM revision of the video bios associated this this GPU.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: hPhysicalGpu or pBiosRevision is NULL
//                NVAPI_OK: *pBiosRevision contains revision number
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//                NVAPI_EXPECTED_PHYSICAL_GPU_HANDLE: hPhysicalGpu was not a physical GPU handle
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GPU_GetVbiosOEMRevision(NvPhysicalGpuHandle hPhysicalGpu,NvU32 *pBiosRevision);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GPU_GetVbiosVersionString
//
//   DESCRIPTION: Returns the full bios version string in the form of xx.xx.xx.xx.yy where
//                the xx numbers come from NvAPI_GPU_GetVbiosRevision and yy comes from 
//                NvAPI_GPU_GetVbiosOEMRevision.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: hPhysicalGpu is NULL
//                NVAPI_OK: szBiosRevision contains version string
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//                NVAPI_EXPECTED_PHYSICAL_GPU_HANDLE: hPhysicalGpu was not a physical GPU handle
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GPU_GetVbiosVersionString(NvPhysicalGpuHandle hPhysicalGpu,NvAPI_ShortString szBiosRevision);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GPU_GetAGPAperture
//
//   DESCRIPTION: Returns AGP aperture in megabytes
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: pSize is NULL
//                NVAPI_OK: call successful
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//                NVAPI_EXPECTED_PHYSICAL_GPU_HANDLE: hPhysicalGpu was not a physical GPU handle
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GPU_GetAGPAperture(NvPhysicalGpuHandle hPhysicalGpu,NvU32 *pSize);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GPU_GetCurrentAGPRate
//
//   DESCRIPTION: Returns the current AGP Rate (1 = 1x, 2=2x etc, 0 = AGP not present)
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: pRate is NULL
//                NVAPI_OK: call successful
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//                NVAPI_EXPECTED_PHYSICAL_GPU_HANDLE: hPhysicalGpu was not a physical GPU handle
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GPU_GetCurrentAGPRate(NvPhysicalGpuHandle hPhysicalGpu,NvU32 *pRate);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GPU_GetCurrentPCIEDownstreamWidth
//
//   DESCRIPTION: Returns the number of PCIE lanes being used for the PCIE interface 
//                downstream from the GPU.
//
//                On systems that do not support PCIE, the maxspeed for the root link
//                will be zero.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: pWidth is NULL
//                NVAPI_OK: call successful
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//                NVAPI_EXPECTED_PHYSICAL_GPU_HANDLE: hPhysicalGpu was not a physical GPU handle
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GPU_GetCurrentPCIEDownstreamWidth(NvPhysicalGpuHandle hPhysicalGpu,NvU32 *pWidth);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GPU_GetPhysicalFrameBufferSize
//
//   DESCRIPTION: Returns the physical size of framebuffer in Kb.  This does NOT include any
//                system RAM that may be dedicated for use by the GPU.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: pSize is NULL
//                NVAPI_OK: call successful
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//                NVAPI_EXPECTED_PHYSICAL_GPU_HANDLE: hPhysicalGpu was not a physical GPU handle
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GPU_GetPhysicalFrameBufferSize(NvPhysicalGpuHandle hPhysicalGpu,NvU32 *pSize);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_GPU_GetVirtualFrameBufferSize
//
//   DESCRIPTION: Returns the virtual size of framebuffer in Kb.  This includes the physical RAM plus any
//                system RAM that has been dedicated for use by the GPU.
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: pSize is NULL
//                NVAPI_OK: call successful
//                NVAPI_NVIDIA_DEVICE_NOT_FOUND: no NVidia GPU driving a display was found
//                NVAPI_EXPECTED_PHYSICAL_GPU_HANDLE: hPhysicalGpu was not a physical GPU handle
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GPU_GetVirtualFrameBufferSize(NvPhysicalGpuHandle hPhysicalGpu,NvU32 *pSize);

///////////////////////////////////////////////////////////////////////////////////
//  Thermal API
//  Provides ability to get temperature levels from the various thermal sensors associated with the GPU

#define NVAPI_MAX_THERMAL_SENSORS_PER_GPU 3

typedef enum 
{
    NVAPI_THERMAL_TARGET_NONE          = 0,
    NVAPI_THERMAL_TARGET_GPU           = 1,
    NVAPI_THERMAL_TARGET_MEMORY        = 2,
    NVAPI_THERMAL_TARGET_POWER_SUPPLY  = 4,
    NVAPI_THERMAL_TARGET_BOARD         = 8,
    NVAPI_THERMAL_TARGET_ALL           = 15,
    NVAPI_THERMAL_TARGET_UNKNOWN       = -1,
} NV_THERMAL_TARGET;

typedef enum
{
    NVAPI_THERMAL_CONTROLLER_NONE = 0,
    NVAPI_THERMAL_CONTROLLER_GPU_INTERNAL,  
    NVAPI_THERMAL_CONTROLLER_ADM1032,
    NVAPI_THERMAL_CONTROLLER_MAX6649,       
    NVAPI_THERMAL_CONTROLLER_MAX1617,      
    NVAPI_THERMAL_CONTROLLER_LM99,      
    NVAPI_THERMAL_CONTROLLER_LM89,         
    NVAPI_THERMAL_CONTROLLER_LM64,         
    NVAPI_THERMAL_CONTROLLER_ADT7473,
    NVAPI_THERMAL_CONTROLLER_SBMAX6649,
    NVAPI_THERMAL_CONTROLLER_VBIOSEVT,  
    NVAPI_THERMAL_CONTROLLER_OS,    
    NVAPI_THERMAL_CONTROLLER_UNKNOWN = -1,
} NV_THERMAL_CONTROLLER;

typedef struct
{
    NvU32   version;                //structure version 
    NvU32   count;                  //number of associated thermal sensors with the selected GPU
    struct 
    {
        NV_THERMAL_CONTROLLER       controller;         //internal, ADM1032, MAX6649...
        NvU32                       defaultMinTemp;     //the min default temperature value of the thermal sensor in degrees centigrade 
        NvU32                       defaultMaxTemp;    //the max default temperature value of the thermal sensor in degrees centigrade 
        NvU32                       currentTemp;       //the current temperature value of the thermal sensor in degrees centigrade 
        NV_THERMAL_TARGET           target;             //thermal senor targetted @ GPU, memory, chipset, powersupply, canoas...
    } sensor[NVAPI_MAX_THERMAL_SENSORS_PER_GPU];

} NV_GPU_THERMAL_SETTINGS;

#define NV_GPU_THERMAL_SETTINGS_VER  MAKE_NVAPI_VERSION(NV_GPU_THERMAL_SETTINGS,1)

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME:   NvAPI_GPU_GetThermalSettings
//
// DESCRIPTION:     Retrieves the thermal information of all thermal sensors or specific thermal sensor associated with the selected GPU.
//                  Thermal sensors are indexed 0 to NVAPI_MAX_THERMAL_SENSORS_PER_GPU-1.
//                  To retrieve specific thermal sensor info set the sensorIndex to the required thermal sensor index. 
//                  To retrieve info for all sensors set sensorIndex to NVAPI_THERMAL_TARGET_ALL. 
//
// PARAMETERS :     hPhysicalGPU(IN) - GPU selection.
//                  sensorIndex(IN)  - Explict thermal sensor index selection. 
//                  pThermalSettings(OUT) - Array of thermal settings.
//
// RETURN STATUS: 
//    NVAPI_OK - completed request
//    NVAPI_ERROR - miscellaneous error occurred
//    NVAPI_INVALID_ARGUMENT - pThermalInfo is NULL
//    NVAPI_HANDLE_INVALIDATED - handle passed has been invalidated (see user guide)
//    NVAPI_EXPECTED_PHYSICAL_GPU_HANDLE - handle passed is not a physical GPU handle
//    NVAPI_INCOMPATIBLE_STRUCT_VERSION - the version of the INFO struct is not supported
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GPU_GetThermalSettings(NvPhysicalGpuHandle hPhysicalGpu, NvU32 sensorIndex, NV_GPU_THERMAL_SETTINGS *pThermalSettings);

typedef enum _NV_DISPLAY_TV_FORMAT
{
    NV_DISPLAY_TV_FORMAT_NONE         = 0,
    NV_DISPLAY_TV_FORMAT_SD_NTSCM     = 0x00000001,
    NV_DISPLAY_TV_FORMAT_SD_NTSCJ     = 0x00000002,
    NV_DISPLAY_TV_FORMAT_SD_PALM      = 0x00000004,
    NV_DISPLAY_TV_FORMAT_SD_PALBDGH   = 0x00000008,
    NV_DISPLAY_TV_FORMAT_SD_PALN      = 0x00000010,
    NV_DISPLAY_TV_FORMAT_SD_PALNC     = 0x00000020,
    NV_DISPLAY_TV_FORMAT_SD_576i      = 0x00000100,
    NV_DISPLAY_TV_FORMAT_SD_480i      = 0x00000200,
    NV_DISPLAY_TV_FORMAT_ED_480p      = 0x00000400,
    NV_DISPLAY_TV_FORMAT_ED_576p      = 0x00000800,
    NV_DISPLAY_TV_FORMAT_HD_720p      = 0x00001000,
    NV_DISPLAY_TV_FORMAT_HD_1080i     = 0x00002000,
    NV_DISPLAY_TV_FORMAT_HD_1080p     = 0x00004000,
    NV_DISPLAY_TV_FORMAT_HD_720p50    = 0x00008000,
    NV_DISPLAY_TV_FORMAT_HD_1080p24   = 0x00010000,
    NV_DISPLAY_TV_FORMAT_HD_1080i50   = 0x00020000,

} NV_DISPLAY_TV_FORMAT;

///////////////////////////////////////////////////////////////////////////////////
//  I2C API
//  Provides ability to read or wirte data using I2C protocol.

#define NVAPI_MAX_SIZEOF_I2C_DATA_BUFFER 256
#define NVAPI_NO_PORTID_FOUND 5
#define NVAPI_DISPLAY_DEVICE_MASK_MAX 24

typedef struct 
{
    NvU32                   version;        //structure version 
    NvU32                   displayMask;    //the Display Mask of the concerned display
    NvU8                    bIsDDCPort;     //Flag indicating DDC port or a communication port
    NvU8                    i2cDevAddress;  //the I2C target device address
    NvU8*                   pbI2cRegAddress;//the I2C target register address  
    NvU32                   regAddrSize;    //the size in bytes of target register address  
    NvU8*                   pbData;         //The buffer of data which is to be read/written
    NvU32                   cbSize;         //The size of Data buffer to be read.
    NvU32                   i2cSpeed;       //The speed at which the transaction is be made(between 28kbps to 40kbps)
} NV_I2C_INFO;

#define NV_I2C_INFO_VER  MAKE_NVAPI_VERSION(NV_I2C_INFO,1)
/***********************************************************************************/

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME:  NvAPI_I2CRead
//
// DESCRIPTION:    Read data buffer from I2C port
//
// PARAMETERS:     hPhysicalGPU(IN) - GPU selection.
//                 NV_I2C_INFO *pI2cInfo -The I2c data input stucture
//
// RETURN STATUS: 
//    NVAPI_OK - completed request
//    NVAPI_ERROR - miscellaneous error occurred
//    NVAPI_HANDLE_INVALIDATED - handle passed has been invalidated (see user guide)
//    NVAPI_EXPECTED_PHYSICAL_GPU_HANDLE - handle passed is not a physical GPU handle
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_I2CRead(NvPhysicalGpuHandle hPhysicalGpu, NV_I2C_INFO *pI2cInfo);

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME:  NvAPI_I2CWrite
//
// DESCRIPTION:    Writes data buffer to I2C port
//
// PARAMETERS:     hPhysicalGPU(IN) - GPU selection.
//                 NV_I2C_INFO *pI2cInfo -The I2c data input stucture
//
// RETURN STATUS: 
//    NVAPI_OK - completed request
//    NVAPI_ERROR - miscellaneous error occurred
//    NVAPI_HANDLE_INVALIDATED - handle passed has been invalidated (see user guide)
//    NVAPI_EXPECTED_PHYSICAL_GPU_HANDLE - handle passed is not a physical GPU handle
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_I2CWrite(NvPhysicalGpuHandle hPhysicalGpu, NV_I2C_INFO *pI2cInfo);

// END OF I2C API



typedef struct
{
    NvU32               version;        //structure version
    NvU32               vendorId;       //vendorId
    NvU32               deviceId;       //deviceId
    NvAPI_ShortString   szVendorName;   //vendor Name
    NvAPI_ShortString   szChipsetName;  //device Name
} NV_CHIPSET_INFO;

#define NV_CHIPSET_INFO_VER  MAKE_NVAPI_VERSION(NV_CHIPSET_INFO,1)

///////////////////////////////////////////////////////////////////////////////
//
// FUNCTION NAME: NvAPI_SYS_GetChipSetInfo
//
//   DESCRIPTION: Returns information about the System's ChipSet
//
// RETURN STATUS: NVAPI_INVALID_ARGUMENT: pChipSetInfo is NULL
//                NVAPI_OK: *pChipSetInfo is now set
//                NVAPI_INCOMPATIBLE_STRUCT_VERSION - NV_CHIPSET_INFO version not compatible with driver
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_SYS_GetChipSetInfo(NV_CHIPSET_INFO *pChipSetInfo);

#define NVAPI_MAX_VIEW_TARGET  2

typedef enum _NV_TARGET_VIEW_MODE
{
    NV_VIEW_MODE_STANDARD  = 0,
    NV_VIEW_MODE_CLONE     = 1,
    NV_VIEW_MODE_HSPAN     = 2,
    NV_VIEW_MODE_VSPAN     = 3,
    NV_VIEW_MODE_DUALVIEW  = 4,
    NV_VIEW_MODE_MULTIVIEW = 5,
} NV_TARGET_VIEW_MODE;


// Following definitions are used in NvAPI_SetViewEx.
// Scaling modes
typedef enum _NV_SCALING
{
    NV_SCALING_DEFAULT          = 0,        // No change
    NV_SCALING_MONITOR_SCALING  = 1,
    NV_SCALING_ADAPTER_SCALING  = 2,
    NV_SCALING_CENTERED         = 3,
    NV_SCALING_ASPECT_SCALING   = 5,
    NV_SCALING_CUSTOMIZED       = 255       // For future use
} NV_SCALING;

// Rotate modes
typedef enum _NV_ROTATE
{
    NV_ROTATE_0           = 0,
    NV_ROTATE_90          = 1,
    NV_ROTATE_180         = 2,
    NV_ROTATE_270         = 3
} NV_ROTATE;

// Color formats
typedef enum _NV_FORMAT
{
    NV_FORMAT_UNKNOWN       =  0,       // unknown. Driver will choose one as following value.
    NV_FORMAT_P8            = 41,       // for 8bpp mode
    NV_FORMAT_R5G6B5        = 23,       // for 16bpp mode
    NV_FORMAT_A8R8G8B8      = 21,       // for 32bpp mode
    NV_FORMAT_A16B16G16R16F = 113       // for 64bpp(floating point) mode.
} NV_FORMAT;

// TV standard



///////////////////////////////////////////////////////////////////////////////
// FUNCTION NAME:   NvAPI_SetView
//
// DESCRIPTION:     This API lets caller to modify target display arrangement for selected source display handle in any of the nview modes.
//                  It also allows to modify or extend the source display in dualview mode.
//
// PARAMETERS:      hNvDisplay(IN) - NVIDIA Display selection. NVAPI_DEFAULT_HANDLE not allowed, it has to be a handle enumerated with NvAPI_EnumNVidiaDisplayHandle().
//                  pTargetInfo(IN) - Pointer to array of NV_VIEW_TARGET_INFO, specifying device properties in this view.
//                                    The first device entry in the array is the physical primary.
//                                    The device entry with the lowest source id is the desktop primary.
//                  targetCount(IN) - Count of target devices specified in pTargetInfo.
//                  targetView(IN) - Target view selected from NV_TARGET_VIEW_MODE.
//
// RETURN STATUS: 
//                  NVAPI_OK - completed request
//                  NVAPI_ERROR - miscellaneous error occurred
//                  NVAPI_INVALID_ARGUMENT: Invalid input parameter.
//
///////////////////////////////////////////////////////////////////////////////

typedef struct
{
    NvU32 version;     // (IN) structure version
    NvU32 count;       // (IN) target count
    struct 
    {
        NvU32 deviceMask;    // (IN/OUT) device mask
        NvU32 sourceId;      // (IN/OUT) source id
        NvU32 bPrimary:1;    // (OUT) Indicates if this is the desktop primary
        NvU32 bInterlaced:1; // (OUT) Indicates if the timing being used on this monitor is interlaced
        NvU32 bGDIPrimary:1;// (IN/OUT) Indicates if this is the desktop GDI primary.
    } target[NVAPI_MAX_VIEW_TARGET];
} NV_VIEW_TARGET_INFO; 

#define NV_VIEW_TARGET_INFO_VER  MAKE_NVAPI_VERSION(NV_VIEW_TARGET_INFO,2)

NVAPI_INTERFACE NvAPI_SetView(NvDisplayHandle hNvDisplay, NV_VIEW_TARGET_INFO *pTargetInfo, NV_TARGET_VIEW_MODE targetView);

///////////////////////////////////////////////////////////////////////////////
// FUNCTION NAME:   NvAPI_GetView
//
// DESCRIPTION:     This API lets caller retrieve the target display arrangement for selected source display handle.
//
// PARAMETERS:      hNvDisplay(IN) - NVIDIA Display selection. NVAPI_DEFAULT_HANDLE not allowed, it has to be a handle enumerated with NvAPI_EnumNVidiaDisplayHandle().
//                  pTargetInfo(OUT) - User allocated storage to retrieve an array of  NV_VIEW_TARGET_INFO. Can be NULL to retrieve the targetCount.
//                  targetMaskCount(IN/OUT) - Count of target device mask specified in pTargetMask.
//                  targetView(OUT) - Target view selected from NV_TARGET_VIEW_MODE.
//
// RETURN STATUS: 
//                  NVAPI_OK - completed request
//                  NVAPI_ERROR - miscellaneous error occurred
//                  NVAPI_INVALID_ARGUMENT: Invalid input parameter.
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GetView(NvDisplayHandle hNvDisplay, NV_VIEW_TARGET_INFO *pTargets, NvU32 *pTargetMaskCount, NV_TARGET_VIEW_MODE *pTargetView);

///////////////////////////////////////////////////////////////////////////////
// FUNCTION NAME:   NvAPI_SetViewEx
//
// DESCRIPTION:     This API lets caller to modify the display arrangement for selected source display handle in any of the nview modes.
//                  It also allows to modify or extend the source display in dualview mode.
//
// PARAMETERS:      hNvDisplay(IN) - NVIDIA Display selection. NVAPI_DEFAULT_HANDLE not allowed, it has to be a handle enumerated with NvAPI_EnumNVidiaDisplayHandle().
//                  pPathInfo(IN)  - Pointer to array of NV_VIEW_PATH_INFO, specifying device properties in this view.
//                                    The first device entry in the array is the physical primary.
//                                    The device entry with the lowest source id is the desktop primary.
//                  pathCount(IN)  - Count of paths specified in pPathInfo.
//                  displayView(IN)- Display view selected from NV_TARGET_VIEW_MODE.
//
// RETURN STATUS: 
//                  NVAPI_OK - completed request
//                  NVAPI_ERROR - miscellaneous error occurred
//                  NVAPI_INVALID_ARGUMENT: Invalid input parameter.
//
///////////////////////////////////////////////////////////////////////////////

#define NVAPI_MAX_DISPLAY_PATH  NVAPI_MAX_VIEW_TARGET

typedef struct
{
    NvU32 version;     // (IN) structure version
    NvU32 count;       // (IN) path count
    struct 
    {
        NvU32                   deviceMask;     // (IN) device mask
        NvU32                   sourceId;       // (IN) source id
        NvU32                   bPrimary:1;     // (IN/OUT) Indicates if this is the GPU's primary view target. This is not the desktop GDI primary.
                                                // NvAPI_SetViewEx automatically selects the first target in NV_DISPLAY_PATH_INFO index 0 as the GPU's primary view.
        NV_GPU_CONNECTOR_TYPE   connector;      // (IN) Specify connector type. For TV only.
        
        // source mode information
        NvU32                   width;          // (IN) width of the mode
        NvU32                   height;         // (IN) height of the mode
        NvU32                   depth;          // (IN) depth of the mode
        NV_FORMAT               colorFormat;    //      color format if needs to specify. Not used now.
        
        //rotation setting of the mode
        NV_ROTATE               rotation;       // (IN) rotation setting.
      
        // the scaling mode
        NV_SCALING              scaling;        // (IN) scaling setting

        // Timing info
        NvU32                   refreshRate;    // (IN) refresh rate of the mode
        NvU32                   interlaced:1;   // (IN) interlaced mode flag

        NV_DISPLAY_TV_FORMAT    tvFormat;       // (IN) to choose the last TV format set this value to NV_DISPLAY_TV_FORMAT_NONE

        // Windows desktop position
        NvU32                   posx;           // (IN/OUT) x offset of this display on the Windows desktop
        NvU32                   posy;           // (IN/OUT) y offset of this display on the Windows desktop
        NvU32                   bGDIPrimary:1;  // (IN/OUT) Indicates if this is the desktop GDI primary.

    } path[NVAPI_MAX_DISPLAY_PATH];
} NV_DISPLAY_PATH_INFO; 

#define NV_DISPLAY_PATH_INFO_VER  MAKE_NVAPI_VERSION(NV_DISPLAY_PATH_INFO,2)

NVAPI_INTERFACE NvAPI_SetViewEx(NvDisplayHandle hNvDisplay, NV_DISPLAY_PATH_INFO *pPathInfo, NV_TARGET_VIEW_MODE displayView);

///////////////////////////////////////////////////////////////////////////////
// FUNCTION NAME:   NvAPI_GetViewEx
//
// DESCRIPTION:     This API lets caller retrieve the target display arrangement for selected source display handle.
//
// PARAMETERS:      hNvDisplay(IN) - NVIDIA Display selection. NVAPI_DEFAULT_HANDLE not allowed, it has to be a handle enumerated with NvAPI_EnumNVidiaDisplayHandle().
//                  pPathInfo(OUT) - User allocated storage to retrieve an array of NV_DISPLAY_PATH_INFO. Can be NULL to retrieve just the pathCount.
//                  pPathCount(IN/OUT) - Number of elements in array pPathInfo.
//                  pTargetViewMode(OUT)- Display view selected from NV_TARGET_VIEW_MODE.
//
// RETURN STATUS: 
//                  NVAPI_OK - completed request
//                  NVAPI_API_NOT_INTIALIZED - NVAPI not initialized
//                  NVAPI_ERROR - miscellaneous error occurred
//                  NVAPI_INVALID_ARGUMENT - Invalid input parameter.
//                  NVAPI_EXPECTED_DISPLAY_HANDLE - hNvDisplay is not a valid display handle.
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GetViewEx(NvDisplayHandle hNvDisplay, NV_DISPLAY_PATH_INFO *pPathInfo, NvU32 *pPathCount, NV_TARGET_VIEW_MODE *pTargetViewMode);

///////////////////////////////////////////////////////////////////////////////
// FUNCTION NAME:   NvAPI_GetSupportedViews
//
// DESCRIPTION:     This API lets caller enumerate all the supported NVIDIA display views - nview and dualview modes.
//
// PARAMETERS:      hNvDisplay(IN) - NVIDIA Display selection. It can be NVAPI_DEFAULT_HANDLE or a handle enumerated from NvAPI_EnumNVidiaDisplayHandle().
//                  pTargetViews(OUT) - Array of supported views. Can be NULL to retrieve the pViewCount first.
//                  pViewCount(IN/OUT) - Count of supported views.
//
// RETURN STATUS: 
//                  NVAPI_OK - completed request
//                  NVAPI_ERROR - miscellaneous error occurred
//                  NVAPI_INVALID_ARGUMENT: Invalid input parameter.
//
///////////////////////////////////////////////////////////////////////////////
NVAPI_INTERFACE NvAPI_GetSupportedViews(NvDisplayHandle hNvDisplay, NV_TARGET_VIEW_MODE *pTargetViews, NvU32 *pViewCount);


#ifdef __cplusplus
}; //extern "C" {
#endif

#pragma pack(pop)

#endif // _NVAPI_H
