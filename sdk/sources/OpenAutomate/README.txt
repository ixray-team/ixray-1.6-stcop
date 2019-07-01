OpenAutomate SDK
  Contents
    This directory contains the OpenAutomate(tm) SDK.

    The SDK root directory is structured as follows:

        COPYRIGHT         : the obligatory copyright notice
        README.html       : HTML version of this README file
        README.txt        : plain text version of this file
        RELEASE.html      : release notes for the SDK in html format
        RELEASE.txt       : release notes for the SDK in plain text format
        docs              : all documentation
        examples          : some examples on how to use the SDK
        inc               : containins the main OpenAutomate headers
        src               : contains the main OpenAutomate source modules
        OpenAutomate.sln  : Visual Studio solution file for all projects within the SDK
        oalib             : Visual Studio project directory for 
        oarpc             : OpenAutomate RPC library and utilities
        oatest            : the OpenAutomate conformance test suite
        plugins           : useful OpenAutomate plugins

  Documentation
    Please refer to the OpenAutomate document to get started using the
    OpenAutomate SDK.

    For details on changes between releases of the SDK, see RELEASE.

  Building OpenAutomate on Windows
    To build all the projects within the SDK, simply load the
    *OpenAutomate.sln* Visual Studio solution file contained in the root of
    the SDK, and build all.

  Building OpenAutomate on non-Windows OSs
    Currently, OA is not fully supported on any OS other than Windows2K or
    newer. But, the main source files *OpenAutomate.h*,
    *OpenAutomate_Internal.h*, and *OpenAutomate.c* should build on any
    UNIX-like OS. If you would like to integrate OpenAutomate into a
    non-Windows application, please contact your account manager at NVIDIA.

  Contact
    If you have any questions or concerns, please contact your the DevTech
    engineer assigned to your organization by NVIDIA.

