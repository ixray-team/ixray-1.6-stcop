find_package(WindowsSDK)

if(WINDOWSSDK_FOUND)
    if(NOT TARGET WindowsSDK::WindowsSDK)
        add_library(WindowsSDK::WindowsSDK INTERFACE IMPORTED)
        
        get_windowssdk_include_dirs_multiple(_inc_dirs ${WINDOWSSDK_DIRS})
        if(_inc_dirs)
            set_target_properties(WindowsSDK::WindowsSDK PROPERTIES
                INTERFACE_INCLUDE_DIRECTORIES "${_inc_dirs}"
            )
        endif()
        
        get_windowssdk_library_dirs_multiple(_lib_dirs ${WINDOWSSDK_DIRS})
        if(_lib_dirs)
            set_target_properties(WindowsSDK::WindowsSDK PROPERTIES
                INTERFACE_LINK_DIRECTORIES "${_lib_dirs}"
            )
        endif()
    endif()
    
    function(_create_windows_sdk_library_target _target_name _library_name)
        if(NOT TARGET WindowsSDK::${_target_name})
            add_library(WindowsSDK::${_target_name} INTERFACE IMPORTED)
            
            target_link_libraries(WindowsSDK::${_target_name} 
                INTERFACE WindowsSDK::WindowsSDK
            )
            
            target_link_libraries(WindowsSDK::${_target_name} 
                INTERFACE "${_library_name}"
            )
        endif()
    endfunction()
    
    # Direct3D библиотеки
    _create_windows_sdk_library_target(D3D11 "d3d11.lib")
    _create_windows_sdk_library_target(D3D12 "d3d12.lib")
    _create_windows_sdk_library_target(D3DCompiler "d3dcompiler.lib")
    _create_windows_sdk_library_target(D3D9 "d3d9.lib")
    _create_windows_sdk_library_target(D3DX11 "d3dx11.lib")
    _create_windows_sdk_library_target(D3DX9 "d3dx9.lib")
    
    # DXGI
    _create_windows_sdk_library_target(DXGI "dxgi.lib")
    _create_windows_sdk_library_target(DXGUID "dxguid.lib")
    
    # XInput
    _create_windows_sdk_library_target(XInput "xinput.lib")
    
    # Windows Runtime библиотеки
    _create_windows_sdk_library_target(RuntimeObject "runtimeobject.lib")
    _create_windows_sdk_library_target(WindowsApp "windowsapp.lib")
    
    # Мультимедиа библиотеки
    _create_windows_sdk_library_target(WinMM "winmm.lib")
    
    # Прочее
    _create_windows_sdk_library_target(WS2_32 "ws2_32.lib")
    _create_windows_sdk_library_target(DBGHELP "dbghelp.lib")
    _create_windows_sdk_library_target(FAULTREP "faultrep.lib")
    _create_windows_sdk_library_target(VFW32 "Vfw32.lib")
endif()

if (IXRAY_CROSS_COMPILATION)
    function(mock_winsdk_target target_name lib_name)
        if(NOT TARGET WindowsSDK::${target_name})
            add_library(WindowsSDK::${target_name} INTERFACE IMPORTED)
            set_target_properties(WindowsSDK::${target_name} PROPERTIES INTERFACE_LINK_LIBRARIES "${lib_name}")
        endif()
    endfunction()

    # Direct3D библиотеки
    mock_winsdk_target(D3D11 "d3d11.lib")
    mock_winsdk_target(D3D12 "d3d12.lib")
    mock_winsdk_target(D3DCompiler "d3dcompiler.lib")
    mock_winsdk_target(D3D9 "d3d9.lib")
    mock_winsdk_target(D3DX11 "d3dx11.lib")
    mock_winsdk_target(D3DX9 "d3dx9.lib")

    # DXGI
    mock_winsdk_target(DXGI "dxgi.lib")
    mock_winsdk_target(DXGUID "dxguid.lib")

    # XInput
    mock_winsdk_target(XInput "xinput.lib")

    # Windows Runtime библиотеки
    mock_winsdk_target(RuntimeObject "runtimeobject.lib")
    mock_winsdk_target(WindowsApp "windowsapp.lib")

    # Мультимедиа библиотеки
    mock_winsdk_target(WinMM "winmm.lib")

    # Прочее
    mock_winsdk_target(WS2_32 "ws2_32.lib")
    mock_winsdk_target(DBGHELP "dbghelp.lib")
    mock_winsdk_target(FAULTREP "faultrep.lib")
    mock_winsdk_target(VFW32 "Vfw32.lib")
endif()