project(xrGameSpy)
set(XGS_SRC_FILES)

# Files import
file(GLOB XGS_SOURCE_ALL_FILES
"DistributionFunc.cpp"
"gamespy/common/gsAvailable.c"
"gamespy/common/gsCore.c"
"gamespy/common/gsCrypt.c"
"gamespy/common/gsLargeInt.c"
"gamespy/common/gsRC4.c"
"gamespy/common/gsSHA1.c"
"gamespy/common/gsSoap.c"
"gamespy/common/gsSSL.c"
"gamespy/common/gsStringUtil.c"
"gamespy/common/gsUdpEngine.c"
"gamespy/common/gsXML.c"
"gamespy/darray.c"
"Gamespy/gcdkey/gcdkeyc.c"
"Gamespy/gcdkey/gcdkeys.c"
"gamespy/ghttp/ghttpBuffer.c"
"gamespy/ghttp/ghttpCallbacks.c"
"gamespy/ghttp/ghttpCommon.c"
"gamespy/ghttp/ghttpConnection.c"
"gamespy/ghttp/ghttpEncryption.c"
"gamespy/ghttp/ghttpMain.c"
"gamespy/ghttp/ghttpPost.c"
"gamespy/ghttp/ghttpProcess.c"
"Gamespy/GP/gp.c"
"Gamespy/GP/gpi.c"
"Gamespy/GP/gpiBuddy.c"
"Gamespy/GP/gpiBuffer.c"
"Gamespy/GP/gpiCallback.c"
"Gamespy/GP/gpiConnect.c"
"Gamespy/GP/gpiInfo.c"
"Gamespy/GP/gpiKeys.c"
"Gamespy/GP/gpiOperation.c"
"Gamespy/GP/gpiPeer.c"
"Gamespy/GP/gpiProfile.c"
"Gamespy/GP/gpiPS3.c"
"Gamespy/GP/gpiSearch.c"
"Gamespy/GP/gpiTransfer.c"
"Gamespy/GP/gpiUnique.c"
"Gamespy/GP/gpiUtility.c"
"Gamespy/gstats/gbucket.c"
"Gamespy/gstats/gstats.c"
"Gamespy/gt2/gt2Auth.c"
"Gamespy/gt2/gt2Buffer.c"
"Gamespy/gt2/gt2Callback.c"
"Gamespy/gt2/gt2Connection.c"
"Gamespy/gt2/gt2Encode.c"
"Gamespy/gt2/gt2Filter.c"
"Gamespy/gt2/gt2Main.c"
"Gamespy/gt2/gt2Message.c"
"Gamespy/gt2/gt2Socket.c"
"Gamespy/gt2/gt2Utility.c"
"gamespy/hashtable.c"
"gamespy/md5c.c"
"Gamespy/natneg/NATify.c"
"Gamespy/natneg/natneg.c"
"gamespy/nonport.c"
"Gamespy/pt/ptMain.c"
"gamespy/qr2/qr2.c"
"gamespy/qr2/qr2regkeys.c"
"Gamespy/sake/sakeMain.c"
"Gamespy/sake/sakeRequest.c"
"Gamespy/sake/sakeRequestMisc.c"
"Gamespy/sake/sakeRequestModify.c"
"Gamespy/sake/sakeRequestRead.c"
"Gamespy/sc/sciInterface.c"
"Gamespy/sc/sciMain.c"
"Gamespy/sc/sciReport.c"
"Gamespy/sc/sciSerialize.c"
"Gamespy/sc/sciWebServices.c"
"gamespy/serverbrowsing/sb_crypt.c"
"gamespy/serverbrowsing/sb_queryengine.c"
"gamespy/serverbrowsing/sb_server.c"
"gamespy/serverbrowsing/sb_serverbrowsing.c"
"gamespy/serverbrowsing/sb_serverlist.c"
"Gamespy/webservices/AuthService.c"
"xrGameSpy.cpp"
"xrGameSpy_ATLAS.cpp"
"xrGameSpy_Available.cpp"
"xrGameSpy_CDKey.cpp"
"xrGameSpy_Core.cpp"
"xrGameSpy_GP.cpp"
"xrGameSpy_HTTP.cpp"
"xrGameSpy_Patching.cpp"
"xrGameSpy_QR2.cpp"
"xrGameSpy_SAKE.cpp"
"xrGameSpy_ServerBrowser.cpp"
)

# Source groups
source_group("kernel" FILES ${XGS_SOURCE_ALL_FILES})

# Apply list
list(APPEND XGS_SRC_FILES ${XGS_SOURCE_ALL_FILES})

# Remove unused files
#list(FILTER XGS_SRC_FILES EXCLUDE REGEX "GameSpy_Patching.cpp$")

# xrGameSpy project
add_library(xrGameSpy SHARED ${XGS_SRC_FILES})
target_precompile_headers(xrGameSpy PRIVATE "stdafx.h")

# Include directories
target_include_directories(xrGameSpy PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrGameSpy PUBLIC "${IXRAY_SDK_INC}")

# project defines 
target_compile_definitions(xrGameSpy PRIVATE _WINDOWS)
target_compile_definitions(xrGameSpy PRIVATE XRGAMESPY_EXPORTS)
target_compile_definitions(xrGameSpy PRIVATE _USRDLL)

add_compile_options(/fp:fast)
target_compile_definitions(xrGameSpy PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrGameSpy PUBLIC xrCore)