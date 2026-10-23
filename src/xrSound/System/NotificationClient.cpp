#include "stdafx.h"
#include "NotificationClient.h"

#include "../SoundRender_CoreA.h"

CNotificationClient::CNotificationClient() :
    m_pEnumerator(nullptr)
{
    Start();
}

CNotificationClient::~CNotificationClient() 
{
    Close();
}

inline bool CNotificationClient::Start() 
{
    // Initialize the COM library for the current thread
    HRESULT ihr = CoInitialize(NULL);
    m_bComInitialized = SUCCEEDED(ihr);

    if (m_bComInitialized) {
        // Create the device enumerator
        IMMDeviceEnumerator* pEnumerator;
        HRESULT hr = CoCreateInstance(__uuidof(MMDeviceEnumerator), NULL, CLSCTX_ALL, __uuidof(IMMDeviceEnumerator), (void**)&pEnumerator);
        if (SUCCEEDED(hr)) {
            // Register for device change notifications
            hr = pEnumerator->RegisterEndpointNotificationCallback(this);
            m_pEnumerator = pEnumerator;

            return true;
        }

        CoUninitialize();
        m_bComInitialized = false;
    }

    return false;
}

inline void CNotificationClient::Close() 
{
    // Unregister the device enumerator
    if (m_pEnumerator) {
        m_pEnumerator->UnregisterEndpointNotificationCallback(this);
        m_pEnumerator->Release();
        m_pEnumerator = nullptr;
    }

    // Uninitialize the COM library for the current thread
    if (m_bComInitialized)
    {
        CoUninitialize();
        m_bComInitialized = false;
    }
}

inline STDMETHODIMP_(HRESULT __stdcall) CNotificationClient::OnDeviceAdded(LPCWSTR pwstrDeviceId) 
{
    // A new audio device has been added.
    return S_OK;
}


// IMMCNotificationClient methods

inline STDMETHODIMP_(HRESULT __stdcall) CNotificationClient::OnDefaultDeviceChanged(EDataFlow flow, ERole role, LPCWSTR pwstrDeviceId)
{
    if (flow == eRender && role == eConsole && SoundRenderA != nullptr)
    {
        SoundRenderA->bDefaultDeviceChanged = true;
    }

    return S_OK;
}

inline STDMETHODIMP_(HRESULT __stdcall) CNotificationClient::OnDeviceRemoved(LPCWSTR pwstrDeviceId)
{
    // An audio device has been removed.
    return S_OK;
}

inline STDMETHODIMP_(HRESULT __stdcall) CNotificationClient::OnDeviceStateChanged(LPCWSTR pwstrDeviceId, DWORD dwNewState) 
{
    // The state of an audio device has changed.
    return S_OK;
}

inline STDMETHODIMP_(HRESULT __stdcall) CNotificationClient::OnPropertyValueChanged(LPCWSTR pwstrDeviceId, const PROPERTYKEY key) 
{
    // A property value of an audio device has changed.
    return S_OK;
}
