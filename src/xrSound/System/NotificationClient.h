#pragma once
#include <mmdeviceapi.h>
#include <endpointvolume.h>

class CNotificationClient : 
    public IMMNotificationClient 
{
public:
    CNotificationClient();
    ~CNotificationClient();

    // IUnknown methods
    STDMETHOD(QueryInterface)(REFIID riid, void** ppvObject) {
        if (riid == IID_IUnknown || riid == __uuidof(IMMNotificationClient)) {
            *ppvObject = static_cast<IMMNotificationClient*>(this);
            AddRef();
            return S_OK;
        }
        return E_NOINTERFACE;
    }

    ULONG STDMETHODCALLTYPE AddRef() {
        return InterlockedIncrement(&m_cRef);
    }

    ULONG STDMETHODCALLTYPE Release() {
        ULONG ulRef = InterlockedDecrement(&m_cRef);
        if (0 == ulRef) {
            delete this;
        }
        return ulRef;
    }

    // IMMNotificationClient methods
    STDMETHOD(OnDefaultDeviceChanged)(EDataFlow flow, ERole role, LPCWSTR pwstrDeviceId);
    STDMETHOD(OnDeviceRemoved)(LPCWSTR pwstrDeviceId);
    STDMETHOD(OnDeviceAdded)(LPCWSTR pwstrDeviceId);
    STDMETHOD(OnDeviceStateChanged)(LPCWSTR pwstrDeviceId, DWORD dwNewState);
    STDMETHOD(OnPropertyValueChanged)(LPCWSTR pwstrDeviceId, const PROPERTYKEY key);

private:
    bool Start();
    void Close();

private:
    LONG m_cRef;
    IMMDeviceEnumerator* m_pEnumerator;
};