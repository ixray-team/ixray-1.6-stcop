#pragma once

namespace Platform
{
	class XRCORE_API CMutexHandle
	{
	public:
		CMutexHandle() : m_handle(nullptr) {}
	
		explicit CMutexHandle(HANDLE handle) : m_handle(handle) {}
	
		~CMutexHandle()
		{
			Close();
		}
	
		// Non-copyable
		CMutexHandle(const CMutexHandle&) = delete;
		CMutexHandle& operator=(const CMutexHandle&) = delete;
	
		// Move semantics
		CMutexHandle(CMutexHandle&& other) noexcept
			: m_handle(other.m_handle)
		{
			other.m_handle = nullptr;
		}
	
		CMutexHandle& operator=(CMutexHandle&& other) noexcept
		{
			if (this != &other)
			{
				Close();
				m_handle = other.m_handle;
				other.m_handle = nullptr;
			}
			return *this;
		}
	
		bool IsValid() const noexcept { return m_handle != nullptr && m_handle != INVALID_HANDLE_VALUE; }
	
		HANDLE Get() const { return m_handle; }
		HANDLE* GetAddressOf() { return &m_handle; }
	
		void Close()
		{
			if (IsValid())
			{
				CloseHandle(m_handle);
				m_handle = nullptr;
			}
		}
	
		void Reset(HANDLE handle = nullptr)
		{
			Close();
			m_handle = handle;
		}
	
		// Release ownership without closing
		HANDLE Release()
		{
			HANDLE temp = m_handle;
			m_handle = nullptr;
			return temp;
		}
	
	private:
		HANDLE m_handle;
	};
}