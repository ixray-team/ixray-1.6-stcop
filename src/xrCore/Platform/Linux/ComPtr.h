#pragma once
#include <atomic>
#include <cassert>

template<typename T>
class ComPtr
{
public:
    ComPtr() noexcept : ptr_(nullptr) {}
    
    ComPtr(std::nullptr_t) noexcept : ptr_(nullptr) {}
    
    explicit ComPtr(T* ptr) noexcept : ptr_(ptr) {
    }
    
    ComPtr(const ComPtr& other) noexcept : ptr_(other.ptr_) {
    }
    
    ComPtr(ComPtr&& other) noexcept : ptr_(other.ptr_) {
        other.ptr_ = nullptr;
    }
    
    ~ComPtr() noexcept {
        if (ptr_) ptr_->Release();
    }
    
    ComPtr& operator=(std::nullptr_t) noexcept {
        Reset();
        return *this;
    }
    
    ComPtr& operator=(T* ptr) noexcept {
        if (ptr_ != ptr) {
            if (ptr) ptr->AddRef();
            if (ptr_) ptr_->Release();
            ptr_ = ptr;
        }
        return *this;
    }
    
    ComPtr& operator=(const ComPtr& other) noexcept {
        if (this != &other) {
            if (other.ptr_) other.ptr_->AddRef();
            if (ptr_) ptr_->Release();
            ptr_ = other.ptr_;
        }
        return *this;
    }
    
    ComPtr& operator=(ComPtr&& other) noexcept {
        if (this != &other) {
            if (ptr_) ptr_->Release();
            ptr_ = other.ptr_;
            other.ptr_ = nullptr;
        }
        return *this;
    }
    
    T* Get() const noexcept {
        return ptr_;
    }
    
    T* operator->() const noexcept {
        assert(ptr_ != nullptr);
        return ptr_;
    }
    
    T& operator*() const noexcept {
        assert(ptr_ != nullptr);
        return *ptr_;
    }
    
    explicit operator bool() const noexcept {
        return ptr_ != nullptr;
    }
    
    bool operator==(std::nullptr_t) const noexcept {
        return ptr_ == nullptr;
    }
    
    bool operator!=(std::nullptr_t) const noexcept {
        return ptr_ != nullptr;
    }
    
    bool operator==(const ComPtr& other) const noexcept {
        return ptr_ == other.ptr_;
    }
    
    void Reset() noexcept {
        if (ptr_) {
            ptr_->Release();
            ptr_ = nullptr;
        }
    }
    
    void Reset(T* ptr) noexcept {
        if (ptr_ != ptr) {
            if (ptr) ptr->AddRef();
            if (ptr_) ptr_->Release();
            ptr_ = ptr;
        }
    }
    
    T* Release() noexcept {
        T* temp = ptr_;
        ptr_ = nullptr;
        return temp;
    }
    
    void Attach(T* ptr) noexcept {
        if (ptr_) ptr_->Release();
        ptr_ = ptr;
    }
    
    T* Detach() noexcept {
        T* temp = ptr_;
        ptr_ = nullptr;
        return temp;
    }
    
    T** GetAddressOf() noexcept {
        assert(ptr_ == nullptr);
        return &ptr_;
    }
    
    T** operator&() noexcept {
        return GetAddressOf();
    }
    
    bool IsValid() const noexcept {
        return ptr_ != nullptr;
    }
    
private:
    T* ptr_;
};