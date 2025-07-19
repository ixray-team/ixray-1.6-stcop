#ifndef _FIXEDMAP_H
#define _FIXEDMAP_H
#pragma once

template<class K, class T, class allocator = xr_allocator>
class FixedMAP {
    enum {
        SG_REALLOC_ADVANCE = 64
    };
public:
    struct TNode {
        K key;
        T val;
        TNode* left, * right;
    };
    typedef void __fastcall callback(TNode*);
    typedef bool __fastcall callback_cmp(TNode& N1, TNode& N2);

private:
    TNode* nodes;
    u32 pool;
    u32 limit;

    IC u32 Size(u32 Count) {
        return Count * sizeof(TNode);
    }

    void Realloc() {
        u32 newLimit = limit + SG_REALLOC_ADVANCE;
        VERIFY(newLimit % SG_REALLOC_ADVANCE == 0);

        // Выделяем новую память
        TNode* newNodes = (TNode*)allocator::alloc(sizeof(TNode) * newLimit);
        VERIFY(newNodes);

        // Инициализируем новую память
        ZeroMemory(newNodes, Size(newLimit));

        // Копируем только используемые узлы (pool вместо limit)
        if (pool > 0) {
            CopyMemory(newNodes, nodes, Size(pool));

            // Обновляем указатели только для используемых узлов
            for (u32 I = 0; I < pool; I++) {
                TNode* Nold = nodes + I;
                TNode* Nnew = newNodes + I;

                if (Nold->left) {
                    size_t Lid = Nold->left - nodes;
                    Nnew->left = newNodes + Lid;
                }
                if (Nold->right) {
                    size_t Rid = Nold->right - nodes;
                    Nnew->right = newNodes + Rid;
                }
            }
        }

        // Освобождаем старую память
        if (nodes) {
            allocator::dealloc(nodes);
        }

        // Обновляем указатели
        nodes = newNodes;
        limit = newLimit;
    }

    IC TNode* Alloc(const K& key) {
        if (pool == limit) Realloc();
        TNode* node = nodes + pool;
        node->key = key;
        node->right = node->left = nullptr;
        pool++;
        return node;
    }

    IC TNode* CreateChild(TNode*& parent, const K& key) {
        size_t PID = size_t(parent - nodes);
        TNode* N = Alloc(key);
        parent = nodes + PID;
        return N;
    }

    IC void recurseLR(TNode* N, callback CB) {
        if (N->left) recurseLR(N->left, CB);
        CB(N);
        if (N->right) recurseLR(N->right, CB);
    }

    IC void recurseRL(TNode* N, callback CB) {
        if (N->right) recurseRL(N->right, CB);
        CB(N);
        if (N->left) recurseRL(N->left, CB);
    }

    IC void getLR(TNode* N, xr_vector<T, typename allocator::template helper<T>::result>& D) {
        if (N->left) getLR(N->left, D);
        D.push_back(N->val);
        if (N->right) getLR(N->right, D);
    }

    IC void getRL(TNode* N, xr_vector<T, typename allocator::template helper<T>::result>& D) {
        if (N->right) getRL(N->right, D);
        D.push_back(N->val);
        if (N->left) getRL(N->left, D);
    }

    IC void getLR_P(TNode* N, xr_vector<TNode*, typename allocator::template helper<TNode*>::result>& D) {
        if (N->left) getLR_P(N->left, D);
        D.push_back(N);
        if (N->right) getLR_P(N->right, D);
    }

    IC void getRL_P(TNode* N, xr_vector<TNode*, typename allocator::template helper<TNode*>::result>& D) {
        if (N->right) getRL_P(N->right, D);
        D.push_back(N);
        if (N->left) getRL_P(N->left, D);
    }

public:
    FixedMAP() : pool(0), limit(0), nodes(nullptr) {}
    ~FixedMAP() { destroy(); }

    void destroy() {
        if (nodes) {
            for (TNode* cur = begin(); cur != end(); cur++)
                cur->~TNode();
            allocator::dealloc(nodes);
        }
        pool = 0;
        limit = 0;
        nodes = 0;
    }

    IC TNode* insert(const K& k) {
        if (pool == 0) {
            return Alloc(k);
        }

        TNode* node = nodes;
        while (true) {
            if (k < node->key) {
                if (node->left) {
                    node = node->left;
                }
                else {
                    TNode* N = CreateChild(node, k);
                    node->left = N;
                    return N;
                }
            }
            else if (k > node->key) {
                if (node->right) {
                    node = node->right;
                }
                else {
                    TNode* N = CreateChild(node, k);
                    node->right = N;
                    return N;
                }
            }
            else {
                return node;
            }
        }
    }

    IC TNode* insertInAnyWay(const K& k) {
        if (pool == 0) {
            return Alloc(k);
        }

        TNode* node = nodes;
        while (true) {
            if (k <= node->key) {
                if (node->left) {
                    node = node->left;
                }
                else {
                    TNode* N = CreateChild(node, k);
                    node->left = N;
                    return N;
                }
            }
            else {
                if (node->right) {
                    node = node->right;
                }
                else {
                    TNode* N = CreateChild(node, k);
                    node->right = N;
                    return N;
                }
            }
        }
    }

    IC void insertInAnyWay(const K& k, const T&& val) {
        if (pool == 0) {
            Alloc(k)->val = val;
            return;
        }

        TNode* node = nodes;
        while (true) {
            if (k <= node->key) {
                if (node->left) {
                    node = node->left;
                }
                else {
                    TNode* N = CreateChild(node, k);
                    node->left = N;
                    N->val = val;
                    return;
                }
            }
            else {
                if (node->right) {
                    node = node->right;
                }
                else {
                    TNode* N = CreateChild(node, k);
                    node->right = N;
                    N->val = val;
                    return;
                }
            }
        }
    }

    IC TNode* find(const K& key) {
        if (pool == 0) return nullptr;

        TNode* node = nodes;
        while (node != nullptr) {
            if (key < node->key) {
                node = node->left;
            }
            else if (key > node->key) {
                node = node->right;
            }
            else {
                return node;
            }
        }
        return nullptr;
    }

    IC TNode* insert(const K& k, const T& v) {
        TNode* N = insert(k);
        N->val = v;
        return N;
    }

    IC TNode* insertInAnyWay(const K& k, const T& v) {
        TNode* N = insertInAnyWay(k);
        N->val = v;
        return N;
    }

    IC void discard() {
        if (nodes) allocator::dealloc(nodes);
        nodes = nullptr;
        pool = 0;
        limit = 0;
    }

    IC u32 allocated() { return limit; }
    IC void clear() { pool = 0; }
    IC TNode* last() { return nodes + limit; } // for setup only
    IC u32 size() { return pool; }
    IC TNode& operator[] (int v) { return nodes[v]; }

    IC void traverseLR(callback CB) {
        if (pool) recurseLR(nodes, CB);
    }

    IC void traverseRL(callback CB) {
        if (pool) recurseRL(nodes, CB);
    }

    IC void traverseANY(callback CB) {
        TNode* _end = end();
        for (TNode* cur = begin(); cur != _end; cur++)
            CB(cur);
    }

    IC void getLR(xr_vector<T, typename allocator::template helper<T>::result>& D) {
        if (pool) getLR(nodes, D);
    }

    IC void getLR_P(xr_vector<TNode*, typename allocator::template helper<TNode*>::result>& D) {
        if (pool) getLR_P(nodes, D);
    }

    IC void getRL(xr_vector<T, typename allocator::template helper<T>::result>& D) {
        if (pool) getRL(nodes, D);
    }

    IC void getRL_P(xr_vector<TNode*, typename allocator::template helper<TNode*>::result>& D) {
        if (pool) getRL_P(nodes, D);
    }

    IC void getANY(xr_vector<T, typename allocator::template helper<T>::result>& D) {
        TNode* _end = end();
        for (TNode* cur = begin(); cur != _end; cur++) D.push_back(cur->val);
    }

    IC void getANY_P(xr_vector<TNode*, typename allocator::template helper<TNode*>::result>& D) {
        D.resize(size());
        TNode** _it = &*D.begin();
        TNode* _end = end();
        for (TNode* cur = begin(); cur != _end; cur++, _it++) *_it = cur;
    }

    IC void getANY_P(xr_vector<void*, typename allocator::template helper<void*>::result>& D) {
        D.resize(size());
        void** _it = &*D.begin();
        TNode* _end = end();
        for (TNode* cur = begin(); cur != _end; cur++, _it++) *_it = cur;
    }

    IC void setup(callback CB) {
        for (int i = 0; i < limit; i++)
            CB(nodes + i);
    }

    IC TNode* begin() {
        VERIFY(nodes);
        return nodes;
    }

    IC TNode* end() {
        VERIFY(nodes + pool >= nodes);
        return nodes + pool;
    }
};

template<class K, class allocator = xr_allocator>
class FixedSet : public FixedMAP<K, void*, allocator> {
    // Переопределить необходимые методы
};
#endif