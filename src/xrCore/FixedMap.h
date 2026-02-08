#ifndef _FIXEDMAP_H
#define _FIXEDMAP_H
#pragma once

template<class K, class T, class allocator = xr_allocator>
class FixedMAP {
	enum	{
		SG_REALLOC_ADVANCE	= 64
	};
public:
	struct TNode {
		K		key;
		T		val;
		TNode	*left,*right;
	};
	typedef void __fastcall callback	(TNode*);
	typedef bool __fastcall callback_cmp(TNode& N1, TNode& N2);

private:
	TNode*		nodes;
	u32			pool;
	u32			limit;

	ICF u32	Size(u32 Count)
	{	return Count*sizeof(TNode);	}

	ICF void		Realloc()
	{
		u32	newLimit = limit + SG_REALLOC_ADVANCE;
		VERIFY(newLimit%SG_REALLOC_ADVANCE == 0);
		TNode*	newNodes = (TNode*)allocator::alloc(sizeof(TNode)*newLimit);
		VERIFY(newNodes);

		ZeroMemory(newNodes, Size(newLimit));
		if (limit) CopyMemory	(newNodes, nodes, Size(limit));

		for (u32 I=0; I<pool; I++)
		{
			VERIFY	(nodes);
			TNode*	Nold	= nodes	+ I;
			TNode*	Nnew	= newNodes + I;

			if (Nold->left) {
				size_t	Lid		= Nold->left  - nodes;
				Nnew->left		= newNodes + Lid;
			}
			if (Nold->right) {
				size_t	Rid		= Nold->right - nodes;
				Nnew->right		= newNodes + Rid;
			}
		}
		if (nodes) allocator::dealloc(nodes);

		nodes = newNodes;
		limit = newLimit;
	}

	ICF TNode*	Alloc		(const K& key)
	{
		if (pool==limit) Realloc();
		TNode *node = nodes + pool;
		node->key	= key;
		node->right = node->left = 0;
		pool++		;
		return node	;
	}
	ICF TNode*	CreateChild	(TNode* &parent, const K& key)
	{
		size_t	PID	= size_t(parent-nodes);
		TNode*	N	= Alloc	(key);
		parent		= nodes+PID;
		return	N;
	}

	ICF void		recurseLR	(TNode* N, callback CB)
	{
		if (N->left)	recurseLR(N->left,CB);
		CB(N);
		if (N->right)	recurseLR(N->right,CB);
	}
	ICF void		recurseRL	(TNode* N, callback CB)
	{
		if (N->right)	recurseRL(N->right,CB);
		CB(N);
		if (N->left)	recurseRL(N->left,CB);
	}
	ICF void		getLR		(TNode* N, xr_vector<T,typename allocator::template helper<T>::result>&	D)
	{
		if (N->left)	getLR(N->left,D);
		D.push_back		(N->val);
		if (N->right)	getLR(N->right,D);
	}
	ICF void		getRL		(TNode* N, xr_vector<T,typename allocator::template helper<T>::result>&	D)
	{
		if (N->right)	getRL(N->right,D);
		D.push_back		(N->val);
		if (N->left)	getRL(N->left,D);
	}
	ICF void		getLR_P		(TNode* N, xr_vector<TNode*,typename allocator::template helper<TNode*>::result>& D)
	{
		if (N->left)	getLR_P(N->left,D);
		D.push_back		(N);
		if (N->right)	getLR_P(N->right,D);
	}
	ICF void		getRL_P		(TNode* N, xr_vector<TNode*,typename allocator::template helper<TNode*>::result>& D)
	{
		if (N->right)	getRL_P(N->right,D);
		D.push_back		(N);
		if (N->left)	getRL_P(N->left,D);
	}
public:
	FixedMAP() {
		pool	= 0;
		limit	= 0;
		nodes	= 0; 
	}
	~FixedMAP() {
		destroy	();
	}
	ICF void		destroy()
	{
		if (nodes) {
			for (TNode* cur = begin(); cur!=last(); cur++)
				cur->~TNode();
			allocator::dealloc(nodes);
		}
	}
	ICF TNode*	insert(const K& k) {
		if (pool) {
			TNode*	node = nodes;

			once_more:
			if (k < node->key) {
				if (node->left) {
					node = node->left;
					goto once_more;
				} else {
					TNode* N = CreateChild(node,k);
					node->left = N;
					return N;
				}
			} else if (k > node->key) {
				if (node->right) {
					node = node->right;
					goto once_more;
				} else {
					TNode* N = CreateChild(node,k);
					node->right = N;
					return N;
				}
			} else return node;
			
		} else {
			return Alloc(k);
		}
	}
	ICF TNode*	insertInAnyWay(const K& k) {
		if (pool) {
			TNode*	node = nodes;

			once_more:
			if (k <= node->key) {
				if (node->left) {
					node = node->left;
					goto once_more;
				} else {
					TNode* N = CreateChild(node,k);
					node->left = N;
					return N;
				}
			} else {
				if (node->right) {
					node = node->right;
					goto once_more;
				} else {
					TNode* N = CreateChild(node,k);
					node->right = N;
					return N;
				}
			}
		} else {
			return Alloc(k);
		}
	}
	ICF TNode*	insert		(const K& k, const T& v)
	{
		TNode*	N	= insert(k);
		N->val		= v;
		return	N;
	}
	ICF TNode*	insertInAnyWay(const K& k, const T& v)
	{
		TNode*	N	= insertInAnyWay(k);
		N->val		= v;
		return	N;
	}
	ICF void		discard()	{ if (nodes) allocator::dealloc(nodes); nodes = 0; pool=0; limit=0;	}
	ICF u32		allocated()	{ return this->limit;				}
	ICF void		clear()		{ pool=0;				}
	ICF TNode*	begin()		{ return nodes;			}
	ICF TNode*	end()		{ return nodes+pool;	}
	ICF TNode*	last()		{ return nodes+limit;	}	// for setup only
	ICF u32		size()		{ return pool;			}
	ICF TNode&	operator[] (int v) { return nodes[v]; }

	ICF void		traverseLR	(callback CB)
	{ if (pool) recurseLR(nodes,CB);  }
	ICF void		traverseRL	(callback CB)
	{ if (pool) recurseRL(nodes,CB);  }
	ICF void		traverseANY	(callback CB) {
		TNode*	_end = end();
		for (TNode* cur = begin(); cur!=_end; cur++)
			CB(cur);
	}

	ICF void		getLR		(xr_vector<T,typename allocator::template helper<T>::result>&	D)
	{ if (pool)	getLR(nodes,D); }
	ICF void		getLR_P		(xr_vector<TNode*,typename allocator::template helper<TNode*>::result>&	D)
	{ if (pool)	getLR_P(nodes,D); }
	ICF void		getRL		(xr_vector<T,typename allocator::template helper<T>::result>&	D)
	{ if (pool)	getRL(nodes,D); }
	ICF void		getRL_P		(xr_vector<TNode*,typename allocator::template helper<TNode*>::result>&	D)
	{ if (pool)	getRL_P(nodes,D); }
	ICF void		getANY		(xr_vector<T,typename allocator::template helper<T>::result>&	D)
	{
		TNode*	_end = end();
		for (TNode* cur = begin(); cur!=_end; cur++) D.push_back(cur->val);
	}
	ICF void		getANY_P	(xr_vector<TNode*,typename allocator::template helper<TNode*>::result>&	D)
	{
		D.resize			(size());
		TNode** _it			= &*D.begin();
		TNode*	_end		= end();
		for (TNode* cur = begin(); cur!=_end; cur++,_it++) *_it = cur;
	}
	ICF void		getANY_P	(xr_vector<void*,typename allocator::template helper<void*>::result>&	D)
	{
		D.resize			(size());
		void** _it			= &*D.begin();
		TNode*	_end		= end();
		for (TNode* cur = begin(); cur!=_end; cur++,_it++) *_it = cur;
	}
	ICF void		setup(callback CB) {
		for (int i=0; i<limit; i++)
			CB(nodes+i);
	}
};
#endif