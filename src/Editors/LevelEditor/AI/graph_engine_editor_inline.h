////////////////////////////////////////////////////////////////////////////
//	Module 		: graph_engine_inline.h
//	Created 	: 21.03.2002
//  Modified 	: 03.03.2004
//	Author		: Dmitriy Iassenev
//	Description : Graph engine inline functions
////////////////////////////////////////////////////////////////////////////

#pragma once

IC	CGraphEngineEditor::CGraphEngineEditor		(u32 max_vertex_count)
{
	m_algorithm			= xr_new<CAlgorithm>				(max_vertex_count);
	m_algorithm->data_storage().set_min_bucket_value		(_dist_type(0));
	m_algorithm->data_storage().set_max_bucket_value		(_dist_type(2000));

}

IC	CGraphEngineEditor::~CGraphEngineEditor			()
{
	xr_delete			(m_algorithm);
}


template <
	typename _Graph,
	typename _Parameters
>
IC	bool CGraphEngineEditor::search		(
		const _Graph			&graph, 
		const _index_type		&start_node, 
		const _index_type		&dest_node, 
		xr_vector<_index_type>	*node_path,
		const _Parameters		&parameters
	)
{
	typedef CPathManager<_Graph, CAlgorithm::CDataStorage, _Parameters, _dist_type,_index_type,_iteration_type>	CPathManagerGeneric;

	CPathManagerGeneric			path_manager;

	path_manager.setup			(
		&graph,
		&m_algorithm->data_storage(),
		node_path,
		start_node,
		dest_node,
		parameters
	);
	
	bool						successfull = m_algorithm->find(path_manager);

	return						(successfull);
}

template <
	typename _Graph,
	typename _Parameters
>
IC	bool CGraphEngineEditor::search			(
		const _Graph			&graph, 
		const _index_type		&start_node, 
		const _index_type		&dest_node, 
		xr_vector<_index_type>	*node_path,
		_Parameters				&parameters
	)
{
	typedef CPathManager<_Graph, CAlgorithm::CDataStorage, _Parameters, _dist_type,_index_type,_iteration_type>	CPathManagerGeneric;

	CPathManagerGeneric			path_manager;

	path_manager.setup			(
		&graph,
		&m_algorithm->data_storage(),
		node_path,
		start_node,
		dest_node,
		parameters
	);
	
	bool						successfull = m_algorithm->find(path_manager);

	return						(successfull);
}

template <
	typename _Graph,
	typename _Parameters,
	typename _PathManager
>
IC	bool CGraphEngineEditor::search			(
		const _Graph			&graph, 
		const _index_type		&start_node, 
		const _index_type		&dest_node, 
		xr_vector<_index_type>	*node_path,
		const _Parameters		&parameters,
		_PathManager			&path_manager
	)
{
	path_manager.setup			(
		&graph,
		&m_algorithm->data_storage(),
		node_path,
		start_node,
		dest_node,
		parameters
	);
	
	bool						successfull = m_algorithm->find(path_manager);

	return						(successfull);
}
