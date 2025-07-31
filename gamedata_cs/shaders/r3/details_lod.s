function l_special	(shader, t_base, t_second, t_detail)
	shader:begin	("lod","lod")
			: blend	(false, blend.one, blend.zero)
			: zb	(true,  true)
			: fog	(false)

	shader:dx10stencil	( 	true, cmp_func.always, 
							255 , 255, 
							stencil_op.keep, stencil_op.replace, stencil_op.keep)

	shader:dx10stencil_ref	(1)
--	shader:sampler	("s_base")      :texture	(t_base)
--	shader:sampler	("s_hemi")      :texture	(t_base .. "_nm")
	shader:dx10texture	("s_base", t_base)
	shader:dx10texture	("s_hemi", t_base .. "_nm")

	shader:dx10sampler	("smp_base");
	shader:dx10sampler	("smp_linear");
end
