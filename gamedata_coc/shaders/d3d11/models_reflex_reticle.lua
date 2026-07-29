function normal(shader, t_base, t_second, t_detail)
	shader:begin("models_reflex_reticle", "models_reflex_reticle")

	: zb(true, false)
	: sorting(2, true)
	: blend(true, blend.one, blend.srcalpha)
	: aref(true, 0)

	shader:dx10texture("s_base", t_base)
	shader:dx10sampler("smp_rtlinear")
end
