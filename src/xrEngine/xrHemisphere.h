#ifndef xrHemisphereH
#define xrHemisphereH

typedef void 		xrHemisphereIterator(float x, float y, float z, float energy, LPVOID param);

void	ENGINE_API	xrHemisphereBuild		(int quality, float energy, xrHemisphereIterator* it, LPVOID param);
int		ENGINE_API	xrHemisphereVertices	(int quality, const Fvector*& verts);
int		ENGINE_API	xrHemisphereIndices		(int quality, const u16*& indices);

#endif //xrHemisphereH
