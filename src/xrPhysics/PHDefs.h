#ifndef PHDEFS_H
#define PHDEFS_H
class CPHElement;
class CPHJoint;
class CPhysicsShell;

//class CPHFracture;
class CShellSplitInfo;

using shell_root = std::pair<CPhysicsShell*,u16>;

using ELEMENT_STORAGE = xr_vector<CPHElement*>;
using ELEMENT_I = ELEMENT_STORAGE::iterator;
using ELEMENT_CI = ELEMENT_STORAGE::const_iterator;
using ELEMENT_RI = ELEMENT_STORAGE::reverse_iterator;

using JOINT_STORAGE = xr_vector<CPHJoint*>;
using JOINT_I = JOINT_STORAGE::iterator;

using PHSHELL_PAIR_VECTOR = xr_vector<shell_root>;
using SHELL_PAIR_I = PHSHELL_PAIR_VECTOR::iterator;
using SHELL_PAIR_RI = PHSHELL_PAIR_VECTOR::reverse_iterator;

#endif