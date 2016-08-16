:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for Ciao-Z3").

'$builder_hook'(desc_name('Ciao-Z3')).

% ============================================================================

:- use_module(library(process), [process_call/3]).
:- use_module(library(bundle/paths_extra), [fsR/2]).
:- use_module(ciaobld(ciaoc_aux), [build_libs/2]).

'$builder_hook'(prebuild_nodocs) :-
	aux_call(['install_bin_dist']), % Use binary distribution
%	aux_call(['install_src_dist']), % Use source distribution
	aux_call(['gen_conf']).

'$builder_hook'(build_libraries) :-
	build_libs(ciao_z3, 'src').

'$builder_hook'(install) :- bundleitem_do(only_global_ins(~ciao_z3_desc), ciao_z3, install).

'$builder_hook'(uninstall) :- bundleitem_do(only_global_ins(~ciao_z3_desc), ciao_z3, uninstall).

ciao_z3_desc := [
  lib(ciao_z3, 'src')
].

% ---------------------------------------------------------------------------
% Run tests

:- use_module(ciaobld(ciaoc_aux), [runtests_dir/3]).

'$builder_hook'(test) :- !,
	% TODO: check output programmatically
	runtests_dir(ciao_z3, 'tests', [rtc_entry, dump_output]).

% ---------------------------------------------------------------------------
% (call external scripts and makefiles)

:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).
:- use_module(ciaobld(third_party_install), [third_party_path/2]).

% (will not work in Windows)
aux_sh := ~fsR(bundle_src(ciao_z3)/'Manifest'/'hooks.sh').

aux_call(Args) :- 
	OS = ~get_bundle_flag(core:os),
	Arch = ~get_bundle_flag(core:arch),
	third_party_path(prefix, ThirdParty),
	Env = ['CIAO_OS'=OS, 'CIAO_ARCH'=Arch, 'THIRDPARTY'=ThirdParty],
	process_call(~aux_sh, Args, [env(Env)]).


