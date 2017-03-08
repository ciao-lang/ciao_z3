:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title, "Bundle Hooks for Ciao-Z3").

:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(ciaobld(builder_aux), [third_party_aux/3]).

'$builder_hook'(prepare_build_bin) :-
	third_party_aux(ciao_z3, z3, ['install_bin_dist']), % Use binary distribution
%	third_party_aux(ciao_z3, z3, ['install_src_dist']), % Use source distribution
	Conf = ~bundle_path(ciao_z3, 'src/ciao_z3_config_auto.pl'),
	third_party_aux(ciao_z3, z3, ['gen_conf', Conf]).

'$builder_hook'(lib('src')).

% ---------------------------------------------------------------------------
% Run tests

:- use_module(ciaobld(ciaoc_aux), [runtests_dir/3]).

'$builder_hook'(test) :- !,
	% TODO: check output programmatically
	runtests_dir(ciao_z3, 'tests', [rtc_entry, dump_output]).
