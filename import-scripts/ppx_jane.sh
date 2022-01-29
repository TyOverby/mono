#!/usr/bin/env bash
source "$(git rev-parse --show-toplevel)/import-scripts/util.sh"

get janestreet ppx_pattern_bind master
get janestreet ppx_css master
get janestreet ppx_typed_fields master
get janestreet record_builder master
get janestreet core_bench master

get janestreet textutils_kernel master
get mirage ocaml-uri master
get janestreet expect_test_helpers_core master
get janestreet timezone master
get janestreet protocol_version_header master
get janestreet sexp_pretty master
get janestreet base_bigstring master
get janestreet int_repr master
get janestreet ppx_jane master
get janestreet ppx_typerep_conv master
get janestreet ppx_string master
get janestreet ppx_stable master
get janestreet ppx_optional master
get janestreet ppx_module_timer master
get janestreet ppx_log master
get janestreet ppx_fixed_literal master
get janestreet ppx_expect master
get janestreet ppx_disable_unused_warnings master
get janestreet ppx_bin_prot master
get janestreet bin_prot master
get janestreet base_quickcheck master
get janestreet typerep master
get janestreet bin_prot master
get janestreet splittable_random master
get janestreet ppx_sexp_value master
get janestreet ppx_let master
get janestreet parsexp master
get janestreet ppx_variants_conv master
get janestreet ppx_fields_conv master
get janestreet ppx_custom_printf master
get janestreet ppx_sexp_message master
get janestreet ppx_bench master
get janestreet variantslib master
get janestreet fieldslib master
get janestreet ppx_inline_test master
get janestreet time_now master
get janestreet ppx_optcomp master
get janestreet ppx_base master
get janestreet jst-config master
get janestreet stdio master
get janestreet ppx_hash master
get janestreet ppx_enumerate master
get janestreet ppx_assert master
get janestreet ppx_sexp_conv master
get janestreet ppx_here master
get janestreet ppx_compare master
get janestreet ppx_cold master
get janestreet ppx_ignore_instrumentation master
get janestreet ppx_pipebang master

get janestreet sexplib master
rm -rf num
