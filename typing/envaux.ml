(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jerome Vouillon, projet Cristal, INRIA Rocquencourt          *)
(*           OCaml port by John Malecki and Xavier Leroy                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Env

type error =
    Module_not_found of Path.t

exception Error of error

let env_cache =
  (Hashtbl.create 59 : ((Env.summary * Subst.t), Env.t) Hashtbl.t)

let reset_cache () =
  Hashtbl.clear env_cache;
  Env.reset_cache()

let rec env_from_summary sum subst =
  try
    Hashtbl.find env_cache (sum, subst)
  with Not_found ->
    let env =
      match sum with
        Env_empty ->
          Env.empty
      | Env_value{next; id; desc} ->
          Env.add_value id (Subst.value_description subst desc)
                        (env_from_summary next subst)
      | Env_type{next; id; desc} ->
          Env.add_type ~check:false id
            (Subst.type_declaration subst desc)
            (env_from_summary next subst)
      | Env_extension{next; id; desc} ->
          Env.add_extension ~check:false id
            (Subst.extension_constructor subst desc)
            (env_from_summary next subst)
      | Env_module{next; id; presence; desc} ->
          Env.add_module_declaration ~check:false id presence
            (Subst.module_declaration Keep subst desc)
            (env_from_summary next subst)
      | Env_modtype{next; id; desc} ->
          Env.add_modtype id (Subst.modtype_declaration Keep subst desc)
                          (env_from_summary next subst)
      | Env_class{next; id; desc} ->
          Env.add_class id (Subst.class_declaration subst desc)
                        (env_from_summary next subst)
      | Env_cltype {next; id; desc} ->
          Env.add_cltype id (Subst.cltype_declaration subst desc)
                         (env_from_summary next subst)
      | Env_open{next; path} ->
          let env = env_from_summary next subst in
          let path' = Subst.module_path subst path in
          begin match Env.open_signature Asttypes.Override path' env with
          | Some env -> env
          | None -> assert false
          | exception Not_found -> raise (Error (Module_not_found path'))
          end
      | Env_functor_arg{next=Env_module{next; id; presence; desc};id=id'}
            when Ident.same id id' ->
          Env.add_module_declaration ~check:false
            id presence (Subst.module_declaration Keep subst desc)
            ~arg:true (env_from_summary next subst)
      | Env_functor_arg _ -> assert false
      | Env_constraints{next; constrs} ->
          Path.Map.fold
            (fun path info ->
              Env.add_local_type (Subst.type_path subst path)
                (Subst.type_declaration subst info))
            constrs (env_from_summary next subst)
      | Env_copy_types {next} ->
          let env = env_from_summary next subst in
          Env.make_copy_of_types env env
      | Env_persistent {next; id} ->
          let env = env_from_summary next subst in
          Env.add_persistent_structure id env
      | Env_value_unbound {next; name; reason} ->
          let env = env_from_summary next subst in
          Env.enter_unbound_value name reason env
      | Env_module_unbound {next; name; reason} ->
          let env = env_from_summary next subst in
          Env.enter_unbound_module name reason env
    in
      Hashtbl.add env_cache (sum, subst) env;
      env

let env_of_only_summary env =
  Env.env_of_only_summary env_from_summary env

(* Error report *)

open Format

let report_error ppf = function
  | Module_not_found p ->
      fprintf ppf "@[Cannot find module %a@].@." Printtyp.path p
