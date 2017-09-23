%%%-------------------------------------------------------------------
%%% @author anatoly
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Sep 2017 03:59
%%%-------------------------------------------------------------------
{application, simple_cache, [
  {description, "A  simple caching system"},
  {vsn, "0.1.0"},
  {registered, [sc_sup]},
  {modules, [
    sc_app,
    sc_sup
  ]},
  {applications, [
    kernel,
    stdlib
  ]},
  {mod, {sc_app, []}},
  {env, []}
]}.