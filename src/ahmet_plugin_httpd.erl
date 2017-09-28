-module(ahmet_plugin_httpd).

-export([handle_req/1, handle_db_req/2]).

-include_lib("couch/include/couch_db.hrl").

handle_req(#httpd{method = 'GET'} = Req) ->
  couch_httpd:send_json(Req, {[
    {<<"name">>, <<"ahmet">>},
    {<<"surname">>, <<"turk">>},
    {<<"age">>, <<"23">>}
  ]});

handle_req(Req) ->
  couch_httpd:send_method_not_allowed(Req, "GET").

handle_db_req(#httpd{method = 'GET'} = Req, Db) ->
  chttpd:send_method_not_allowed(Req, "POST,HEAD").