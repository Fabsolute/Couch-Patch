-module(patch_plugin_httpd).

-export([handle_req/2]).

-include_lib("couch/include/couch_db.hrl").

handle_req(#httpd{
  method = 'PATCH',
  path_parts = [_DbName, _Patch, DocId]
} = Req, Db) ->
  couch_httpd:validate_ctype(Req, "application/json"),
  Body = couch_httpd:json_body_obj(Req),
  Doc = couch_httpd_db:couch_doc_open(Db, DocId, nil, [ejson_body]),

  couch_httpd:send_json(Req, {[
    {<<"doc">>, couch_doc:to_json_obj(Doc,[{user_ctx, Req#httpd.user_ctx}])},
    {<<"body">>, Body}
  ]});

handle_req(Req, _Db) ->
  couch_httpd:send_method_not_allowed(Req, "PATCH").
