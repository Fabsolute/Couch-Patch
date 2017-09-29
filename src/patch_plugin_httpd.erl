-module(patch_plugin_httpd).

-export([handle_req/2]).

-include_lib("couch/include/couch_db.hrl").

handle_req(#httpd{
  method = 'PATCH',
  path_parts = [_DbName, _Patch, DocId]
} = Req, Db) ->
  couch_httpd:validate_ctype(Req, "application/json"),
  {Obj} = couch_httpd:json_body_obj(Req),
  PatchData = proplists:get_value(<<"ops">>, Obj),

  Doc = couch_httpd_db:couch_doc_open(Db, DocId, nil, [ejson_body]),
  {ok, ParsedPatch} = patch_plugin_json_path:parse(PatchData),
  CDoc = couch_doc:to_json_obj(Doc, [{user_ctx, Req#httpd.user_ctx}]),
  {ok, PatchedData} = patch_plugin_json_path:apply(ParsedPatch, CDoc),
  {ok, NewRev} = couch_db:update_doc(Db, couch_doc:from_json_obj({PatchedData}), [], interactive_edit),
  couch_httpd:send_json(Req, {[
    {<<"doc">>, CDoc},
    {<<"patch_data">>, PatchData},
    {<<"patch">>, {PatchedData}}
  ]});

handle_req(Req, _Db) ->
  couch_httpd:send_method_not_allowed(Req, "PATCH").
