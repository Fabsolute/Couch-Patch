-module(patch_plugin_httpd).

-export([handle_req/2]).

-include_lib("couch/include/couch_db.hrl").

handle_req(#httpd{
  method = 'PATCH',
  path_parts = [_DbName, _Patch, DocId],
  user_ctx = UserCTX
} = Req, Db) ->
  couch_httpd:validate_ctype(Req, "application/json"),
  {Body} = couch_httpd:json_body_obj(Req),
  Ops = proplists:get_value(<<"ops">>, Body),

  CouchDocument = couch_httpd_db:couch_doc_open(Db, DocId, nil, [ejson_body]),
  Doc = couch_doc:to_json_obj(CouchDocument, [{user_ctx, UserCTX}]),
  PatchedDocument = patch(Doc, Ops),
  NewCouchDoc = couch_doc:from_json_obj(PatchedDocument),
  Response = couch_db:update_doc(Db, NewCouchDoc, []),
  couch_httpd:send_json(Req, couch_httpd_db:update_doc_result_to_json(DocId, Response));

handle_req(Req, _Db) ->
  couch_httpd:send_method_not_allowed(Req, "PATCH").

patch(Doc, RawOps) ->
  {ok, Ops} = patch_plugin:parse_ops(RawOps),
  {ok, PatchedDoc} = patch_plugin:apply_patch(Ops, Doc),
  PatchedDoc.
