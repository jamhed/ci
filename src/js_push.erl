-module(js_push).
-import(typecast, [a2b/1]).
-export([
	repo_name/1,
	repo_id/1,
	repo_url/1,
	commit_sha/1,
	sender_id/1,
	sender_login/1
]).

repo_name(Data) -> path([repository, full_name], Data).
repo_id(Data) -> path([repository, id], Data).
repo_url(Data) -> path([repository, url], Data).
commit_sha(Data) -> path(['after'], Data).
sender_id(Data) -> path([sender, id], Data).
sender_login(Data) -> path([sender, login], Data).

path(_, undefined) -> undefined;
path([], M) -> M;
path([K|Rest], M) -> path(Rest, maps:get(a2b(K), M, undefined)).
