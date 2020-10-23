%Copyright 2019 Vadim Pavlov ioc2rpz[at]gmail[.]com
%
%Licensed under the Apache License, Version 2.0 (the "License");
%you may not use this file except in compliance with the License.
%You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
%Unless required by applicable law or agreed to in writing, software
%distributed under the License is distributed on an "AS IS" BASIS,
%WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%See the License for the specific language governing permissions and
%limitations under the License.
%%%-------------------------------------------------------------------
%% @doc piDNS public API
%% @end
%%%-------------------------------------------------------------------

-module(piDNS_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Import headers
-include_lib("piDNS.hrl").

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    IPv4=piDNS_fun:get_env(piDNS, ipv4, ""),
    IPv6=piDNS_fun:get_env(piDNS, ipv6, ""),
    Conf_File=piDNS_fun:get_env(piDNS, piDNS_conf, ?DefConf),
    DB=piDNS_fun:get_env(ioc2rpz, piDNS_db, ?DefDB),
    {ok, CWD} = file:get_cwd(),
    Dir=piDNS_fun:get_env(piDNS, cd, CWD),
    file:set_cwd(Dir),
    piDNS_fun:logMessage("piDNS ver ~p ip4: ~p ip6: ~p conf: ~p db: ~p cwd: ~p~n",[?piDNS_ver,IPv4,IPv6,Conf_File,DB,Dir]),

%%% Start supervisor of supervisors
    piDNS_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
