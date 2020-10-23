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

%piDNS common functions
-module(piDNS_fun).
-include_lib("eunit/include/eunit.hrl").
-include_lib("piDNS.hrl").

-export([get_env/3,logMessage/2,logMessageCEF/2,ip_to_bin/1,ip_in_netcidr/3]).

% Get an enviroment variable. If not defined or empty - return the passed default value.
get_env(App, Param, Default) ->
  case application:get_env(App, Param) of
    undefined   ->  Default;
    {ok, []}    ->  Default;
    {ok, X}     ->  X
  end.
	
% Log a message
logMessage(Message, Vars) ->
  logMessage(group_leader(), Message, Vars).

logMessage(Dest, Message, Vars) ->
 ?addTS(Dest),
 io:fwrite(Dest,Message,Vars).

% Log a message in CEF format
logMessageCEF(Message, Vars) -> % "Device Event Class ID|Name|Severity|[Extension]" must be passed
  logMessageCEF(group_leader(), Message, Vars).

logMessageCEF(Dest, Message, Vars) ->
 ?addTS(Dest),
 io:fwrite(Dest,"CEF:0|piDNS|piDNS|~s"++Message,[?piDNS_ver|Vars]).

% Converts IP from binary string into a binary.
% E.g. "10.10.10.10" => <<10,10,10,10>>, "fc00::01" => <<16#fc00:16,0:16,0:16,0:16,0:16,0:16,0:16,1:16>>
ip_to_bin(IP) when is_list(IP)->
  ip_to_bin(inet:parse_address(IP));

ip_to_bin({ok,{IP1,IP2,IP3,IP4}}) ->
  <<IP1,IP2,IP3,IP4>>;

ip_to_bin({ok,{IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8}}) ->
  <<IP1:16,IP2:16,IP3:16,IP4:16,IP5:16,IP6:16,IP7:16,IP8:16>>.

% Checks if an IP is in the network. IPv4 and IPv6 are supported
% usage: piDNS_fun:ip_in_netcidr(IP, Network, Bitlength)
ip_in_netcidr(IP, Net, Mask) ->
	<<Net1:Mask/bits,_/bitstring>> = IP,
	<<Net2:Mask/bits,_/bitstring>> = Net,
	if Net1 /= Net2 ->
		false;
		true -> true
	end.

% Split a list into two parts; the length of the first part is given.
% usage: piDNS_fun:split(List,Length)
% example:
% piDNS_fun:split([a,b,c],2). =>  [[a,b],[c]]
% piDNS_fun:split([a,b,c],1). =>  [[a],[b,c]]

split([],_)->
    [];
split([H|T],Index) when Index>0,T==[] ->
    [[H],T];
split([H|T],1)->
    [[H],T];
split([H|T],Index)->
    [RH,RT]=split(T,Index-1),
    [[H|RH],RT].


%%%%
%%%% EUnit tests
%%%%
get_env_test() -> [
	?assert(get_env(piDNS,notexists,"notexists") =:= "notexists") %env variable is not exists
].

ip_to_bin_test() ->[
	?assert(ip_to_bin("10.10.10.10") =:= <<10,10,10,10>>),
	?assert(ip_to_bin("fc00::01") =:= <<16#fc00:16,0:16,0:16,0:16,0:16,0:16,0:16,1:16>>)
].

ip_in_netcidr_test() ->[
	?assert(ip_in_netcidr(<<10,10,10,10>>,<<10,0,0,0>>,9)),
	?assert(not ip_in_netcidr(<<10,10,10,10>>,<<11,0,0,0>>,8)),
	?assert(ip_in_netcidr(<<16#fc00:16,0:16,0:16,0:16,0:16,0:16,0:16,1:16>>,<<16#fc00:16,0:16,0:16,0:16,0:16,0:16,0:16,0:16>>,64)),
	?assert(not ip_in_netcidr(<<16#ff00:16,0:16,0:16,0:16,0:16,0:16,0:16,1:16>>,<<16#fc00:16,0:16,0:16,0:16,0:16,0:16,0:16,0:16>>,64))
].