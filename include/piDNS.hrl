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

%%%piDNS params 
-define(DNSPort,53).		%DNS Port
-define(DoTPort,853).		%DoT Port
-define(DoHPort,853).		%DoH Port
-define(RESTPort,8443). %REST Port
%-define(logTS, true).	% Log timestamps 
-define(debug, true).		% Log debug messages

-define(DefConf,"./cfg/piDNS.conf"). %Default configuration file
-define(DefDB,"./db"). %Default DB location



%%%%%%
%%%%%% Do not modify any settings below the line
%%%%%%
-define(piDNS_ver, "0.0.0.1-2019081101").

%%% Log timestamps
-ifdef(logTS).
-define(addTS(Dest),(fun() ->
		{{Y,M,D},{HH,MM,SS}}=calendar:local_time(),io:fwrite(Dest,"~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w ",[Y,M,D,HH,MM,SS])
	end)()).
-else.
-define(addTS(Dest),true).
-endif.

%%% Debug messages
-ifdef(debug).
-define(logDebugMSG(Message, Vars),ioc2rpz_fun:logMessage(Message, Vars)).
-else.
-define(logDebugMSG(Message, Vars),true).
-endif.

%%% DNS Response codes
-define(NOERROR,0).
-define(FORMERR,1).
-define(SERVFAIL,2).
-define(NXDOMAIN,3).
-define(NOTIMP,4).
-define(REFUSED,5).
-define(NOTAUTH,9).

%%% TSIG Errors
-define(TSIG_BADSIG,16).
-define(TSIG_BADKEY,17).
-define(TSIG_BADTIME,18).

%%%DNS Request Class
-define(C_IN,1).
-define(C_CHAOS,3).
-define(C_ANY,255).

%%%DNS Request/Record Type
-define(T_A,1).
-define(T_NS,2).
-define(T_CNAME,5).
-define(T_SOA,6).
-define(T_TXT,16).
-define(T_AAAA,28).
-define(T_OPT,41).
-define(T_IXFR,251).
-define(T_AXFR,252).
-define(T_ANY,255).

-define(RT_TSIG,250).

%%%DNS Operation
-define(OP_QUERY,0:4).
-define(OP_NOTIFY,4:4).
