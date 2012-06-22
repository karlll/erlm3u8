%%%-------------------------------------------------------------------
%%% @author karl l <karl@ninjacontrol.com>
%%% @doc
%%% Based on http://tools.ietf.org/html/draft-pantos-http-live-streaming-08
%%% @end
%%%-------------------------------------------------------------------

-module(m3u8).

-export([attribute_list/1]).
-export([header/0]).
-export([segment_duration/1,segment_duration/2]).
-export([max_segment_duration/1]).
-export([media_sequence/1]).
-export([segment_key/1]).
-export([segment_subrange/1,segment_subrange/2]).
-export([program_date_time/0,program_date_time/6, program_date_time/7]).
-export([allow_cache/1]).
-export([endlist/0]).
-export([media/1]).
-export([i_frame_stream/1]).
-export([stream/2]).
-export([i_frames_only/0]).
-export([disontinutiy/0]).
-export([version/1]).


%% 3.2 Attribute Lists

kv_pair(KeyValuePair) ->
    {Key,Value} = KeyValuePair,
    lists:concat([Key,"=",Value]).

attribute_list(KeyValuePair) when is_tuple(KeyValuePair) ->
    kv_pair(KeyValuePair);

attribute_list([KeyValuePair|T]) when is_tuple(KeyValuePair) ->
    attribute_list(T,kv_pair(KeyValuePair)).

attribute_list([KeyValuePair|T], Acc) when is_tuple(KeyValuePair) ->
    attribute_list(T, lists:concat([kv_pair(KeyValuePair),",",Acc]));

attribute_list([], Acc) ->
    Acc.


% Attribute values
 
% TODO : hexadecimal_integer
 
quoted_string(String) when is_list(String) ->
    
    "\"" ++ String ++ "\"".

decimal_resolution(Width,Height) when is_integer(Width),
				      is_integer(Height) ->
    integer_to_list(Width)++"x"++integer_to_list(Height).


%% 3.3 Standard Tags


%% 3.3.1 EXTM3U

header()->
   "#EXTM3U". 

%% 3.3.2 EXTINF

segment_duration(Duration,Title) when is_integer(Duration) ->			    
    "#EXTINF:"++integer_to_list(Duration)++","++"\""++Title++"\"".

segment_duration(Duration) when is_integer(Duration) ->			    
    "#EXTINF:"++integer_to_list(Duration)++",".

%% 3.4 New Tags

%% 3.4.1 EXT-X-BYTERANGE

segment_subrange(Length,Offset) when is_integer(Length),
				     is_integer(Offset) ->
    "#EXT-X-BYTERANGE:"++integer_to_list(Length)++"@"++integer_to_list(Offset).

segment_subrange(Length) when is_integer(Length) ->
    "#EXT-X-BYTERANGE:"++integer_to_list(Length).
    
%% 3.4.2 EXT-X-TARGETDURATION

max_segment_duration(Seconds) when is_integer(Seconds) ->
    "#EXT-X-TARGETDURATION:"++integer_to_list(Seconds).

%% 3.4.3 EXT-X-MEDIA-SEQUENCE

media_sequence(Number) when is_integer(Number) ->
    "#EXT-X-MEDIA-SEQUENCE:"++integer_to_list(Number).


%% 3.4.4 EXT-X-KEY

segment_key(Attributes)->
    segment_key(Attributes,[]).

segment_key([method_none|T],Acc)->
    segment_key(T,[{"METHOD","NONE"}]++Acc);

segment_key([method_aes128|T],Acc)->
    segment_key(T,[{"METHOD","AES-128"}]++Acc);

segment_key([{key_uri,URI}|T],Acc)->
    segment_key(T,[{"URI",quoted_string(URI)}]++Acc);

segment_key([],Acc) ->
    "#EXT-X-KEY:"++attribute_list(Acc).

%% 3.4.5  EXT-X-PROGRAM-DATE-TIME

program_date_time() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:localtime(),
    Format="~4..0s-~2..0s-~2..0sT~2..0s:~2..0s:~2..0s",
    program_date_time(Format,[ integer_to_list(Year),
			       integer_to_list(Month),
			       integer_to_list(Day),
			       integer_to_list(Hour),
			       integer_to_list(Minute),
			       integer_to_list(Second) ]).


program_date_time(Year, Month, Day, Hour, Minute, Second) when is_integer(Year),
								is_integer(Month),
								is_integer(Day),
								is_integer(Hour),
								is_integer(Minute),
								is_integer(Second) ->
    Args = [ integer_to_list(Year),
	     integer_to_list(Month),
	     integer_to_list(Day),
	     integer_to_list(Hour),
	     integer_to_list(Minute),
	     integer_to_list(Second) ],
    Format="~4..0s-~2..0s-~2..0sT~2..0s:~2..0s:~2..0s",
    program_date_time(Format,Args).

% Offset = list()
% Offset = [ utc | { OffsetSign, OffsetHour::integer() } | 
%                  { OffsetSign, OffsetHour::integer(), OffsetMinute::integer() } ]
% OffsetSign = atom()
% OffsetSign = '-' | '+'
program_date_time(Year, Month, Day, Hour, Minute, Second, Offset) when is_integer(Year),
									is_integer(Month),
									is_integer(Day),
									is_integer(Hour),
									is_integer(Minute),
									is_integer(Second),
									is_list(Offset) ->
    Args = [ integer_to_list(Year),
	     integer_to_list(Month),
	     integer_to_list(Day),
	     integer_to_list(Hour),
	     integer_to_list(Minute),
	     integer_to_list(Second) ],
    {OffsetFormat,OffsetArg} = case lists:member(utc,Offset) of
				   true -> 
				       {"Z", []};
				   false -> 
				       case Offset of
					   [{OffsetSign, OffsetHour, OffsetMinute}] ->
					       {atom_to_list(OffsetSign)++"~2..0s~2..0s",
						[integer_to_list(OffsetHour),
						 integer_to_list(OffsetMinute)]};
					   [{OffsetSign, OffsetHour}] ->
					       {atom_to_list(OffsetSign)++"~2..0s",
						[integer_to_list(OffsetHour)]}
				       end
				       
			       end,
    
						    
		    
    Format="~4..0s-~2..0s-~2..0sT~2..0s:~2..0s:~2..0s",
    program_date_time(Format++OffsetFormat,Args++OffsetArg).

program_date_time(Format,Args) when is_list(Format),
				    is_list(Args) ->
    
    DateTimeStr = lists:flatten([io_lib:format(Format, Args)]),
    "#EXT-X-PROGRAM-DATE-TIME:" ++ DateTimeStr.
    

%% 3.4.6.  EXT-X-ALLOW-CACHE

allow_cache(yes) ->
    "#EXT-X-ALLOW-CACHE:YES";

allow_cache(no) ->
    "#EXT-X-ALLOW-CACHE:NO".

%% 3.4.8.  EXT-X-ENDLIST

endlist() ->
    "#EXT-X-ENDLIST".

%% 3.4.9.  EXT-X-MEDIA

media(Attributes) ->
    media(Attributes,[]).

media([{uri, URI}|T], Acc) ->
    media(T,[{"URI", quoted_string(URI)}|Acc]);   
    
media([type_audio|T], Acc) ->
    media(T,[{"TYPE", "AUDIO"}|Acc]);

media([type_video|T], Acc) ->
    media(T,[{"TYPE", "VIDEO"}|Acc]);    

media([{group_id,Id}|T], Acc) ->
    media(T,[{"GROUP-ID",quoted_string(Id)}|Acc]);

media([{name,Name}|T], Acc) ->
    media(T,[{"NAME",quoted_string(Name)}|Acc]);

media([default|T], Acc) ->
    media(T,[{"DEFAULT","YES"}|Acc]);

media([not_default|T], Acc) ->
    media(T,[{"DEFAULT","NO"}|Acc]);

media([autoselect|T], Acc) ->
    media(T,[{"AUTOSELECT","YES"}|Acc]);

media([no_autoselect|T], Acc) ->
    media(T,[{"AUTOSELECT","NO"}|Acc]);

media([],Acc) ->
    "#EXT-X-MEDIA:"++attribute_list(Acc).

%% 3.4.10.  EXT-X-STREAM-INF

stream(Attributes, URI) ->
    stream(Attributes, URI, []).

stream([{bandwidth, BPS}|T], URI, Acc) when is_integer(BPS) ->
    stream(T,URI,[{"BANDWIDTH",integer_to_list(BPS)}|Acc]);

stream([{program_id,Id}|T], URI, Acc) when is_integer(Id) ->
    stream(T,URI,[{"PROGRAM-ID",integer_to_list(Id)}|Acc]);

stream([{codecs,Codecs}|T], URI, Acc) ->
    stream(T,URI,[{"CODECS",quoted_string(Codecs)}|Acc]);

stream([{audio,Audio}|T], URI, Acc) ->
    stream(T,URI,[{"AUDIO",quoted_string(Audio)}|Acc]);

stream([{video,Video}|T], URI, Acc) ->
    stream(T,URI,[{"VIDEO",quoted_string(Video)}|Acc]);

stream([],URI,Acc) ->
    "#EXT-X-STREAM-INF:"++attribute_list(Acc)++"\n"++URI.

%% 3.4.11.  EXT-X-DISCONTINUITY

disontinutiy() ->
    "#EXT-X-DISCONTINUITY".

%% 3.4.12.  EXT-X-I-FRAMES-ONLY

i_frames_only() ->
    "#EXT-X-I-FRAMES-ONLY".

%% 3.4.13.  EXT-X-I-FRAME-STREAM-INF

i_frame_stream(Attributes) ->
    i_frame_stream(Attributes, []).

i_frame_stream([{bandwidth, BPS}|T], Acc) when is_integer(BPS) ->
    i_frame_stream(T,[{"BANDWIDTH",integer_to_list(BPS)}|Acc]);

i_frame_stream([{program_id,Id}|T], Acc) when is_integer(Id) ->
    i_frame_stream(T,[{"PROGRAM-ID",integer_to_list(Id)}|Acc]);

i_frame_stream([{codecs,Codecs}|T], Acc) ->
    i_frame_stream(T,[{"CODECS",quoted_string(Codecs)}|Acc]);

i_frame_stream([{uri,URI}|T], Acc) ->
    i_frame_stream(T,[{"URI",quoted_string(URI)}|Acc]);

i_frame_stream([{video,Video}|T], Acc) ->
    i_frame_stream(T,[{"VIDEO",quoted_string(Video)}|Acc]);

i_frame_stream([],Acc) ->
    "#EXT-X-I-FRAME-STREAM-INF:"++attribute_list(Acc).

%% 3.4.14. EXT-X-VERSION

version(Version) when is_integer(Version) ->
    "#EXT-X-VERSION:"++ integer_to_list(Version).
