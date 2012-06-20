%%%-------------------------------------------------------------------
%%% @author karl l <karl@ninjacontrol.com>
%%% @copyright (C) 2012
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(m3u8_parser_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%  Returns list of tuples to set default properties
%%  for the suite.
%%
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%
%% @spec suite() -> Info
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{minutes,10}}].

%%--------------------------------------------------------------------
%% @doc
%% Initialization before the whole suite
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    CWD = filename:absname(""),
    ct:pal("CWD = ~p",[CWD]),
    LexerXrlPath = "../../src/m3u8_lex.xrl",
    {ok, LexerErl} = leex:file(LexerXrlPath),
    {ok ,LexerMod} = compile:file(LexerErl),
    [{lexer, LexerMod} | Config].

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after the whole suite
%%
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_suite(Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Initialization before each test case group.
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after each test case group.
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Initialization before each test case
%%
%% TestCase - atom()
%%   Name of the test case that is about to be run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after each test case
%%
%% TestCase - atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of test case group definitions.
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% @spec: groups() -> [Group]
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @doc
%%  Returns the list of groups and test cases that
%%  are to be executed.
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%% Reason = term()
%%   The reason for skipping all groups and test cases.
%%
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% @end
%%--------------------------------------------------------------------
all() -> 
    [m3u8_lexer_tags, m3u8_lexer_timestamp, m3u8_lexer_qstring, m3u8_lexer_integers,
     m3u8_lexer_floats,m3u8_lexer_resolution, m3u8_lexer_enums,
     m3u8_lexer_example_pl].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc 
%% Lexer test, handling of extended tags 
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% @spec TestCase(Config0) ->
%%           ok | exit() | {skip,Reason} | {comment,Comment} |
%%           {save_config,Config1} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
m3u8_lexer_tags(Config) -> 
    LexerMod = proplists:get_value(lexer,Config),
    TagsStr = [{"#EXTM3U" ,tag_m3u, "#EXTM3U"},
	       {"#EXTINF" ,tag_inf, "#EXTINF"},
	       {"#EXT-X-BYTERANGE" ,tag_byterange, "#EXT-X-BYTERANGE"},
	       {"#EXT-X-TARGETDURATION" ,tag_targetduration, "#EXT-X-TARGETDURATION"},
	       {"#EXT-X-MEDIA-SEQUENCE" ,tag_media_sequence, "#EXT-X-MEDIA-SEQUENCE"},
	       {"#EXT-X-KEY" ,tag_key, "#EXT-X-KEY"},
	       {"#EXT-X-PROGRAM-DATE-TIME",tag_program_date_time, "#EXT-X-PROGRAM-DATE-TIME"},
	       {"#EXT-X-ALLOW-CACHE",tag_allow_cache, "#EXT-X-ALLOW-CACHE"},
	       {"#EXT-X-PLAYLIST-TYPE",tag_playlist_type, "#EXT-X-PLAYLIST-TYPE"},
	       {"#EXT-X-ENDLIST",tag_endlist, "#EXT-X-ENDLIST"},
	       {"#EXT-X-MEDIA",tag_media, "#EXT-X-MEDIA"},
	       {"#EXT-X-STREAM-INF",tag_steam_inf, "#EXT-X-STREAM-INF"},
	       {"#EXT-X-DISCONTINUITY",tag_discontinuity, "#EXT-X-DISCONTINUITY"},
	       {"#EXT-X-I-FRAMES-ONLY",tag_i_frames_only, "#EXT-X-I-FRAMES-ONLY"},
	       {"#EXT-X-I-FRAME-STREAM-INF",tag_i_frames_stream_inf, "#EXT-X-I-FRAME-STREAM-INF"},
	       {"#EXT-X-VERSION",tag_version, "#EXT-X-VERSION"} ],
    ok = simple_token_check(TagsStr,LexerMod),    
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% Lexer test, handling of timestamp
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% @spec TestCase(Config0) ->
%%           ok | exit() | {skip,Reason} | {comment,Comment} |
%%           {save_config,Config1} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
m3u8_lexer_timestamp(Config) -> 
    LexerMod = proplists:get_value(lexer,Config),
    TimestampStr = [
	       {"2012-01-01T12:12:12" ,time_stamp, "2012-01-01T12:12:12"},
	       {"2012-01-01T12:12:12Z" ,time_stamp, "2012-01-01T12:12:12Z"},
	       {"2012-01-01T12:12:12.1234Z" ,time_stamp, "2012-01-01T12:12:12.1234Z"},
	       {"2012-01-01T12:12:12.1234+12" ,time_stamp, "2012-01-01T12:12:12.1234+12"},
	       {"2012-01-01T12:12:12.12-10:14" ,time_stamp, "2012-01-01T12:12:12.12-10:14"},
	       {"2012-01-01T12:12:12.123+0909" ,time_stamp, "2012-01-01T12:12:12.123+0909"}
		   ],
    
    ok = simple_token_check(TimestampStr,LexerMod),
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% Lexer test, handling of quoted string
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% @spec TestCase(Config0) ->
%%           ok | exit() | {skip,Reason} | {comment,Comment} |
%%           {save_config,Config1} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
m3u8_lexer_qstring(Config) -> 
    LexerMod = proplists:get_value(lexer,Config),
    QStr = [
	       {"\"This is a string\"" ,quoted_string, "This is a string"},
	       {"\"\"" ,quoted_string, ""} % empty string
		   ],
    
    ok = simple_token_check(QStr,LexerMod),
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% Lexer test, handling of integers
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% @spec TestCase(Config0) ->
%%           ok | exit() | {skip,Reason} | {comment,Comment} |
%%           {save_config,Config1} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
m3u8_lexer_integers(Config) -> 
    LexerMod = proplists:get_value(lexer,Config),
    IStr = [
	       {"1" ,decimal_integer, 1},
	       {"0" ,decimal_integer, 0},
	       {"0x10" ,decimal_integer, 16}, % hex converted to integer
	       {"0XABCDEF" ,decimal_integer, 11259375}, % hex converted to integer
	       {"123123123123123123" ,decimal_integer, 123123123123123123}
	   ],
    
    ok = simple_token_check(IStr,LexerMod),
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% Lexer test, handling of floats
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% @spec TestCase(Config0) ->
%%           ok | exit() | {skip,Reason} | {comment,Comment} |
%%           {save_config,Config1} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
m3u8_lexer_floats(Config) -> 
    LexerMod = proplists:get_value(lexer,Config),
    IStr = [
	       {"1.0" ,decimal_float, 1.0},
	       {"0.99999999" ,decimal_float, 0.99999999}
	   ],
    
    ok = simple_token_check(IStr,LexerMod),
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% Lexer test, handling of decimal resolution
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% @spec TestCase(Config0) ->
%%           ok | exit() | {skip,Reason} | {comment,Comment} |
%%           {save_config,Config1} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
m3u8_lexer_resolution(Config) -> 
    LexerMod = proplists:get_value(lexer,Config),
    IStr = [
	       {"1080x1080" ,decimal_resolution, {1080,1080}},
	       {"1x1" ,decimal_resolution, {1,1}}
	   ],
    
    ok = simple_token_check(IStr,LexerMod),
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% Lexer test, handling of enum values 
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% @spec TestCase(Config0) ->
%%           ok | exit() | {skip,Reason} | {comment,Comment} |
%%           {save_config,Config1} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
m3u8_lexer_enums(Config) -> 
    LexerMod = proplists:get_value(lexer,Config),
    IStr = [
	       {"YES" ,e_choice_yes, "YES"},
	       {"NO" ,e_choice_no, "NO"},
	       {"NONE" ,e_method_none, "NONE"},
	       {"AES-128" ,e_method_aes_128, "AES-128"},
	       {"EVENT" ,e_playlist_type_event, "EVENT"},
	       {"VOD" ,e_playlist_type_vod, "VOD"},
	       {"AUDIO" ,e_media_type_audio, "AUDIO"},
	       {"VIDEO" ,e_media_type_video, "VIDEO"}
	   ],
    
    ok = simple_token_check(IStr,LexerMod),
    ok.


%%--------------------------------------------------------------------
%% @doc 
%% Lexer test, example playlists
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% @spec TestCase(Config0) ->
%%           ok | exit() | {skip,Reason} | {comment,Comment} |
%%           {save_config,Config1} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
m3u8_lexer_example_pl(Config) -> 
    LexerMod = proplists:get_value(lexer,Config),
    PL1 = simple_pl(),
    PL2 = sliding_window_pl(),
    PL3 = enc_media_segments_pl(),
    PL4 = variant_pl(),
    PL5 = variant_i_frames_pl(),
    PL6 = variant_alt_audio_pl(),
    PL7 = variant_alt_video_pl(),
    PLS = [PL1,PL2,PL3,PL4,PL5,PL6,PL7],
    PLFun = fun(PLLine) ->
		    {ok, Res, _} = LexerMod:string(PLLine),
		    ct:pal("Res = ~p",[Res])
			
		end,
    lists:map(PLFun,PL1),
    lists:map(PLFun,PL2),
    lists:map(PLFun,PL3),
    lists:map(PLFun,PL4),
    lists:map(PLFun,PL5),
    lists:map(PLFun,PL6),
    lists:map(PLFun,PL7),
    ok.


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

% @ doc
% Test the provided StringTokenList in the lexer. Calls ct:fail/1 if the
% returned token or value  from the lexer does not match the ones frome the StringTokenList
% StringTokenList = [{string(), token(), value()}]
% LexerMod = module() 
%  a module containing string/1
% @spec simple_token_check -> ok | ct:fail()
% @hidden
simple_token_check(StringTokenList,LexerMod) ->
    CheckTokens = fun(T) ->
			{Str,Token,Value} = T,
			case LexerMod:string(Str) of
			    {ok,[{ParsedToken,_,ParsedValue}],_} ->
				ct:pal("String = ~p, Expected token = ~p, Parsed token = ~p - Expected Value = ~p, Parsed Value = ~p.",[Str,Token,ParsedToken,Value,ParsedValue]),
				case ParsedToken of
					Token -> ok;
					    _ -> ct:fail("Expected token and parsed token doesn't match.")
				end,
				case ParsedValue of
				    Value -> ok;
				    _ ->  ct:fail("Expected value and parsed value doesn't match.")
				end;
			    			    
			    Other ->
				ct:fail("Error, unexpeted result = ~p", [Other])
			end
		end,
    lists:map(CheckTokens,StringTokenList),
    ok.
    
%%--------------------------------------------------------------------
%% Test data (from "HTTP Live Streaming",
%%  draft-pantos-http-live-streaming-08)
%%--------------------------------------------------------------------


simple_pl() ->
    ["#EXTM3U\n#EXT-X-TARGETDURATION:5220\n#EXTINF:5220,\nhttp://media.example.com/entire.ts\n#EXT-X-ENDLIST\n"].

sliding_window_pl() ->
    ["#EXTM3U\n",
     "#EXT-X-TARGETDURATION:8\n",
     "#EXT-X-MEDIA-SEQUENCE:2680\n",
     "\n", 
     "#EXTINF:8,\n",
     "https://priv.example.com/fileSequence2680.ts\n",
     "#EXTINF:8,\n",
     "https://priv.example.com/fileSequence2681.ts\n",
     "#EXTINF:8,\n",
     "https://priv.example.com/fileSequence2682.ts\n"].

enc_media_segments_pl() ->
    ["#EXTM3U\n",
     "#EXT-X-MEDIA-SEQUENCE:7794\n",
     "#EXT-X-TARGETDURATION:15\n",
     "\n",
     "#EXT-X-KEY:METHOD=AES-128,URI=\"https://priv.example.com/key.php?r=52\"\n",
     "\n",
     "#EXTINF:15,\n",
     "http://media.example.com/fileSequence52-1.ts\n",
     "#EXTINF:15,\n",
     "http://media.example.com/fileSequence52-2.ts\n",
     "#EXTINF:15,\n",
     "http://media.example.com/fileSequence52-3.ts\n",
     "\n",
     "#EXT-X-KEY:METHOD=AES-128,URI=\"https://priv.example.com/key.php?r=53\"\n",
     "\n",
     "#EXTINF:15,\n",
     "http://media.example.com/fileSequence53-1.ts\n"].


variant_pl() ->
    ["#EXTM3U\n",
     "#EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=1280000\n",
     "http://example.com/low.m3u8\n",
     "#EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=2560000\n",
     "http://example.com/mid.m3u8\n",
     "#EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=7680000\n",
     "http://example.com/hi.m3u8\n",
     "#EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=65000,CODECS=\"mp4a.40.5\"\n",
     "http://example.com/audio-only.m3u8\n"].


variant_i_frames_pl() ->
    ["#EXTM3U\n",
     "#EXT-X-STREAM-INF:BANDWIDTH=1280000\n",
     "http://example.com/low/audio-video.m3u8\n",
     "#EXT-X-I-FRAME-STREAM-INF:BANDWIDTH=86000,URI=\"low/iframe.m3u8\"\n",
     "#EXT-X-STREAM-INF:BANDWIDTH=2560000\n",
     "http://example.com/mid/audio-video.m3u8\n",
     "#EXT-X-I-FRAME-STREAM-INF:BANDWIDTH=150000,URI=\"mid/iframe.m3u8\"\n",
     "#EXT-X-STREAM-INF:BANDWIDTH=7680000\n",
     "http://example.com/hi/audio-video.m3u8\n",
     "#EXT-X-I-FRAME-STREAM-INF:BANDWIDTH=550000,URI=\"hi/iframe.m3u8\"\n",
     "#EXT-X-STREAM-INF:BANDWIDTH=65000,CODECS=\"mp4a.40.5\"\n",
     "http://example.com/audio-only.m3u8\n"].

variant_alt_audio_pl() ->
    ["#EXTM3U\n",
     "#EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID=\"aac\",NAME=\"English\",DEFAULT=YES,AUTOSELECT=YES,LANGUAGE=\"en\",URI=\"main/english-audio.m3u8\"\n",
     "#EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID=\"aac\",NAME=\"Deutsche\",DEFAULT=NO,AUTOSELECT=YES,LANGUAGE=\"de\",URI=\"main/german-audio.m3u8\"\n",
     "#EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID=\"aac\",NAME=\"Commentary\",DEFAULT=NO,AUTOSELECT=NO,URI=\"commentary/audio-only.m3u8\"\n",
     "#EXT-X-STREAM-INF:BANDWIDTH=1280000,CODECS=\"...\",AUDIO=\"aac\"\n",
     "http://example.com/low/video-only.m3u8\n",
     "#EXT-X-STREAM-INF:BANDWIDTH=2560000,CODECS=\"...\",AUDIO=\"aac\"\n",
     "http://example.com/mid/video-only.m3u8\n",
     "#EXT-X-STREAM-INF:BANDWIDTH=7680000,CODECS=\"...\",AUDIO=\"aac\"\n",
     "http://example.com/hi/video-only.m3u8\n",
     "#EXT-X-STREAM-INF:BANDWIDTH=65000,CODECS=\"mp4a.40.5\",AUDIO=\"aac\"\n",
     "http://example.com/main/english-audio.m3u8\n"].

variant_alt_video_pl() ->
    ["#EXTM3U\n",
     "#EXT-X-MEDIA:TYPE=VIDEO,GROUP-ID=\"low\",NAME=\"Main\",DEFAULT=YES,URI=\"low/main/audio-video.m3u8\"\n",
     "#EXT-X-MEDIA:TYPE=VIDEO,GROUP-ID=\"low\",NAME=\"Centerfield\",DEFAULT=NO,URI=\"low/centerfield/audio-video.m3u8\"\n",
     "#EXT-X-MEDIA:TYPE=VIDEO,GROUP-ID=\"low\",NAME=\"Dugout\",DEFAULT=NO,URI=\"low/dugout/audio-video.m3u8\"\n",
     "\n",
     "#EXT-X-STREAM-INF:BANDWIDTH=1280000,CODECS=\"...\",VIDEO=\"low\"\n",
     "http://example.com/low/main/audio-video.m3u8\n",
     "\n",
     "#EXT-X-MEDIA:TYPE=VIDEO,GROUP-ID=\"mid\",NAME=\"Main\",DEFAULT=YES,URI=\"mid/main/audio-video.m3u8\"\n",
     "#EXT-X-MEDIA:TYPE=VIDEO,GROUP-ID=\"mid\",NAME=\"Centerfield\",DEFAULT=NO,URI=\"mid/centerfield/audio-video.m3u8\"\n",
     "#EXT-X-MEDIA:TYPE=VIDEO,GROUP-ID=\"mid\",NAME=\"Dugout\",DEFAULT=NO,URI=\"mid/dugout/audio-video.m3u8\"\n",
     "\n",
     "#EXT-X-STREAM-INF:BANDWIDTH=2560000,CODECS=\"...\",VIDEO=\"mid\"\n",
     "http://example.com/mid/main/audio-video.m3u8\n",
     "\n",
     "#EXT-X-MEDIA:TYPE=VIDEO,GROUP-ID=\"hi\",NAME=\"Main\",DEFAULT=YES,URI=\"hi/main/audio-video.m3u8\"\n",
     "#EXT-X-MEDIA:TYPE=VIDEO,GROUP-ID=\"hi\",NAME=\"Centerfield\",DEFAULT=NO,URI=\"hi/centerfield/audio-video.m3u8\"\n",
     "#EXT-X-MEDIA:TYPE=VIDEO,GROUP-ID=\"hi\",NAME=\"Dugout\",DEFAULT=NO,URI=\"hi/dugout/audio-video.m3u8\"\n",
     "\n",
     "#EXT-X-STREAM-INF:BANDWIDTH=7680000,CODECS=\"...\",VIDEO=\"hi\"\n",
     "http://example.com/hi/main/audio-video.m3u8\n",
     "\n",
     "#EXT-X-STREAM-INF:BANDWIDTH=65000,CODECS=\"mp4a.40.5\"\n",
     "http://example.com/main/audio-only.m3u8\n"].
