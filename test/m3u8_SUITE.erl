%%%-------------------------------------------------------------------
%%% @author karl l <karl@ninjacontrol.com>
%%% @copyright (C) 2012
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(m3u8_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [m3u8_tags].

%
% M3U8 tags
%
m3u8_tags(_Config) -> 

    eq(m3u8:header(),"#EXTM3U"),
    eq(m3u8:segment_duration(10000,"Test"),"#EXTINF:10000,\"Test\""),
    eq(m3u8:segment_duration(10000),"#EXTINF:10000,"),
    eq(m3u8:segment_subrange(99,100),"#EXT-X-BYTERANGE:99@100"),
    eq(m3u8:segment_subrange(100),"#EXT-X-BYTERANGE:100"),
    eq(m3u8:max_segment_duration(100),"#EXT-X-TARGETDURATION:100"),
    eq(m3u8:media_sequence(2),"#EXT-X-MEDIA-SEQUENCE:2"),
    eq(m3u8:segment_key([method_none]),"#EXT-X-KEY:METHOD=NONE"),
    eq(m3u8:segment_key([method_aes128]),"#EXT-X-KEY:METHOD=AES-128"),
    eq(m3u8:segment_key([{key_uri,"http://test.com/key"}]),"#EXT-X-KEY:URI=\"http://test.com/key\""),
    eq(m3u8:segment_key([method_aes128, {key_uri,"http://test.com/aes-key"}]),
       "#EXT-X-KEY:METHOD=AES-128,URI=\"http://test.com/aes-key\""),
    eq(m3u8:program_date_time(2000,5,11,12,13,14),
       "#EXT-X-PROGRAM-DATE-TIME:2000-05-11T12:13:14"),
    eq(m3u8:program_date_time(2000,10,11,12,13,14,[utc]),
       "#EXT-X-PROGRAM-DATE-TIME:2000-10-11T12:13:14Z"),
    eq(m3u8:program_date_time(2000,10,11,12,13,14,[{'-',12,10}]),
       "#EXT-X-PROGRAM-DATE-TIME:2000-10-11T12:13:14-1210"),
    eq(m3u8:program_date_time(2000,10,11,12,13,14,[{'+',15}]),
       "#EXT-X-PROGRAM-DATE-TIME:2000-10-11T12:13:14+15"),
    ct:pal("Current time : ~p",[m3u8:program_date_time()]),
    eq(m3u8:allow_cache(yes),"#EXT-X-ALLOW-CACHE:YES"),
    eq(m3u8:allow_cache(no),"#EXT-X-ALLOW-CACHE:NO"),
    eq(m3u8:endlist(),"#EXT-X-ENDLIST"),
    eq(m3u8:media([{uri,"http://test.com/media.ts"},
		   type_video,
		   {group_id, "id1"},
		   {name, "testname"},
		   default,
		   autoselect]),
       "#EXT-X-MEDIA:URI=\"http://test.com/media.ts\",TYPE=VIDEO,GROUP-ID=\"id1\",NAME=\"testname\",DEFAULT=YES,AUTOSELECT=YES"),
    eq(m3u8:stream([{bandwidth, 64000},
		   {program_id, 10},
		   {codecs, "mp4v.20.9, mp4a.E1"},
		   {audio, "id1"},
		   {video, "id2"}],
		   "http://www.test.com/video.ts"),
       "#EXT-X-STREAM-INF:BANDWIDTH=64000,PROGRAM-ID=10,CODECS=\"mp4v.20.9, mp4a.E1\",AUDIO=\"id1\",VIDEO=\"id2\"\nhttp://www.test.com/video.ts"),
    eq(m3u8:disontinutiy(),"#EXT-X-DISCONTINUITY"),
    eq(m3u8:i_frames_only(),"#EXT-X-I-FRAMES-ONLY"),
    eq(m3u8:i_frame_stream([{bandwidth, 64000},
			     {program_id, 10},
			     {codecs, "mp4v.20.9, mp4a.E1"},
			     {video, "id2"},
			     {uri,"http://www.test.com/video.ts"}]),
       "#EXT-X-I-FRAME-STREAM-INF:BANDWIDTH=64000,PROGRAM-ID=10,CODECS=\"mp4v.20.9, mp4a.E1\",VIDEO=\"id2\",URI=\"http://www.test.com/video.ts\""),
    eq(m3u8:version(4),"#EXT-X-VERSION:4"),
    ok.




eq(Str1,Str2) ->
    case string:equal(Str1,Str2) of
	true -> ct:pal("~p = ~p",[Str1,Str2]),
		ok;
	_ -> ct:pal("~p != ~p",[Str1,Str2]),
	     ct:fail("Strings doesnt match")
    end.
