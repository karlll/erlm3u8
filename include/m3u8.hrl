%%%-------------------------------------------------------------------
%%% @author karl l <karl@ninjacontrol.com>
%%% @doc
%%% Records representing the tags described in the draft 'HTTP Live Streaming'
%%% > http://tools.ietf.org/html/draft-pantos-http-live-streaming-08
%%% 
%%% @end
%%%-------------------------------------------------------------------


%% @doc EXTM3U (3.3.1)
-record(header,{}).


%% @doc EXTINF-tag (3.3.2)
-record(segment_duration,
	{
	  duration :: non_neg_integer(),
	  title    :: 'undefined' | string() 
	}
       ). 

%% @doc EXT-X-MEDIA (3.4.9)
-record(media,
	{
	  uri        :: 'undefined' | string(),
	  type       :: 'audio' | 'video',
	  group_id   :: string(),
	  language   :: 'undefined' | string(),
	  name       :: 'undefined' | string(),
	  default    :: 'undefined' | 'yes' | 'no',
	  autoselect :: 'undefined' | 'yes' | 'no'
	}
       ). 

-type resolution() :: {non_neg_integer(), non_neg_integer()}.

%% @doc EXT-X-STREAM-INF
-record(stream,
	{
	  uri        :: string(),
	  bandwidth  :: non_neg_integer(), 
	  program_id :: non_neg_integer(),
	  codecs     :: string(),
	  resolution :: resolution(), 
	  audio      :: string(),
	  video      :: string()
	}
       ).
%% @doc EXT-X-I-FRAME-STREAM-INF (3.4.13)
-record(i_frame_stream,
	{
	  uri        :: string(),
	  bandwidth  :: non_neg_integer(),
	  program_id :: 'undefined' | non_neg_integer(),
	  codecs     :: 'undefined' | string(),
	  resolution :: 'undefined' | resolution(), 
	  video      :: 'undefined' | string()
	}
       ).

%% @doc 3.4.5.  EXT-X-PROGRAM-DATE-TIME
%%
%% If utc == 'true' offset_sign, offset_hour & offset_minute is disregarded.
%% Valid offset combinations: { offset_sign, offset_hour, offset_minute } =
%% [ {undefined, undefined, undefined} |
%%   {OSign,OHour,undefined} |
%%   {OSign,OHour,OMinute} ]
%%
-record(program_date_time,
	{
	  year          :: non_neg_integer(),
	  month         :: non_neg_integer(),
	  day           :: non_neg_integer(),
	  hour          :: non_neg_integer(),
	  minute        :: non_neg_integer(),
	  second        :: non_neg_integer(),
	  utc           :: 'undefined' | 'true',
	  offset_sign   :: 'undefined' | '+' | '-',
	  offset_hour   :: 'undefined' | non_neg_integer(),
	  offset_minute :: 'undefined' | non_neg_integer()
	}
       ).

%% @doc 3.4.5.  EXT-X-PROGRAM-DATE-TIME
%% Current local time
-record(program_date_time_now,
	{}
       ).

%% @doc EXT-X-DISCONTINUITY (3.4.11)
-record(discontinuity,
	{}
       ).
%% @doc EXT-X-I-FRAMES-ONLY (3.4.12)
-record(i_frames_only,
	{}
       ).

%% EXT-X-VERSION (3.4.14)
-record(version,
	{
	  version    :: non_neg_integer()
	}
       ).

%% @doc EXT-X-ENDLIST (3.4.8)
-record(endlist,
	{}
       ).

%% @doc EXT-X-ALLOW-CACHE (3.4.6)
-record(cache,
	{
	  allow       :: 'yes' | 'no'
	}
       ).

%% @doc  EXT-X-KEY (3.4.4)
-record(segment_key,
	{
	  uri         :: string(),
	  method      :: 'none' | 'aes128' 
	}
       ).

%% @doc EXT-X-MEDIA-SEQUENCE (3.4.3)
-record(media_sequence,
	{
	  number      :: non_neg_integer()
	}
       ).

%% @doc  EXT-X-PLAYLIST-TYPE (3.4.7)
-record(playlist_type,
	{
	  type         :: 'event' | 'vod'
	}
       ).
	
%% @doc  EXT-X-TARGETDURATION (3.4.2)
-record(max_segment_duration,
	{
	  seconds     :: non_neg_integer()
	}
       ).

%% @doc EXT-X-BYTERANGE (3.4.1)
-record(segment_subrange,
	{
	  length      :: non_neg_integer(),
	  offset      :: 'undefined' | non_neg_integer()
	}
       ).

