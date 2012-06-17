%
% m3u8 lexer, leex
%
% based on http://tools.ietf.org/html/draft-pantos-http-live-streaming-08
%
% NB: Only works with URIs on the form 'scheme ":" ...."
%

Definitions.

D = [0-9]
L = [a-z|A-Z]
UL = [A-Z]
EOL = [\n|\r\n]
Dash = -
HexPfx = 0[X|x]
HexL = [A-F]


Rules.

% Attributes 

({UL}|{Dash})+=		: S = string:substr(TokenChars, 1, TokenLen - 1),
			  {token,{attribute_name,TokenLine,S},"="}.
{D}+ 			: {token,{decimal_integer,TokenLine,list_to_integer(TokenChars)}}.
{HexPfx}({D}|{HexL})+	: HexStr = lists:sublist(TokenChars,3,TokenLen),
			  {token,{decimal_integer,TokenLine,list_to_integer(HexStr,16)}}.
"(\\.|[^"])*" 		: S = string:substr(TokenChars, 2, TokenLen - 2),
			  {token,{quoted_string,TokenLine,S}}.

{D}+x{D}+       	: [W,H]=string:tokens(TokenChars,"x"),
			  {token,{decimal_resolution,TokenLine,{list_to_integer(W),list_to_integer(H)}}}.
{D}+\.{D}+      	: {token,{decimal_float,TokenLine,list_to_float(TokenChars)}}.

, 			: {token,{',',TokenLine,TokenChars}}.
\: 			: {token,{':',TokenLine,TokenChars}}.
@ 			: {token,{'@',TokenLine,TokenChars}}.
= 			: {token,{'=',TokenLine,TokenChars}}.

%% Timestamp 

% Format <YYYY-MM-DDThh:mm:ssZ>
%
% where 'ss' could contains fractions = ss(.f+)
% 
% Z =
%  'Z' (Time Zone UTC)
%   or
%  ±[hh]:[mm], ±[hh][mm], or ±[hh] (Time Zone Offset from UTC)
%
{D}{D}{D}{D}-{D}{D}-{D}{D}T{D}{D}:{D}{D}:{D}{D}(\.{D}+)?(Z|([\+|-]{D}{D}(:?{D}{D})?))? : {token,{time_stamp,TokenLine,TokenChars}}.

%% Tags

#EXTM3U			: {token,{tag_m3u,TokenLine,TokenChars}}.
#EXTINF 		: {token,{tag_inf,TokenLine,TokenChars}}.
#EXT-X-BYTERANGE 	: {token,{tag_byterange,TokenLine,TokenChars}}.
#EXT-X-TARGETDURATION 	: {token,{tag_targetduration,TokenLine,TokenChars}}.
#EXT-X-MEDIA-SEQUENCE 	: {token,{tag_media_sequence,TokenLine,TokenChars}}.
#EXT-X-KEY 		: {token,{tag_key,TokenLine,TokenChars}}.
#EXT-X-PROGRAM-DATE-TIME: {token,{tag_program_date_time,TokenLine,TokenChars}}.
#EXT-X-ALLOW-CACHE 	: {token,{tag_allow_cache,TokenLine,TokenChars}}.
#EXT-X-PLAYLIST-TYPE 	: {token,{tag_playlist_type,TokenLine,TokenChars}}.
#EXT-X-ENDLIST 		: {token,{tag_endlist,TokenLine,TokenChars}}.
#EXT-X-MEDIA 		: {token,{tag_media,TokenLine,TokenChars}}.
#EXT-X-STREAM-INF 	: {token,{tag_steam_inf,TokenLine,TokenChars}}.
#EXT-X-DISCONTINUITY 	: {token,{tag_discontinuity,TokenLine,TokenChars}}.
#EXT-X-I-FRAMES-ONLY 	: {token,{tag_i_frames_only,TokenLine,TokenChars}}.
#EXT-X-I-FRAME-STREAM-INF : {token,{tag_i_frames_stream_inf,TokenLine,TokenChars}}.
#EXT-X-VERSION 		: {token,{tag_version,TokenLine,TokenChars}}.

%% Enums

% Choice
YES			: {token,{e_choice_yes,TokenLine,TokenChars}}.
NO  			: {token,{e_choice_no,TokenLine,TokenChars}}.
% Encryption methods
NONE			: {token,{e_method_none,TokenLine,TokenChars}}.
AES-128 		: {token,{e_method_aes_128,TokenLine,TokenChars}}.
% Playlist type
EVENT			: {token,{e_playlist_type_event,TokenLine,TokenChars}}.
VOD 			: {token,{e_playlist_type_vod,TokenLine,TokenChars}}.
 % Media Type
AUDIO			: {token,{e_media_type_audio,TokenLine,TokenChars}}.
VIDEO 			: {token,{e_media_type_video,TokenLine,TokenChars}}.
 

%% uri
{L}({L}|{D}|\+|-|\.|)*\:.+ : {token,{uri,TokenLine,TokenChars}}.


\n : skip_token.
\n\r : skip_token.

Erlang code.