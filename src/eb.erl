-module('eb').

%% API exports
-export([exif_data_for/1]).

-define(SOI, 16#FFD8).
-define(APP1, 16#FFE1).
-define(DIRECTORY_ENTRY_PATTERN,
  <<TagNumber:16/integer,
    DataFormat:16/integer,
    NumberOfComponents:32/integer,
    Data:4/binary,
    Rest/binary
  >>).

%%====================================================================
%% API functions
%%====================================================================
exif_data_for(FilePath) ->
  {ok, Fd} = file:open(FilePath, [read, binary, raw]),
  {ok, FileContent} = file:read(Fd, 10000),
  file:close(Fd),
  extract_exif_data(FileContent).

%%====================================================================
%% Internal functions
%%====================================================================
extract_exif_data(<<?SOI:16/integer, ?APP1:16/integer, _DataAreaSize:16/integer, Rest/binary>>) ->
  {_ExifHeader, RestFromExif} = extract_exif_header(Rest),
  {_TiffHeader, RestFromTiff} = extract_tiff_header(RestFromExif),
  extract_image_file_directory(RestFromTiff, RestFromTiff);
extract_exif_data(_) ->
  it_is_not_a_jpeg_file.

extract_exif_header(<<Header:48, Rest/binary>>) ->
  {Header, Rest}.

extract_tiff_header(<<ByteAlign:2/binary, TagMark:2/binary, OffsetToIFD:4/binary, Rest/binary>>) ->
  TiffHeader = #{
    byte_align => ByteAlign,
    tag_mark => TagMark,
    offset_to_ifd => OffsetToIFD
  },
  {TiffHeader, Rest}.

extract_image_file_directory(<<DirectoryEntriesNumber:16/integer, Rest/binary>>, Original) ->
  extract_image_file_directory(DirectoryEntriesNumber, Rest, Original, []).

extract_image_file_directory(1, Rest, _Original, Acc) ->
  {Acc, Rest};
extract_image_file_directory(DirectoryEntries, Rest, Original, Acc) ->
  {Directory, NewRest} = extract_directory_entry(Rest),
  NewDirectory = process_directory_entry(Directory, Original),
  extract_image_file_directory(DirectoryEntries - 1, NewRest, Original, [NewDirectory | Acc]).

extract_directory_entry(?DIRECTORY_ENTRY_PATTERN) ->
  {DataTypeName, BytesPerComponent} = data_type(DataFormat),
  DirectoryEntry = #{
    tag_name    => tag_name(TagNumber),
    data        => Data,
    data_type   => DataTypeName,
    data_length => NumberOfComponents * BytesPerComponent,
    components  => NumberOfComponents
 },
 {DirectoryEntry, Rest}.

process_directory_entry(#{data_type := ascii_strings, data_length := Len, data := DirectoryData} = DirectoryEntry, Bin) ->
  NewData = case Len > bits(4) of
    true ->
      Offset = binary:decode_unsigned(DirectoryData, big) - 8,
      NewLen = Len - 1,
      <<_:Offset/binary, Data:NewLen/binary, _/binary>> = Bin,
      Data;
    false ->
      <<Data:Len/binary, _/binary>> = DirectoryData,
      Data
  end,
  DirectoryEntry#{data => NewData};
process_directory_entry(#{data_type := unsigned_short, data_length := Len, data := DirectoryData} = DirectoryEntry, _Bin) ->
  <<Data:Len/binary, _/binary>> = DirectoryData,
  DirectoryEntry#{data => binary:decode_unsigned(Data)};
process_directory_entry(#{data_type := unsigned_rational, data_length := _Len, data := DirectoryData} = DirectoryEntry, _Bin) ->
  DirectoryEntry#{data => binary:decode_unsigned(DirectoryData)};
process_directory_entry(_, _) -> undefined.


tag_name(16#010F) -> make;
tag_name(16#0110) -> model;
tag_name(16#0112) -> orientation;
tag_name(16#011A) -> x_resolution;
tag_name(16#011B) -> y_resolution;
tag_name(16#0128) -> resolution_unit;
tag_name(16#0131) -> software;
tag_name(16#0132) -> date_time;
tag_name(16#0213) -> ycb_cr_positioning;
tag_name(_) -> undefined.

data_type(2) -> {ascii_strings, 1};
data_type(3) -> {unsigned_short, 2};
data_type(5) -> {unsigned_rational, 8};
data_type(X) -> erlang:display(X), undefined.

bits(Number) -> Number.

