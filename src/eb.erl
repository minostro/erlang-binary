-module('eb').
-compile(export_all).

%% API exports
-export([exif_data_for/1]).

-define(SOI, 16#FFD8).
-define(APP1, 16#FFE1).

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
  extract_image_file_directory(RestFromTiff);
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

extract_image_file_directory(<<_DirectoryEntries:2/binary, Rest/binary>>) ->
  {Directory, NewRest} = extract_directory_entry(Rest),
  process_directory_entry(Directory, NewRest).

extract_directory_entry(<<TagNumber:16/integer, DataFormat:16/integer, NumberOfComponents:32/integer, Data:4/bytes, Rest/binary>>) ->
  {DataTypeName, BytesPerComponent} = data_type(DataFormat),
  DirectoryEntry = #{
    tag_name    => tag_name(TagNumber),
    data        => Data,
    data_type   => DataTypeName,
    data_length => NumberOfComponents * BytesPerComponent,
    components  => NumberOfComponents
 },
 {DirectoryEntry, Rest}.

process_directory_entry(#{data_type := ascii_strings, data_length := Len, data := DirectoryData} = DirectoryEntry, Rest) ->
  NewData = case Len > bits(4) of
    true ->
      Offset = binary:decode_unsigned(DirectoryData) - 2,
      NewLen = Len - 1,
      <<_:Offset/binary, Data:NewLen/binary, _/binary>> = Rest,
      Data;
    false ->
      DirectoryData
  end,
  {DirectoryEntry#{data => NewData}, Rest};
process_directory_entry(_, Rest) -> {undefined, Rest}.


tag_name(16#010F) -> make;
tag_name(_) -> undefined.

data_type(2) -> {ascii_strings, 1};
data_type(_) -> undefined.

bits(Number) -> Number.

