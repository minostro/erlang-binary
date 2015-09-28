eb
=====

An OTP library for manipulating EXIF File Formats (JPEG).  The main idea of this project is to learn binary data
manipulation by using Erlang.

Run
-----

  ```erlang
  ./rebar3 shell
  R = eb:exif_data_for("data/lake.JPG").
  {[#{components => 1,
    data => 2,
    data_length => 2,
    data_type => unsigned_short,
    tag_name => y_cb_cr_positioning},
  #{components => 20,
    data => <<"2009:01:07 12:21:55">>,
    data_length => 20,
    data_type => ascii_strings,
    tag_name => date_time},
  #{components => 10,
    data => <<"Ver.1.01 ">>,
    data_length => 10,
    data_type => ascii_strings,
    tag_name => software},
  #{components => 1,
    data => 2,
    data_length => 2,
    data_type => unsigned_short,
    tag_name => resolution_unit},
  undefined,undefined,
  #{components => 1,
    data => 1,
    data_length => 2,
    data_type => unsigned_short,
    tag_name => orientation},
  #{components => 10,
    data => <<"NIKON D60">>,
    data_length => 10,
    data_type => ascii_strings,
    tag_name => model},
  #{components => 18,
    data => <<"NIKON CORPORATION">>,
    data_length => 18,
    data_type => ascii_strings,
    tag_name => make}],
 <<135,105,0,4,0,0,0,1,0,0,0,216,0,0,109,8,0,0,78,73,75,
   79,78,32,67,79,82,...>>}
  ```

Pointers
--------
http://www.media.mit.edu/pia/Research/deepview/exif.html