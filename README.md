eb
=====

An OTP library for manipulating EXIF File Formats (JPEG).  The main idea of this project is to learn binary data
manipulation by using Erlang.

Run
-----

  ```erlang
  ./rebar3 shell
  R = eb:exif_data_for("data/lake.JPG").
  ```