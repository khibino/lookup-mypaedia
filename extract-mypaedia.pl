#                                                         -*- Perl -*-
# Copyright (C) 2000  Kazuhiko Shiozaki
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#

require 5.005;

use English;
use FileHandle;

$file = shift;
$offset = hex shift;
$size = hex shift;
$tmp_file = shift;

# print "File:$file\n";
# print "Offset:$offset\n";
# print "Size:$size\n";
# print "Tmp_File:$tmp_file\n";

$handle_in = FileHandle->new();
if (!$handle_in->open("$file", 'r')) {
  die "$PROGRAM_NAME: failed to open the file, $ERRNO: $file\n";
}
binmode $handle_in;

$handle_out = FileHandle->new();
if (!$handle_out->open("$tmp_file", 'w')) {
  die "$PROGRAM_NAME: failed to open the file, $ERRNO: $tmp_file\n";
}
binmode $handle_out;

seek($handle_in, $offset, 0) || die "Seek Error\n";
read $handle_in, $content, $size || die "Read Error\n";
print $handle_out $content || die "Write Error\n";
