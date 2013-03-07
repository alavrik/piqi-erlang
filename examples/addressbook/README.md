This directory contains an example of an addressbook data structure definition
and several Erlang programs that rely on it to manipulate addressbook data.

This example is based on the Google Protocol Buffers examples:
<http://protobuf.googlecode.com/svn/trunk/examples/> (See their README.txt file
for details).


Running
-------

    make

    make test


Files
-----

`addressbook.proto` -- the original Google Protocol Buffers definition copied
from examples directory of the protobuf distribution

`src/addressbook.proto.piqi` -- definition of the addressbook data structure
converted from the original `addressbook.proto` using `piqi of-proto` command

`src/addressbook.erlang.piqi` -- Erlang-specific extensions to the converted
`addressbook.proto.piqi` specificaion

`src/add_person.erl` -- adds a person to the addressbook

`src/list_people.erl` -- lists the contents of the addressbook

`src/io_json_xml_pb.erl` -- example of how to read and write the addressbook
represented in XML, JSON, Protocol Buffers and Piq formats

`./test` --  shell script containing tests for the `add_person` and
`list_people` Erlang programs


The `add_person` and `list_people` Erlang programs implement exactly the same
functionality as Python, Java and C++ programs from the Protobuf examples. The
data structure and the binary encoding of the addressbook data structure is
fully compatible as well. As a result, the Erlang programs can read an
addressbook structure created by Python/C++/Java programs and vice versa.

This example also contains the `addressbook.piq` file which is a sample
addressbook data structure represented as a Piq file. It can be converted to the
binary Protocol Buffers format (using `piqi convert`) and then can be
manipulated by the Erlang programs.

