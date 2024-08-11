default: help
# 
# 
#                                   88
#                                   88
#                                   88
# 8b,     ,d8  8b,     ,d8  ,adPPYb,88
#  `Y8, ,8P'    `Y8, ,8P'  a8"    `Y88
#    )888(        )888(    8b       88
#  ,d8" "8b,    ,d8" "8b,  "8a,   ,d88
# 8P'     `Y8  8P'     `Y8  `"8bbdP"Y8
# 
# 
# all   - Do it all
all: clean build test run

# build - Build server
build:
	gcc -Wall ./xxd.c -o ./xxd-c
	g++ -std=c++20 -Wall ./xxd.cpp -o ./xxd-cpp
	go build -o ./xxd-go ./xxd.go
	cargo build --manifest-path=./rust/Cargo.toml

# clean - Remove all generated files [TODO: unfinished]
clean:
	rm ./xxd-c
	rm ./xxd-cpp
	rm ./xxd-go
	cargo clean --manifest-path=./rust/Cargo.toml

# help  - Display all commands
help:
	@egrep "^#" [Mm]akefile

# run   - Run server
run: #migrate
	npm run dev

# test  - Run tests
test:
	./test.py

#       -
# - - - -
#       -

# c     - C20
c: build
	./xxd-c zen.txt

# cpp   - C++20
cpp: build
	./xxd-cpp zen.txt

# go    - Go
go: build
	cat zen.txt | ./xxd-go

# js    - JavaScript (Node v20)
js:
	./xxd.js zen.txt

# lisp  - Emacs Lisp
list:
	emacs --script ./xxd.el zen.txt

# rust  - Rust
rust: build
	./rust/target/debug/xxd zen.txt

# py    - Python 3
py:
	./xxd.py zen.txt
