# mcu_bootloaer_cli

This repository contains the CLI for the [Ada Microcontroller Bootloader](https://github.com/GNAT-Academic-Program/mcu_bootloader). It was designed to run on Ubuntu Linux, but may also work on other operating systems.

## Building
This project uses the [Alire package manager](https://alire.ada.dev). The CLI can be built by running `alr build`, or built and ran by running `alr run`. Either option creates the output executable at `bin/mcu_cli`.

## Commands

All available commands can be listed by running `help` from the CLI. Running `help <command>`, like `help flash` will get detailed usage information for the command.

## Tests
This project uses [ShUnit2](https://github.com/kward/shunit2) to run automated test cases. All tests can be ran using `./tests/test.sh` from the project root, or a specific test can be ran using `./tests/test.sh -- <insert test name here>`. For example, to run the clear command test case, execute `./tests/test.sh -- test_clear_command`. Multiple tests can be run by listing multiple names separated by spaces.