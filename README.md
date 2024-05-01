# mcu_bootloader_cli

This repository contains the CLI for the [Ada Microcontroller Bootloader](https://github.com/GNAT-Academic-Program/mcu_bootloader). It was designed to run on Ubuntu Linux, but may also work on other operating systems.

## Introduction
This project was made for a senior design project at [Pennsylvania State University, Behrend College](https://behrend.psu.edu/), and was sponsored by [AdaCore](https://www.adacore.com/) and part of its [GNAT Academic Program](https://github.com/GNAT-Academic-Program). The goal of the project is to create a bootloader written in Ada for the STM32 microcontroller family. This project was tested for the STM32F746GDISCOVERY Board. The following repository is for the command line interface application that directly communicates with the [Ada Microcontroller Bootloader](https://github.com/GNAT-Academic-Program/mcu_bootloader) that has been loaded on the [STM32F746G-DISCO](https://www.st.com/en/evaluation-tools/32f746gdiscovery.html) Board. More about the project can be found on the project's [senior design web page](https://sites.psu.edu/behrendseniordesign/2024/04/23/ada-mcu-bootloader/).

## Building
This project uses the [Alire package manager](https://alire.ada.dev). The CLI can be built by running `alr build`, or built and ran by running `alr run`. Either option creates the output executable at `bin/mcu_cli`.

## Setting Up the Environment
This project requires you to load the [Ada Microcontroller Bootloader](https://github.com/GNAT-Academic-Program/mcu_bootloader) onto your STM32 microcontroller. Instructions to load the bootloader can be found in the README file for it. Before running any of the commands on the CLI, make sure your microcontroller is running in the bootloader first. 
1. Reset your microcontroller by clicking on the reset button on the back of the board.
2. Interrupt the booting sequence by pressing the user button on the back of the board within the first 100ms of start up.

Interrupting the board at start up:

<img src="https://github.com/GNAT-Academic-Program/mcu_bootloader_cli/blob/main/assets/Interrupting%20Board.gif" width="400" height="400">

[STM32F746G-DISCO](https://www.st.com/en/evaluation-tools/32f746gdiscovery.html) User and Reset Buttons:

<img src="https://github.com/GNAT-Academic-Program/mcu_bootloader_cli/blob/main/assets/STM32F746G-DISCO.png" width="828" height="442">

## Commands
Running the command is as simple as typing it in the command line interface and pressing `Enter`.

Running the `info` command:

<img src="https://github.com/GNAT-Academic-Program/mcu_bootloader_cli/blob/main/assets/Info%20Command%20Demo.gif" width="400" height="400">

All available commands can be listed by running `help` from the CLI. Running `help <command>`, like `help flash` will get detailed usage information for the command.

Current implemented commands:
1. Help
2. Info
3. Flash
4. Erase
5. Clear
6. Quit
7. Verify
8. Reset

## Tests
This project uses [ShUnit2](https://github.com/kward/shunit2) to run automated test cases. All tests can be ran using `./tests/test.sh` from the project root, or a specific test can be ran using `./tests/test.sh -- <insert test name here>`. For example, to run the clear command test case, execute `./tests/test.sh -- test_clear_command`. Multiple tests can be run by listing multiple names separated by spaces.

## Credits
Thank you to [Pennsylvania State University, Behrend College](https://behrend.psu.edu/), and [AdaCore](https://www.adacore.com/) for sponsoring and hosting this project. 

Further credits goes to [Mr. Olivier Henley](https://blog.adacore.com/author/henley) (GitHub: [ohenley](https://github.com/ohenley)), our industry advisor from AdaCore, and [Dr. Chen Cao](https://behrend.psu.edu/person/chen-cao-phd), our project advisor for senior design.

Final credits goes to the senior design team:
1. [Alan Everett](https://www.linkedin.com/in/alan-everett-3215b6223/) (GitHub: [thatcomputerguy0101 ](https://github.com/thatcomputerguy0101))
2. [Joseph Smith](https://www.linkedin.com/in/joseph-a-smith-erie/) (GitHub: [Jas8101](https://github.com/Jas8101))
3. [Xavier Zhang](https://www.linkedin.com/in/zhangqx/) (GitHub: [Lereona](https://github.com/Lereona))
