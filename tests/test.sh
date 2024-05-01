#!/bin/bash

# oneTimeSetUp() {
#   gnome-terminal -- /bin/bash -c "./bin/mcu_cli"
#   cli_id=$(xdotool getactivewindow)
# }

# simulate_cli() {
#   xdotool windowactivate $cli_id
#   xdotool type --delay 100 "$1"
#   xdotool key Return
#   sleep 3
#   xdotool key ctrl+shift+c
#   xclip -selection clipboard -o > output1.txt
# }
# \x1b[32muser > 

# test cases
test_info_command() {
  output=$(printf "info\nquit\n" | ./bin/mcu_cli)
  expected=$(echo -ne "\x1b[32muser\x1b[0m > \nDevice ID: 0x449\nRevision ID: 0x1001\nVersion: 0.1.0-alpha\n\n\x1b[32muser\x1b[0m > \n")
  assertEquals "$expected" "$output" 
}

test_erase_with2subs_command() {
  output=$(printf "erase 6 7\nquit\n" | ./bin/mcu_cli)
  expected=$(echo -ne "\x1b[32muser\x1b[0m > Erasing...\nErasing succeeded.\n\n\x1b[32muser\x1b[0m > \n")
  assertEquals "$expected" "$output" 
}

test_erase_with1sub_command() {
  output=$(printf "erase 7\nquit\n" | ./bin/mcu_cli)
  expected=$(echo -ne "\x1b[32muser\x1b[0m > Erasing...\nErasing succeeded.\n\n\x1b[32muser\x1b[0m > \n")
  assertEquals "$expected" "$output" 
}

test_erase_command() {
  output=$(printf "erase\n7\n7\nquit\n" | ./bin/mcu_cli)
  expected=$(echo -ne "\x1b[32muser\x1b[0m > \n\x1b[1mErase Program\x1b[22m\n\nEnter the starting sector to erase: Enter the ending sector to erase: Erasing...\nErasing succeeded.\n\n\x1b[32muser\x1b[0m > \n")
  assertEquals "$expected" "$output" 
}

test_help_command() {
  output=$(printf "help\n\x1B\nquit\n" | ./bin/mcu_cli)
  assertContains "$output" "The manual for command line commands for the MCU bootloader"
}

test_help_help_command() {
  output=$(printf "help help\n\x1B\nquit\n" | ./bin/mcu_cli)
  assertContains "$output" "help (1)"
}

test_help_helpnum_command() {
  output=$(printf "help 1\n\x1B\nquit\n" | ./bin/mcu_cli)
  assertContains "$output" "help (1)"
}

test_help_info_command() {
  output=$(printf "help info\n\x1B\nquit\n" | ./bin/mcu_cli)
  assertContains "$output" "info (2)"
}

test_help_infonum_command() {
  output=$(printf "help 2\n\x1B\nquit\n" | ./bin/mcu_cli)
  assertContains "$output" "info (2)"
}

test_help_flash_command() {
  output=$(printf "help flash\n\x1B\nquit\n" | ./bin/mcu_cli)
  assertContains "$output" "flash (3)"
}

test_help_flashnum_command() {
  output=$(printf "help 3\n\x1B\nquit\n" | ./bin/mcu_cli)
  assertContains "$output" "flash (3)"
}

test_help_erase_command() {
  output=$(printf "help erase\n\x1B\nquit\n" | ./bin/mcu_cli)
  assertContains "$output" "erase (4)"
}

test_help_erasenum_command() {
  output=$(printf "help 4\n\x1B\nquit\n" | ./bin/mcu_cli)
  assertContains "$output" "erase (4)"
}

test_help_clear_command() {
  output=$(printf "help clear\n\x1B\nquit\n" | ./bin/mcu_cli)
  assertContains "$output" "clear (5)"
}

test_help_clearnum_command() {
  output=$(printf "help 5\n\x1B\nquit\n" | ./bin/mcu_cli)
  assertContains "$output" "clear (5)"
}

test_help_quit_command() {
  output=$(printf "help quit\n\x1B\nquit\n" | ./bin/mcu_cli)
  assertContains "$output" "quit (6)"
}

test_help_quitnum_command() {
  output=$(printf "help 6\n\x1B\nquit\n" | ./bin/mcu_cli)
  assertContains "$output" "quit (6)"
}

test_help_verify_command() {
  output=$(printf "help verify\n\x1B\nquit\n" | ./bin/mcu_cli)
  assertContains "$output" "verify (7)"
}

test_help_verifynum_command() {
  output=$(printf "help 7\n\x1B\nquit\n" | ./bin/mcu_cli)
  assertContains "$output" "verify (7)"
}

test_help_notfound_command() {
  output=$(printf "help 8\n\x1B\nquit\n" | ./bin/mcu_cli)
  assertContains "$output" "Unknown sub command:"
}

test_clear_command() {
  output=$(printf "clear\nquit\n" | ./bin/mcu_cli)
  expected=$(echo -ne "\x1b[32muser\x1b[0m > \x1b[2J\x1b[;H\n\x1b[32muser\x1b[0m > \n")
  assertEquals "$expected" "$output" 
}

test_quit_command() {
  printf "quit\n" | ./bin/mcu_cli # &> /dev/null
  assertEquals 0 "$?" 
}

test_unknown_command() {
  output=$(printf "jump\n\x1B\nquit\n" | ./bin/mcu_cli)
  assertContains "$output" "Unknown command:"
}

test_erase_toomanysubs_command() {
  output=$(printf "erase 5 6 7\n\x1B\nquit\n" | ./bin/mcu_cli)
  assertContains "$output" "Too many arguments for the erase command."
}

test_help_multiple_subs_command() {
  output=$(printf "help 1 2\n\x1B\x1B\nquit\n" | ./bin/mcu_cli)
  assertContains "$output" "info (2)"
}

test_clear_ignoresub_command() {
  output=$(printf "clear 1\nquit\n" | ./bin/mcu_cli)
  expected=$(echo -ne "\x1b[32muser\x1b[0m > \x1b[2J\x1b[;H\n\x1b[32muser\x1b[0m > \n")
  assertEquals "$expected" "$output" 
}

test_quit_ignoresub_command() {
  printf "quit 1\n" | ./bin/mcu_cli &> /dev/null
  assertEquals 0 "$?" 
}

test_info_ignoresub_command() {
  output=$(printf "info 1\nquit\n" | ./bin/mcu_cli)
  expected=$(echo -ne "\x1b[32muser\x1b[0m > \nDevice ID: 0x449\nRevision ID: 0x1001\nVersion: 0.1.0-alpha\n\n\x1b[32muser\x1b[0m > \n")
  assertEquals "$expected" "$output" 
}

test_erase_flash_verify_reset_command() {
  output=$(printf "flash ./bin/firmware.bin 08080000\n\x1B\x1B\nquit\n" | ./bin/mcu_cli)
  assertContains "$output" "Flashing succeeded."
}

# flash already tests these commands

# test_verify_command() {
#   output=$(printf "verify ./bin/firmware.bin 08080000\nquit\n" | ./bin/mcu_cli)
#   assertContains "$output" "Verification succeeded."
# }

# test_reset_command() {
#   output=$(printf "reset\n\x1B\x1B\nquit\n" | ./bin/mcu_cli)
#   assertContains "$output" "Reset succeeded."
# }

parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )
cd $parent_path/..

. ${parent_path}/shunit2