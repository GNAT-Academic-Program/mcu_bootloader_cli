#! /bin/bash

# oneTimeSetUp() {
#   gnome-terminal -- /bin/bash -c "../bin/mcu_cli"
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

# test cases
test_info_command() {
  output=$(printf "info\nquit\n" | ../bin/mcu_cli)
  expected=$(echo -ne "> Info page will go here\n\nDetected sub commands: \n\n> \n")
  assertEquals "$expected" "$output" 
}

test_flash_command() {
  output=$(printf "flash\nquit\n" | ../bin/mcu_cli)
  expected=$(echo -ne "> Flash command placeholder\n\nDetected sub commands: \n\n> \n")
  assertEquals "$expected" "$output" 
}

test_erase_command() {
  output=$(printf "erase 6 7\nquit\n" | ../bin/mcu_cli)
  expected=$(echo -ne "Erasing...\nErasing suceeded.\n\nuser > \n")
  assertEquals "$expected" "$output" 
}

test_help_command() {
  output=$(printf "help\n\x1B\nquit\n" | ../bin/mcu_cli)
  assertContains "$output" "The manual for command line commands for the MCU bootloader"
}

test_clear_command() {
  output=$(printf "clear\nquit\n" | ../bin/mcu_cli)
  expected=$(echo -ne "> \x1b[2J\x1b[;H\nDetected sub commands: \n\n> \n")
  assertEquals "$expected" "$output" 
}

test_quit_command() {
  printf "quit\n" | ../bin/mcu_cli &> /dev/null
  assertEquals 0 "$?" 
}

test_sub_command() {
  output=$(printf "delete sub1 sub2\nquit\n" | ../bin/mcu_cli)
  expected=$(echo -ne "> Delete command placeholder\n\nDetected sub commands: \nsub1\nsub2\n> \n")
  assertEquals "$expected" "$output" 
}

parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )

. ${parent_path}/shunit2