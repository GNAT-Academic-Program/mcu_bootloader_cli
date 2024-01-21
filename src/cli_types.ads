--this package is only for types which are shared among ALL commands as well as the main cli
--any type or enum which is only used in one command will be created within that command package
package cli_types is 
    type subCommands is array (1..5) of Character;
end cli_types;