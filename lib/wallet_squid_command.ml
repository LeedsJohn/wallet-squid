open! Core

let command = Command.group ~summary:"Wallet Squid" [ "tag", Tag_commands.command ]
