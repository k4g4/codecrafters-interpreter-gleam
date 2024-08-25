import gleam/io
import gleam/string
import interpreter/lexer

import argv
import simplifile

pub fn main() {
  let args = argv.load().arguments

  case args {
    ["tokenize", filename] -> {
      case simplifile.read(filename) {
        Ok(contents) -> {
          case string.is_empty(contents) {
            True -> io.println("EOF  null")
            _ ->
              case lexer.scan(contents) {
                Ok(out) -> io.println(out)
                Error(out) -> io.println_error(out)
              }
          }
        }
        Error(error) -> {
          io.println_error("Error: " <> simplifile.describe_error(error))
          exit(1)
        }
      }
    }
    _ -> {
      io.println_error("Usage: ./your_program.sh tokenize <filename>")
      exit(1)
    }
  }
}

@external(erlang, "erlang", "halt")
pub fn exit(code: Int) -> Nil
