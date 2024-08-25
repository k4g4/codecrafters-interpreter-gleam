import gleam/bool
import gleam/string

pub fn scan(in: String) -> Result(String, String) {
  use <- bool.guard(string.is_empty(in), Ok("EOF  null"))

  Ok("scan not implemented")
}
