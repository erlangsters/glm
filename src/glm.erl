-module(glm).

-export([
    foo/0
]).

-doc("""
To be written.
""").
-spec foo() -> ok.
foo() ->
    glm_raw:foo().
