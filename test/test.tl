

CONSTANT = 42


fn elem(index, arr) {
    if index == 0 { first(arr) }
    else { elem(index - 1, rest(elem)) }
}


fn lookup(path, nested) {

    fragment = split(path, ".", 1)
    fst = first(fragment)
    lst = last(fragment)

    if is_key_of(nested, fst) { lookup(lst, get(fst, nested)) }
    else { error("path " + path + "does not exist in map") }
}

fn await_all(futs) { await map(futs, async identity) }

fn fanout(seq, f) { async map(seq, f) }

fn main() {

    nested = {
        "foo": [1, 2, 3, something(42)],
        "bar": {
            "qux": { "data": 1 },
            "foo": 3.142 * sq(x)
        }
    }

    heavy = async fanout(get("foo", nested))
    cond = (
        lookup("bar.qux.data", nested)
        == lookup("bar.foo", nested)
    )

    result = await (if cond { heavy } else { async 42 })
    print(result)

}
