ELL_DEFCLASS(symbol)
ELL_DEFGENERIC(name) // symbol -> string

ELL_DEFCLASS(string)
ELL_DEFGENERIC(code_points) // string -> wchar_t[]

ELL_DEFCLASS(function)

ELL_DEFCLASS(list) // <T>
ELL_DEFGENERIC(elements) // list<T> -> range<T>
ELL_DEFCLASS(linked_list)
ELL_DEFCLASS(linked_list_range)

ELL_DEFCLASS(map) // <K V>
ELL_DEFGENERIC(entry_set) // map<K V> -> set<map_entry<K V>>
ELL_DEFCLASS(map_entry) // <K V>
ELL_DEFCLASS(tree_map)
ELL_DEFCLASS(tree_map_entry_range)

ELL_DEFCLASS(set) // <T>
ELL_DEFCLASS(tree_set) // <T>
ELL_DEFCLASS(tree_set_entry_range) // <T>

ELL_DEFCLASS(range) // <T>

ELL_DEFCLASS(boolean)

ELL_DEFCLASS(option) // <T>
ELL_DEFCLASS(some) // <T>
ELL_DEFCLASS(none) // <T>

ELL_DEFCLASS(unbound)

ELL_DEFCLASS(syntax)
