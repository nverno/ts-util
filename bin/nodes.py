#!/usr/bin/env python3
"""
Get node/field names for parsers.
"""
import os

from tree_sitter import Language


def get_lang(name: str, path: str):
    path = os.path.expanduser(path)

    if not os.path.exists(path):
        raise FileNotFoundError(f"Parser path '{path}' doesn't exist.")

    return Language(path, name)


def get_types(lang: Language):
    named_nodes = []
    anon_nodes = []
    fields = []

    for i in range(lang.node_kind_count):
        if lang.node_kind_is_named(i):
            named_nodes.append(lang.node_kind_for_id(i))
        elif lang.node_kind_is_visible(i):
            anon_nodes.append(lang.node_kind_for_id(i))
    for i in range(1, lang.field_count + 1):
        fields.append(lang.field_name_for_id(i))

    return named_nodes, anon_nodes, fields


def main():
    import argparse

    parser = argparse.ArgumentParser(description="Get node info for parser")
    parser.add_argument(
        "-t",
        "--type",
        choices=["named", "anon", "field", "all"],
        default="all",
        help="Type of nodes to process",
    )
    parser.add_argument("name", type=str, help="Name of parser")
    parser.add_argument("path", type=str, help="Path to parser.so")
    args = parser.parse_args()

    lang = get_lang(args.name, args.path)
    named, anon, fields = get_types(lang)
    if args.type == "named":
        print("\n".join(named))
    elif args.type == "field":
        print("\n".join(fields))
    elif args.type == "anon":
        print("\n".join(anon))
    else:
        print("Named:\n\t", "\n\t ".join(named))
        print("Anon:\n\t", "\n\t ".join(anon))
        print("Fields:\n\t", "\n\t ".join(fields))


if __name__ == "__main__":
    main()
