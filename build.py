from tree_sitter import Language
Language.build_library(
    'build/languages.so',
    [
        './tree-sitter-python',
        './tree-sitter-javascript',
        './tree-sitter-typescript/typescript'
    ] 
)