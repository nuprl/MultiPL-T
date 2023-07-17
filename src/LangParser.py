from tree_sitter import Language, Parser
from pathlib import Path
class LangParser:
    '''
    Wrapper class for the boilerplate surrounding tree-sitter
    '''
    def __init__(self, lang):
        self.language = Language('build/languages.so', lang)
        self.queries = {}
        self.parser = Parser()
        self.parser.set_language(self.language)

    def add_query(self, name, querystr):
        self.queries[name] = self.language.query(querystr)

    def query(self, name, node):
        query = self.queries[name]
        captures = query.captures(node)
        return captures

    def parse(self, code: bytes):
        tree = self.parser.parse(code)
        return tree

