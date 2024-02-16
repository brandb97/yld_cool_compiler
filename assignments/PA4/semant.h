#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include <string>
#include <unordered_map>
#include <unordered_set> // acyclic check
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

using std::string;
using std::unordered_map;
using std::unordered_set; // acyclic check
using std::hash;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

inline bool eqOp(const Symbol &lhs, const Symbol &rhs)
{
	return lhs->get_string() == rhs->get_string() && 
	       lhs->get_len() == rhs->get_len();
}

inline size_t hasher(const Symbol &sym)
{
	return hash<string>()(string(sym->get_string(), sym->get_len()));
}

// old compiler only understand typedef
// using InheritGraph = unordered_map<Symbol, Symbol, decltype(hasher)*, decltype(eqOp)*>;
// using SymbolSet = unordered_set<Symbol, decltype(hasher)*, decltype(eqOp)*>;
typedef unordered_map<Symbol, Symbol, decltype(hasher)*, decltype(eqOp)*> InheritGraph;
typedef unordered_map<Symbol, Class_, decltype(hasher)*, decltype(eqOp)*> SymToClass;
typedef unordered_set<Symbol, decltype(hasher)*, decltype(eqOp)*> SymbolSet;

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  bool acyclic_check();
  ostream& error_stream;
  InheritGraph ig;
  SymToClass sc;
  Classes basic_classes;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  Class_ sym_to_class(Symbol sym);
  bool less_equal(Symbol ltype, Symbol htype);
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};

#endif

