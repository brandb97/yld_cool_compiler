

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}



ClassTable::ClassTable(Classes classes) : 
    semant_errors(0) , error_stream(cerr), 
    ig(2 * classes->len(), hasher, eqOp), 
    basic_classes(nil_Classes()),
    sc(2 * classes->len(), hasher, eqOp) {
    install_basic_classes();

    // build inheritance graph
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        auto class_ = classes->nth(i);
        ig[class_->get_name()] = class_->get_parent();
        sc[class_->get_name()] = class_;

        if (semant_debug) {
            error_stream << "insert ig " << class_->get_name() << "->" << class_->get_parent() << endl;
        }
    }

    // acyclic check
    if (!acyclic_check()) {
        error_stream << "acyclic check failed\n";
        exit(1);
    } else if (semant_debug) {
        error_stream << "acyclic check passed\n"; // should I use error_stream?
    }
}

bool ClassTable::acyclic_check() {
    auto sym_set = SymbolSet(ig.bucket_count(), hasher, eqOp);
    sym_set.insert(Object); // add Object so that dfs can stop

    // for (auto sym_pair : ig) { // old compiler didn't understand this
    for (auto sym_itr = ig.begin(); sym_itr != ig.end(); sym_itr++) {
        auto sym_pair = *sym_itr;
        auto sym_path = SymbolSet(ig.bucket_count(), hasher, eqOp);
        auto child = sym_pair.first;
        auto parent = sym_pair.second;

        while (sym_set.find(child) == sym_set.end()) {
            if (semant_debug) {
                error_stream << "reach " << child << endl;
            }

            sym_set.insert(child);
            sym_path.insert(child);
            child = parent;
            if (ig.find(child) != ig.end()) {
                parent = ig[child];
            } else {
                error_stream << "reach undefined class " << parent << endl;
                return false;
            }
        }

        if (sym_path.find(child) != sym_path.end()) {
            if (semant_debug) {
                error_stream << "reach " << child << " again!\n";
            }
            return false;
        }
    }
    return true;
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // IMPORTANT: only add ig support

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);
    ig[Object] = No_class;
    basic_classes = single_Classes(Object_class);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);
    basic_classes = append_Classes(basic_classes, single_Classes(IO_class));
    ig[IO] = Object;

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);
    basic_classes = append_Classes(basic_classes, single_Classes(Int_class));
    ig[Int] = Object;

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);
    basic_classes = append_Classes(basic_classes, single_Classes(Bool_class));
    ig[Bool] = Object;

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);
    basic_classes = append_Classes(basic_classes, single_Classes(Str_class));           
    ig[Str] = Object;
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 

Class_ ClassTable::sym_to_class(Symbol sym)
{
    if (sc.find(sym) == sc.end()) {
        for (int i = basic_classes->first(); basic_classes->more(i); i = basic_classes->next(i)) {
            auto cls = basic_classes->nth(i);
            if (cls->get_name()->equal_string(sym->get_string(), sym->get_len())) {
                return cls;
            }
        }
    } // must be basic_classes

    return sc[sym];
}

bool ClassTable::less_equal(Symbol ltype, Symbol htype)
{
    if (ltype == Object) {
        return true;
    }

    while (htype != Object) {
        if (ltype == htype) {
            return true;
        }

        /* htype = get_parent(htype) */
        htype = ig[htype];
    }

    return false;
}

Formals ClassTable::sym_to_formals(Symbol sym, Symbol class_name)
{
    while (true) {
        auto cls = sym_to_class(class_name);
        auto fts = cls->get_features();
        for (int i = fts->first(); fts->more(i); i = fts->next(i)) {
            if (method_class *m = dynamic_cast<method_class *>(fts->nth(i))) {
                if (m->get_name() == sym) {
                    return m->get_formals();
                }
            }
        }

        if (class_name == Object) {
            break;
        }
        class_name = ig[class_name];
    }

    cerr << "Compiler bug: can't reach here" << endl;
    exit(1);
}


/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        auto class_ = classes->nth(i);
        class_->semant(classtable);
    }

    if (classtable->errors()) {
	    cerr << "Compilation halted due to static semantic errors." << endl;
	    exit(1);
    }
}

void class__class::semant(ClassTableP ct)
{
    /* use ct->semant_error(filename, t) to output error */
    /* use attrs to build symbol table */
    SymTab *st = new SymTab();
    /* build symbol table for parent first */
    st->enterscope();
    add_attr(st, ct);

    /* semantic checks */
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        features->nth(i)->semant(st, ct, this);
    }
    st->exitscope();
}

void class__class::add_attr(SymTab *st, ClassTableP ct)
{
    // Optimize this code
    if (name->equal_string(Object->get_string(), Object->get_len())) {
        return;
    }

    ct->sym_to_class(get_parent())->add_attr(st, ct);
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        if (attr_class *at = dynamic_cast<attr_class *>(features->nth(i))) {
            /* check repeat value */
            if (st->probe(at->get_name()) != NULL) {
                /* repeated attribute error */
                ct->semant_error(this) << "repeated attr " << at->get_name() << endl;
                continue;
            }

            st->addid(at->get_name(), at->get_type_decl());
        }
    }
}

void attr_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    init->semant(st, ct, cur);
    if (!ct->less_equal(type_decl, init->get_type())) {
        ct->semant_error(cur->get_filename(), this) 
            << "can't assign " << init->get_type() << " to" << type_decl << endl;
    }
}

void method_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    st->enterscope();
    /* add formals to symbol table */
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        auto form = formals->nth(i);
        st->addid(form->get_name(), form->get_type_decl());
    }

    /* expr check */
    expr->semant(st, ct, cur);

    /* return type check */
    if (!ct->less_equal(return_type, expr->get_type())) {
        ct->semant_error(cur->get_filename(), this) 
            << "can't return " << expr->get_type() 
            << " to a fuction which returns " << return_type << endl;
    }
    st->exitscope();
}

