

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

bool ClassTable::less_equal(Symbol ltype, Symbol rtype, Symbol cur_type)
{
    if (rtype == SELF_TYPE) {
        return ltype == SELF_TYPE;
    } else if (ltype == SELF_TYPE) {
        ltype = cur_type;
    }

    /* both ltype and rtype are not SELF_TYPE now */
    if (rtype == Object) {
        return true;
    }

    while (ltype != Object) {
        if (ltype == rtype) {
            return true;
        }

        /* htype = get_parent(htype) */
        ltype = ig[ltype];
    }

    return false;
}

Symbols ClassTable::sym_to_types(Symbol mth_name, Symbol class_name)
{
    while (true) {
        auto cls = sym_to_class(class_name);
        auto fts = cls->get_features();
        for (int i = fts->first(); fts->more(i); i = fts->next(i)) {
            if (method_class *m = dynamic_cast<method_class *>(fts->nth(i))) {
                if (m->get_name() == mth_name) {
                    return m->to_types();
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

Symbol ClassTable::lub(Symbol t1, Symbol t2)
{
    // build t1 path
    SymbolSet t1_path(ig.bucket_count(), hasher, eqOp);
    t1_path.insert(Object);
    while (t1 != Object) { // buggy code
        t1_path.insert(t1);
        t1 = ig[t1];
    }

    while (t2 != Object) { // buggy code
        if (t1_path.find(t2) != t1_path.end()) {
            return t2;
        }
        t2 = ig[t2];
    }
    return Object;
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
        if (semant_debug) {
            cerr << "semant check class" << class_->get_name() << endl;
        }
        class_->semant(classtable);
    }

    if (classtable->errors()) {
	    cerr << "Compilation halted due to static semantic errors." << endl;
	    exit(1);
    }
}

void class__class::semant(ClassTableP ct)
{
    if (name == SELF_TYPE) {
        ct->semant_error(this) << "class name can't be SELF_TYPE" << endl;
        return;
    } else if (parent == SELF_TYPE) {
        ct->semant_error(this) << "class can't inherits SELF_TYPE" << endl;
        return;
    }

    /* use ct->semant_error(filename, t) to output error */
    /* use attrs to build symbol table */
    SymTab *st = new SymTab();
    /* build symbol table for parent first */
    st->enterscope();
    st->addid(self, SELF_TYPE); /* O[SELF_TYPE/self] */
    add_attr(st, ct);

    /* semantic checks */
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        features->nth(i)->semant(st, ct, this);
        if (semant_debug) {
            ct->semant_error(this)
                << "finish semnat check feature " << i << endl;
        }
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
    if (semant_debug) {
        ct->semant_error(cur->get_filename(), this)
            << "semnat check attr " << name << ":" << type_decl << endl;
    }

    init->semant(st, ct, cur);
    if (init->get_type() && !ct->less_equal(init->get_type(), type_decl, cur->get_name())) {
        ct->semant_error(cur->get_filename(), this) 
            << "can't assign " << init->get_type() << " to" << type_decl << endl;
    }
}

void method_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    if (semant_debug) {
        ct->semant_error(cur->get_filename(), this)
            << "semnat check feature " << name << endl;
    }

    st->enterscope();
    /* add formals to symbol table */
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        auto form = formals->nth(i);
        auto td = form->get_type_decl();
        if (td == SELF_TYPE) {
            ct->semant_error(cur->get_filename(), this) 
                << form << "argument can't be SELF_TYPE" << endl;
            form->set_type_decl(cur->get_name());
            st->addid(form->get_name(), cur->get_name());
        } else {
            st->addid(form->get_name(), td);
        }
    }

    /* expr check */
    expr->semant(st, ct, cur);

    /* return type check */
    if (!ct->less_equal(expr->get_type(), return_type, cur->get_name())) {
        ct->semant_error(cur->get_filename(), this) 
            << "can't return " << expr->get_type() 
            << " to a fuction which returns " << return_type << endl;
    }
    st->exitscope();
}

Symbols method_class::to_types()
{
    auto syms = vector<Symbol>();
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        auto form = formals->nth(i);
        syms.push_back(form->get_type_decl());
    }

    syms.push_back(return_type);
    return syms;
}


void assign_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    if (auto e = dynamic_cast<no_expr_class *>(expr)) {
        return;
    }

    expr->semant(st, ct, cur);

    auto ltype = st->lookup(name);
    if (!ct->less_equal(expr->get_type(), ltype, cur->get_name())) {
        ct->semant_error(cur->get_filename(), this) 
            << "can't assign " << expr->get_type() 
            << " to " << name << " which has type " << ltype << endl;
    }
}

void static_dispatch_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    expr->semant(st, ct, cur);
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        actual->nth(i)->semant(st, ct, cur);
    }

    if (type_name == SELF_TYPE) {
        ct->semant_error(cur->get_filename(), this)
            << "dispatch type name can't be SELF_TYPE" << endl;
    }
    auto types = ct->sym_to_types(name, type_name);


    for (int i = actual->first(), t_idx = 0; actual->more(i); i = actual->next(i), t_idx++) {
        auto expr_type = actual->nth(i)->get_type();
        auto type_decl = types[t_idx];
        if (!ct->less_equal(expr_type, type_decl, cur->get_name())) {
            ct->semant_error(cur->get_filename(), this) 
                << "can't assign " << expr->get_type() 
                << " to " << name << " which has type " << type_decl << endl;
        }
    }

    auto return_type = types.back();
    if (return_type == SELF_TYPE) {
        type = expr->get_type();
    } else {
        type = return_type;
    }
}

void dispatch_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    expr->semant(st, ct, cur);
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        actual->nth(i)->semant(st, ct, cur);
    }

    Symbols types;
    if (expr->get_type() == SELF_TYPE) {
        types = ct->sym_to_types(name, cur->get_name());
    } else {
        types = ct->sym_to_types(name, expr->get_type());
    }


    for (int i = actual->first(), t_idx = 0; actual->more(i); i = actual->next(i), t_idx++) {
        auto expr_type = actual->nth(i)->get_type();
        auto type_decl = types[t_idx];
        if (!ct->less_equal(expr_type, type_decl, cur->get_name())) {
            ct->semant_error(cur->get_filename(), this) 
                << "can't assign " << expr->get_type() 
                << " to " << name << " which has type " << type_decl << endl;
        }
    }

    auto return_type = types.back();
    if (return_type == SELF_TYPE) {
        type = expr->get_type();
    } else {
        type = return_type;
    }    
}

void cond_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    pred->semant(st, ct, cur);
    then_exp->semant(st, ct, cur);
    else_exp->semant(st, ct, cur);
    
    if (pred->get_type() != Bool) {
        ct->semant_error(cur->get_filename(), this)
            << "conditional predication need to have Bool type" 
            << " which is" << pred->get_type() << endl;
        type = Object;
    }
    type = ct->lub(then_exp->get_type(), else_exp->get_type());
}

void loop_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    pred->semant(st, ct, cur);
    if (pred->get_type() != Bool) {
        ct->semant_error(cur->get_filename(), this)
            << "predication of while loop should have Bool type" 
            << " which have type " << pred->get_type() << endl;
    }
    body->semant(st, ct, cur);
    type = Object;
}

void typcase_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    expr->semant(st, ct, cur);

    /* check all branch should have distinct types */
    SymbolSet type_set(cases->len(), hasher, eqOp);
    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        auto t = cases->nth(i)->get_type_decl();
        if (type_set.find(t) != type_set.end()) {
            ct->semant_error(cur->get_filename(), this)
                << "repeated type " << t << " in case branches" << endl;
        }
    }
    
    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        auto cas = cases->nth(i);
        cas->semant(st, ct, cur);

        auto cas_return_type = cas->get_return_type();
        if (i == cases->first()) {
            type = cas_return_type;
        } else {
            type = ct->lub(type, cas_return_type);
        }
    }
}

void branch_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    st->enterscope();
    st->addid(name, type_decl);
    expr->semant(st, ct, cur);
    st->exitscope();
}

void block_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    int i = body->first();
    while (true) {
        auto e = body->nth(i);
        e->semant(st, ct, cur);
        i = body->next(i);
        if (!body->more(i)) {
            type = e->get_type();
            break;
        }
    }
}

void let_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    if (auto ie = dynamic_cast<no_expr_class *>(init)) {
        goto finish_init_check;
    }

    init->semant(st, ct, cur);
    if (!ct->less_equal(init->get_type(), type_decl, cur->get_name())) {
        ct->semant_error(cur->get_filename(), this) 
            << "can't assign " << init->get_type() 
            << " to " << identifier << " which has type " << type_decl << endl;
    }

finish_init_check:
    st->enterscope();
    st->addid(identifier, type_decl);
    body->semant(st, ct, cur);
    st->exitscope();

    type = body->get_type();
}

bool op_semant(Expression e1, Expression e2, SymTab *st, ClassTableP ct, Class_ cur, tree_node *n)
{
    e1->semant(st, ct, cur);
    e2->semant(st, ct, cur);
    if (e1->get_type() != Int || e2->get_type() != Int) {
        ct->semant_error(cur->get_name(), n)
            << "Only Int can do arithmetic operation" << endl;
        return false;
    }
    return true;
}

void plus_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    if (op_semant(e1, e2, st, ct, cur, this)) {
        type = Int;
    } else {
        type = Object;
    }
}

void sub_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    if (op_semant(e1, e2, st, ct, cur, this)) {
        type = Int;
    } else {
        type = Object;
    }
}

void mul_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    if (op_semant(e1, e2, st, ct, cur, this)) {
        type = Int;
    } else {
        type = Object;
    }
}

void divide_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    if (op_semant(e1, e2, st, ct, cur, this)) {
        type = Int;
    } else {
        type = Object;
    }
}


void neg_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    e1->semant(st, ct, cur);
    if (e1->get_type() != Bool) {
        ct->semant_error(cur->get_filename(), this)
            << "neg can only operate on Bool expression" << endl;
        type = Object;
    } else {
        type = Bool;
    }
}

void lt_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    if (op_semant(e1, e2, st, ct, cur, this)) {
        type = Bool;
    } else {
        type = Object;
    }
}

void eq_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    if ((e1->get_type() == Int && e1->get_type() == Int)
        || (e1->get_type() == Bool && e1->get_type() == Bool)
        || (e1->get_type() == Str && e1->get_type() == Str)) {
        ct->semant_error(cur->get_filename(), this)
            << "eq can only operate on " 
            << "{Int, Int}, {Bool, Bool} or {Str, Str}" << endl;
        type = Object;
    } else {
        type = Int;
    }
}


void leq_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    if (op_semant(e1, e2, st, ct, cur, this)) {
        type = Bool;
    } else {
        type = Object;
    }    
}

void comp_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    e1->semant(st, ct, cur);
    if (e1->get_type() != Int) {
        ct->semant_error(cur->get_filename(), this) 
            << "complement can only operate on Int expression" << endl;
        type = Object;
    } else {
        type = Int;
    }
}

void int_const_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    type = Int;
}

void bool_const_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    type = Bool;
}

void string_const_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    type = Str;   
}

void new__class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    type = type_name;
}

void isvoid_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    e1->semant(st, ct, cur);
    type = Bool;
}

void no_expr_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
    // nothing need to be done
}

void object_class::semant(SymTab *st, ClassTableP ct, Class_ cur)
{
   type = st->lookup(name);
}