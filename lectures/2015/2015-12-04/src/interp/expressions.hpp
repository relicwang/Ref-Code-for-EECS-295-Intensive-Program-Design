#pragma once

#include <list>
#include <memory>
#include <iostream>
#include <unordered_map>

namespace expressions
{

  template <typename T>
  using list = std::list<T>;

  using std::shared_ptr;

  using string = std::string;

  template <typename K, typename V>
  using map = std::unordered_map<K,V>;

  /*********************
   values
  *********************/

  class exp;
  class environment;

  class value
  {
  public:
    virtual int getValue() {
      throw "not a number";
    } 
    virtual shared_ptr<value> apply(list<shared_ptr<value>> args) {
      throw "not a function";
    }
    virtual string toString() = 0;
  };

  class numVal : public value
  {

  public:
    numVal(int num) : num{num} {}
    int getValue() {
      return num;
    }
    string toString() {
      return std::to_string(num);
    }

  private: 
    int num;
  };

  struct closVal : public value
  {

  public:
    closVal(shared_ptr<exp> body, shared_ptr<environment> env, list<string> params) :
      body{body}, env{env}, params{params} {}

    shared_ptr<value> apply(list<shared_ptr<value>>);

    string toString() {
      return "function";
    }

  private:    
    shared_ptr<exp> body;
    shared_ptr<environment> env;
    list<string> params;

  };


  /*********************
   environments
  *********************/
    
  class environment 
  {
    
  public:
    
    shared_ptr<value> lookup(string) const;
    void bind(string, shared_ptr<value>);
    
    environment() : parent{nullptr} {}
    environment(shared_ptr<environment> par) : parent{par} {}
    
  private:
    
    shared_ptr<environment> parent;
    map<string, shared_ptr<value>> bindings;

  };

 /*********************
   expressions
  *********************/

  class exp
  {
  public:
    virtual shared_ptr<value> eval(shared_ptr<environment>) = 0;
  };

  class num : public exp
  {

  public:
    shared_ptr<value> eval(shared_ptr<environment> env) {
      return std::shared_ptr<value>{new numVal(val)};
    }
    num(int n) : val{n} {}
    
  private:
    
    int val;
  };

  class numeric_op : public exp 
  {

  public:
    shared_ptr<value> eval(shared_ptr<environment>);
    numeric_op(list<shared_ptr<exp>> args) : args{args} {}

  private:
    virtual int combine(int, int) const = 0;   
    list<shared_ptr<exp>> args;
 
  };

  class add : public numeric_op 
  {
  public:
    using numeric_op::numeric_op;
  private:
    int combine(int, int) const;
  };

  class sub : public numeric_op
  {
  public:
    using numeric_op::numeric_op;
  private:
    int combine(int, int) const;
  };

  class mul : public numeric_op
  {
  public:
    using numeric_op::numeric_op;
  private:
    int combine(int, int) const;
  };

  struct div : public numeric_op
  {
  public:
    using numeric_op::numeric_op;
  private:
    int combine(int, int) const;
  };

  class var : public exp
  {

  public:

    shared_ptr<value> eval(shared_ptr<environment>);
    var(string id) : id{id} {}
    
  private:
    
    string id;
    
  };

  class lambda : public exp
  {
    
  public:

    shared_ptr<value> eval(shared_ptr<environment>);
    lambda(list<string> params, shared_ptr<exp> body) :
      body{body}, params{params} {}

  private:
    
    shared_ptr<exp> body;
    list<string> params;
    
  };

  class app : public exp
  {

  public:

    shared_ptr<value> eval(shared_ptr<environment>);
    
    app(list<shared_ptr<exp>> exps) :
      exps{exps} {}

  private:
    
    list<shared_ptr<exp>> exps;
    
  };

  class if0 : public exp
  {
    
  public:

    shared_ptr<value> eval(shared_ptr<environment>);

    if0(shared_ptr<exp> t, shared_ptr<exp> tb, shared_ptr<exp> fb) :
      test{t}, true_branch{tb}, false_branch{fb} {}
    
  private:

    shared_ptr<exp> test;
    shared_ptr<exp> true_branch;
    shared_ptr<exp> false_branch;
  };

  class let : public exp
  {
    
  public:

    shared_ptr<value> eval(shared_ptr<environment>);

    let(string id, shared_ptr<exp> bound, shared_ptr<exp>body) :
      id{id}, bound{bound}, body{body} {}
    
  private:

    string id;
    shared_ptr<exp> bound;
    shared_ptr<exp> body;

  };

  
} // namespace expressions
