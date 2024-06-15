#!/usr/bin/env python3

# Python because I'm a lazy fuck

CHAR_GROUP_IDENT = 'abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
CHAR_GROUP_OP = '+*/%<>=.'
CHAR_GROUP_SPACE = ' \t\r'
CHAR_GROUP_SINGLE = '\n()[]{},'
CHAR_GROUP_STRING = '"\''

def is_builtin_type(ty):
    assert isinstance(ty, str)
    return is_integer_type(ty) is not None or ty in ('Fp32', 'ConstStr', 'Bool')

def is_integer_type(ty):
    '''
    Returns None or the amount of bits
    '''
    if ty.startswith('Int'):
        try:
            return int(ty[3:])
        except ValueError:
            pass

def _trace_iter(it):
    for item in it:
        print(repr(item))
        yield item

def str_to_il(s):
    remap = { ' ': '\s', '\n': '\\n', '\t': '\\t', '\r': '\\r', '\v': '\\v' }
    return '"' + ''.join(remap.get(c, c) for c in s) + '"'

class Token:
    def __init__(self, value):
        self.value = value

    def __eq__(self, other):
        assert isinstance(other, Token), f"comparison of token with non-token ({repr(other)})"
        return type(self) is type(other) and self.value == other.value

    def __repr__(self):
        return f'{type(self).__name__}[{repr(self.value)}]'

class TokenSingle(Token): pass
class TokenIdent(Token): pass
class TokenOp(Token): pass
class TokenStr(Token): pass

def parse_str(s):
    assert s[0] == '"' and s[-1] == '"'
    ns = []
    for c in s[1:-1]:
        if c == '\\':
            assert 0, 'todo'
        ns.append(c)
    return ''.join(ns)

def tokenize(text: str):
    def f():
        i = 0
        while i < len(text):
            if text[i] == '#':
                while text[i] != '\n':
                    i += 1
            elif text[i] in CHAR_GROUP_SPACE:
                i += 1
            elif text[i] in CHAR_GROUP_SINGLE:
                yield TokenSingle(text[i])
                i += 1
            elif text[i] in CHAR_GROUP_IDENT:
                k = i + 1
                while k < len(text) and text[k] in CHAR_GROUP_IDENT:
                    k += 1
                yield TokenIdent(text[i:k])
                i = k
            elif text[i] in CHAR_GROUP_OP + '-':
                k = i + 1
                if text[i] == '-' and text[k] in '0123456789':
                    # special case to handle negative numbers
                    while k < len(text) and text[k] in text[k] in CHAR_GROUP_IDENT:
                        k += 1
                    yield TokenIdent(text[i:k])
                else:
                    while k < len(text) and text[k] in CHAR_GROUP_OP:
                        k += 1
                    yield TokenOp(text[i:k])
                i = k
            elif text[i] in CHAR_GROUP_STRING:
                k = i + 1
                while True:
                    if text[k] == text[i]:
                        break
                    if text[k] == '\\':
                        assert 0, "todo: escape char"
                    k += 1
                k += 1
                yield TokenStr(parse_str(text[i:k]))
                i = k
            else:
                raise Exception(f'invalid char {repr(text[i])}')

    #return _trace_iter(f())
    return f()

class Statement: pass

class Standard(Statement):
    def __init__(self, ty, assign, expr):
        assert ty is None or isinstance(ty, TokenIdent)
        assert assign is None or isinstance(assign, Assign)
        assert expr is None or isinstance(expr, Expr)
        self.ty = ty and ty.value
        self.assign = assign
        self.expr = expr

    def __repr__(self):
        s = ''
        if self.ty is not None:
            s += f'{self.ty} {self.assign}'
        elif self.assign is not None:
            s += f'{self.assign}'
        if self.expr is not None:
            if self.assign is not None:
                s += ' = '
            s += repr(self.expr)
        return s

class Return(Statement):
    def __init__(self, expr):
        self.expr = expr

    def __repr__(self):
        return f'return {self.expr}'

class Become(Statement):
    def __init__(self, function, args):
        self.function = function
        self.args = args

    def __repr__(self):
        return f'become {self.function}({", ".join(map(repr, self.args))})'

class If(Statement):
    def __init__(self, expr, scope):
        self.expr = expr
        self.scope = scope

class For(Statement): pass

class ForIn(For):
    def __init__(self, index, value_ref, value, array, scope):
        self.index = index
        self.value_ref = value_ref
        self.value = value
        self.array = array
        self.scope = scope

class Loop(Statement):
    def __init__(self, scope):
        self.scope = scope

class Break(Statement):
    pass


class Expr: pass

class ExprValue(Expr):
    def __init__(self, value):
        assert isinstance(value, Path) or isinstance(value, TokenStr)
        self.value = value

    def __repr__(self):
        return f'{self.value}'

class ExprCall(Expr):
    def __init__(self, function, args):
        assert isinstance(function, Path)
        assert all(isinstance(e, Expr) for e in args)
        self.function = function
        self.args = args

    def __repr__(self):
        return f'{self.function}({", ".join(map(repr, self.args))})'

class Scope:
    def __init__(self):
        self.statements = []

    def push(self, statement):
        assert isinstance(statement, Statement)
        self.statements.append(statement)

    def __repr__(self):
        return '(' + ', '.join(map(repr, self.statements)) + ')'

class Function:
    def __init__(self, args, ret_type, root):
        assert all(isinstance(a, Arg) for a in args)
        assert ret_type is None or isinstance(ret_type, TokenIdent)
        assert isinstance(root, Scope)
        self.args = args
        self.ret_type = ret_type and ret_type.value
        self.root = root

    def __repr__(self):
        return repr({'args': self.args, 'ret_type': self.ret_type, 'root': self.root})

class Arg:
    def __init__(self, is_ref: bool, ty, name):
        assert ty is None or isinstance(ty, TokenIdent)
        assert name is None or isinstance(name, TokenIdent)
        self.is_ref = is_ref
        self.ty = ty.value
        self.name = name.value

    def __repr__(self):
        return f'{("", "ref ")[self.is_ref]}{self.ty} {self.name}'

class IlFunction:
    def __init__(self, name, args, ret_type, ret_reg):
        assert name is None or isinstance(name, TokenStr)
        assert all(isinstance(a, IlArg) for a in args)
        assert ret_type is None or isinstance(ret_type, TokenIdent)
        assert ret_reg is None or isinstance(ret_reg, TokenStr)
        self.name = name.value
        self.args = args
        self.ret_type = ret_type and ret_type.value
        self.ret_reg = ret_reg and ret_reg.value

    def __repr__(self):
        return repr({'name': self.name, 'args': self.args, 'ret_type': self.ret_type, 'ret_reg': self.ret_reg})

class IlArg:
    def __init__(self, is_ref: bool, ty, register):
        assert ty is None or isinstance(ty, TokenIdent)
        assert register is None or isinstance(register, TokenStr)
        self.is_ref = is_ref
        self.ty = ty.value
        self.register = register.value

    def __repr__(self):
        return f'{("", "ref ")[self.is_ref]}{self.ty} {self.register}'

class Type: pass

class UnitType(Type):
    def __init__(self, name):
        assert isinstance(name, TokenIdent)
        self.name = name.value

    def __repr__(self):
        return f'{self.name}'

class ArrayType(Type):
    def __init__(self, index, value):
        assert isinstance(index, TokenIdent)
        assert isinstance(value, TokenIdent)
        self.index = index.value
        self.value = value.value

    def __repr__(self):
        return f'[{self.index} -> {self.value}]'

class Path:
    def __init__(self, components):
        assert all(isinstance(e, TokenIdent) for e in components)
        self.components = [e.value for e in components]

    def __repr__(self):
        return '<' + '.'.join(self.components) + '>'

class Assign: pass

class AssignUnit(Assign):
    def __init__(self, path):
        assert isinstance(path, Path)
        self.path = path

class AssignArray(Assign):
    def __init__(self, path, index):
        assert isinstance(path, Path)
        assert isinstance(index, Expr)
        self.path = path
        self.index = index

class Unit:
    def __init__(self):
        self.enums = {}
        self.constants = {}
        self.records = {}
        self.functions = {}
        self.registers = {}

    def to_il(self):
        unit = self
        temp_register_counter = 0

        registers = []
        array_registers = []

        def flush_registers():
            nonlocal registers, array_registers
            for name, ty in registers:
                yield f'$ {name} {ty}\n'
            for name, index_ty, value_ty in array_registers:
                yield f'@ {name} {index_ty} {value_ty}\n'
            registers = []
            array_registers = []

        def add_register(regname, ty):
            registers.append((regname, ty))

        class Context:
            def __init__(self, prev, name):
                assert prev is None or isinstance(prev, Context)
                self.prev = prev
                if prev is not None and prev.name is not None:
                    name = prev.name + ':' + name
                self.name = name
                self.registers = {}
                self.array_registers = {}

            def _gen_tempreg(self, name):
                nonlocal temp_register_counter
                regname = f'{self.name}:${temp_register_counter}'
                temp_register_counter += 1
                return regname

            def add_register(self, name, ty, regname = None):
                assert name is None or isinstance(name, str)
                assert isinstance(ty, str)

                regname = self._gen_tempreg(name) if regname is None else regname

                if name is not None:
                    try:
                        self.get_register(name)
                        self.get_array_register(name)
                    except KeyError:
                        pass
                    else:
                        raise Exception(f'register "{name}" is shadowed')
                    self.registers[name] = regname, ty

                add_register(regname, ty)
                return regname

            def add_array_register(self, name, index_ty, value_ty, regname = None):
                assert isinstance(index_ty, str)
                assert isinstance(value_ty, str)
                try:
                    self.get_register(name)
                    self.get_array_register(name)
                except KeyError:
                    pass
                else:
                    raise Exception(f'array register "{name}" is shadowed')
                regname = self._gen_tempreg() if regname is None else regname
                self.array_registers[name] = regname, index_ty, value_ty
                array_registers.append((regname, index_ty, value_ty))

            def get_register(self, name):
                assert isinstance(name, str)
                try:
                    return self.registers[name]
                except KeyError:
                    pass
                if self.prev is None:
                    raise KeyError(f'register "{name}" is not defined')
                return self.prev.get_register(name)

            def get_array_register(self, name):
                assert isinstance(name, str)
                try:
                    return self.array_registers[name]
                except KeyError:
                    pass
                if self.prev is None:
                    raise KeyError(f'array register "{name}" is not defined')
                return self.prev.get_array_register(name)

            def assign_to(self, assign, handler, expr):
                if isinstance(assign, AssignUnit):
                    var, *rest = assign.path.components
                    reg, reg_ty = self.get_register(var)

                    def f(reg, reg_ty, components):
                        if components == []:
                            yield from handler(reg, reg_ty, expr)
                            return
                        field, *rest = components
                        f_reg_ty = unit.records[reg_ty][field]
                        f_reg = self.add_register(None, f_reg_ty)
                        yield from f(f_reg, f_reg_ty, rest)
                        yield f'( {reg} {field} {f_reg}\n'

                    yield from f(reg, reg_ty, rest)
                elif isinstance(assign, AssignArray):
                    var, = assign.path.components
                    reg, index_ty, value_ty = self.get_array_register(var)
                    v_reg, _ = yield from handler(None, value_ty, expr)
                    i_reg, _ = yield from handler(None, index_ty, assign.index)
                    yield '{' f' {reg} {i_reg} {v_reg}\n'
                else:
                    assert 0, repr(assign)

        for name, variants in self.enums.items():
            yield f'% '
            yield name
            for v in variants:
                yield ' '
                yield v
            yield '\n'
        yield '\n'

        for name, rec in self.records.items():
            yield f'( {name}\n'
            for field, ty in rec.items():
                yield f'& {field} {ty}\n'
            yield ')\n\n'

        for (ty, name), fields in self.constants.items():
            yield f'_ {ty} {name}\n'
            for k, v in fields.items():
                yield f'+ {k} {v}\n'
            yield f'-\n\n'

        root_ctx = Context(None, None)

        for name, ty in self.registers.items():
            if isinstance(ty, UnitType):
                root_ctx.add_register(name, ty.name, name)
            else:
                assert isinstance(ty, ArrayType)
                root_ctx.add_array_register(name, ty.index, ty.value, name)
        yield from flush_registers()
        yield '\n'

        functions_arg_to_reg = {}

        for name, fn in self.functions.items():
            args = []
            for a in fn.args:
                if isinstance(fn, IlFunction):
                    args.append((a.ty, a.register))
                else:
                    args.append((a.ty, f'{name}:{a.name}'))
            functions_arg_to_reg[name] = args

        for name, fn in self.functions.items():
            if isinstance(fn, IlFunction):
                continue

            def get_reg_or_value(ctx, ty, path_or_value):
                if isinstance(path_or_value, TokenStr):
                    return str_to_il(path_or_value.value), 'ConstStr', True
                assert isinstance(path_or_value, Path)
                path_or_value, *rest = path_or_value.components
                try:
                    reg, reg_ty = ctx.get_register(path_or_value)
                except KeyError:
                    pass
                else:
                    for field in rest:
                        f_ty = unit.records[reg_ty][field]
                        f_reg = ctx.add_register(None, ty)
                        yield f') {reg} {field} {f_reg}\n'
                        reg, reg_ty = f_reg, f_ty
                    assert ty is None or ty == reg_ty, (ty, reg_ty, rest)
                    return reg, reg_ty, False
                assert rest == []
                if (ty, path_or_value) in unit.constants:
                    return path_or_value, ty, True
                if is_builtin_type(ty):
                    # FIXME validate
                    return path_or_value, None, True
                assert False, repr((ty, path_or_value, rest))

            def parse_expr(ctx, assign, expr, expect_ty):
                if isinstance(expr, ExprCall):
                    assert isinstance(expr.function, Path)
                    function, = expr.function.components
                    call_fn = self.functions[function]
                    arg_regs = functions_arg_to_reg[function]
                    for arg, (ty, reg) in zip(expr.args, arg_regs):
                        _, expr_ty = yield from parse_expr(ctx, reg, arg, ty)
                        assert ty == expr_ty, f'{ty} == {expr_ty}'
                    if isinstance(call_fn, IlFunction):
                        yield f'| {call_fn.name}\n'
                    else:
                        yield f'| {function}\n'
                    if call_fn.ret_type is not None:
                        if isinstance(call_fn, IlFunction):
                            reg = call_fn.ret_reg
                        else:
                            reg = f'{function}:return'
                        if assign is None:
                            assign = reg
                        if assign != reg:
                            yield f'. {assign} {reg}\n'
                        return assign, call_fn.ret_type
                elif isinstance(expr, ExprValue):
                    reg_or_value, ty, is_value = yield from get_reg_or_value(ctx, expect_ty, expr.value)
                    if ty is None:
                        assert expect_ty is not None
                        ty = expect_ty
                    if assign is None:
                        assign = ctx.add_register(None, ty)
                    assert expect_ty is None or ty == expect_ty, f'{ty} == {expect_ty}'
                    instr = '.+'[is_value]
                    yield f'{instr} {assign} {reg_or_value}\n'
                    return assign, ty
                else:
                    assert 0, repr(type(expr))

            loop_stack = []

            def parse_scope(ctx, scope, prefix = []) -> bool:
                """
                Returns whether an exit instruction is necessary
                """
                scope_counter = 0
                og_name = ctx.name

                def next_block():
                    nonlocal scope_counter
                    ctx.name = f'{og_name}.{scope_counter}'
                    scope_counter += 1

                yield f'> {ctx.name}\n'
                yield from prefix

                for stmt in scope.statements:
                    if isinstance(stmt, Standard):
                        if stmt.ty is not None:
                            assign, = stmt.assign.path.components
                            ctx.add_register(assign, stmt.ty)
                        if stmt.expr is not None:
                            def handler(reg, reg_ty, expr):
                                return (yield from parse_expr(ctx, reg, expr, reg_ty))
                            if stmt.assign is not None:
                                yield from ctx.assign_to(stmt.assign, handler, stmt.expr)
                            else:
                                yield from handler(None, None, stmt.expr)
                    elif isinstance(stmt, Return):
                        if stmt.expr is not None:
                            assert fn.ret_type is not None
                            reg, ty = ctx.get_register('return')
                            assert ty == fn.ret_type
                            yield from parse_expr(ctx, reg, stmt.expr, fn.ret_type)
                        else:
                            assert fn.ret_type is None
                        yield '<\n'
                        return False
                    elif isinstance(stmt, Become):
                        become_name, = stmt.function.components
                        become_fn = unit.functions[become_name]
                        if isinstance(become_fn, IlFunction):
                            yield f'= {become_fn.name}\n'
                        else:
                            yield f'= {become_name}\n'
                        return False
                    elif isinstance(stmt, ForIn):
                        new_ctx = Context(ctx, f'for_in.{scope_counter}')
                        new_name = new_ctx.name
                        scope_counter += 1

                        array_reg, index_ty, value_ty = ctx.get_array_register(stmt.array)

                        bits = is_integer_type(index_ty)
                        if bits is None:
                            raise Exception('enumerating non-integer indices unsupported')

                        index_reg, _ = new_ctx.add_register(None, index_ty)
                        value_reg, _ = new_ctx.add_register(stmt.value, value_ty)

                        yield f'+ {index_reg} 0'
                        yield f'= {new_ctx.name}\n'

                        next_block()

                        loop_stack.append(new_ctx)
                        prefix = [f'}} {array_reg} {index_reg} {value_reg}\n']
                        cont = yield from parse_scope(new_ctx, stmt.scope, prefix)
                        if not cont:
                            return False
                        loop_stack.pop()

                        if stmt.value_ref:
                            yield '{' f' {array_reg} {index_reg} {value_reg}\n'

                        yield f'. int{bits}:0 {index_reg}\n'
                        yield f'| int{bits}.inc\n'
                        yield f'. {index_reg} int{bits}:0\n'
                        yield f'= {new_name}.switch\n'

                        yield f'[ {new_name}.switch {index_reg}\n'
                        yield f'? 0 {ctx.name}\n'
                        yield f'! {new_name}\n'

                        yield f'> {ctx.name}\n'
                    elif isinstance(stmt, Loop):
                        new_ctx = Context(ctx, f'loop.{scope_counter}')
                        new_name = new_ctx.name
                        scope_counter += 1

                        yield f'= {new_ctx.name}\n'
                        next_block()

                        ctx.name = f'{og_name}.{scope_counter}'
                        scope_counter += 1

                        loop_stack.append(new_ctx)
                        cont = yield from parse_scope(new_ctx, stmt.scope)
                        if not cont:
                            return False
                        yield f'= {new_name}\n'
                        loop_stack.pop()

                        yield f'> {ctx.name}\n'
                    elif isinstance(stmt, If):
                        new_ctx = Context(ctx, f'if.{scope_counter}')
                        scope_counter += 1
                        next_block()

                        reg, ty = yield from parse_expr(ctx, None, stmt.expr, None)

                        yield f'= {new_ctx.name}.switch\n'
                        yield f'[ {new_ctx.name}.switch {reg}\n'
                        yield f'? 0 {ctx.name}\n'
                        yield f'! {new_ctx.name}\n'
                        cont = yield from parse_scope(new_ctx, stmt.scope)
                        if cont:
                            yield f'= {ctx.name}\n'
                        yield f'> {ctx.name}\n'
                    elif isinstance(stmt, Break):
                        yield f'= {loop_stack[-1].prev.name}\n'
                        return False
                    else:
                        assert 0, repr(type(stmt))
                return True

            ctx = Context(root_ctx, name)

            for a in fn.args:
                ctx.add_register(a.name, a.ty, f'{name}:{a.name}')
            if fn.ret_type is not None:
                ctx.add_register('return', fn.ret_type, f'{name}:return')

            cont = yield from parse_scope(ctx, fn.root)
            if cont:
                yield '<\n'

            yield from flush_registers()

            yield '\n'

def parse(path, unit = None) -> Unit:
    import pathlib
    path = pathlib.Path(path)
    with path.open() as f:
        text = f.read()

    if unit is None:
        unit = Unit()
    assert isinstance(unit, Unit)
    
    tokens = tokenize(text)

    def next_without_nl():
        while True:
            tk = next(tokens)
            if tk != TokenSingle('\n'):
                return tk

    def next_token(skip_nl, expect_ty):
        tk = next(tokens)
        while skip_nl and tk == TokenSingle('\n'):
            tk = next(tokens)
        if not isinstance(tk, expect_ty):
            raise Exception(f'expected {expect_ty.__name__}, got {tk}')
        return tk

    def next_ident(skip_nl = False):
        return next_token(skip_nl, TokenIdent)

    def next_str(skip_nl = False):
        return next_token(skip_nl, TokenStr)

    def next_single(skip_nl = False):
        return next_token(skip_nl, TokenSingle)

    def assert_is(tk, expect, skip_nl = False):
        while True:
            if not skip_nl or tk != '\n':
                break
            tk = next(tokens)
        if type(expect) is tuple:
            if tk in expect:
                return tk
        elif tk == expect:
            return tk
        printmap = lambda x: {'\n': '\\n', '\t': '\\t'}.get(x, x)
        expect = printmap(expect.value)
        tk = printmap(tk.value)
        raise Exception(f'expected "{expect}", got "{tk}"')

    def assert_next(expect, skip_nl = False):
        return assert_is(next(tokens), expect, skip_nl)

    def assert_next(expect, skip_nl = False):
        return assert_is(next(tokens), expect, skip_nl)

    def parse_args(handler, tk = None):
        if tk is None:
            assert_next(TokenSingle('('))
        else:
            assert_is(tk, TokenSingle('('))
        tk = next_without_nl()
        while tk != TokenSingle(')'):
            tk = handler(tk)
            if tk is None:
                tk = next_single(True)
            assert_is(tk, (TokenSingle(','), TokenSingle(')')))
            if tk == TokenSingle(')'):
                break
            tk = next_without_nl()

    def parse_expr(tk):
        if tk is None:
            tk = next(tokens)
        if isinstance(tk, TokenStr):
            return ExprValue(tk), None
        fn, tk = parse_path(tk)
        # standalone value
        if tk in [*map(TokenSingle, ('\n', ',', ')', ']', '{'))]:
            return ExprValue(fn), tk
        # function call
        if tk == TokenSingle('('):
            return parse_expr_args(fn, tk), None
        if tk == TokenSingle('['):
            assert 0, 'array access'
        raise Exception(f'can\'t parse this {tk}')

    def parse_expr_args(fn, tk):
        if fn is None:
            assert tk is None
            fn, tk = parse_path(None)
        assert isinstance(fn, Path)
        # FIXME
        args = []
        def expr_handler(tk):
            if tk == TokenIdent('ref'):
                # TODO ref
                tk = next(tokens)
            expr, tk = parse_expr(tk)
            args.append(expr)
            return tk
        parse_args(expr_handler, tk)
        return ExprCall(fn, args)

    def parse_path(tk):
        if tk is None:
            tk = next_ident()
        elif not isinstance(tk, TokenIdent):
            raise Exception(f'expected identifier, got {tk}')
        path = [tk]
        while True:
            tk = next(tokens)
            if tk == TokenOp('.'):
                path.append(next_ident())
            else:
                return Path(path), tk

    def handle_function():
        function = next_ident().value
        if function in unit.functions:
            raise Exception(f'function "{function}" already defined')
        args = []
        def arg_handler(tk):
            is_ref = tk == TokenIdent('ref')
            if is_ref:
                tk = next_without_nl()
            ty = tk
            name = next_without_nl()
            args.append(Arg(is_ref, ty, name))
        parse_args(arg_handler)

        tk = assert_next((TokenSingle('{'), TokenOp('->')))
        if tk == TokenOp('->'):
            ret_type = next(tokens)
            assert_next(TokenSingle('{'))
        else:
            ret_type = None

        assert_next(TokenSingle('\n'))

        def parse_scope():
            f = Scope()

            while True:
                thing = next_without_nl()
                if thing == TokenSingle('}'):
                    assert_next(TokenSingle('\n'))
                    break
                if thing == TokenIdent('return'):
                    tk = next(tokens)
                    if tk != TokenSingle('\n'):
                        expr, tk = parse_expr(tk)
                    else:
                        expr = None
                    if tk is None:
                        tk = next(tokens)
                    assert_is(tk, TokenSingle('\n'))
                    f.push(Return(expr))
                elif thing == TokenIdent('become'):
                    if ret_type is not None:
                        raise Exception('become with return type is not valid')
                    expr_call = parse_expr_args(None, None)
                    assert_next(TokenSingle('\n'))
                    f.push(Become(expr_call.function, expr_call.args))
                elif thing == TokenIdent('il.type'):
                    assert 0
                elif thing == TokenIdent('for'):
                    index = None
                    value = next(tokens)
                    if value == 'index':
                        index = next(token)
                        assert_next(TokenSingle(','))
                        value = next(token)
                    value_ref = value == TokenIdent('ref')
                    if value_ref:
                        value = next(tokens)
                    assert_next(TokenIdent('in'))
                    array = next(tokens)
                    assert_next(TokenSingle('{'))
                    assert_next(TokenSingle('\n'))
                    scope = parse_scope()
                    f.push(ForIn(index, value_ref, value, array, scope))
                elif thing == TokenIdent('if'):
                    expr, tk = parse_expr(None)
                    if tk is None:
                        tk = next_single()
                    assert_is(tk, TokenSingle('{'))
                    assert_next(TokenSingle('\n'))
                    scope = parse_scope()
                    f.push(If(expr, scope))
                elif thing == TokenIdent('break'):
                    assert_next(TokenSingle('\n'))
                    f.push(Break())
                elif thing == TokenIdent('loop'):
                    assert_next(TokenSingle('{'))
                    assert_next(TokenSingle('\n'))
                    scope = parse_scope()
                    f.push(Loop(scope))
                else:
                    thing, tk = parse_path(thing)

                    if tk == TokenSingle('('):
                        f.push(Standard(None, None, parse_expr_args(thing, tk)))
                    elif tk == TokenOp('='):
                        expr, tk = parse_expr(None)
                        if tk is None:
                            tk = next(tokens)
                        assert_is(tk, TokenSingle('\n'))
                        f.push(Standard(None, AssignUnit(thing), expr))
                    elif tk == TokenSingle('\n'):
                        assert 0
                    elif tk == TokenSingle('['):
                        index, tk = parse_expr(None)
                        if tk is None:
                            tk = next(tokens)
                        assert_is(tk, TokenSingle(']'))
                        assert_next(TokenOp('='))
                        value, tk = parse_expr(None)
                        if tk is None:
                            tk = next(tokens)
                        assert_is(tk, TokenSingle('\n'))
                        f.push(Standard(None, AssignArray(thing, index), value))
                    elif isinstance(tk, TokenIdent):
                        ty, = map(TokenIdent, thing.components)
                        var, tk = parse_path(tk)
                        assert len(var.components) == 1
                        if tk == TokenOp('='):
                            expr, tk = parse_expr(None)
                            if tk is None:
                                tk = next_single()
                            assert_is(tk, TokenSingle('\n'))
                            f.push(Standard(ty, AssignUnit(var), expr))
                        elif tk == TokenSingle('\n'):
                            f.push(Standard(ty, AssignUnit(var), None))
                        elif isinstance(tk, TokenIdent):
                            assert 0, repr((ty, var, tk))
                        else:
                            raise Exception("expected =, ., \\n or identifier")
                    else:
                        raise Exception(f'expected (, =, [ or identifier, got {tk}')

            return f


        root = parse_scope()
        unit.functions[function] = Function(args, ret_type, root)

    def handle_record():
        record = next_ident().value
        assert_next(TokenSingle('{'))
        assert_next(TokenSingle('\n'))
        if record in unit.records or record in unit.enums:
            raise Exception(f'record "{record}" already defined')
        fields = {}
        while True:
            tk = next(tokens)
            if tk == TokenSingle('}'):
                assert_next(TokenSingle('\n'))
                break
            assert isinstance(tk, TokenIdent)
            ty = tk.value
            field = next_ident().value
            assert_next(TokenSingle('\n'))
            if field in fields:
                raise Exception(f'field "{field}" in record "{record}" already defined')
            fields[field] = ty
        unit.records[record] = fields

    def handle_enum():
        enum = next(tokens)
        assert_next(TokenSingle('{'))
        assert_next(TokenSingle('\n'))
        if enum in unit.enums or enum in unit.records:
            raise Exception(f'enum "{enum}" already defined')
        variants = []
        while True:
            variant = next(tokens)
            assert_next(TokenSingle('\n'))
            if variant == TokenSingle('}'):
                break
            if variant in variants:
                raise Exception(f'variant "{variant}" in enum "{enum}" already defined')
            variants.append(variant)
        unit.enums[enum] = variants

    def handle_register():
        ty = next(tokens)
        if ty == TokenSingle('['):
            index = next(tokens)
            assert_next(TokenOp('->'))
            value = next(tokens)
            assert_next(TokenSingle(']'))
            ty = ArrayType(index, value)
        else:
            ty = UnitType(ty)
        name = next_ident().value
        assert_next(TokenSingle('\n'))
        if name in unit.registers:
            raise Exception(f'register "{name}" already defined')
        unit.registers[name] = ty

    def handle_include():
        p = next_str()
        parse(path.parent / p.value, unit)

    def handle_constant():
        ty = next_ident().value
        name = next_ident().value
        assert_next(TokenSingle('{'))
        assert_next(TokenSingle('\n'))
        fields = {}
        while True:
            field = next(tokens)
            if field == TokenSingle('}'):
                assert_next(TokenSingle('\n'))
                break
            assert isinstance(field, TokenIdent)
            field = field.value
            assert_next(TokenOp('='))
            value = next_ident().value
            assert_next(TokenSingle('\n'))
            if field in fields:
                raise Exception(f'field "{field}" already defined')
            fields[field] = value
        unit.constants[ty, name] = fields

    def handle_il_function():
        name = next_ident().value
        if name in unit.functions:
            raise Exception(f'function "{name}" already defined')

        fn_name = next_str()

        args = []
        def arg_handler(ty):
            is_ref = ty == TokenIdent('ref')
            if is_ref:
                ty = next_without_nl()
            reg = next_str(True)
            args.append(IlArg(is_ref, ty, reg))
        parse_args(arg_handler)

        tk = assert_next((TokenSingle('\n'), TokenOp('->')))
        ret_type = None
        if tk == TokenOp('->'):
            ret_type = next(tokens)
            ret_reg = next(tokens)
            ret = ret_type, ret_reg
            assert_next(TokenSingle('\n'))
        else:
            ret_type = ret_reg = None
        unit.functions[name] = IlFunction(fn_name, args, ret_type, ret_reg)

    def handle_il_type():
        name = next(tokens)
        ty = next_str()
        assert_next(TokenSingle('\n'))

    def handle_il():
        assert_next(TokenOp('.'))
        tk = next_ident()
        {
            'function': handle_il_function,
            'type': handle_il_type,
        }[tk.value]()

    keyword_handlers = {
        'il': handle_il,
        'function': handle_function,
        'constant': handle_constant,
        'record': handle_record,
        'enum': handle_enum,
        'register': handle_register,
        'include': handle_include,
    }

    for tk in tokens:
        if tk == TokenSingle('\n'):
            continue
        if not isinstance(tk, TokenIdent):
            raise Exception(f'expected identifier, got {tk}')
        keyword_handlers[tk.value]()

    return unit


if __name__ == '__main__':
    import sys

    path = sys.argv[1]
    unit = parse(path)

    def dump():
        yield 'Enums:'
        for name, rec in unit.enums.items():
            yield f'  {name} {rec}'
        yield 'Records:'
        for name, rec in unit.records.items():
            yield f'  {name} {rec}'
        yield 'Constants:'
        for (ty, name), values in unit.constants.items():
            yield f'  {ty} {name}'
            for field, value in values.items():
                yield f'    {field} {value}'
        yield 'Registers:'
        for name, ty in unit.registers.items():
            yield f'  {ty} {name}'
        yield 'Functions:'
        for name, fn in unit.functions.items():
            yield f'  {name}'
            if isinstance(fn, IlFunction):
                yield '    (il.function)'
            else:
                for stmt in fn.root.statements:
                    yield f'    {stmt}'

    for l in dump():
        break
        print('#', l)

    for s in unit.to_il():
        print(s, end='')
