#!/usr/bin/env python3

# Python because I'm a lazy fuck

CHAR_GROUP_IDENT = 'abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.'
CHAR_GROUP_OP = '+-*/%<>=[]'
CHAR_GROUP_SPACE = ' \t\r'
CHAR_GROUP_SINGLE = '\n(){},'
CHAR_GROUP_STRING = '"\''

def is_builtin_type(ty):
    return ty in ('Float32', 'Natural32', 'ConstantString', 'Boolean')

def _trace_iter(it):
    for item in it:
        print(repr(item))
        yield item

def tokenize(text: str):
    def f():
        i = 0
        while i < len(text):
            if text[i] in CHAR_GROUP_SPACE:
                i += 1
            elif text[i] in CHAR_GROUP_SINGLE:
                yield text[i]
                i += 1
            elif text[i] in CHAR_GROUP_IDENT:
                k = i + 1
                while k < len(text) and text[k] in CHAR_GROUP_IDENT:
                    k += 1
                yield text[i:k]
                i = k
            elif text[i] in CHAR_GROUP_OP:
                k = i + 1
                while k < len(text) and text[k] in CHAR_GROUP_OP:
                    k += 1
                yield text[i:k]
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
                yield text[i:k]
                i = k
            else:
                raise Exception(f'invalid char {repr(text[i])}')

    return _trace_iter(f())

class Statement: pass

class Standard(Statement):
    def __init__(self, ty, assign, expr):
        assert expr is None or isinstance(expr, Expr)
        self.ty = ty
        self.assign = assign
        self.expr = expr

    def __repr__(self):
        s = ''
        if self.ty is not None:
            s += self.ty + ' ' + self.assign
        elif self.assign is not None:
            s += self.assign
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

class IlCall(Statement):
    def __init__(self, function):
        self.function = function

    def __repr__(self):
        return f'il.call {self.function}'

class IlMove(Statement):
    def __init__(self, reg, var, out):
        self.reg = reg
        self.var = var
        self.out = out

    def __repr__(self):
        return f'il.move {self.reg} {["<-", "->"][self.out]} {self.var}'

class If(Statement):
    def __init__(self, variant, value_ref, value, expr, scope):
        self.variant = variant
        self.value_ref = value_ref
        self.value = value
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
        self.value = value

    def __repr__(self):
        return f'{self.value}'

class ExprCall(Expr):
    def __init__(self, function, args):
        self.function = function
        self.args = args

    def __repr__(self):
        return f'{self.function}({", ".join(map(repr, self.args))})'

class ExprUnionVariant(Expr):
    def __init__(self, variant, expr):
        assert isinstance(expr, Expr)
        self.variant = variant
        self.expr = expr

    def __repr__(self):
        return f'{self.variant} {self.expr}'

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
        self.args = args
        self.ret_type = ret_type
        self.root = root

    def __repr__(self):
        return repr({'args': self.args, 'ret_type': self.ret_type, 'root': self.root})

class Arg:
    def __init__(self, is_ref: bool, ty, name):
        self.is_ref = is_ref
        self.ty = ty
        self.name = name

    def __repr__(self):
        return f'{("", "ref ")[self.is_ref]}{self.ty} {self.name}'

class Type: pass

class UnitType(Type):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return f'{self.name}'

class ArrayType(Type):
    def __init__(self, index, value):
        self.index = index
        self.value = value

    def __repr__(self):
        return f'[{self.index} -> {self.value}]'

class Unit:
    def __init__(self):
        self.type_aliases = {}
        self.records = {}
        self.unions = {}
        self.functions = {}
        self.registers = {}

    def union_is_dataless(self, ty):
        return all(ty is None for ty in self.unions[ty].values())

    def to_il(self):
        unit = self
        temp_register_counter = 0

        registers = []
        array_registers = []

        extra_instructions = []

        def flush_registers():
            nonlocal registers, array_registers, extra_instructions
            for name, ty in registers:
                yield f'$ {name} {ty}\n'
            for name, index_ty, value_ty in array_registers:
                yield f'@ {name} {index_ty} {value_ty}\n'
            registers = []
            array_registers = []

            yield from extra_instructions
            extra_instructions = []

        def add_register(regname, ty):
            registers.append((regname, ty))

        def add_temp_register(ty):
            nonlocal temp_register_counter
            regname = f'${temp_register_counter}'
            temp_register_counter += 1
            add_register(regname, ty)
            return regname

        class Context:
            def __init__(self, prev, name):
                assert prev is None or isinstance(prev, Context)
                self.prev = prev
                if prev is not None and prev.name is not None:
                    name = prev.name + ':' + name
                self.name = name
                self.registers = {}
                self.array_registers = {}

            def _gen_tempreg(self):
                nonlocal temp_register_counter
                regname = f'{self.name}:{temp_register_counter}'
                temp_register_counter += 1
                return regname

            def add_register(self, name, ty, regname = None):
                assert isinstance(ty, str)

                regname = self._gen_tempreg() if regname is None else regname

                def f(ty, name, regname):
                    try:
                        self.get_register(name)
                        self.get_array_register(name)
                    except KeyError:
                        pass
                    else:
                        raise Exception(f'register "{name}" is shadowed')
                    self.registers[name] = regname, ty
                    if ty in unit.unions:
                        self.registers[f'{name}:tag'] = f'{regname}:tag', ty
                        for variant, ty in unit.unions[ty].items():
                            if ty is not None:
                                self.registers[f'{name}:data:{variant}'] = f'{regname}:data:{variant}', ty
                    elif ty in unit.records:
                        for f_name, f_ty in unit.records[ty].items():
                            f(f_ty, f'{name}.{f_name}', f'{regname}.{f_name}')
                f(ty, name, regname)

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
                try:
                    return self.registers[name]
                except KeyError:
                    pass
                if self.prev is None:
                    raise KeyError(f'register "{name}" is not defined')
                return self.prev.get_register(name)

            def get_array_register(self, name):
                try:
                    return self.array_registers[name]
                except KeyError:
                    pass
                if self.prev is None:
                    raise KeyError(f'array register "{name}" is not defined')
                return self.prev.get_array_register(name)

        fn_ty_step_memoize = set()
        def fn_ty_step_next(ty):
            assert self.union_is_dataless(ty)
            name = f'{ty}:step.next'
            value = f'{ty}:step.next:value'
            l = [*self.unions[ty]]
            if ty in fn_ty_step_memoize:
                return name, value, l[0]
            fn_ty_step_memoize.add(ty)
            extra_instructions.append(f'[ {name} {value}\n')
            for i, variant in enumerate(l):
                extra_instructions.append(f'? {l[i - 1]} {variant}\n')
            extra_instructions.append(']\n')
            return name, value, l[0]

        for name, variants in self.unions.items():
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
                yield f'& {ty} .{field}\n'
            yield ')\n\n'

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
                args.append((a.ty, a.name))
            functions_arg_to_reg[name] = args

        for name, fn in self.functions.items():
            def get_reg_or_value(ctx, ty, path_or_value):
                try:
                    return *ctx.get_register(path_or_value), False
                except KeyError:
                    pass
                if is_builtin_type(ty):
                    # FIXME validate
                    return path_or_value, None, True
                assert False, repr((ty, path_or_value))

            def parse_expr(ctx, assign, expr, expect_ty):
                print('>>>>>>', ctx, assign, expr, expect_ty)
                if isinstance(expr, ExprCall):
                    call_fn = self.functions[expr.function]
                    arg_regs = functions_arg_to_reg[expr.function]
                    for arg, (ty, reg) in zip(expr.args, arg_regs):
                        _, expr_ty = yield from parse_expr(ctx, reg, arg, ty)
                        assert ty == expr_ty, f'{ty} == {expr_ty}'
                    yield from ('| ', expr.function, '\n')
                    if call_fn.ret_type is not None:
                        assert assign is not None
                        reg = f'{expr.function}:return'
                        yield f'. {assign} {reg}\n'
                        return assign, call_fn.ret_type
                elif isinstance(expr, ExprValue):
                    reg_or_value, ty, is_value = get_reg_or_value(ctx, expect_ty, expr.value)
                    if ty is None:
                        assert expect_ty is not None
                        ty = expect_ty
                    if assign is None:
                        assign = add_temp_register(ty)
                    assert expect_ty is None or ty == expect_ty, f'{ty} == {expect_ty}'
                    instr = '.+'[is_value]
                    yield f'{instr} {assign} {reg_or_value}\n'
                    return assign, ty
                elif isinstance(expr, ExprUnionVariant):
                    assert expect_ty is not None
                    tag_reg, _ = ctx.get_register(f'{assign}:tag')
                    yield f'+ {tag_reg} {expr.variant}\n'
                    if expr.expr is not None:
                        data_reg, data_ty = ctx.get_register(f'{assign}:data:{expr.variant}')
                        yield from parse_expr(ctx, data_reg, expr.expr, data_ty)
                    return assign, expect_ty
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
                            ctx.add_register(stmt.assign, stmt.ty)
                        if stmt.expr is not None:
                            reg = reg_ty = None
                            if stmt.assign is not None:
                                reg, reg_ty = ctx.get_register(stmt.assign)
                            yield from parse_expr(ctx, reg, stmt.expr, reg_ty)
                    elif isinstance(stmt, IlCall):
                        yield from ('| ', stmt.function, '\n')
                    elif isinstance(stmt, IlMove):
                        var, _ = ctx.get_register(stmt.var)
                        if stmt.out:
                            yield ' '.join(('.', var, stmt.reg)) + '\n'
                        else:
                            yield ' '.join(('.', stmt.reg, var)) + '\n'
                    elif isinstance(stmt, Return):
                        assert fn.ret_type is not None
                        if stmt.expr is not None:
                            reg, ty = ctx.get_register('return')
                            assert ty == fn.ret_type
                            yield from parse_expr(ctx, reg, stmt.expr, fn.ret_type)
                        yield '<\n'
                        return False
                    elif isinstance(stmt, Become):
                        yield f'= {stmt.function}\n'
                        return False
                    elif isinstance(stmt, ForIn):
                        new_ctx = Context(ctx, f'for_in.{scope_counter}')
                        new_name = new_ctx.name
                        scope_counter += 1

                        array_reg, index_ty, value_ty = ctx.get_array_register(stmt.array)
                        fn_step, fn_step_reg, first_variant = fn_ty_step_next(index_ty)

                        index_reg = new_ctx.add_register(f'{new_ctx.name}:index', index_ty)
                        value_reg = new_ctx.add_register(stmt.value, value_ty)

                        yield f'+ {index_reg} {first_variant}\n'
                        yield f'= {new_ctx.name}\n'

                        next_block()

                        loop_stack.append(new_ctx)
                        prefix = [f'}} {array_reg} {index_reg} {value_reg}\n']
                        cont = yield from parse_scope(new_ctx, stmt.scope, prefix)
                        if not cont:
                            return False
                        loop_stack.pop()

                        if stmt.value_ref:
                            yield f'{{ {array_reg} {index_reg} {value_reg}\n'

                        yield f'. {fn_step_reg} {index_reg}\n'
                        yield f'| {fn_step}\n'
                        yield f'. {index_reg} {fn_step_reg}\n'
                        yield f'= {new_name}.switch\n'

                        yield f'[ {new_name}.switch {index_reg}\n'
                        yield f'? {first_variant} {ctx.name}\n'
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
                        if stmt.value is not None:
                            new_ctx.add_register(stmt.value, ty, reg)

                        yield f'= {new_ctx.name}.switch\n'
                        yield f'[ {new_ctx.name}.switch {reg}:tag\n'
                        yield f'? {stmt.variant} {new_ctx.name}\n'
                        yield f'! {ctx.name}\n'
                        cont = yield from parse_scope(new_ctx, stmt.scope)
                        if cont:
                            assert 0
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

def parse(text: str) -> Unit:
    unit = Unit()
    
    tokens = tokenize(text)

    def next_without_nl():
        while True:
            tk = next(tokens)
            if tk != '\n':
                return tk

    def assert_is(tk, expect, skip_nl = False):
        while True:
            if not skip_nl or tk != '\n':
                break
            tk = next(tokens)
        if type(expect) is tuple and tk in expect:
            return tk
        if tk == expect:
            return tk
        printmap = {'\n': '\\n', '\t': '\\t'}.get
        expect = printmap(expect, expect)
        tk = printmap(tk, tk)
        raise Exception(f'expected "{expect}", got "{tk}"')

    def assert_next(expect, skip_nl = False):
        return assert_is(next(tokens), expect, skip_nl)

    def parse_args(handler, tk = None):
        if tk is None:
            assert_next('(')
        else:
            assert_is(tk, '(')
        tk = next_without_nl()
        if tk != ')':
            while True:
                tk = handler(tk)
                if tk is None:
                    tk = next(tokens)
                assert_is(tk, (',', ')'))
                if tk == ')':
                    break
                tk = next_without_nl()

    def parse_expr(fn = None):
        if fn is None:
            fn = next(tokens)
        tk = next(tokens)
        # standalone value
        if tk in ('\n', ',', ')', ']', '{'):
            return ExprValue(fn), tk
        # function call
        if tk == '(':
            return parse_expr_args(fn, tk), None
        if tk == '[':
            assert 0, 'array access'
        # enum variant with associated data
        value, tk = parse_expr(tk)
        return ExprUnionVariant(fn, value), tk

    def parse_expr_args(fn, tk):
        if fn is None:
            fn = next(tokens)
        args = []
        def expr_handler(tk):
            expr, tk = parse_expr(tk)
            args.append(expr)
            return tk
        parse_args(expr_handler, tk)
        return ExprCall(fn, args)

    def handle_alias_type():
        alias = next(tokens)
        assert_next('=')
        ty = next(tokens)
        assert_next('\n')
        if alias in unit.type_aliases:
            raise Exception(f'type alias {alias} already defined')
        unit.type_aliases[alias] = ty

    def handle_function():
        function = next(tokens)
        if function in unit.functions:
            raise Exception(f'function "{function}" already defined')
        args = []
        def arg_handler(tk):
            is_ref = tk == 'ref'
            if is_ref:
                tk = next_without_nl()
            ty = tk
            name = next_without_nl()
            args.append(Arg(is_ref, tk, name))
        parse_args(arg_handler)

        tk = assert_next(('{', '->'))
        if tk == '->':
            ret_type = next(tokens)
            assert_next('{')
        else:
            ret_type = None

        assert_next('\n')

        def parse_scope():
            f = Scope()

            while True:
                thing = next_without_nl()
                if thing == '}':
                    assert_next('\n')
                    break
                if thing == 'return':
                    if ret_type is None:
                        raise Exception('return without return type is not valid')
                    expr, tk = parse_expr()
                    if tk is None:
                        tk = next(tokens)
                    assert_is(tk, '\n')
                    f.push(Return(expr))
                elif thing == 'become':
                    if ret_type is not None:
                        raise Exception('become with return type is not valid')
                    expr_call = parse_expr_args(None, None)
                    assert_next('\n')
                    f.push(Become(expr_call.function, expr_call.args))
                elif thing == 'il.type':
                    assert 0
                elif thing == 'il.move':
                    reg = next(tokens)
                    out = assert_next(('->', '<-')) == '->'
                    var = next(tokens)
                    assert_next('\n')
                    f.push(IlMove(reg, var, out))
                elif thing == 'il.call':
                    fn = next(tokens)
                    assert_next('\n')
                    f.push(IlCall(fn))
                elif thing == 'for':
                    index = None
                    value = next(tokens)
                    if value == 'index':
                        index = next(token)
                        assert_next(',')
                        value = next(token)
                    value_ref = value == 'ref'
                    if value_ref:
                        value = next(tokens)
                    assert_next('in')
                    array = next(tokens)
                    assert_next('{')
                    assert_next('\n')
                    scope = parse_scope()
                    f.push(ForIn(index, value_ref, value, array, scope))
                elif thing == 'if':
                    variant = next(tokens)
                    value = next(tokens)
                    is_ref = value == 'ref'
                    if is_ref:
                        value = next(tokens)
                    if value == '=':
                        value = None
                    else:
                        assert_next('=')
                    expr, tk = parse_expr()
                    if tk is None:
                        tk = next(tokens)
                    assert_is(tk, '{')
                    assert_next('\n')
                    scope = parse_scope()
                    f.push(If(variant, is_ref, value, expr, scope))
                elif thing == 'break':
                    assert_next('\n')
                    f.push(Break())
                elif thing == 'loop':
                    assert_next('{')
                    assert_next('\n')
                    scope = parse_scope()
                    f.push(Loop(scope))
                else:
                    tk = next(tokens)
                    if tk == '(':
                        f.push(Standard(None, None, parse_expr_args(thing, tk)))
                    elif tk == '=':
                        expr, tk = parse_expr()
                        if tk is None:
                            tk = next(tokens)
                        assert_is(tk, '\n')
                        f.push(Standard(None, thing, expr))
                    elif tk == '\n':
                        assert 0
                    elif tk == '[':
                        index, tk = parse_expr()
                        if tk is None:
                            tk = next(tokens)
                        assert_is(tk, ']')
                        assert_next('=')
                        value, tk = parse_expr()
                        if tk is None:
                            tk = next(tokens)
                        assert_is(tk, '\n')
                    else:
                        ty = thing
                        var = tk
                        tk = assert_next(('\n', '='))
                        if tk == '=':
                            f.push(Standard(ty, var, parse_expr_args(None, None)))
                        else:
                            f.push(Standard(ty, var, None))

            return f


        root = parse_scope()
        unit.functions[function] = Function(args, ret_type, root)

    def handle_record():
        record = next(tokens)
        assert_next('{')
        assert_next('\n')
        if record in unit.records or record in unit.unions:
            raise Exception(f'record "{record}" already defined')
        fields = {}
        while True:
            tk = next(tokens)
            if tk == '}':
                assert_next('\n')
                break
            ty = tk
            field = next(tokens)
            assert_next('\n')
            if field in fields:
                raise Exception(f'field "{field}" in record "{record}" already defined')
            fields[field] = ty
        unit.records[record] = fields

    def handle_union():
        union = next(tokens)
        assert_next('{')
        assert_next('\n')
        if union in unit.unions or union in unit.records:
            raise Exception(f'union "{union}" already defined')
        variants = {}
        while True:
            tk = next(tokens)
            if tk == '}':
                assert_next('\n')
                break
            variant = tk
            tk = next(tokens)
            if tk == '\n':
                value = None
            else:
                value = tk
                assert_next('\n')
            if variant in variants:
                raise Exception(f'variant "{variant}" in union "{union}" already defined')
            variants[variant] = value
        unit.unions[union] = variants

    def handle_register():
        ty = next(tokens)
        if ty == '[':
            index = next(tokens)
            assert_next('->')
            value = next(tokens)
            assert_next(']')
            ty = ArrayType(index, value)
        else:
            ty = UnitType(ty)
        name = next(tokens)
        assert_next('\n')
        if name in unit.registers:
            raise Exception(f'register "{name}" already defined')
        unit.registers[name] = ty

    def handle_il_type():
        # TODO what do?
        ty = next(tokens)
        assert_next('\n')

    keyword_handlers = {
        'alias.type': handle_alias_type,
        'function': handle_function,
        'il.type': handle_il_type,
        'record': handle_record,
        'register': handle_register,
        'union': handle_union,
        '\n': lambda: None,
    }

    for tk in tokens:
        keyword_handlers[tk]()

    return unit


if __name__ == '__main__':
    import sys

    path = sys.argv[1]

    with open(path) as f:
        text = f.read()

    unit = parse(text)

    print('Records:')
    for name, rec in unit.records.items():
        print(' ', name, rec)
    print('Unions:')
    for name, rec in unit.unions.items():
        print(' ', name, rec)
    print('Type aliases:')
    for name, rec in unit.type_aliases.items():
        print(' ', name, '=', rec)
    print('Functions:')
    for name, fn in unit.functions.items():
        print(' ', name)
        for stmt in fn.root.statements:
            print('   ', stmt)

    for s in unit.to_il():
        print(s, end='')
