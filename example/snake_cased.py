def some_function(arg1, arg2, **many_args):
    if arg1 + arg2 < 3:
        return many_args
    else:
        a_variable_with_a_long_name = 10
        return [a_variable_with_a_long_name, many_args['c']]
