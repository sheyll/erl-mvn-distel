-module(erl_mvn_source_utils).

-compile([export_all]).

load_module(Module, File) ->
    code:purge(Module),
    {module, Module} = code:load_abs(File).
    
get_test_for_line(SourceFile, Line) ->       
    case foldr_mfals(
           fun 
               ({Module, Name, 0, StartLine}, {module, _})
                 when StartLine =< Line ->
                   {function, Module, Name};
               ({Module, _, _, _}, {module, undefined}) -> 
                   {module, Module};
               (_Arg, Acc) -> 
                   Acc
           end,
           {module, undefined},
           SourceFile)
    of
        {function, Module, FunName} ->
            {ok, {Module, FunName}};
        {module, Module} ->
            {ok, Module};
        Error ->
            Error
    end.
                
get_line_of_function(SourceFile, Function, ArgCount) ->
    foldr_mfals(fun({_Mod, F, A, L}, _Acc)
                      when F == Function, A == ArgCount ->
                        L;
                   (_, Acc) -> Acc
                end,
                0,
                SourceFile).


get_module(Forms) ->
    lists:foldr(fun
                    ({attribute, _Line, module, Module}, _Acc) ->
                        {ok, Module};
                    (_, Acc) ->
                        Acc
                end,
                {error, no_module_form},
                Forms).
                
foldr_mfals(F, Initial, SourceFile) ->
        case epp_dodger:quick_parse_file(SourceFile) of
        {ok, Forms} ->
            case get_module(Forms) of
                {ok, Module} ->
                    lists:foldr(
                      fun 
                          (Form, Acc) ->
                              case Form of
                                  {function, StartLine, Name, ArgCount, _Def} ->
                                      F({Module, Name, ArgCount, StartLine}, Acc);
                                  _ -> Acc
                              end
                      end,
                      Initial,
                      Forms);

                NoModuleError ->
                    NoModuleError
            end;
        ParseError ->
            {error, {parse_error, ParseError}}
    end.

