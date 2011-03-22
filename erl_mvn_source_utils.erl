-module(erl_mvn_source_utils).

-compile([export_all]).


get_test_for_line(SourceFile, Line) ->       
    case epp_dodger:quick_parse_file(SourceFile) of
        {ok, Forms} ->
            case get_module(Forms) of
                {ok, Module} ->
                    case lists:foldr(
                           fun 
                               (Form, nothing) ->
                                   case Form of
                                       {function, StartLine, Name, _ArgCount, _Def} 
                                         when StartLine =< Line ->
                                           {just, Name};
                                       _ ->
                                           nothing
                                   end;
                               (_, Acc) -> 
                                   Acc
                           end,
                           nothing,
                           Forms) 
                    of
                        {just, FunName} ->
                            {ok, {Module, FunName}};
                        nothing ->
                            {ok, Module}
                    end;
                NoModuleError ->
                    NoModuleError
            end;
        ParseError ->
            {error, {parse_error, ParseError}}
    end.

                
get_module(Forms) ->
    lists:foldr(fun
                    ({attribute, _Line, module, Module}, _Acc) ->
                        {ok, Module};
                    (_, Acc) ->
                        Acc
                end,
                {error, no_module_form},
                Forms).
                
