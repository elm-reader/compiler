/*

import Elm.Kernel.Utils exposing (Tuple0)

*/

var _Reader_recordExpr = F2(function(exprId, val)
{
    // TODO: Record expression!
    return val;
});

var _Reader_recordCall = F3(function(exprId, func, body)
{
    // TODO: Record call!
    return body(__Utils_Tuple0);
});

var _Reader_recordFrame = F2(function(frameId, body)
{
    // TODO: Record frame!
    return body(__Utils_Tuple0);
});

var _Reader_markInstrumented = function(func)
{
    // TODO: Mark instrumented!
    return func;
};

var _Reader_seq = F2(function(sideEffect, val)
{
    return val;
});
