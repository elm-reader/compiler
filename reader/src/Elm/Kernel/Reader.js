/*

import Elm.Kernel.Utils exposing (Tuple0)
import Browser exposing (sandbox)
import Reader exposing (init, update, view, rawConfig, Model)

*/

var _Reader_main = F2(function (decoder, debugData)
{
  return __Browser_sandbox({
    init: A2(
      __Reader_Model,
      __Reader_init.count,
      __Reader_rawConfig(JSON.stringify(debugData)),
    ),
    update: __Reader_update,
    view: __Reader_view
  })(decoder)(debugData);
});

var _Reader_context = {
  $: __1_NON_INSTRUMENTED_FRAME,
  __childFrames: [],
};

var _Reader_recordExpr = F2(function(exprId, val)
{
  if (_Reader_context.$ !== __1_INSTRUMENTED_FRAME)
  {
    console.warn(
      'Elm Reader: Someone tried to record an expression from a non-instrumented context.',
      'expression id: ', exprId,
      'value: ', val
    );
  }
  else
  {
    _Reader_context.__exprs[exprId] = {
      __val: val,
      __childFrame: null,
    };
  }

  return val;
});

var _Reader_recordCall = F3(function(exprId, func, body)
{
  if (_Reader_context.$ !== __1_INSTRUMENTED_FRAME)
  {
    var result = body(__Utils_Tuple0);
    console.warn(
      'Elm Reader: Someone tried to record a function call from a non-instrumented context.',
      'expression id: ', exprId,
      'value: ', result
    );
    return result;
  }

  if (!func.elmReaderInstrumented)
  {
    var newContext = {
      $: __1_NON_INSTRUMENTED_FRAME,
      __childFrames: [],
    }

    var oldContext = _Reader_context;
    _Reader_context = newContext;
    var result = body(__Utils_Tuple0);
    _Reader_context = oldContext;

    _Reader_context.__exprs[exprId] = {
      __val: result,
      __childFrame: {
        $: __2_NON_INSTRUMENTED,
        __childFrames: newContext.__childFrames,
      },
    };

    return result;
  }
  else
  {
    var newContext = {
      $: __1_CALL,
      __childFrame: null,
    };

    var oldContext = _Reader_context;
    _Reader_context = newContext;
    var result = body(__Utils_Tuple0);
    _Reader_context = oldContext;

    _Reader_context.__exprs[exprId] = {
      __val: result,
      __childFrame: newContext.__childFrame,
    };

    return result;
  }
});

var _Reader_recordFrame = F2(function(frameIdRaw, body)
{
  var frameId = JSON.parse(frameIdRaw);
  var newContext = {
    $: __1_INSTRUMENTED_FRAME,
    __exprs: [],
  };

  var oldContext = _Reader_context;
  _Reader_context = newContext;
  var result = body(__Utils_Tuple0);
  _Reader_context = oldContext;

  var newFrame = {
    $: __2_INSTRUMENTED,
    __id: frameId,
    __exprs: newContext.__exprs,
  };

  if (_Reader_context.$ === __1_CALL)
  {
    _Reader_context.__childFrame = newFrame;
  }
  else if (_Reader_context.$ === __1_NON_INSTRUMENTED_FRAME)
  {
    _Reader_context.__childFrames.push(newFrame);
  }

  return result;
});

var _Reader_markInstrumented = function(func)
{
  func.elmReaderInstrumented = true;
  return func;
};

var _Reader_seq = F2(function(sideEffect, val)
{
  return val;
});

// Tools for debugging the debugger from the JavaScript console

function _Reader_toHumanReadable(frame)
{
  if (frame.$ === __2_INSTRUMENTED)
  {
    return {
      tag: 'Instrumented',
      id: frame.__id,
      exprs: frame.__exprs.map(function(expr) {
        var childFrame = null;
        if (expr.__childFrame !== null) {
          childFrame = _Reader_toHumanReadable(expr.__childFrame);
        }
        return {
          val: expr.__val,
          childFrame: childFrame,
        };
      }),
    };
  }
  else
  {
    return {
      tag: 'NonInstrumented',
      childFrames: frame.__childFrames.map(_Reader_toHumanReadable),
    };
  }
}

// Need to export as global to be available from the console
window.elmReaderRoot = function()
{
  return _Reader_toHumanReadable(_Reader_context);
};
