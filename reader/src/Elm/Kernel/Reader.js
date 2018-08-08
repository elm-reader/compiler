/*

import Elm.Kernel.Utils exposing (Tuple0)
import Tuple exposing (pair)
import Browser exposing (element)
import Platform.Sub as Sub exposing (none)
import Platform.Cmd as Cmd exposing (none)

import Reader exposing (updateConsideringInit, viewConsideringInit, parseConfig)

*/

var _Reader_main = F2(function (decoder, debugData)
{
  var programData = Object.assign({}, debugData, {traces: _Reader_contextJSON()});
  console.info("Program data:", programData);
  return __Browser_element({
    init: function () {
      return A2(__Tuple_pair,
        __Reader_parseConfig(JSON.stringify(programData)),
        __Cmd_none);
    },
    update: __Reader_updateConsideringInit,
    view: __Reader_viewConsideringInit,
    subscriptions: function () { return __Sub_none; }
  })(decoder)(debugData);
});

var _Reader_context = {
  $: __1_NON_INSTRUMENTED_FRAME,
  __childFrames: [],
};

var _Reader_nextFrameId = 0;

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
        __runtimeId: _Reader_nextFrameId++,
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
    __exprs: {},
  };

  var oldContext = _Reader_context;
  _Reader_context = newContext;
  var result = body(__Utils_Tuple0);
  _Reader_context = oldContext;

  var newFrame = {
    $: __2_INSTRUMENTED,
    __id: frameId,
    __exprs: newContext.__exprs,
    __runtimeId: _Reader_nextFrameId++,
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

var _Reader_contextJSON = function ()
{
  return _Reader_toHumanReadable(_Reader_context).child_frames;
};

function _Reader_toHumanReadable(frame)
{
  if (frame.$ === __2_INSTRUMENTED)
  {
    var readableExprs = [];
    Object.keys(frame.__exprs).forEach(function (id) {
      var expr = frame.__exprs[id];
      var val =
        (typeof expr.__val === "function"
          ? {"#<function>": {}}
          : expr.__val);
      readableExprs.push({
        id: +id,
        expr: {
          val: val,
          child_frame: expr.__childFrame && _Reader_toHumanReadable(expr.__childFrame),
        }
      });
    });
    return {
      tag: 'Instrumented',
      source_map_id: frame.__id,
      runtime_id: frame.__runtimeId,
      exprs: readableExprs
    };
  }
  else
  {
    return {
      tag: 'NonInstrumented',
      child_frames: frame.__childFrames.map(_Reader_toHumanReadable),
      runtime_id: frame.__runtimeId,
    };
  }
}

// Tools for debugging the debugger from the JavaScript console

// Need to export as global to be available from the console
window.elmReaderRoot = function()
{
  return _Reader_toHumanReadable(_Reader_context);
};
