CodeMirror.defineMode("paradocs", function (config) {
  var reservedNames = /^%rule|^%extend|^%def|^%escape|^%before|^%after|^%include|%read/
  var instruction = /^%[^"%\s]+/
  var backslash = /^%?\\.?/
  var quote1 = /^%?".*?[^\\]"/

  function base (stream, state) {
    if (stream.match(reservedNames)) {
      return "keyword";
    } else if (stream.match(backslash)) {
      return "string";
    } else if (stream.match(/^%?""+/)) {
      beginMark = stream.current()
      if (beginMark[0] == '%') {
        beginMark = beginMark.slice(1)
      }
      var ch;
      var endMark = ""
      while (ch = stream.next()) {
        if (ch != '"') {
          endMark = "";
          continue;
        }

        endMark += '"'

        while (ch = stream.next()) {
          if (ch == '"') {
            endMark += '"';
            continue;
          } else {
            stream.backUp(1);
            break;
          }
        }

        if (beginMark == endMark) {
          break;
        }
      }

      if (beginMark != endMark) {
        state.tokenize = string(beginMark);
      }
      return "string";
    } else if (stream.match(quote1)) {
      return "string";
    } else if (stream.match(instruction)) {
      return "variable-2";
    } else {
      stream.next();
      return null;
    }
  }

  function string (beginMark) {
    return function (stream, state) {
      var ch;
      var endMark = ""
      while (ch = stream.next()) {
        if (ch != '"') {
          endMark = "";
          continue;
        }

        endMark += '"'

        while (ch = stream.next()) {
          if (ch == '"') {
            endMark += '"';
            continue;
          } else {
            stream.backUp(1);
            break;
          }
        }

        if (beginMark == endMark) {
          break;
        }
      }

      if (beginMark == endMark) {
        state.tokenize = base;
      }

      return "string";
    }
  }

  return {
    startState: function () {
      return { tokenize: base };
    },
    token: function (stream, state) {
      return state.tokenize(stream, state);
    }
  };
});