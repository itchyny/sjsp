module Profiler (profiler) where

import Data.Char (isSpace)

import Config

profiler :: Config -> String
profiler config = concatMap (dropWhile isSpace)
 [ ";(function(global) { "
 , "  var interval = " ++ show (interval config) ++ "; "
 , "  var top = " ++ show (top config) ++ "; "
 , "  global.sjsp__result = global.sjsp__result || {}; "
 , "  var sjsp__state = global.sjsp__state = { time: 0, count: 0, line: 0, col: 0, name: '', fname: '', linestr: '' }; "
 , "  global.sjsp__start = function(fname, line, col, name, linestr) { "
 , "    return { time: Date.now(), line: line, col: col, name: name, fname: fname, linestr: linestr }; "
 , "  }; "
 , "  global.sjsp__end = function(x) { "
 , "    if (!x.time) { "
 , "      return; "
 , "    } "
 , "    var key = [ x.fname, x.line, x.col, x.name ].join('/'); "
 , "    global.sjsp__result[key] = global.sjsp__result[key] "
 , "      || { time: 0, count: 0, line: x.line, col: x.col, name: x.name, fname: x.fname, linestr: x.linestr }; "
 , "    global.sjsp__result[key].time += (Date.now() - x.time); "
 , "    global.sjsp__result[key].count += 1; "
 , "  }; "
 , "  global.sjsp__result_time = global.sjsp__result_time || []; "
 , "  global.sjsp__result_count = global.sjsp__result_count || []; "
 , "  if (global.hasOwnProperty('sjsp__interval')) { "
 , "    return; "
 , "  } "
 , "  var space = function(x, n) { "
 , "    return Array(Math.max(0, n - x.toString().length + 1)).join(' ') + x; "
 , "  }; "
 , "  var format = function(x, y) { "
 , "    return [ 'time: ' + space((x.time / 1000).toFixed(3), y.time) + 'sec' "
 , "           , 'count: ' + space(x.count, y.count)"
 , "           , space(x.name, y.name)"
 , "           , space(x.fname, y.fname)"
 , "           , '(line: ' + space(x.line, y.line) + ', col: ' + space(x.col, y.col) + ')' "
 , "           , x.linestr ].join('  '); }; "
 , "  var max = function(x) { "
 , "    return Math.max.apply(null, x); "
 , "  }; "
 , "  var lengths = function(result) { "
 , "    return { "
 , "      time:  max(result.map(function(x) { return (x.time / 1000).toFixed(2).length; })), "
 , "      count: max(result.map(function(x) { return x.count.toString().length; })), "
 , "      fname: max(result.map(function(x) { return x.fname.length; })), "
 , "      name:  max(result.map(function(x) { return x.name.length; })), "
 , "      line:  max(result.map(function(x) { return x.line.toString().length; })), "
 , "      col:   max(result.map(function(x) { return x.col.toString().length; })) "
 , "    }; "
 , "  }; "
 , "  var gather = function(sorter) { "
 , "    var result = "
 , "      Object.keys(global.sjsp__result)"
 , "            .map(function(key) { return global.sjsp__result[key]; })"
 , "            .sort(sorter)"
 , "            .slice(0, top); "
 , "    var l = lengths(result); "
 , "    return result.map(function(x) { return format(x, l); }); "
 , "  }; "
 , "  global.sjsp__interval = setInterval(function() { "
 , "    global.sjsp__result_time = gather(function(x, y) { return y.time - x.time; }); "
 , "    global.sjsp__result_count = gather(function(x, y) { return y.count - x.count; }); "
 , "    console.log('========== SORT BY TIME ==========\\n'"
 , "               + global.sjsp__result_time.join('\\n')"
 , "               + '\\n========== SORT BY COUNT ==========\\n' "
 , "               + global.sjsp__result_count.join('\\n')); "
 , "  }, interval * 1000); "
 , "})(typeof window !== 'undefined' ? window : this); "
 ]
