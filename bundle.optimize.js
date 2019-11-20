(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File === 'function' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[94m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.br.aN === region.bA.aN)
	{
		return 'on line ' + region.br.aN;
	}
	return 'on lines ' + region.br.aN + ' through ' + region.bA.aN;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});


/*
 * Copyright (c) 2010 Mozilla Corporation
 * Copyright (c) 2010 Vladimir Vukicevic
 * Copyright (c) 2013 John Mayer
 * Copyright (c) 2018 Andrey Kuzmin
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

// Vector2

var _MJS_v2 = F2(function(x, y) {
    return new Float64Array([x, y]);
});

var _MJS_v2getX = function(a) {
    return a[0];
};

var _MJS_v2getY = function(a) {
    return a[1];
};

var _MJS_v2setX = F2(function(x, a) {
    return new Float64Array([x, a[1]]);
});

var _MJS_v2setY = F2(function(y, a) {
    return new Float64Array([a[0], y]);
});

var _MJS_v2toRecord = function(a) {
    return { ck: a[0], cl: a[1] };
};

var _MJS_v2fromRecord = function(r) {
    return new Float64Array([r.ck, r.cl]);
};

var _MJS_v2add = F2(function(a, b) {
    var r = new Float64Array(2);
    r[0] = a[0] + b[0];
    r[1] = a[1] + b[1];
    return r;
});

var _MJS_v2sub = F2(function(a, b) {
    var r = new Float64Array(2);
    r[0] = a[0] - b[0];
    r[1] = a[1] - b[1];
    return r;
});

var _MJS_v2negate = function(a) {
    var r = new Float64Array(2);
    r[0] = -a[0];
    r[1] = -a[1];
    return r;
};

var _MJS_v2direction = F2(function(a, b) {
    var r = new Float64Array(2);
    r[0] = a[0] - b[0];
    r[1] = a[1] - b[1];
    var im = 1.0 / _MJS_v2lengthLocal(r);
    r[0] = r[0] * im;
    r[1] = r[1] * im;
    return r;
});

function _MJS_v2lengthLocal(a) {
    return Math.sqrt(a[0] * a[0] + a[1] * a[1]);
}
var _MJS_v2length = _MJS_v2lengthLocal;

var _MJS_v2lengthSquared = function(a) {
    return a[0] * a[0] + a[1] * a[1];
};

var _MJS_v2distance = F2(function(a, b) {
    var dx = a[0] - b[0];
    var dy = a[1] - b[1];
    return Math.sqrt(dx * dx + dy * dy);
});

var _MJS_v2distanceSquared = F2(function(a, b) {
    var dx = a[0] - b[0];
    var dy = a[1] - b[1];
    return dx * dx + dy * dy;
});

var _MJS_v2normalize = function(a) {
    var r = new Float64Array(2);
    var im = 1.0 / _MJS_v2lengthLocal(a);
    r[0] = a[0] * im;
    r[1] = a[1] * im;
    return r;
};

var _MJS_v2scale = F2(function(k, a) {
    var r = new Float64Array(2);
    r[0] = a[0] * k;
    r[1] = a[1] * k;
    return r;
});

var _MJS_v2dot = F2(function(a, b) {
    return a[0] * b[0] + a[1] * b[1];
});

// Vector3

var _MJS_v3temp1Local = new Float64Array(3);
var _MJS_v3temp2Local = new Float64Array(3);
var _MJS_v3temp3Local = new Float64Array(3);

var _MJS_v3 = F3(function(x, y, z) {
    return new Float64Array([x, y, z]);
});

var _MJS_v3getX = function(a) {
    return a[0];
};

var _MJS_v3getY = function(a) {
    return a[1];
};

var _MJS_v3getZ = function(a) {
    return a[2];
};

var _MJS_v3setX = F2(function(x, a) {
    return new Float64Array([x, a[1], a[2]]);
});

var _MJS_v3setY = F2(function(y, a) {
    return new Float64Array([a[0], y, a[2]]);
});

var _MJS_v3setZ = F2(function(z, a) {
    return new Float64Array([a[0], a[1], z]);
});

var _MJS_v3toRecord = function(a) {
    return { ck: a[0], cl: a[1], cn: a[2] };
};

var _MJS_v3fromRecord = function(r) {
    return new Float64Array([r.ck, r.cl, r.cn]);
};

var _MJS_v3add = F2(function(a, b) {
    var r = new Float64Array(3);
    r[0] = a[0] + b[0];
    r[1] = a[1] + b[1];
    r[2] = a[2] + b[2];
    return r;
});

function _MJS_v3subLocal(a, b, r) {
    if (r === undefined) {
        r = new Float64Array(3);
    }
    r[0] = a[0] - b[0];
    r[1] = a[1] - b[1];
    r[2] = a[2] - b[2];
    return r;
}
var _MJS_v3sub = F2(_MJS_v3subLocal);

var _MJS_v3negate = function(a) {
    var r = new Float64Array(3);
    r[0] = -a[0];
    r[1] = -a[1];
    r[2] = -a[2];
    return r;
};

function _MJS_v3directionLocal(a, b, r) {
    if (r === undefined) {
        r = new Float64Array(3);
    }
    return _MJS_v3normalizeLocal(_MJS_v3subLocal(a, b, r), r);
}
var _MJS_v3direction = F2(_MJS_v3directionLocal);

function _MJS_v3lengthLocal(a) {
    return Math.sqrt(a[0] * a[0] + a[1] * a[1] + a[2] * a[2]);
}
var _MJS_v3length = _MJS_v3lengthLocal;

var _MJS_v3lengthSquared = function(a) {
    return a[0] * a[0] + a[1] * a[1] + a[2] * a[2];
};

var _MJS_v3distance = F2(function(a, b) {
    var dx = a[0] - b[0];
    var dy = a[1] - b[1];
    var dz = a[2] - b[2];
    return Math.sqrt(dx * dx + dy * dy + dz * dz);
});

var _MJS_v3distanceSquared = F2(function(a, b) {
    var dx = a[0] - b[0];
    var dy = a[1] - b[1];
    var dz = a[2] - b[2];
    return dx * dx + dy * dy + dz * dz;
});

function _MJS_v3normalizeLocal(a, r) {
    if (r === undefined) {
        r = new Float64Array(3);
    }
    var im = 1.0 / _MJS_v3lengthLocal(a);
    r[0] = a[0] * im;
    r[1] = a[1] * im;
    r[2] = a[2] * im;
    return r;
}
var _MJS_v3normalize = _MJS_v3normalizeLocal;

var _MJS_v3scale = F2(function(k, a) {
    return new Float64Array([a[0] * k, a[1] * k, a[2] * k]);
});

var _MJS_v3dotLocal = function(a, b) {
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
};
var _MJS_v3dot = F2(_MJS_v3dotLocal);

function _MJS_v3crossLocal(a, b, r) {
    if (r === undefined) {
        r = new Float64Array(3);
    }
    r[0] = a[1] * b[2] - a[2] * b[1];
    r[1] = a[2] * b[0] - a[0] * b[2];
    r[2] = a[0] * b[1] - a[1] * b[0];
    return r;
}
var _MJS_v3cross = F2(_MJS_v3crossLocal);

var _MJS_v3mul4x4 = F2(function(m, v) {
    var w;
    var tmp = _MJS_v3temp1Local;
    var r = new Float64Array(3);

    tmp[0] = m[3];
    tmp[1] = m[7];
    tmp[2] = m[11];
    w = _MJS_v3dotLocal(v, tmp) + m[15];
    tmp[0] = m[0];
    tmp[1] = m[4];
    tmp[2] = m[8];
    r[0] = (_MJS_v3dotLocal(v, tmp) + m[12]) / w;
    tmp[0] = m[1];
    tmp[1] = m[5];
    tmp[2] = m[9];
    r[1] = (_MJS_v3dotLocal(v, tmp) + m[13]) / w;
    tmp[0] = m[2];
    tmp[1] = m[6];
    tmp[2] = m[10];
    r[2] = (_MJS_v3dotLocal(v, tmp) + m[14]) / w;
    return r;
});

// Vector4

var _MJS_v4 = F4(function(x, y, z, w) {
    return new Float64Array([x, y, z, w]);
});

var _MJS_v4getX = function(a) {
    return a[0];
};

var _MJS_v4getY = function(a) {
    return a[1];
};

var _MJS_v4getZ = function(a) {
    return a[2];
};

var _MJS_v4getW = function(a) {
    return a[3];
};

var _MJS_v4setX = F2(function(x, a) {
    return new Float64Array([x, a[1], a[2], a[3]]);
});

var _MJS_v4setY = F2(function(y, a) {
    return new Float64Array([a[0], y, a[2], a[3]]);
});

var _MJS_v4setZ = F2(function(z, a) {
    return new Float64Array([a[0], a[1], z, a[3]]);
});

var _MJS_v4setW = F2(function(w, a) {
    return new Float64Array([a[0], a[1], a[2], w]);
});

var _MJS_v4toRecord = function(a) {
    return { ck: a[0], cl: a[1], cn: a[2], ch: a[3] };
};

var _MJS_v4fromRecord = function(r) {
    return new Float64Array([r.ck, r.cl, r.cn, r.ch]);
};

var _MJS_v4add = F2(function(a, b) {
    var r = new Float64Array(4);
    r[0] = a[0] + b[0];
    r[1] = a[1] + b[1];
    r[2] = a[2] + b[2];
    r[3] = a[3] + b[3];
    return r;
});

var _MJS_v4sub = F2(function(a, b) {
    var r = new Float64Array(4);
    r[0] = a[0] - b[0];
    r[1] = a[1] - b[1];
    r[2] = a[2] - b[2];
    r[3] = a[3] - b[3];
    return r;
});

var _MJS_v4negate = function(a) {
    var r = new Float64Array(4);
    r[0] = -a[0];
    r[1] = -a[1];
    r[2] = -a[2];
    r[3] = -a[3];
    return r;
};

var _MJS_v4direction = F2(function(a, b) {
    var r = new Float64Array(4);
    r[0] = a[0] - b[0];
    r[1] = a[1] - b[1];
    r[2] = a[2] - b[2];
    r[3] = a[3] - b[3];
    var im = 1.0 / _MJS_v4lengthLocal(r);
    r[0] = r[0] * im;
    r[1] = r[1] * im;
    r[2] = r[2] * im;
    r[3] = r[3] * im;
    return r;
});

function _MJS_v4lengthLocal(a) {
    return Math.sqrt(a[0] * a[0] + a[1] * a[1] + a[2] * a[2] + a[3] * a[3]);
}
var _MJS_v4length = _MJS_v4lengthLocal;

var _MJS_v4lengthSquared = function(a) {
    return a[0] * a[0] + a[1] * a[1] + a[2] * a[2] + a[3] * a[3];
};

var _MJS_v4distance = F2(function(a, b) {
    var dx = a[0] - b[0];
    var dy = a[1] - b[1];
    var dz = a[2] - b[2];
    var dw = a[3] - b[3];
    return Math.sqrt(dx * dx + dy * dy + dz * dz + dw * dw);
});

var _MJS_v4distanceSquared = F2(function(a, b) {
    var dx = a[0] - b[0];
    var dy = a[1] - b[1];
    var dz = a[2] - b[2];
    var dw = a[3] - b[3];
    return dx * dx + dy * dy + dz * dz + dw * dw;
});

var _MJS_v4normalize = function(a) {
    var r = new Float64Array(4);
    var im = 1.0 / _MJS_v4lengthLocal(a);
    r[0] = a[0] * im;
    r[1] = a[1] * im;
    r[2] = a[2] * im;
    r[3] = a[3] * im;
    return r;
};

var _MJS_v4scale = F2(function(k, a) {
    var r = new Float64Array(4);
    r[0] = a[0] * k;
    r[1] = a[1] * k;
    r[2] = a[2] * k;
    r[3] = a[3] * k;
    return r;
});

var _MJS_v4dot = F2(function(a, b) {
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2] + a[3] * b[3];
});

// Matrix4

var _MJS_m4x4temp1Local = new Float64Array(16);
var _MJS_m4x4temp2Local = new Float64Array(16);

var _MJS_m4x4identity = new Float64Array([
    1.0, 0.0, 0.0, 0.0,
    0.0, 1.0, 0.0, 0.0,
    0.0, 0.0, 1.0, 0.0,
    0.0, 0.0, 0.0, 1.0
]);

var _MJS_m4x4fromRecord = function(r) {
    var m = new Float64Array(16);
    m[0] = r.bH;
    m[1] = r.bL;
    m[2] = r.bP;
    m[3] = r.bT;
    m[4] = r.bI;
    m[5] = r.bM;
    m[6] = r.bQ;
    m[7] = r.bU;
    m[8] = r.bJ;
    m[9] = r.bN;
    m[10] = r.bR;
    m[11] = r.bV;
    m[12] = r.bK;
    m[13] = r.bO;
    m[14] = r.bS;
    m[15] = r.bW;
    return m;
};

var _MJS_m4x4toRecord = function(m) {
    return {
        bH: m[0], bL: m[1], bP: m[2], bT: m[3],
        bI: m[4], bM: m[5], bQ: m[6], bU: m[7],
        bJ: m[8], bN: m[9], bR: m[10], bV: m[11],
        bK: m[12], bO: m[13], bS: m[14], bW: m[15]
    };
};

var _MJS_m4x4inverse = function(m) {
    var r = new Float64Array(16);

    r[0] = m[5] * m[10] * m[15] - m[5] * m[11] * m[14] - m[9] * m[6] * m[15] +
        m[9] * m[7] * m[14] + m[13] * m[6] * m[11] - m[13] * m[7] * m[10];
    r[4] = -m[4] * m[10] * m[15] + m[4] * m[11] * m[14] + m[8] * m[6] * m[15] -
        m[8] * m[7] * m[14] - m[12] * m[6] * m[11] + m[12] * m[7] * m[10];
    r[8] = m[4] * m[9] * m[15] - m[4] * m[11] * m[13] - m[8] * m[5] * m[15] +
        m[8] * m[7] * m[13] + m[12] * m[5] * m[11] - m[12] * m[7] * m[9];
    r[12] = -m[4] * m[9] * m[14] + m[4] * m[10] * m[13] + m[8] * m[5] * m[14] -
        m[8] * m[6] * m[13] - m[12] * m[5] * m[10] + m[12] * m[6] * m[9];
    r[1] = -m[1] * m[10] * m[15] + m[1] * m[11] * m[14] + m[9] * m[2] * m[15] -
        m[9] * m[3] * m[14] - m[13] * m[2] * m[11] + m[13] * m[3] * m[10];
    r[5] = m[0] * m[10] * m[15] - m[0] * m[11] * m[14] - m[8] * m[2] * m[15] +
        m[8] * m[3] * m[14] + m[12] * m[2] * m[11] - m[12] * m[3] * m[10];
    r[9] = -m[0] * m[9] * m[15] + m[0] * m[11] * m[13] + m[8] * m[1] * m[15] -
        m[8] * m[3] * m[13] - m[12] * m[1] * m[11] + m[12] * m[3] * m[9];
    r[13] = m[0] * m[9] * m[14] - m[0] * m[10] * m[13] - m[8] * m[1] * m[14] +
        m[8] * m[2] * m[13] + m[12] * m[1] * m[10] - m[12] * m[2] * m[9];
    r[2] = m[1] * m[6] * m[15] - m[1] * m[7] * m[14] - m[5] * m[2] * m[15] +
        m[5] * m[3] * m[14] + m[13] * m[2] * m[7] - m[13] * m[3] * m[6];
    r[6] = -m[0] * m[6] * m[15] + m[0] * m[7] * m[14] + m[4] * m[2] * m[15] -
        m[4] * m[3] * m[14] - m[12] * m[2] * m[7] + m[12] * m[3] * m[6];
    r[10] = m[0] * m[5] * m[15] - m[0] * m[7] * m[13] - m[4] * m[1] * m[15] +
        m[4] * m[3] * m[13] + m[12] * m[1] * m[7] - m[12] * m[3] * m[5];
    r[14] = -m[0] * m[5] * m[14] + m[0] * m[6] * m[13] + m[4] * m[1] * m[14] -
        m[4] * m[2] * m[13] - m[12] * m[1] * m[6] + m[12] * m[2] * m[5];
    r[3] = -m[1] * m[6] * m[11] + m[1] * m[7] * m[10] + m[5] * m[2] * m[11] -
        m[5] * m[3] * m[10] - m[9] * m[2] * m[7] + m[9] * m[3] * m[6];
    r[7] = m[0] * m[6] * m[11] - m[0] * m[7] * m[10] - m[4] * m[2] * m[11] +
        m[4] * m[3] * m[10] + m[8] * m[2] * m[7] - m[8] * m[3] * m[6];
    r[11] = -m[0] * m[5] * m[11] + m[0] * m[7] * m[9] + m[4] * m[1] * m[11] -
        m[4] * m[3] * m[9] - m[8] * m[1] * m[7] + m[8] * m[3] * m[5];
    r[15] = m[0] * m[5] * m[10] - m[0] * m[6] * m[9] - m[4] * m[1] * m[10] +
        m[4] * m[2] * m[9] + m[8] * m[1] * m[6] - m[8] * m[2] * m[5];

    var det = m[0] * r[0] + m[1] * r[4] + m[2] * r[8] + m[3] * r[12];

    if (det === 0) {
        return $elm$core$Maybe$Nothing;
    }

    det = 1.0 / det;

    for (var i = 0; i < 16; i = i + 1) {
        r[i] = r[i] * det;
    }

    return $elm$core$Maybe$Just(r);
};

var _MJS_m4x4inverseOrthonormal = function(m) {
    var r = _MJS_m4x4transposeLocal(m);
    var t = [m[12], m[13], m[14]];
    r[3] = r[7] = r[11] = 0;
    r[12] = -_MJS_v3dotLocal([r[0], r[4], r[8]], t);
    r[13] = -_MJS_v3dotLocal([r[1], r[5], r[9]], t);
    r[14] = -_MJS_v3dotLocal([r[2], r[6], r[10]], t);
    return r;
};

function _MJS_m4x4makeFrustumLocal(left, right, bottom, top, znear, zfar) {
    var r = new Float64Array(16);

    r[0] = 2 * znear / (right - left);
    r[1] = 0;
    r[2] = 0;
    r[3] = 0;
    r[4] = 0;
    r[5] = 2 * znear / (top - bottom);
    r[6] = 0;
    r[7] = 0;
    r[8] = (right + left) / (right - left);
    r[9] = (top + bottom) / (top - bottom);
    r[10] = -(zfar + znear) / (zfar - znear);
    r[11] = -1;
    r[12] = 0;
    r[13] = 0;
    r[14] = -2 * zfar * znear / (zfar - znear);
    r[15] = 0;

    return r;
}
var _MJS_m4x4makeFrustum = F6(_MJS_m4x4makeFrustumLocal);

var _MJS_m4x4makePerspective = F4(function(fovy, aspect, znear, zfar) {
    var ymax = znear * Math.tan(fovy * Math.PI / 360.0);
    var ymin = -ymax;
    var xmin = ymin * aspect;
    var xmax = ymax * aspect;

    return _MJS_m4x4makeFrustumLocal(xmin, xmax, ymin, ymax, znear, zfar);
});

function _MJS_m4x4makeOrthoLocal(left, right, bottom, top, znear, zfar) {
    var r = new Float64Array(16);

    r[0] = 2 / (right - left);
    r[1] = 0;
    r[2] = 0;
    r[3] = 0;
    r[4] = 0;
    r[5] = 2 / (top - bottom);
    r[6] = 0;
    r[7] = 0;
    r[8] = 0;
    r[9] = 0;
    r[10] = -2 / (zfar - znear);
    r[11] = 0;
    r[12] = -(right + left) / (right - left);
    r[13] = -(top + bottom) / (top - bottom);
    r[14] = -(zfar + znear) / (zfar - znear);
    r[15] = 1;

    return r;
}
var _MJS_m4x4makeOrtho = F6(_MJS_m4x4makeOrthoLocal);

var _MJS_m4x4makeOrtho2D = F4(function(left, right, bottom, top) {
    return _MJS_m4x4makeOrthoLocal(left, right, bottom, top, -1, 1);
});

function _MJS_m4x4mulLocal(a, b) {
    var r = new Float64Array(16);
    var a11 = a[0];
    var a21 = a[1];
    var a31 = a[2];
    var a41 = a[3];
    var a12 = a[4];
    var a22 = a[5];
    var a32 = a[6];
    var a42 = a[7];
    var a13 = a[8];
    var a23 = a[9];
    var a33 = a[10];
    var a43 = a[11];
    var a14 = a[12];
    var a24 = a[13];
    var a34 = a[14];
    var a44 = a[15];
    var b11 = b[0];
    var b21 = b[1];
    var b31 = b[2];
    var b41 = b[3];
    var b12 = b[4];
    var b22 = b[5];
    var b32 = b[6];
    var b42 = b[7];
    var b13 = b[8];
    var b23 = b[9];
    var b33 = b[10];
    var b43 = b[11];
    var b14 = b[12];
    var b24 = b[13];
    var b34 = b[14];
    var b44 = b[15];

    r[0] = a11 * b11 + a12 * b21 + a13 * b31 + a14 * b41;
    r[1] = a21 * b11 + a22 * b21 + a23 * b31 + a24 * b41;
    r[2] = a31 * b11 + a32 * b21 + a33 * b31 + a34 * b41;
    r[3] = a41 * b11 + a42 * b21 + a43 * b31 + a44 * b41;
    r[4] = a11 * b12 + a12 * b22 + a13 * b32 + a14 * b42;
    r[5] = a21 * b12 + a22 * b22 + a23 * b32 + a24 * b42;
    r[6] = a31 * b12 + a32 * b22 + a33 * b32 + a34 * b42;
    r[7] = a41 * b12 + a42 * b22 + a43 * b32 + a44 * b42;
    r[8] = a11 * b13 + a12 * b23 + a13 * b33 + a14 * b43;
    r[9] = a21 * b13 + a22 * b23 + a23 * b33 + a24 * b43;
    r[10] = a31 * b13 + a32 * b23 + a33 * b33 + a34 * b43;
    r[11] = a41 * b13 + a42 * b23 + a43 * b33 + a44 * b43;
    r[12] = a11 * b14 + a12 * b24 + a13 * b34 + a14 * b44;
    r[13] = a21 * b14 + a22 * b24 + a23 * b34 + a24 * b44;
    r[14] = a31 * b14 + a32 * b24 + a33 * b34 + a34 * b44;
    r[15] = a41 * b14 + a42 * b24 + a43 * b34 + a44 * b44;

    return r;
}
var _MJS_m4x4mul = F2(_MJS_m4x4mulLocal);

var _MJS_m4x4mulAffine = F2(function(a, b) {
    var r = new Float64Array(16);
    var a11 = a[0];
    var a21 = a[1];
    var a31 = a[2];
    var a12 = a[4];
    var a22 = a[5];
    var a32 = a[6];
    var a13 = a[8];
    var a23 = a[9];
    var a33 = a[10];
    var a14 = a[12];
    var a24 = a[13];
    var a34 = a[14];

    var b11 = b[0];
    var b21 = b[1];
    var b31 = b[2];
    var b12 = b[4];
    var b22 = b[5];
    var b32 = b[6];
    var b13 = b[8];
    var b23 = b[9];
    var b33 = b[10];
    var b14 = b[12];
    var b24 = b[13];
    var b34 = b[14];

    r[0] = a11 * b11 + a12 * b21 + a13 * b31;
    r[1] = a21 * b11 + a22 * b21 + a23 * b31;
    r[2] = a31 * b11 + a32 * b21 + a33 * b31;
    r[3] = 0;
    r[4] = a11 * b12 + a12 * b22 + a13 * b32;
    r[5] = a21 * b12 + a22 * b22 + a23 * b32;
    r[6] = a31 * b12 + a32 * b22 + a33 * b32;
    r[7] = 0;
    r[8] = a11 * b13 + a12 * b23 + a13 * b33;
    r[9] = a21 * b13 + a22 * b23 + a23 * b33;
    r[10] = a31 * b13 + a32 * b23 + a33 * b33;
    r[11] = 0;
    r[12] = a11 * b14 + a12 * b24 + a13 * b34 + a14;
    r[13] = a21 * b14 + a22 * b24 + a23 * b34 + a24;
    r[14] = a31 * b14 + a32 * b24 + a33 * b34 + a34;
    r[15] = 1;

    return r;
});

var _MJS_m4x4makeRotate = F2(function(angle, axis) {
    var r = new Float64Array(16);
    axis = _MJS_v3normalizeLocal(axis, _MJS_v3temp1Local);
    var x = axis[0];
    var y = axis[1];
    var z = axis[2];
    var c = Math.cos(angle);
    var c1 = 1 - c;
    var s = Math.sin(angle);

    r[0] = x * x * c1 + c;
    r[1] = y * x * c1 + z * s;
    r[2] = z * x * c1 - y * s;
    r[3] = 0;
    r[4] = x * y * c1 - z * s;
    r[5] = y * y * c1 + c;
    r[6] = y * z * c1 + x * s;
    r[7] = 0;
    r[8] = x * z * c1 + y * s;
    r[9] = y * z * c1 - x * s;
    r[10] = z * z * c1 + c;
    r[11] = 0;
    r[12] = 0;
    r[13] = 0;
    r[14] = 0;
    r[15] = 1;

    return r;
});

var _MJS_m4x4rotate = F3(function(angle, axis, m) {
    var r = new Float64Array(16);
    var im = 1.0 / _MJS_v3lengthLocal(axis);
    var x = axis[0] * im;
    var y = axis[1] * im;
    var z = axis[2] * im;
    var c = Math.cos(angle);
    var c1 = 1 - c;
    var s = Math.sin(angle);
    var xs = x * s;
    var ys = y * s;
    var zs = z * s;
    var xyc1 = x * y * c1;
    var xzc1 = x * z * c1;
    var yzc1 = y * z * c1;
    var t11 = x * x * c1 + c;
    var t21 = xyc1 + zs;
    var t31 = xzc1 - ys;
    var t12 = xyc1 - zs;
    var t22 = y * y * c1 + c;
    var t32 = yzc1 + xs;
    var t13 = xzc1 + ys;
    var t23 = yzc1 - xs;
    var t33 = z * z * c1 + c;
    var m11 = m[0], m21 = m[1], m31 = m[2], m41 = m[3];
    var m12 = m[4], m22 = m[5], m32 = m[6], m42 = m[7];
    var m13 = m[8], m23 = m[9], m33 = m[10], m43 = m[11];
    var m14 = m[12], m24 = m[13], m34 = m[14], m44 = m[15];

    r[0] = m11 * t11 + m12 * t21 + m13 * t31;
    r[1] = m21 * t11 + m22 * t21 + m23 * t31;
    r[2] = m31 * t11 + m32 * t21 + m33 * t31;
    r[3] = m41 * t11 + m42 * t21 + m43 * t31;
    r[4] = m11 * t12 + m12 * t22 + m13 * t32;
    r[5] = m21 * t12 + m22 * t22 + m23 * t32;
    r[6] = m31 * t12 + m32 * t22 + m33 * t32;
    r[7] = m41 * t12 + m42 * t22 + m43 * t32;
    r[8] = m11 * t13 + m12 * t23 + m13 * t33;
    r[9] = m21 * t13 + m22 * t23 + m23 * t33;
    r[10] = m31 * t13 + m32 * t23 + m33 * t33;
    r[11] = m41 * t13 + m42 * t23 + m43 * t33;
    r[12] = m14,
    r[13] = m24;
    r[14] = m34;
    r[15] = m44;

    return r;
});

function _MJS_m4x4makeScale3Local(x, y, z) {
    var r = new Float64Array(16);

    r[0] = x;
    r[1] = 0;
    r[2] = 0;
    r[3] = 0;
    r[4] = 0;
    r[5] = y;
    r[6] = 0;
    r[7] = 0;
    r[8] = 0;
    r[9] = 0;
    r[10] = z;
    r[11] = 0;
    r[12] = 0;
    r[13] = 0;
    r[14] = 0;
    r[15] = 1;

    return r;
}
var _MJS_m4x4makeScale3 = F3(_MJS_m4x4makeScale3Local);

var _MJS_m4x4makeScale = function(v) {
    return _MJS_m4x4makeScale3Local(v[0], v[1], v[2]);
};

var _MJS_m4x4scale3 = F4(function(x, y, z, m) {
    var r = new Float64Array(16);

    r[0] = m[0] * x;
    r[1] = m[1] * x;
    r[2] = m[2] * x;
    r[3] = m[3] * x;
    r[4] = m[4] * y;
    r[5] = m[5] * y;
    r[6] = m[6] * y;
    r[7] = m[7] * y;
    r[8] = m[8] * z;
    r[9] = m[9] * z;
    r[10] = m[10] * z;
    r[11] = m[11] * z;
    r[12] = m[12];
    r[13] = m[13];
    r[14] = m[14];
    r[15] = m[15];

    return r;
});

var _MJS_m4x4scale = F2(function(v, m) {
    var r = new Float64Array(16);
    var x = v[0];
    var y = v[1];
    var z = v[2];

    r[0] = m[0] * x;
    r[1] = m[1] * x;
    r[2] = m[2] * x;
    r[3] = m[3] * x;
    r[4] = m[4] * y;
    r[5] = m[5] * y;
    r[6] = m[6] * y;
    r[7] = m[7] * y;
    r[8] = m[8] * z;
    r[9] = m[9] * z;
    r[10] = m[10] * z;
    r[11] = m[11] * z;
    r[12] = m[12];
    r[13] = m[13];
    r[14] = m[14];
    r[15] = m[15];

    return r;
});

function _MJS_m4x4makeTranslate3Local(x, y, z) {
    var r = new Float64Array(16);

    r[0] = 1;
    r[1] = 0;
    r[2] = 0;
    r[3] = 0;
    r[4] = 0;
    r[5] = 1;
    r[6] = 0;
    r[7] = 0;
    r[8] = 0;
    r[9] = 0;
    r[10] = 1;
    r[11] = 0;
    r[12] = x;
    r[13] = y;
    r[14] = z;
    r[15] = 1;

    return r;
}
var _MJS_m4x4makeTranslate3 = F3(_MJS_m4x4makeTranslate3Local);

var _MJS_m4x4makeTranslate = function(v) {
    return _MJS_m4x4makeTranslate3Local(v[0], v[1], v[2]);
};

var _MJS_m4x4translate3 = F4(function(x, y, z, m) {
    var r = new Float64Array(16);
    var m11 = m[0];
    var m21 = m[1];
    var m31 = m[2];
    var m41 = m[3];
    var m12 = m[4];
    var m22 = m[5];
    var m32 = m[6];
    var m42 = m[7];
    var m13 = m[8];
    var m23 = m[9];
    var m33 = m[10];
    var m43 = m[11];

    r[0] = m11;
    r[1] = m21;
    r[2] = m31;
    r[3] = m41;
    r[4] = m12;
    r[5] = m22;
    r[6] = m32;
    r[7] = m42;
    r[8] = m13;
    r[9] = m23;
    r[10] = m33;
    r[11] = m43;
    r[12] = m11 * x + m12 * y + m13 * z + m[12];
    r[13] = m21 * x + m22 * y + m23 * z + m[13];
    r[14] = m31 * x + m32 * y + m33 * z + m[14];
    r[15] = m41 * x + m42 * y + m43 * z + m[15];

    return r;
});

var _MJS_m4x4translate = F2(function(v, m) {
    var r = new Float64Array(16);
    var x = v[0];
    var y = v[1];
    var z = v[2];
    var m11 = m[0];
    var m21 = m[1];
    var m31 = m[2];
    var m41 = m[3];
    var m12 = m[4];
    var m22 = m[5];
    var m32 = m[6];
    var m42 = m[7];
    var m13 = m[8];
    var m23 = m[9];
    var m33 = m[10];
    var m43 = m[11];

    r[0] = m11;
    r[1] = m21;
    r[2] = m31;
    r[3] = m41;
    r[4] = m12;
    r[5] = m22;
    r[6] = m32;
    r[7] = m42;
    r[8] = m13;
    r[9] = m23;
    r[10] = m33;
    r[11] = m43;
    r[12] = m11 * x + m12 * y + m13 * z + m[12];
    r[13] = m21 * x + m22 * y + m23 * z + m[13];
    r[14] = m31 * x + m32 * y + m33 * z + m[14];
    r[15] = m41 * x + m42 * y + m43 * z + m[15];

    return r;
});

var _MJS_m4x4makeLookAt = F3(function(eye, center, up) {
    var z = _MJS_v3directionLocal(eye, center, _MJS_v3temp1Local);
    var x = _MJS_v3normalizeLocal(_MJS_v3crossLocal(up, z, _MJS_v3temp2Local), _MJS_v3temp2Local);
    var y = _MJS_v3normalizeLocal(_MJS_v3crossLocal(z, x, _MJS_v3temp3Local), _MJS_v3temp3Local);
    var tm1 = _MJS_m4x4temp1Local;
    var tm2 = _MJS_m4x4temp2Local;

    tm1[0] = x[0];
    tm1[1] = y[0];
    tm1[2] = z[0];
    tm1[3] = 0;
    tm1[4] = x[1];
    tm1[5] = y[1];
    tm1[6] = z[1];
    tm1[7] = 0;
    tm1[8] = x[2];
    tm1[9] = y[2];
    tm1[10] = z[2];
    tm1[11] = 0;
    tm1[12] = 0;
    tm1[13] = 0;
    tm1[14] = 0;
    tm1[15] = 1;

    tm2[0] = 1; tm2[1] = 0; tm2[2] = 0; tm2[3] = 0;
    tm2[4] = 0; tm2[5] = 1; tm2[6] = 0; tm2[7] = 0;
    tm2[8] = 0; tm2[9] = 0; tm2[10] = 1; tm2[11] = 0;
    tm2[12] = -eye[0]; tm2[13] = -eye[1]; tm2[14] = -eye[2]; tm2[15] = 1;

    return _MJS_m4x4mulLocal(tm1, tm2);
});


function _MJS_m4x4transposeLocal(m) {
    var r = new Float64Array(16);

    r[0] = m[0]; r[1] = m[4]; r[2] = m[8]; r[3] = m[12];
    r[4] = m[1]; r[5] = m[5]; r[6] = m[9]; r[7] = m[13];
    r[8] = m[2]; r[9] = m[6]; r[10] = m[10]; r[11] = m[14];
    r[12] = m[3]; r[13] = m[7]; r[14] = m[11]; r[15] = m[15];

    return r;
}
var _MJS_m4x4transpose = _MJS_m4x4transposeLocal;

var _MJS_m4x4makeBasis = F3(function(vx, vy, vz) {
    var r = new Float64Array(16);

    r[0] = vx[0];
    r[1] = vx[1];
    r[2] = vx[2];
    r[3] = 0;
    r[4] = vy[0];
    r[5] = vy[1];
    r[6] = vy[2];
    r[7] = 0;
    r[8] = vz[0];
    r[9] = vz[1];
    r[10] = vz[2];
    r[11] = 0;
    r[12] = 0;
    r[13] = 0;
    r[14] = 0;
    r[15] = 1;

    return r;
});



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}


var _Benchmark_getTimestamp =
  typeof performance !== "undefined"
    ? performance.now.bind(performance)
    : Date.now;

// sample : Int -> Operation -> Task Error Float
var _Benchmark_sample = F2(function(n, fn) {
  return _Scheduler_binding(function(callback) {
    var start = _Benchmark_getTimestamp();

    try {
      for (var i = 0; i < n; i++) {
        fn();
      }
    } catch (error) {
      if (error instanceof RangeError) {
        callback(_Scheduler_fail($elm_explorations$benchmark$Benchmark$LowLevel$StackOverflow));
      } else {
        callback(_Scheduler_fail($elm_explorations$benchmark$Benchmark$LowLevel$UnknownError(error.message)));
      }
      return;
    }

    var end = _Benchmark_getTimestamp();

    callback(_Scheduler_succeed(end - start));
  });
});

// operation : (() -> a) -> Operation
function _Benchmark_operation(thunk) {
  return thunk;
}



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return word
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.cI,
		impl.cY,
		impl.cW,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_dispatchEffects(managers, result.b, subscriptions(model));
	}

	_Platform_dispatchEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				p: bag.n,
				q: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.q)
		{
			x = temp.p(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		r: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		r: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		X: func(record.X),
		bs: record.bs,
		bl: record.bl
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.X;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.bs;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.bl) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.cI,
		impl.cY,
		impl.cW,
		function(sendToApp, initialModel) {
			var view = impl.c_;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.cI,
		impl.cY,
		impl.cW,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.bo && impl.bo(sendToApp)
			var view = impl.c_;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.cq);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.cX) && (_VirtualDom_doc.title = title = doc.cX);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.cQ;
	var onUrlRequest = impl.cR;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		bo: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.b6 === next.b6
							&& curr.bE === next.bE
							&& curr.b1.a === next.b1.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		cI: function(flags)
		{
			return A3(impl.cI, flags, _Browser_getUrl(), key);
		},
		c_: impl.c_,
		cY: impl.cY,
		cW: impl.cW
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { cF: 'hidden', ct: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { cF: 'mozHidden', ct: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { cF: 'msHidden', ct: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { cF: 'webkitHidden', ct: 'webkitvisibilitychange' }
		: { cF: 'hidden', ct: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		cb: _Browser_getScene(),
		cg: {
			ck: _Browser_window.pageXOffset,
			cl: _Browser_window.pageYOffset,
			ci: _Browser_doc.documentElement.clientWidth,
			bD: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		ci: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		bD: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			cb: {
				ci: node.scrollWidth,
				bD: node.scrollHeight
			},
			cg: {
				ck: node.scrollLeft,
				cl: node.scrollTop,
				ci: node.clientWidth,
				bD: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			cb: _Browser_getScene(),
			cg: {
				ck: x,
				cl: y,
				ci: _Browser_doc.documentElement.clientWidth,
				bD: _Browser_doc.documentElement.clientHeight
			},
			cA: {
				ck: x + rect.left,
				cl: y + rect.top,
				ci: rect.width,
				bD: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $author$project$AltMath$Alternative$ADT$Matrix4$Mat4 = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return function (k) {
											return function (l) {
												return function (m) {
													return function (n) {
														return function (o) {
															return function (p) {
																return {$: 0, a: a, b: b, c: c, d: d, e: e, f: f, g: g, h: h, i: i, j: j, k: k, l: l, m: m, n: n, o: o, p: p};
															};
														};
													};
												};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $author$project$AltMath$Alternative$ADT$Matrix4$fromRecord = function (_v0) {
	var m11 = _v0.bH;
	var m21 = _v0.bL;
	var m31 = _v0.bP;
	var m41 = _v0.bT;
	var m12 = _v0.bI;
	var m22 = _v0.bM;
	var m32 = _v0.bQ;
	var m42 = _v0.bU;
	var m13 = _v0.bJ;
	var m23 = _v0.bN;
	var m33 = _v0.bR;
	var m43 = _v0.bV;
	var m14 = _v0.bK;
	var m24 = _v0.bO;
	var m34 = _v0.bS;
	var m44 = _v0.bW;
	return $author$project$AltMath$Alternative$ADT$Matrix4$Mat4(m11)(m21)(m31)(m41)(m12)(m22)(m32)(m42)(m13)(m23)(m33)(m43)(m14)(m24)(m34)(m44);
};
var $author$project$AltMath$Alternative$Record$Matrix4$fromRecord = function (a) {
	return a;
};
var $author$project$AltMath$Alternative$Tuple$Matrix4$fromRecord = function (_v0) {
	var m11 = _v0.bH;
	var m21 = _v0.bL;
	var m31 = _v0.bP;
	var m41 = _v0.bT;
	var m12 = _v0.bI;
	var m22 = _v0.bM;
	var m32 = _v0.bQ;
	var m42 = _v0.bU;
	var m13 = _v0.bJ;
	var m23 = _v0.bN;
	var m33 = _v0.bR;
	var m43 = _v0.bV;
	var m14 = _v0.bK;
	var m24 = _v0.bO;
	var m34 = _v0.bS;
	var m44 = _v0.bW;
	return _Utils_Tuple2(
		_Utils_Tuple2(
			_Utils_Tuple2(
				_Utils_Tuple2(m11, m21),
				_Utils_Tuple2(m31, m41)),
			_Utils_Tuple2(
				_Utils_Tuple2(m12, m22),
				_Utils_Tuple2(m32, m42))),
		_Utils_Tuple2(
			_Utils_Tuple2(
				_Utils_Tuple2(m13, m23),
				_Utils_Tuple2(m33, m43)),
			_Utils_Tuple2(
				_Utils_Tuple2(m14, m24),
				_Utils_Tuple2(m34, m44))));
};
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm_explorations$linear_algebra$Math$Matrix4$fromRecord = _MJS_m4x4fromRecord;
var $author$project$Matrix4$AdtMat4 = function (a) {
	return {$: 1, a: a};
};
var $author$project$Matrix4$GLMat4 = function (a) {
	return {$: 0, a: a};
};
var $author$project$Matrix4$RecordMat4 = function (a) {
	return {$: 2, a: a};
};
var $author$project$Matrix4$TupleMat4 = function (a) {
	return {$: 3, a: a};
};
var $author$project$AltMath$Alternative$ADT$Matrix4$identity = $author$project$AltMath$Alternative$ADT$Matrix4$Mat4(1)(0)(0)(0)(0)(1)(0)(0)(0)(0)(1)(0)(0)(0)(0)(1);
var $author$project$AltMath$Alternative$Record$Matrix4$identity = {bH: 1, bI: 0, bJ: 0, bK: 0, bL: 0, bM: 1, bN: 0, bO: 0, bP: 0, bQ: 0, bR: 1, bS: 0, bT: 0, bU: 0, bV: 0, bW: 1};
var $author$project$AltMath$Alternative$Tuple$Matrix4$identity = _Utils_Tuple2(
	_Utils_Tuple2(
		_Utils_Tuple2(
			_Utils_Tuple2(1, 0),
			_Utils_Tuple2(0, 0)),
		_Utils_Tuple2(
			_Utils_Tuple2(0, 1),
			_Utils_Tuple2(0, 0))),
	_Utils_Tuple2(
		_Utils_Tuple2(
			_Utils_Tuple2(0, 0),
			_Utils_Tuple2(1, 0)),
		_Utils_Tuple2(
			_Utils_Tuple2(0, 0),
			_Utils_Tuple2(0, 1))));
var $elm_explorations$linear_algebra$Math$Matrix4$identity = _MJS_m4x4identity;
var $elm_explorations$benchmark$Benchmark$Status$Cold = {$: 0};
var $elm_explorations$benchmark$Benchmark$Benchmark$Series = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Basics$add = _Basics_add;
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm_explorations$benchmark$Benchmark$LowLevel$StackOverflow = {$: 0};
var $elm_explorations$benchmark$Benchmark$LowLevel$UnknownError = function (a) {
	return {$: 1, a: a};
};
var $elm_explorations$benchmark$Benchmark$LowLevel$operation = function (fn) {
	return _Benchmark_operation(fn);
};
var $elm_explorations$benchmark$Benchmark$scale = F2(
	function (name, series) {
		return A2(
			$elm_explorations$benchmark$Benchmark$Benchmark$Series,
			name,
			A2(
				$elm$core$List$map,
				function (_v0) {
					var subName = _v0.a;
					var fn = _v0.b;
					return _Utils_Tuple3(
						subName,
						$elm_explorations$benchmark$Benchmark$LowLevel$operation(fn),
						$elm_explorations$benchmark$Benchmark$Status$Cold);
				},
				series));
	});
var $author$project$Matrix4$identity = A2(
	$elm_explorations$benchmark$Benchmark$scale,
	'identity',
	_List_fromArray(
		[
			_Utils_Tuple2(
			'Linear Algebra',
			function (_v0) {
				return $author$project$Matrix4$GLMat4($elm_explorations$linear_algebra$Math$Matrix4$identity);
			}),
			_Utils_Tuple2(
			'ADT',
			function (_v1) {
				return $author$project$Matrix4$AdtMat4($author$project$AltMath$Alternative$ADT$Matrix4$identity);
			}),
			_Utils_Tuple2(
			'Record',
			function (_v2) {
				return $author$project$Matrix4$RecordMat4($author$project$AltMath$Alternative$Record$Matrix4$identity);
			}),
			_Utils_Tuple2(
			'Tuple',
			function (_v3) {
				return $author$project$Matrix4$TupleMat4($author$project$AltMath$Alternative$Tuple$Matrix4$identity);
			})
		]));
var $author$project$Matrix4$MaybeAdtMat4 = function (a) {
	return {$: 1, a: a};
};
var $author$project$Matrix4$MaybeGLMat4 = function (a) {
	return {$: 0, a: a};
};
var $author$project$Matrix4$MaybeRecordMat4 = function (a) {
	return {$: 2, a: a};
};
var $author$project$Matrix4$MaybeTupleMat4 = function (a) {
	return {$: 3, a: a};
};
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Basics$sub = _Basics_sub;
var $author$project$AltMath$Alternative$ADT$Matrix4$inverse = function (_v0) {
	var m11 = _v0.a;
	var m21 = _v0.b;
	var m31 = _v0.c;
	var m41 = _v0.d;
	var m12 = _v0.e;
	var m22 = _v0.f;
	var m32 = _v0.g;
	var m42 = _v0.h;
	var m13 = _v0.i;
	var m23 = _v0.j;
	var m33 = _v0.k;
	var m43 = _v0.l;
	var m14 = _v0.m;
	var m24 = _v0.n;
	var m34 = _v0.o;
	var m44 = _v0.p;
	var r44 = ((((((m11 * m22) * m33) - ((m11 * m32) * m23)) - ((m12 * m21) * m33)) + ((m12 * m31) * m23)) + ((m13 * m21) * m32)) - ((m13 * m31) * m22);
	var r43 = (((((((-m11) * m22) * m43) + ((m11 * m42) * m23)) + ((m12 * m21) * m43)) - ((m12 * m41) * m23)) - ((m13 * m21) * m42)) + ((m13 * m41) * m22);
	var r42 = ((((((m11 * m32) * m43) - ((m11 * m42) * m33)) - ((m12 * m31) * m43)) + ((m12 * m41) * m33)) + ((m13 * m31) * m42)) - ((m13 * m41) * m32);
	var r41 = (((((((-m21) * m32) * m43) + ((m21 * m42) * m33)) + ((m22 * m31) * m43)) - ((m22 * m41) * m33)) - ((m23 * m31) * m42)) + ((m23 * m41) * m32);
	var r34 = (((((((-m11) * m22) * m34) + ((m11 * m32) * m24)) + ((m12 * m21) * m34)) - ((m12 * m31) * m24)) - ((m14 * m21) * m32)) + ((m14 * m31) * m22);
	var r33 = ((((((m11 * m22) * m44) - ((m11 * m42) * m24)) - ((m12 * m21) * m44)) + ((m12 * m41) * m24)) + ((m14 * m21) * m42)) - ((m14 * m41) * m22);
	var r32 = (((((((-m11) * m32) * m44) + ((m11 * m42) * m34)) + ((m12 * m31) * m44)) - ((m12 * m41) * m34)) - ((m14 * m31) * m42)) + ((m14 * m41) * m32);
	var r31 = ((((((m21 * m32) * m44) - ((m21 * m42) * m34)) - ((m22 * m31) * m44)) + ((m22 * m41) * m34)) + ((m24 * m31) * m42)) - ((m24 * m41) * m32);
	var r24 = ((((((m11 * m23) * m34) - ((m11 * m33) * m24)) - ((m13 * m21) * m34)) + ((m13 * m31) * m24)) + ((m14 * m21) * m33)) - ((m14 * m31) * m23);
	var r23 = (((((((-m11) * m23) * m44) + ((m11 * m43) * m24)) + ((m13 * m21) * m44)) - ((m13 * m41) * m24)) - ((m14 * m21) * m43)) + ((m14 * m41) * m23);
	var r22 = ((((((m11 * m33) * m44) - ((m11 * m43) * m34)) - ((m13 * m31) * m44)) + ((m13 * m41) * m34)) + ((m14 * m31) * m43)) - ((m14 * m41) * m33);
	var r21 = (((((((-m21) * m33) * m44) + ((m21 * m43) * m34)) + ((m23 * m31) * m44)) - ((m23 * m41) * m34)) - ((m24 * m31) * m43)) + ((m24 * m41) * m33);
	var r14 = (((((((-m12) * m23) * m34) + ((m12 * m33) * m24)) + ((m13 * m22) * m34)) - ((m13 * m32) * m24)) - ((m14 * m22) * m33)) + ((m14 * m32) * m23);
	var r13 = ((((((m12 * m23) * m44) - ((m12 * m43) * m24)) - ((m13 * m22) * m44)) + ((m13 * m42) * m24)) + ((m14 * m22) * m43)) - ((m14 * m42) * m23);
	var r12 = (((((((-m12) * m33) * m44) + ((m12 * m43) * m34)) + ((m13 * m32) * m44)) - ((m13 * m42) * m34)) - ((m14 * m32) * m43)) + ((m14 * m42) * m33);
	var r11 = ((((((m22 * m33) * m44) - ((m22 * m43) * m34)) - ((m23 * m32) * m44)) + ((m23 * m42) * m34)) + ((m24 * m32) * m43)) - ((m24 * m42) * m33);
	var det = (((m11 * r11) + (m21 * r12)) + (m31 * r13)) + (m41 * r14);
	var idet = 1 / det;
	return (!det) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
		$author$project$AltMath$Alternative$ADT$Matrix4$Mat4(r11 * idet)(r21 * idet)(r31 * idet)(r41 * idet)(r12 * idet)(r22 * idet)(r32 * idet)(r42 * idet)(r13 * idet)(r23 * idet)(r33 * idet)(r43 * idet)(r14 * idet)(r24 * idet)(r34 * idet)(r44 * idet));
};
var $author$project$AltMath$Alternative$Record$Matrix4$inverse = function (m) {
	var r44 = ((((((m.bH * m.bM) * m.bR) - ((m.bH * m.bQ) * m.bN)) - ((m.bI * m.bL) * m.bR)) + ((m.bI * m.bP) * m.bN)) + ((m.bJ * m.bL) * m.bQ)) - ((m.bJ * m.bP) * m.bM);
	var r43 = (((((((-m.bH) * m.bM) * m.bV) + ((m.bH * m.bU) * m.bN)) + ((m.bI * m.bL) * m.bV)) - ((m.bI * m.bT) * m.bN)) - ((m.bJ * m.bL) * m.bU)) + ((m.bJ * m.bT) * m.bM);
	var r42 = ((((((m.bH * m.bQ) * m.bV) - ((m.bH * m.bU) * m.bR)) - ((m.bI * m.bP) * m.bV)) + ((m.bI * m.bT) * m.bR)) + ((m.bJ * m.bP) * m.bU)) - ((m.bJ * m.bT) * m.bQ);
	var r41 = (((((((-m.bL) * m.bQ) * m.bV) + ((m.bL * m.bU) * m.bR)) + ((m.bM * m.bP) * m.bV)) - ((m.bM * m.bT) * m.bR)) - ((m.bN * m.bP) * m.bU)) + ((m.bN * m.bT) * m.bQ);
	var r34 = (((((((-m.bH) * m.bM) * m.bS) + ((m.bH * m.bQ) * m.bO)) + ((m.bI * m.bL) * m.bS)) - ((m.bI * m.bP) * m.bO)) - ((m.bK * m.bL) * m.bQ)) + ((m.bK * m.bP) * m.bM);
	var r33 = ((((((m.bH * m.bM) * m.bW) - ((m.bH * m.bU) * m.bO)) - ((m.bI * m.bL) * m.bW)) + ((m.bI * m.bT) * m.bO)) + ((m.bK * m.bL) * m.bU)) - ((m.bK * m.bT) * m.bM);
	var r32 = (((((((-m.bH) * m.bQ) * m.bW) + ((m.bH * m.bU) * m.bS)) + ((m.bI * m.bP) * m.bW)) - ((m.bI * m.bT) * m.bS)) - ((m.bK * m.bP) * m.bU)) + ((m.bK * m.bT) * m.bQ);
	var r31 = ((((((m.bL * m.bQ) * m.bW) - ((m.bL * m.bU) * m.bS)) - ((m.bM * m.bP) * m.bW)) + ((m.bM * m.bT) * m.bS)) + ((m.bO * m.bP) * m.bU)) - ((m.bO * m.bT) * m.bQ);
	var r24 = ((((((m.bH * m.bN) * m.bS) - ((m.bH * m.bR) * m.bO)) - ((m.bJ * m.bL) * m.bS)) + ((m.bJ * m.bP) * m.bO)) + ((m.bK * m.bL) * m.bR)) - ((m.bK * m.bP) * m.bN);
	var r23 = (((((((-m.bH) * m.bN) * m.bW) + ((m.bH * m.bV) * m.bO)) + ((m.bJ * m.bL) * m.bW)) - ((m.bJ * m.bT) * m.bO)) - ((m.bK * m.bL) * m.bV)) + ((m.bK * m.bT) * m.bN);
	var r22 = ((((((m.bH * m.bR) * m.bW) - ((m.bH * m.bV) * m.bS)) - ((m.bJ * m.bP) * m.bW)) + ((m.bJ * m.bT) * m.bS)) + ((m.bK * m.bP) * m.bV)) - ((m.bK * m.bT) * m.bR);
	var r21 = (((((((-m.bL) * m.bR) * m.bW) + ((m.bL * m.bV) * m.bS)) + ((m.bN * m.bP) * m.bW)) - ((m.bN * m.bT) * m.bS)) - ((m.bO * m.bP) * m.bV)) + ((m.bO * m.bT) * m.bR);
	var r14 = (((((((-m.bI) * m.bN) * m.bS) + ((m.bI * m.bR) * m.bO)) + ((m.bJ * m.bM) * m.bS)) - ((m.bJ * m.bQ) * m.bO)) - ((m.bK * m.bM) * m.bR)) + ((m.bK * m.bQ) * m.bN);
	var r13 = ((((((m.bI * m.bN) * m.bW) - ((m.bI * m.bV) * m.bO)) - ((m.bJ * m.bM) * m.bW)) + ((m.bJ * m.bU) * m.bO)) + ((m.bK * m.bM) * m.bV)) - ((m.bK * m.bU) * m.bN);
	var r12 = (((((((-m.bI) * m.bR) * m.bW) + ((m.bI * m.bV) * m.bS)) + ((m.bJ * m.bQ) * m.bW)) - ((m.bJ * m.bU) * m.bS)) - ((m.bK * m.bQ) * m.bV)) + ((m.bK * m.bU) * m.bR);
	var r11 = ((((((m.bM * m.bR) * m.bW) - ((m.bM * m.bV) * m.bS)) - ((m.bN * m.bQ) * m.bW)) + ((m.bN * m.bU) * m.bS)) + ((m.bO * m.bQ) * m.bV)) - ((m.bO * m.bU) * m.bR);
	var det = (((m.bH * r11) + (m.bL * r12)) + (m.bP * r13)) + (m.bT * r14);
	var idet = 1 / det;
	return (!det) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
		{bH: r11 * idet, bI: r12 * idet, bJ: r13 * idet, bK: r14 * idet, bL: r21 * idet, bM: r22 * idet, bN: r23 * idet, bO: r24 * idet, bP: r31 * idet, bQ: r32 * idet, bR: r33 * idet, bS: r34 * idet, bT: r41 * idet, bU: r42 * idet, bV: r43 * idet, bW: r44 * idet});
};
var $author$project$AltMath$Alternative$Tuple$Matrix4$inverse = function (_v0) {
	var _v1 = _v0.a;
	var _v2 = _v1.a;
	var _v3 = _v2.a;
	var m11 = _v3.a;
	var m21 = _v3.b;
	var _v4 = _v2.b;
	var m31 = _v4.a;
	var m41 = _v4.b;
	var _v5 = _v1.b;
	var _v6 = _v5.a;
	var m12 = _v6.a;
	var m22 = _v6.b;
	var _v7 = _v5.b;
	var m32 = _v7.a;
	var m42 = _v7.b;
	var _v8 = _v0.b;
	var _v9 = _v8.a;
	var _v10 = _v9.a;
	var m13 = _v10.a;
	var m23 = _v10.b;
	var _v11 = _v9.b;
	var m33 = _v11.a;
	var m43 = _v11.b;
	var _v12 = _v8.b;
	var _v13 = _v12.a;
	var m14 = _v13.a;
	var m24 = _v13.b;
	var _v14 = _v12.b;
	var m34 = _v14.a;
	var m44 = _v14.b;
	var r44 = ((((((m11 * m22) * m33) - ((m11 * m32) * m23)) - ((m12 * m21) * m33)) + ((m12 * m31) * m23)) + ((m13 * m21) * m32)) - ((m13 * m31) * m22);
	var r43 = (((((((-m11) * m22) * m43) + ((m11 * m42) * m23)) + ((m12 * m21) * m43)) - ((m12 * m41) * m23)) - ((m13 * m21) * m42)) + ((m13 * m41) * m22);
	var r42 = ((((((m11 * m32) * m43) - ((m11 * m42) * m33)) - ((m12 * m31) * m43)) + ((m12 * m41) * m33)) + ((m13 * m31) * m42)) - ((m13 * m41) * m32);
	var r41 = (((((((-m21) * m32) * m43) + ((m21 * m42) * m33)) + ((m22 * m31) * m43)) - ((m22 * m41) * m33)) - ((m23 * m31) * m42)) + ((m23 * m41) * m32);
	var r34 = (((((((-m11) * m22) * m34) + ((m11 * m32) * m24)) + ((m12 * m21) * m34)) - ((m12 * m31) * m24)) - ((m14 * m21) * m32)) + ((m14 * m31) * m22);
	var r33 = ((((((m11 * m22) * m44) - ((m11 * m42) * m24)) - ((m12 * m21) * m44)) + ((m12 * m41) * m24)) + ((m14 * m21) * m42)) - ((m14 * m41) * m22);
	var r32 = (((((((-m11) * m32) * m44) + ((m11 * m42) * m34)) + ((m12 * m31) * m44)) - ((m12 * m41) * m34)) - ((m14 * m31) * m42)) + ((m14 * m41) * m32);
	var r31 = ((((((m21 * m32) * m44) - ((m21 * m42) * m34)) - ((m22 * m31) * m44)) + ((m22 * m41) * m34)) + ((m24 * m31) * m42)) - ((m24 * m41) * m32);
	var r24 = ((((((m11 * m23) * m34) - ((m11 * m33) * m24)) - ((m13 * m21) * m34)) + ((m13 * m31) * m24)) + ((m14 * m21) * m33)) - ((m14 * m31) * m23);
	var r23 = (((((((-m11) * m23) * m44) + ((m11 * m43) * m24)) + ((m13 * m21) * m44)) - ((m13 * m41) * m24)) - ((m14 * m21) * m43)) + ((m14 * m41) * m23);
	var r22 = ((((((m11 * m33) * m44) - ((m11 * m43) * m34)) - ((m13 * m31) * m44)) + ((m13 * m41) * m34)) + ((m14 * m31) * m43)) - ((m14 * m41) * m33);
	var r21 = (((((((-m21) * m33) * m44) + ((m21 * m43) * m34)) + ((m23 * m31) * m44)) - ((m23 * m41) * m34)) - ((m24 * m31) * m43)) + ((m24 * m41) * m33);
	var r14 = (((((((-m12) * m23) * m34) + ((m12 * m33) * m24)) + ((m13 * m22) * m34)) - ((m13 * m32) * m24)) - ((m14 * m22) * m33)) + ((m14 * m32) * m23);
	var r13 = ((((((m12 * m23) * m44) - ((m12 * m43) * m24)) - ((m13 * m22) * m44)) + ((m13 * m42) * m24)) + ((m14 * m22) * m43)) - ((m14 * m42) * m23);
	var r12 = (((((((-m12) * m33) * m44) + ((m12 * m43) * m34)) + ((m13 * m32) * m44)) - ((m13 * m42) * m34)) - ((m14 * m32) * m43)) + ((m14 * m42) * m33);
	var r11 = ((((((m22 * m33) * m44) - ((m22 * m43) * m34)) - ((m23 * m32) * m44)) + ((m23 * m42) * m34)) + ((m24 * m32) * m43)) - ((m24 * m42) * m33);
	var det = (((m11 * r11) + (m21 * r12)) + (m31 * r13)) + (m41 * r14);
	var idet = 1 / det;
	return (!det) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
		_Utils_Tuple2(
			_Utils_Tuple2(
				_Utils_Tuple2(
					_Utils_Tuple2(r11 * idet, r21 * idet),
					_Utils_Tuple2(r31 * idet, r41 * idet)),
				_Utils_Tuple2(
					_Utils_Tuple2(r12 * idet, r22 * idet),
					_Utils_Tuple2(r32 * idet, r42 * idet))),
			_Utils_Tuple2(
				_Utils_Tuple2(
					_Utils_Tuple2(r13 * idet, r23 * idet),
					_Utils_Tuple2(r33 * idet, r43 * idet)),
				_Utils_Tuple2(
					_Utils_Tuple2(r14 * idet, r24 * idet),
					_Utils_Tuple2(r34 * idet, r44 * idet)))));
};
var $elm_explorations$linear_algebra$Math$Matrix4$inverse = _MJS_m4x4inverse;
var $author$project$Matrix4$inverse = function (_v0) {
	var glMat1 = _v0.D;
	var adtMat1 = _v0.y;
	var recMat1 = _v0.F;
	var tupleMat1 = _v0.G;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'inverse',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Matrix4$MaybeGLMat4(
						$elm_explorations$linear_algebra$Math$Matrix4$inverse(glMat1));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Matrix4$MaybeAdtMat4(
						$author$project$AltMath$Alternative$ADT$Matrix4$inverse(adtMat1));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Matrix4$MaybeRecordMat4(
						$author$project$AltMath$Alternative$Record$Matrix4$inverse(recMat1));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Matrix4$MaybeTupleMat4(
						$author$project$AltMath$Alternative$Tuple$Matrix4$inverse(tupleMat1));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Matrix4$inverseOrthonormal = function (_v0) {
	var m11 = _v0.a;
	var m21 = _v0.b;
	var m31 = _v0.c;
	var m41 = _v0.d;
	var m12 = _v0.e;
	var m22 = _v0.f;
	var m32 = _v0.g;
	var m42 = _v0.h;
	var m13 = _v0.i;
	var m23 = _v0.j;
	var m33 = _v0.k;
	var m43 = _v0.l;
	var m14 = _v0.m;
	var m24 = _v0.n;
	var m34 = _v0.o;
	var m44 = _v0.p;
	return $author$project$AltMath$Alternative$ADT$Matrix4$Mat4(m11)(m12)(m13)(0)(m21)(m22)(m23)(0)(m31)(m32)(m33)(0)(-(((m11 * m14) + (m21 * m24)) + (m31 * m34)))(-(((m12 * m14) + (m22 * m24)) + (m32 * m34)))(-(((m13 * m14) + (m23 * m24)) + (m33 * m34)))(m44);
};
var $author$project$AltMath$Alternative$Record$Matrix4$inverseOrthonormal = function (m) {
	return {bH: m.bH, bI: m.bL, bJ: m.bP, bK: -(((m.bH * m.bK) + (m.bL * m.bO)) + (m.bP * m.bS)), bL: m.bI, bM: m.bM, bN: m.bQ, bO: -(((m.bI * m.bK) + (m.bM * m.bO)) + (m.bQ * m.bS)), bP: m.bJ, bQ: m.bN, bR: m.bR, bS: -(((m.bJ * m.bK) + (m.bN * m.bO)) + (m.bR * m.bS)), bT: 0, bU: 0, bV: 0, bW: m.bW};
};
var $author$project$AltMath$Alternative$Tuple$Matrix4$inverseOrthonormal = function (_v0) {
	var _v1 = _v0.a;
	var _v2 = _v1.a;
	var _v3 = _v2.a;
	var m11 = _v3.a;
	var m21 = _v3.b;
	var _v4 = _v2.b;
	var m31 = _v4.a;
	var m41 = _v4.b;
	var _v5 = _v1.b;
	var _v6 = _v5.a;
	var m12 = _v6.a;
	var m22 = _v6.b;
	var _v7 = _v5.b;
	var m32 = _v7.a;
	var m42 = _v7.b;
	var _v8 = _v0.b;
	var _v9 = _v8.a;
	var _v10 = _v9.a;
	var m13 = _v10.a;
	var m23 = _v10.b;
	var _v11 = _v9.b;
	var m33 = _v11.a;
	var m43 = _v11.b;
	var _v12 = _v8.b;
	var _v13 = _v12.a;
	var m14 = _v13.a;
	var m24 = _v13.b;
	var _v14 = _v12.b;
	var m34 = _v14.a;
	var m44 = _v14.b;
	return $author$project$AltMath$Alternative$Tuple$Matrix4$fromRecord(
		{bH: m11, bI: m21, bJ: m31, bK: -(((m11 * m14) + (m21 * m24)) + (m31 * m34)), bL: m12, bM: m22, bN: m32, bO: -(((m12 * m14) + (m22 * m24)) + (m32 * m34)), bP: m13, bQ: m23, bR: m33, bS: -(((m13 * m14) + (m23 * m24)) + (m33 * m34)), bT: 0, bU: 0, bV: 0, bW: m44});
};
var $elm_explorations$linear_algebra$Math$Matrix4$inverseOrthonormal = _MJS_m4x4inverseOrthonormal;
var $author$project$Matrix4$inverseOrthonormal = function (_v0) {
	var glMat1 = _v0.D;
	var adtMat1 = _v0.y;
	var recMat1 = _v0.F;
	var tupleMat1 = _v0.G;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'inverseOrthonormal',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Matrix4$GLMat4(
						$elm_explorations$linear_algebra$Math$Matrix4$inverseOrthonormal(glMat1));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Matrix4$AdtMat4(
						$author$project$AltMath$Alternative$ADT$Matrix4$inverseOrthonormal(adtMat1));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Matrix4$RecordMat4(
						$author$project$AltMath$Alternative$Record$Matrix4$inverseOrthonormal(recMat1));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Matrix4$TupleMat4(
						$author$project$AltMath$Alternative$Tuple$Matrix4$inverseOrthonormal(tupleMat1));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Matrix4$makeBasis = F3(
	function (_v0, _v1, _v2) {
		var xx = _v0.a;
		var xy = _v0.b;
		var xz = _v0.c;
		var yx = _v1.a;
		var yy = _v1.b;
		var yz = _v1.c;
		var zx = _v2.a;
		var zy = _v2.b;
		var zz = _v2.c;
		return $author$project$AltMath$Alternative$ADT$Matrix4$Mat4(xx)(xy)(xz)(0)(yx)(yy)(yz)(0)(zx)(zy)(zz)(0)(0)(0)(0)(1);
	});
var $author$project$AltMath$Alternative$Record$Matrix4$makeBasis = F3(
	function (vx, vy, vz) {
		return {bH: vx.ck, bI: vy.ck, bJ: vz.ck, bK: 0, bL: vx.cl, bM: vy.cl, bN: vz.cl, bO: 0, bP: vx.cn, bQ: vy.cn, bR: vz.cn, bS: 0, bT: 0, bU: 0, bV: 0, bW: 1};
	});
var $author$project$AltMath$Alternative$Tuple$Matrix4$makeBasis = F3(
	function (_v0, _v1, _v2) {
		var v1x = _v0.a;
		var v1y = _v0.b;
		var v1z = _v0.c;
		var v2x = _v1.a;
		var v2y = _v1.b;
		var v2z = _v1.c;
		var v3x = _v2.a;
		var v3y = _v2.b;
		var v3z = _v2.c;
		return $author$project$AltMath$Alternative$Tuple$Matrix4$fromRecord(
			{bH: v1x, bI: v2x, bJ: v3x, bK: 0, bL: v1y, bM: v2y, bN: v3y, bO: 0, bP: v1z, bQ: v2z, bR: v3z, bS: 0, bT: 0, bU: 0, bV: 0, bW: 1});
	});
var $elm_explorations$linear_algebra$Math$Matrix4$makeBasis = _MJS_m4x4makeBasis;
var $author$project$Matrix4$makeBasis = function (_v0) {
	var glVec1 = _v0.K;
	var glVec2 = _v0.bg;
	var glVec3 = _v0.bh;
	var adtVec1 = _v0.J;
	var adtVec2 = _v0.bc;
	var adtVec3 = _v0.bd;
	var recVec1 = _v0.N;
	var recVec2 = _v0.bm;
	var recVec3 = _v0.bn;
	var tupleVec1 = _v0.O;
	var tupleVec2 = _v0.bt;
	var tupleVec3 = _v0.bu;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'makeBasis',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Matrix4$GLMat4(
						A3($elm_explorations$linear_algebra$Math$Matrix4$makeBasis, glVec1, glVec2, glVec3));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Matrix4$AdtMat4(
						A3($author$project$AltMath$Alternative$ADT$Matrix4$makeBasis, adtVec1, adtVec2, adtVec3));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Matrix4$RecordMat4(
						A3($author$project$AltMath$Alternative$Record$Matrix4$makeBasis, recVec1, recVec2, recVec3));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Matrix4$TupleMat4(
						A3($author$project$AltMath$Alternative$Tuple$Matrix4$makeBasis, tupleVec1, tupleVec2, tupleVec3));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Matrix4$makeFrustum = F6(
	function (left, right, bottom, top, znear, zfar) {
		return $author$project$AltMath$Alternative$ADT$Matrix4$Mat4((2 * znear) / (right - left))(0)(0)(0)(0)((2 * znear) / (top - bottom))(0)(0)((right + left) / (right - left))((top + bottom) / (top - bottom))((-(zfar + znear)) / (zfar - znear))(-1)(0)(0)((((-2) * zfar) * znear) / (zfar - znear))(0);
	});
var $author$project$AltMath$Alternative$Record$Matrix4$makeFrustum = F6(
	function (left, right, bottom, top, znear, zfar) {
		return {bH: (2 * znear) / (right - left), bI: 0, bJ: (right + left) / (right - left), bK: 0, bL: 0, bM: (2 * znear) / (top - bottom), bN: (top + bottom) / (top - bottom), bO: 0, bP: 0, bQ: 0, bR: (-(zfar + znear)) / (zfar - znear), bS: (((-2) * zfar) * znear) / (zfar - znear), bT: 0, bU: 0, bV: -1, bW: 0};
	});
var $author$project$AltMath$Alternative$Tuple$Matrix4$makeFrustum = F6(
	function (left, right, bottom, top, znear, zfar) {
		return $author$project$AltMath$Alternative$Tuple$Matrix4$fromRecord(
			{bH: (2 * znear) / (right - left), bI: 0, bJ: (right + left) / (right - left), bK: 0, bL: 0, bM: (2 * znear) / (top - bottom), bN: (top + bottom) / (top - bottom), bO: 0, bP: 0, bQ: 0, bR: (-(zfar + znear)) / (zfar - znear), bS: (((-2) * zfar) * znear) / (zfar - znear), bT: 0, bU: 0, bV: -1, bW: 0});
	});
var $elm_explorations$linear_algebra$Math$Matrix4$makeFrustum = _MJS_m4x4makeFrustum;
var $author$project$Matrix4$makeFrustum = F6(
	function (left, right, bottom, top, znear, zfar) {
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'makeFrustum',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v0) {
						return $author$project$Matrix4$GLMat4(
							A6($elm_explorations$linear_algebra$Math$Matrix4$makeFrustum, left, right, bottom, top, znear, zfar));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v1) {
						return $author$project$Matrix4$AdtMat4(
							A6($author$project$AltMath$Alternative$ADT$Matrix4$makeFrustum, left, right, bottom, top, znear, zfar));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v2) {
						return $author$project$Matrix4$RecordMat4(
							A6($author$project$AltMath$Alternative$Record$Matrix4$makeFrustum, left, right, bottom, top, znear, zfar));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v3) {
						return $author$project$Matrix4$TupleMat4(
							A6($author$project$AltMath$Alternative$Tuple$Matrix4$makeFrustum, left, right, bottom, top, znear, zfar));
					})
				]));
	});
var $author$project$AltMath$Alternative$ADT$Vector3$Vec3 = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $author$project$AltMath$Alternative$ADT$Vector3$cross = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var az = _v0.c;
		var bx = _v1.a;
		var by = _v1.b;
		var bz = _v1.c;
		return A3($author$project$AltMath$Alternative$ADT$Vector3$Vec3, (ay * bz) - (az * by), (az * bx) - (ax * bz), (ax * by) - (ay * bx));
	});
var $elm$core$Basics$sqrt = _Basics_sqrt;
var $author$project$AltMath$Alternative$ADT$Vector3$length = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	var z = _v0.c;
	return $elm$core$Basics$sqrt(((x * x) + (y * y)) + (z * z));
};
var $author$project$AltMath$Alternative$ADT$Vector3$sub = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var az = _v0.c;
		var bx = _v1.a;
		var by = _v1.b;
		var bz = _v1.c;
		return A3($author$project$AltMath$Alternative$ADT$Vector3$Vec3, ax - bx, ay - by, az - bz);
	});
var $author$project$AltMath$Alternative$ADT$Vector3$direction = F2(
	function (a, b) {
		var c = A2($author$project$AltMath$Alternative$ADT$Vector3$sub, a, b);
		var x = c.a;
		var y = c.b;
		var z = c.c;
		var len = $author$project$AltMath$Alternative$ADT$Vector3$length(c);
		return A3($author$project$AltMath$Alternative$ADT$Vector3$Vec3, x / len, y / len, z / len);
	});
var $author$project$AltMath$Alternative$ADT$Matrix4$mul = F2(
	function (_v0, _v1) {
		var am11 = _v0.a;
		var am21 = _v0.b;
		var am31 = _v0.c;
		var am41 = _v0.d;
		var am12 = _v0.e;
		var am22 = _v0.f;
		var am32 = _v0.g;
		var am42 = _v0.h;
		var am13 = _v0.i;
		var am23 = _v0.j;
		var am33 = _v0.k;
		var am43 = _v0.l;
		var am14 = _v0.m;
		var am24 = _v0.n;
		var am34 = _v0.o;
		var am44 = _v0.p;
		var bm11 = _v1.a;
		var bm21 = _v1.b;
		var bm31 = _v1.c;
		var bm41 = _v1.d;
		var bm12 = _v1.e;
		var bm22 = _v1.f;
		var bm32 = _v1.g;
		var bm42 = _v1.h;
		var bm13 = _v1.i;
		var bm23 = _v1.j;
		var bm33 = _v1.k;
		var bm43 = _v1.l;
		var bm14 = _v1.m;
		var bm24 = _v1.n;
		var bm34 = _v1.o;
		var bm44 = _v1.p;
		return $author$project$AltMath$Alternative$ADT$Matrix4$Mat4((((am11 * bm11) + (am12 * bm21)) + (am13 * bm31)) + (am14 * bm41))((((am21 * bm11) + (am22 * bm21)) + (am23 * bm31)) + (am24 * bm41))((((am31 * bm11) + (am32 * bm21)) + (am33 * bm31)) + (am34 * bm41))((((am41 * bm11) + (am42 * bm21)) + (am43 * bm31)) + (am44 * bm41))((((am11 * bm12) + (am12 * bm22)) + (am13 * bm32)) + (am14 * bm42))((((am21 * bm12) + (am22 * bm22)) + (am23 * bm32)) + (am24 * bm42))((((am31 * bm12) + (am32 * bm22)) + (am33 * bm32)) + (am34 * bm42))((((am41 * bm12) + (am42 * bm22)) + (am43 * bm32)) + (am44 * bm42))((((am11 * bm13) + (am12 * bm23)) + (am13 * bm33)) + (am14 * bm43))((((am21 * bm13) + (am22 * bm23)) + (am23 * bm33)) + (am24 * bm43))((((am31 * bm13) + (am32 * bm23)) + (am33 * bm33)) + (am34 * bm43))((((am41 * bm13) + (am42 * bm23)) + (am43 * bm33)) + (am44 * bm43))((((am11 * bm14) + (am12 * bm24)) + (am13 * bm34)) + (am14 * bm44))((((am21 * bm14) + (am22 * bm24)) + (am23 * bm34)) + (am24 * bm44))((((am31 * bm14) + (am32 * bm24)) + (am33 * bm34)) + (am34 * bm44))((((am41 * bm14) + (am42 * bm24)) + (am43 * bm34)) + (am44 * bm44));
	});
var $author$project$AltMath$Alternative$ADT$Vector3$normalize = function (v3) {
	var x = v3.a;
	var y = v3.b;
	var z = v3.c;
	var len = $author$project$AltMath$Alternative$ADT$Vector3$length(v3);
	return A3($author$project$AltMath$Alternative$ADT$Vector3$Vec3, x / len, y / len, z / len);
};
var $author$project$AltMath$Alternative$ADT$Matrix4$makeLookAt = F3(
	function (eye, center, up) {
		var eyex = eye.a;
		var eyey = eye.b;
		var eyez = eye.c;
		var z = A2($author$project$AltMath$Alternative$ADT$Vector3$direction, eye, center);
		var zx = z.a;
		var zy = z.b;
		var zz = z.c;
		var x = $author$project$AltMath$Alternative$ADT$Vector3$normalize(
			A2($author$project$AltMath$Alternative$ADT$Vector3$cross, up, z));
		var xx = x.a;
		var xy = x.b;
		var xz = x.c;
		var y = $author$project$AltMath$Alternative$ADT$Vector3$normalize(
			A2($author$project$AltMath$Alternative$ADT$Vector3$cross, z, x));
		var yx = y.a;
		var yy = y.b;
		var yz = y.c;
		return A2(
			$author$project$AltMath$Alternative$ADT$Matrix4$mul,
			$author$project$AltMath$Alternative$ADT$Matrix4$Mat4(xx)(yx)(zx)(0)(xy)(yy)(zy)(0)(xz)(yz)(zz)(0)(0)(0)(0)(1),
			$author$project$AltMath$Alternative$ADT$Matrix4$Mat4(1)(0)(0)(0)(0)(1)(0)(0)(0)(0)(1)(0)(-eyex)(-eyey)(-eyez)(1));
	});
var $author$project$AltMath$Alternative$Record$Vector3$Vec3 = F3(
	function (x, y, z) {
		return {ck: x, cl: y, cn: z};
	});
var $author$project$AltMath$Alternative$Record$Vector3$cross = F2(
	function (a, b) {
		return A3($author$project$AltMath$Alternative$Record$Vector3$Vec3, (a.cl * b.cn) - (a.cn * b.cl), (a.cn * b.ck) - (a.ck * b.cn), (a.ck * b.cl) - (a.cl * b.ck));
	});
var $author$project$AltMath$Alternative$Record$Vector3$length = function (_v0) {
	var x = _v0.ck;
	var y = _v0.cl;
	var z = _v0.cn;
	return $elm$core$Basics$sqrt(((x * x) + (y * y)) + (z * z));
};
var $author$project$AltMath$Alternative$Record$Vector3$sub = F2(
	function (a, b) {
		return A3($author$project$AltMath$Alternative$Record$Vector3$Vec3, a.ck - b.ck, a.cl - b.cl, a.cn - b.cn);
	});
var $author$project$AltMath$Alternative$Record$Vector3$direction = F2(
	function (a, b) {
		var c = A2($author$project$AltMath$Alternative$Record$Vector3$sub, a, b);
		var len = $author$project$AltMath$Alternative$Record$Vector3$length(c);
		return A3($author$project$AltMath$Alternative$Record$Vector3$Vec3, c.ck / len, c.cl / len, c.cn / len);
	});
var $author$project$AltMath$Alternative$Record$Matrix4$mul = F2(
	function (a, b) {
		return {bH: (((a.bH * b.bH) + (a.bI * b.bL)) + (a.bJ * b.bP)) + (a.bK * b.bT), bI: (((a.bH * b.bI) + (a.bI * b.bM)) + (a.bJ * b.bQ)) + (a.bK * b.bU), bJ: (((a.bH * b.bJ) + (a.bI * b.bN)) + (a.bJ * b.bR)) + (a.bK * b.bV), bK: (((a.bH * b.bK) + (a.bI * b.bO)) + (a.bJ * b.bS)) + (a.bK * b.bW), bL: (((a.bL * b.bH) + (a.bM * b.bL)) + (a.bN * b.bP)) + (a.bO * b.bT), bM: (((a.bL * b.bI) + (a.bM * b.bM)) + (a.bN * b.bQ)) + (a.bO * b.bU), bN: (((a.bL * b.bJ) + (a.bM * b.bN)) + (a.bN * b.bR)) + (a.bO * b.bV), bO: (((a.bL * b.bK) + (a.bM * b.bO)) + (a.bN * b.bS)) + (a.bO * b.bW), bP: (((a.bP * b.bH) + (a.bQ * b.bL)) + (a.bR * b.bP)) + (a.bS * b.bT), bQ: (((a.bP * b.bI) + (a.bQ * b.bM)) + (a.bR * b.bQ)) + (a.bS * b.bU), bR: (((a.bP * b.bJ) + (a.bQ * b.bN)) + (a.bR * b.bR)) + (a.bS * b.bV), bS: (((a.bP * b.bK) + (a.bQ * b.bO)) + (a.bR * b.bS)) + (a.bS * b.bW), bT: (((a.bT * b.bH) + (a.bU * b.bL)) + (a.bV * b.bP)) + (a.bW * b.bT), bU: (((a.bT * b.bI) + (a.bU * b.bM)) + (a.bV * b.bQ)) + (a.bW * b.bU), bV: (((a.bT * b.bJ) + (a.bU * b.bN)) + (a.bV * b.bR)) + (a.bW * b.bV), bW: (((a.bT * b.bK) + (a.bU * b.bO)) + (a.bV * b.bS)) + (a.bW * b.bW)};
	});
var $author$project$AltMath$Alternative$Record$Vector3$normalize = function (v3) {
	var len = $author$project$AltMath$Alternative$Record$Vector3$length(v3);
	return A3($author$project$AltMath$Alternative$Record$Vector3$Vec3, v3.ck / len, v3.cl / len, v3.cn / len);
};
var $author$project$AltMath$Alternative$Record$Matrix4$makeLookAt = F3(
	function (eye, center, up) {
		var z = A2($author$project$AltMath$Alternative$Record$Vector3$direction, eye, center);
		var x = $author$project$AltMath$Alternative$Record$Vector3$normalize(
			A2($author$project$AltMath$Alternative$Record$Vector3$cross, up, z));
		var y = $author$project$AltMath$Alternative$Record$Vector3$normalize(
			A2($author$project$AltMath$Alternative$Record$Vector3$cross, z, x));
		return A2(
			$author$project$AltMath$Alternative$Record$Matrix4$mul,
			{bH: x.ck, bI: x.cl, bJ: x.cn, bK: 0, bL: y.ck, bM: y.cl, bN: y.cn, bO: 0, bP: z.ck, bQ: z.cl, bR: z.cn, bS: 0, bT: 0, bU: 0, bV: 0, bW: 1},
			{bH: 1, bI: 0, bJ: 0, bK: -eye.ck, bL: 0, bM: 1, bN: 0, bO: -eye.cl, bP: 0, bQ: 0, bR: 1, bS: -eye.cn, bT: 0, bU: 0, bV: 0, bW: 1});
	});
var $author$project$AltMath$Alternative$Tuple$Vector3$cross = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var az = _v0.c;
		var bx = _v1.a;
		var by = _v1.b;
		var bz = _v1.c;
		return _Utils_Tuple3((ay * bz) - (az * by), (az * bx) - (ax * bz), (ax * by) - (ay * bx));
	});
var $author$project$AltMath$Alternative$Tuple$Vector3$length = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	var z = _v0.c;
	return $elm$core$Basics$sqrt(((x * x) + (y * y)) + (z * z));
};
var $author$project$AltMath$Alternative$Tuple$Vector3$sub = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var az = _v0.c;
		var bx = _v1.a;
		var by = _v1.b;
		var bz = _v1.c;
		return _Utils_Tuple3(ax - bx, ay - by, az - bz);
	});
var $author$project$AltMath$Alternative$Tuple$Vector3$direction = F2(
	function (a, b) {
		var c = A2($author$project$AltMath$Alternative$Tuple$Vector3$sub, a, b);
		var x = c.a;
		var y = c.b;
		var z = c.c;
		var len = $author$project$AltMath$Alternative$Tuple$Vector3$length(c);
		return _Utils_Tuple3(x / len, y / len, z / len);
	});
var $author$project$AltMath$Alternative$Tuple$Matrix4$mul = F2(
	function (_v0, _v15) {
		var _v1 = _v0.a;
		var _v2 = _v1.a;
		var _v3 = _v2.a;
		var am11 = _v3.a;
		var am21 = _v3.b;
		var _v4 = _v2.b;
		var am31 = _v4.a;
		var am41 = _v4.b;
		var _v5 = _v1.b;
		var _v6 = _v5.a;
		var am12 = _v6.a;
		var am22 = _v6.b;
		var _v7 = _v5.b;
		var am32 = _v7.a;
		var am42 = _v7.b;
		var _v8 = _v0.b;
		var _v9 = _v8.a;
		var _v10 = _v9.a;
		var am13 = _v10.a;
		var am23 = _v10.b;
		var _v11 = _v9.b;
		var am33 = _v11.a;
		var am43 = _v11.b;
		var _v12 = _v8.b;
		var _v13 = _v12.a;
		var am14 = _v13.a;
		var am24 = _v13.b;
		var _v14 = _v12.b;
		var am34 = _v14.a;
		var am44 = _v14.b;
		var _v16 = _v15.a;
		var _v17 = _v16.a;
		var _v18 = _v17.a;
		var bm11 = _v18.a;
		var bm21 = _v18.b;
		var _v19 = _v17.b;
		var bm31 = _v19.a;
		var bm41 = _v19.b;
		var _v20 = _v16.b;
		var _v21 = _v20.a;
		var bm12 = _v21.a;
		var bm22 = _v21.b;
		var _v22 = _v20.b;
		var bm32 = _v22.a;
		var bm42 = _v22.b;
		var _v23 = _v15.b;
		var _v24 = _v23.a;
		var _v25 = _v24.a;
		var bm13 = _v25.a;
		var bm23 = _v25.b;
		var _v26 = _v24.b;
		var bm33 = _v26.a;
		var bm43 = _v26.b;
		var _v27 = _v23.b;
		var _v28 = _v27.a;
		var bm14 = _v28.a;
		var bm24 = _v28.b;
		var _v29 = _v27.b;
		var bm34 = _v29.a;
		var bm44 = _v29.b;
		return $author$project$AltMath$Alternative$Tuple$Matrix4$fromRecord(
			{bH: (((am11 * bm11) + (am12 * bm21)) + (am13 * bm31)) + (am14 * bm41), bI: (((am11 * bm12) + (am12 * bm22)) + (am13 * bm32)) + (am14 * bm42), bJ: (((am11 * bm13) + (am12 * bm23)) + (am13 * bm33)) + (am14 * bm43), bK: (((am11 * bm14) + (am12 * bm24)) + (am13 * bm34)) + (am14 * bm44), bL: (((am21 * bm11) + (am22 * bm21)) + (am23 * bm31)) + (am24 * bm41), bM: (((am21 * bm12) + (am22 * bm22)) + (am23 * bm32)) + (am24 * bm42), bN: (((am21 * bm13) + (am22 * bm23)) + (am23 * bm33)) + (am24 * bm43), bO: (((am21 * bm14) + (am22 * bm24)) + (am23 * bm34)) + (am24 * bm44), bP: (((am31 * bm11) + (am32 * bm21)) + (am33 * bm31)) + (am34 * bm41), bQ: (((am31 * bm12) + (am32 * bm22)) + (am33 * bm32)) + (am34 * bm42), bR: (((am31 * bm13) + (am32 * bm23)) + (am33 * bm33)) + (am34 * bm43), bS: (((am31 * bm14) + (am32 * bm24)) + (am33 * bm34)) + (am34 * bm44), bT: (((am41 * bm11) + (am42 * bm21)) + (am43 * bm31)) + (am44 * bm41), bU: (((am41 * bm12) + (am42 * bm22)) + (am43 * bm32)) + (am44 * bm42), bV: (((am41 * bm13) + (am42 * bm23)) + (am43 * bm33)) + (am44 * bm43), bW: (((am41 * bm14) + (am42 * bm24)) + (am43 * bm34)) + (am44 * bm44)});
	});
var $author$project$AltMath$Alternative$Tuple$Vector3$normalize = function (v3) {
	var x = v3.a;
	var y = v3.b;
	var z = v3.c;
	var len = $author$project$AltMath$Alternative$Tuple$Vector3$length(v3);
	return _Utils_Tuple3(x / len, y / len, z / len);
};
var $author$project$AltMath$Alternative$Tuple$Matrix4$makeLookAt = F3(
	function (eye, center, up) {
		var eyex = eye.a;
		var eyey = eye.b;
		var eyez = eye.c;
		var z = A2($author$project$AltMath$Alternative$Tuple$Vector3$direction, eye, center);
		var zx = z.a;
		var zy = z.b;
		var zz = z.c;
		var x = $author$project$AltMath$Alternative$Tuple$Vector3$normalize(
			A2($author$project$AltMath$Alternative$Tuple$Vector3$cross, up, z));
		var xx = x.a;
		var xy = x.b;
		var xz = x.c;
		var _v0 = $author$project$AltMath$Alternative$Tuple$Vector3$normalize(
			A2($author$project$AltMath$Alternative$Tuple$Vector3$cross, z, x));
		var yx = _v0.a;
		var yy = _v0.b;
		var yz = _v0.c;
		return A2(
			$author$project$AltMath$Alternative$Tuple$Matrix4$mul,
			$author$project$AltMath$Alternative$Tuple$Matrix4$fromRecord(
				{bH: xx, bI: xy, bJ: xz, bK: 0, bL: yx, bM: yy, bN: yz, bO: 0, bP: zx, bQ: zy, bR: zz, bS: 0, bT: 0, bU: 0, bV: 0, bW: 1}),
			$author$project$AltMath$Alternative$Tuple$Matrix4$fromRecord(
				{bH: 1, bI: 0, bJ: 0, bK: -eyex, bL: 0, bM: 1, bN: 0, bO: -eyey, bP: 0, bQ: 0, bR: 1, bS: -eyez, bT: 0, bU: 0, bV: 0, bW: 1}));
	});
var $elm_explorations$linear_algebra$Math$Matrix4$makeLookAt = _MJS_m4x4makeLookAt;
var $author$project$Matrix4$makeLookAt = function (_v0) {
	var glVec1 = _v0.K;
	var glVec2 = _v0.bg;
	var glVec3 = _v0.bh;
	var adtVec1 = _v0.J;
	var adtVec2 = _v0.bc;
	var adtVec3 = _v0.bd;
	var recVec1 = _v0.N;
	var recVec2 = _v0.bm;
	var recVec3 = _v0.bn;
	var tupleVec1 = _v0.O;
	var tupleVec2 = _v0.bt;
	var tupleVec3 = _v0.bu;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'makeLookAt',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Matrix4$GLMat4(
						A3($elm_explorations$linear_algebra$Math$Matrix4$makeLookAt, glVec1, glVec2, glVec3));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Matrix4$AdtMat4(
						A3($author$project$AltMath$Alternative$ADT$Matrix4$makeLookAt, adtVec1, adtVec2, adtVec3));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Matrix4$RecordMat4(
						A3($author$project$AltMath$Alternative$Record$Matrix4$makeLookAt, recVec1, recVec2, recVec3));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Matrix4$TupleMat4(
						A3($author$project$AltMath$Alternative$Tuple$Matrix4$makeLookAt, tupleVec1, tupleVec2, tupleVec3));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Matrix4$makeOrtho = F6(
	function (left, right, bottom, top, znear, zfar) {
		return $author$project$AltMath$Alternative$ADT$Matrix4$Mat4(2 / (right - left))(0)(0)(0)(0)(2 / (top - bottom))(0)(0)(0)(0)((-2) / (zfar - znear))(0)((-(right + left)) / (right - left))((-(top + bottom)) / (top - bottom))((-(zfar + znear)) / (zfar - znear))(1);
	});
var $author$project$AltMath$Alternative$Record$Matrix4$makeOrtho = F6(
	function (left, right, bottom, top, znear, zfar) {
		return {bH: 2 / (right - left), bI: 0, bJ: 0, bK: (-(right + left)) / (right - left), bL: 0, bM: 2 / (top - bottom), bN: 0, bO: (-(top + bottom)) / (top - bottom), bP: 0, bQ: 0, bR: (-2) / (zfar - znear), bS: (-(zfar + znear)) / (zfar - znear), bT: 0, bU: 0, bV: 0, bW: 1};
	});
var $author$project$AltMath$Alternative$Tuple$Matrix4$makeOrtho = F6(
	function (left, right, bottom, top, znear, zfar) {
		return $author$project$AltMath$Alternative$Tuple$Matrix4$fromRecord(
			{bH: 2 / (right - left), bI: 0, bJ: 0, bK: (-(right + left)) / (right - left), bL: 0, bM: 2 / (top - bottom), bN: 0, bO: (-(top + bottom)) / (top - bottom), bP: 0, bQ: 0, bR: (-2) / (zfar - znear), bS: (-(zfar + znear)) / (zfar - znear), bT: 0, bU: 0, bV: 0, bW: 1});
	});
var $elm_explorations$linear_algebra$Math$Matrix4$makeOrtho = _MJS_m4x4makeOrtho;
var $author$project$Matrix4$makeOrtho = F6(
	function (left, right, bottom, top, znear, zfar) {
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'makeOrtho',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v0) {
						return $author$project$Matrix4$GLMat4(
							A6($elm_explorations$linear_algebra$Math$Matrix4$makeOrtho, left, right, bottom, top, znear, zfar));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v1) {
						return $author$project$Matrix4$AdtMat4(
							A6($author$project$AltMath$Alternative$ADT$Matrix4$makeOrtho, left, right, bottom, top, znear, zfar));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v2) {
						return $author$project$Matrix4$RecordMat4(
							A6($author$project$AltMath$Alternative$Record$Matrix4$makeOrtho, left, right, bottom, top, znear, zfar));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v3) {
						return $author$project$Matrix4$TupleMat4(
							A6($author$project$AltMath$Alternative$Tuple$Matrix4$makeOrtho, left, right, bottom, top, znear, zfar));
					})
				]));
	});
var $author$project$AltMath$Alternative$ADT$Matrix4$makeOrtho2D = F4(
	function (left, right, bottom, top) {
		return A6($author$project$AltMath$Alternative$ADT$Matrix4$makeOrtho, left, right, bottom, top, -1, 1);
	});
var $author$project$AltMath$Alternative$Record$Matrix4$makeOrtho2D = F4(
	function (left, right, bottom, top) {
		return A6($author$project$AltMath$Alternative$Record$Matrix4$makeOrtho, left, right, bottom, top, -1, 1);
	});
var $author$project$AltMath$Alternative$Tuple$Matrix4$makeOrtho2D = F4(
	function (left, right, bottom, top) {
		return A6($author$project$AltMath$Alternative$Tuple$Matrix4$makeOrtho, left, right, bottom, top, -1, 1);
	});
var $elm_explorations$linear_algebra$Math$Matrix4$makeOrtho2D = _MJS_m4x4makeOrtho2D;
var $author$project$Matrix4$makeOrtho2D = F4(
	function (left, right, bottom, top) {
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'makeOrtho2D',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v0) {
						return $author$project$Matrix4$GLMat4(
							A4($elm_explorations$linear_algebra$Math$Matrix4$makeOrtho2D, left, right, bottom, top));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v1) {
						return $author$project$Matrix4$AdtMat4(
							A4($author$project$AltMath$Alternative$ADT$Matrix4$makeOrtho2D, left, right, bottom, top));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v2) {
						return $author$project$Matrix4$RecordMat4(
							A4($author$project$AltMath$Alternative$Record$Matrix4$makeOrtho2D, left, right, bottom, top));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v3) {
						return $author$project$Matrix4$TupleMat4(
							A4($author$project$AltMath$Alternative$Tuple$Matrix4$makeOrtho2D, left, right, bottom, top));
					})
				]));
	});
var $elm$core$Basics$pi = _Basics_pi;
var $elm$core$Basics$tan = _Basics_tan;
var $author$project$AltMath$Alternative$ADT$Matrix4$makePerspective = F4(
	function (fovy, aspect, znear, zfar) {
		var ymax = znear * $elm$core$Basics$tan((fovy * $elm$core$Basics$pi) / 360.0);
		var ymin = -ymax;
		var xmin = ymin * aspect;
		var xmax = ymax * aspect;
		return A6($author$project$AltMath$Alternative$ADT$Matrix4$makeFrustum, xmin, xmax, ymin, ymax, znear, zfar);
	});
var $author$project$AltMath$Alternative$Record$Matrix4$makePerspective = F4(
	function (fovy, aspect, znear, zfar) {
		var ymax = znear * $elm$core$Basics$tan((fovy * $elm$core$Basics$pi) / 360.0);
		var ymin = -ymax;
		var xmin = ymin * aspect;
		var xmax = ymax * aspect;
		return A6($author$project$AltMath$Alternative$Record$Matrix4$makeFrustum, xmin, xmax, ymin, ymax, znear, zfar);
	});
var $author$project$AltMath$Alternative$Tuple$Matrix4$makePerspective = F4(
	function (fovy, aspect, znear, zfar) {
		var ymax = znear * $elm$core$Basics$tan((fovy * $elm$core$Basics$pi) / 360.0);
		var ymin = -ymax;
		var xmin = ymin * aspect;
		var xmax = ymax * aspect;
		return A6($author$project$AltMath$Alternative$Tuple$Matrix4$makeFrustum, xmin, xmax, ymin, ymax, znear, zfar);
	});
var $elm_explorations$linear_algebra$Math$Matrix4$makePerspective = _MJS_m4x4makePerspective;
var $author$project$Matrix4$makePerspective = F4(
	function (fovy, aspect, znear, zfar) {
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'makePerspective',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v0) {
						return $author$project$Matrix4$GLMat4(
							A4($elm_explorations$linear_algebra$Math$Matrix4$makePerspective, fovy, aspect, znear, zfar));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v1) {
						return $author$project$Matrix4$AdtMat4(
							A4($author$project$AltMath$Alternative$ADT$Matrix4$makePerspective, fovy, aspect, znear, zfar));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v2) {
						return $author$project$Matrix4$RecordMat4(
							A4($author$project$AltMath$Alternative$Record$Matrix4$makePerspective, fovy, aspect, znear, zfar));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v3) {
						return $author$project$Matrix4$TupleMat4(
							A4($author$project$AltMath$Alternative$Tuple$Matrix4$makePerspective, fovy, aspect, znear, zfar));
					})
				]));
	});
var $elm$core$Basics$cos = _Basics_cos;
var $elm$core$Basics$sin = _Basics_sin;
var $author$project$AltMath$Alternative$ADT$Matrix4$makeRotate = F2(
	function (angle, axis) {
		var s = $elm$core$Basics$sin(angle);
		var c = $elm$core$Basics$cos(angle);
		var c1 = 1 - c;
		var _v0 = $author$project$AltMath$Alternative$ADT$Vector3$normalize(axis);
		var x = _v0.a;
		var y = _v0.b;
		var z = _v0.c;
		return $author$project$AltMath$Alternative$ADT$Matrix4$Mat4(((x * x) * c1) + c)(((y * x) * c1) + (z * s))(((z * x) * c1) - (y * s))(0)(((x * y) * c1) - (z * s))(((y * y) * c1) + c)(((y * z) * c1) + (x * s))(0)(((x * z) * c1) + (y * s))(((y * z) * c1) - (x * s))(((z * z) * c1) + c)(0)(0)(0)(0)(1);
	});
var $author$project$AltMath$Alternative$Record$Matrix4$makeRotate = F2(
	function (angle, axis) {
		var s = $elm$core$Basics$sin(angle);
		var c = $elm$core$Basics$cos(angle);
		var c1 = 1 - c;
		var _v0 = $author$project$AltMath$Alternative$Record$Vector3$normalize(axis);
		var x = _v0.ck;
		var y = _v0.cl;
		var z = _v0.cn;
		return {bH: ((x * x) * c1) + c, bI: ((x * y) * c1) - (z * s), bJ: ((x * z) * c1) + (y * s), bK: 0, bL: ((y * x) * c1) + (z * s), bM: ((y * y) * c1) + c, bN: ((y * z) * c1) - (x * s), bO: 0, bP: ((z * x) * c1) - (y * s), bQ: ((y * z) * c1) + (x * s), bR: ((z * z) * c1) + c, bS: 0, bT: 0, bU: 0, bV: 0, bW: 1};
	});
var $author$project$AltMath$Alternative$Tuple$Matrix4$makeRotate = F2(
	function (angle, axis) {
		var s = $elm$core$Basics$sin(angle);
		var c = $elm$core$Basics$cos(angle);
		var c1 = 1 - c;
		var _v0 = $author$project$AltMath$Alternative$Tuple$Vector3$normalize(axis);
		var x = _v0.a;
		var y = _v0.b;
		var z = _v0.c;
		return $author$project$AltMath$Alternative$Tuple$Matrix4$fromRecord(
			{bH: ((x * x) * c1) + c, bI: ((x * y) * c1) - (z * s), bJ: ((x * z) * c1) + (y * s), bK: 0, bL: ((y * x) * c1) + (z * s), bM: ((y * y) * c1) + c, bN: ((y * z) * c1) - (x * s), bO: 0, bP: ((z * x) * c1) - (y * s), bQ: ((y * z) * c1) + (x * s), bR: ((z * z) * c1) + c, bS: 0, bT: 0, bU: 0, bV: 0, bW: 1});
	});
var $elm_explorations$linear_algebra$Math$Matrix4$makeRotate = _MJS_m4x4makeRotate;
var $author$project$Matrix4$makeRotate = F2(
	function (angle, _v0) {
		var glVec1 = _v0.K;
		var adtVec1 = _v0.J;
		var recVec1 = _v0.N;
		var tupleVec1 = _v0.O;
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'makeRotate',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v1) {
						return $author$project$Matrix4$GLMat4(
							A2($elm_explorations$linear_algebra$Math$Matrix4$makeRotate, angle, glVec1));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v2) {
						return $author$project$Matrix4$AdtMat4(
							A2($author$project$AltMath$Alternative$ADT$Matrix4$makeRotate, angle, adtVec1));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v3) {
						return $author$project$Matrix4$RecordMat4(
							A2($author$project$AltMath$Alternative$Record$Matrix4$makeRotate, angle, recVec1));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v4) {
						return $author$project$Matrix4$TupleMat4(
							A2($author$project$AltMath$Alternative$Tuple$Matrix4$makeRotate, angle, tupleVec1));
					})
				]));
	});
var $author$project$AltMath$Alternative$ADT$Matrix4$makeScale = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	var z = _v0.c;
	return $author$project$AltMath$Alternative$ADT$Matrix4$Mat4(x)(0)(0)(0)(0)(y)(0)(0)(0)(0)(z)(0)(0)(0)(0)(1);
};
var $author$project$AltMath$Alternative$Record$Matrix4$makeScale = function (_v0) {
	var x = _v0.ck;
	var y = _v0.cl;
	var z = _v0.cn;
	return {bH: x, bI: 0, bJ: 0, bK: 0, bL: 0, bM: y, bN: 0, bO: 0, bP: 0, bQ: 0, bR: z, bS: 0, bT: 0, bU: 0, bV: 0, bW: 1};
};
var $author$project$AltMath$Alternative$Tuple$Matrix4$makeScale = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	var z = _v0.c;
	return $author$project$AltMath$Alternative$Tuple$Matrix4$fromRecord(
		{bH: x, bI: 0, bJ: 0, bK: 0, bL: 0, bM: y, bN: 0, bO: 0, bP: 0, bQ: 0, bR: z, bS: 0, bT: 0, bU: 0, bV: 0, bW: 1});
};
var $elm_explorations$linear_algebra$Math$Matrix4$makeScale = _MJS_m4x4makeScale;
var $author$project$Matrix4$makeScale = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'makeScale',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Matrix4$GLMat4(
						$elm_explorations$linear_algebra$Math$Matrix4$makeScale(glVec1));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Matrix4$AdtMat4(
						$author$project$AltMath$Alternative$ADT$Matrix4$makeScale(adtVec1));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Matrix4$RecordMat4(
						$author$project$AltMath$Alternative$Record$Matrix4$makeScale(recVec1));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Matrix4$TupleMat4(
						$author$project$AltMath$Alternative$Tuple$Matrix4$makeScale(tupleVec1));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Matrix4$makeScale3 = F3(
	function (x, y, z) {
		return $author$project$AltMath$Alternative$ADT$Matrix4$Mat4(x)(0)(0)(0)(0)(y)(0)(0)(0)(0)(z)(0)(0)(0)(0)(1);
	});
var $author$project$AltMath$Alternative$Record$Matrix4$makeScale3 = F3(
	function (x, y, z) {
		return {bH: x, bI: 0, bJ: 0, bK: 0, bL: 0, bM: y, bN: 0, bO: 0, bP: 0, bQ: 0, bR: z, bS: 0, bT: 0, bU: 0, bV: 0, bW: 1};
	});
var $author$project$AltMath$Alternative$Tuple$Matrix4$makeScale3 = F3(
	function (x, y, z) {
		return $author$project$AltMath$Alternative$Tuple$Matrix4$fromRecord(
			{bH: x, bI: 0, bJ: 0, bK: 0, bL: 0, bM: y, bN: 0, bO: 0, bP: 0, bQ: 0, bR: z, bS: 0, bT: 0, bU: 0, bV: 0, bW: 1});
	});
var $elm_explorations$linear_algebra$Math$Matrix4$makeScale3 = _MJS_m4x4makeScale3;
var $author$project$Matrix4$makeScale3 = F3(
	function (x, y, z) {
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'makeScale3',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v0) {
						return $author$project$Matrix4$GLMat4(
							A3($elm_explorations$linear_algebra$Math$Matrix4$makeScale3, x, y, z));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v1) {
						return $author$project$Matrix4$AdtMat4(
							A3($author$project$AltMath$Alternative$ADT$Matrix4$makeScale3, x, y, z));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v2) {
						return $author$project$Matrix4$RecordMat4(
							A3($author$project$AltMath$Alternative$Record$Matrix4$makeScale3, x, y, z));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v3) {
						return $author$project$Matrix4$TupleMat4(
							A3($author$project$AltMath$Alternative$Tuple$Matrix4$makeScale3, x, y, z));
					})
				]));
	});
var $author$project$AltMath$Alternative$ADT$Matrix4$makeTranslate = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	var z = _v0.c;
	return $author$project$AltMath$Alternative$ADT$Matrix4$Mat4(1)(0)(0)(0)(0)(1)(0)(0)(0)(0)(1)(0)(x)(y)(z)(1);
};
var $author$project$AltMath$Alternative$Record$Matrix4$makeTranslate = function (_v0) {
	var x = _v0.ck;
	var y = _v0.cl;
	var z = _v0.cn;
	return {bH: 1, bI: 0, bJ: 0, bK: x, bL: 0, bM: 1, bN: 0, bO: y, bP: 0, bQ: 0, bR: 1, bS: z, bT: 0, bU: 0, bV: 0, bW: 1};
};
var $author$project$AltMath$Alternative$Tuple$Matrix4$makeTranslate = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	var z = _v0.c;
	return $author$project$AltMath$Alternative$Tuple$Matrix4$fromRecord(
		{bH: 1, bI: 0, bJ: 0, bK: x, bL: 0, bM: 1, bN: 0, bO: y, bP: 0, bQ: 0, bR: 1, bS: z, bT: 0, bU: 0, bV: 0, bW: 1});
};
var $elm_explorations$linear_algebra$Math$Matrix4$makeTranslate = _MJS_m4x4makeTranslate;
var $author$project$Matrix4$makeTranslate = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'makeTranslate',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Matrix4$GLMat4(
						$elm_explorations$linear_algebra$Math$Matrix4$makeTranslate(glVec1));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Matrix4$AdtMat4(
						$author$project$AltMath$Alternative$ADT$Matrix4$makeTranslate(adtVec1));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Matrix4$RecordMat4(
						$author$project$AltMath$Alternative$Record$Matrix4$makeTranslate(recVec1));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Matrix4$TupleMat4(
						$author$project$AltMath$Alternative$Tuple$Matrix4$makeTranslate(tupleVec1));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Matrix4$makeTranslate3 = F3(
	function (x, y, z) {
		return $author$project$AltMath$Alternative$ADT$Matrix4$Mat4(1)(0)(0)(0)(0)(1)(0)(0)(0)(0)(1)(0)(x)(y)(z)(1);
	});
var $author$project$AltMath$Alternative$Record$Matrix4$makeTranslate3 = F3(
	function (x, y, z) {
		return {bH: 1, bI: 0, bJ: 0, bK: x, bL: 0, bM: 1, bN: 0, bO: y, bP: 0, bQ: 0, bR: 1, bS: z, bT: 0, bU: 0, bV: 0, bW: 1};
	});
var $author$project$AltMath$Alternative$Tuple$Matrix4$makeTranslate3 = F3(
	function (x, y, z) {
		return $author$project$AltMath$Alternative$Tuple$Matrix4$fromRecord(
			{bH: 1, bI: 0, bJ: 0, bK: x, bL: 0, bM: 1, bN: 0, bO: y, bP: 0, bQ: 0, bR: 1, bS: z, bT: 0, bU: 0, bV: 0, bW: 1});
	});
var $elm_explorations$linear_algebra$Math$Matrix4$makeTranslate3 = _MJS_m4x4makeTranslate3;
var $author$project$Matrix4$makeTranslate3 = F3(
	function (x, y, z) {
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'makeTranslate3',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v0) {
						return $author$project$Matrix4$GLMat4(
							A3($elm_explorations$linear_algebra$Math$Matrix4$makeTranslate3, x, y, z));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v1) {
						return $author$project$Matrix4$AdtMat4(
							A3($author$project$AltMath$Alternative$ADT$Matrix4$makeTranslate3, x, y, z));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v2) {
						return $author$project$Matrix4$RecordMat4(
							A3($author$project$AltMath$Alternative$Record$Matrix4$makeTranslate3, x, y, z));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v3) {
						return $author$project$Matrix4$TupleMat4(
							A3($author$project$AltMath$Alternative$Tuple$Matrix4$makeTranslate3, x, y, z));
					})
				]));
	});
var $elm_explorations$linear_algebra$Math$Matrix4$mul = _MJS_m4x4mul;
var $author$project$Matrix4$mul = function (_v0) {
	var glMat1 = _v0.D;
	var glMat2 = _v0.aZ;
	var adtMat1 = _v0.y;
	var adtMat2 = _v0.aW;
	var recMat1 = _v0.F;
	var recMat2 = _v0.a4;
	var tupleMat1 = _v0.G;
	var tupleMat2 = _v0.a9;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'mul',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Matrix4$GLMat4(
						A2($elm_explorations$linear_algebra$Math$Matrix4$mul, glMat1, glMat2));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Matrix4$AdtMat4(
						A2($author$project$AltMath$Alternative$ADT$Matrix4$mul, adtMat1, adtMat2));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Matrix4$RecordMat4(
						A2($author$project$AltMath$Alternative$Record$Matrix4$mul, recMat1, recMat2));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Matrix4$TupleMat4(
						A2($author$project$AltMath$Alternative$Tuple$Matrix4$mul, tupleMat1, tupleMat2));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Matrix4$mulAffine = F2(
	function (_v0, _v1) {
		var am11 = _v0.a;
		var am21 = _v0.b;
		var am31 = _v0.c;
		var am41 = _v0.d;
		var am12 = _v0.e;
		var am22 = _v0.f;
		var am32 = _v0.g;
		var am42 = _v0.h;
		var am13 = _v0.i;
		var am23 = _v0.j;
		var am33 = _v0.k;
		var am43 = _v0.l;
		var am14 = _v0.m;
		var am24 = _v0.n;
		var am34 = _v0.o;
		var am44 = _v0.p;
		var bm11 = _v1.a;
		var bm21 = _v1.b;
		var bm31 = _v1.c;
		var bm41 = _v1.d;
		var bm12 = _v1.e;
		var bm22 = _v1.f;
		var bm32 = _v1.g;
		var bm42 = _v1.h;
		var bm13 = _v1.i;
		var bm23 = _v1.j;
		var bm33 = _v1.k;
		var bm43 = _v1.l;
		var bm14 = _v1.m;
		var bm24 = _v1.n;
		var bm34 = _v1.o;
		var bm44 = _v1.p;
		return $author$project$AltMath$Alternative$ADT$Matrix4$Mat4(((am11 * bm11) + (am12 * bm21)) + (am13 * bm31))(((am21 * bm11) + (am22 * bm21)) + (am23 * bm31))(((am31 * bm11) + (am32 * bm21)) + (am33 * bm31))(0)(((am11 * bm12) + (am12 * bm22)) + (am13 * bm32))(((am21 * bm12) + (am22 * bm22)) + (am23 * bm32))(((am31 * bm12) + (am32 * bm22)) + (am33 * bm32))(0)(((am11 * bm13) + (am12 * bm23)) + (am13 * bm33))(((am21 * bm13) + (am22 * bm23)) + (am23 * bm33))(((am31 * bm13) + (am32 * bm23)) + (am33 * bm33))(0)((((am11 * bm14) + (am12 * bm24)) + (am13 * bm34)) + am14)((((am21 * bm14) + (am22 * bm24)) + (am23 * bm34)) + am24)((((am31 * bm14) + (am32 * bm24)) + (am33 * bm34)) + am34)(1);
	});
var $author$project$AltMath$Alternative$Record$Matrix4$mulAffine = F2(
	function (a, b) {
		return {bH: ((a.bH * b.bH) + (a.bI * b.bL)) + (a.bJ * b.bP), bI: ((a.bH * b.bI) + (a.bI * b.bM)) + (a.bJ * b.bQ), bJ: ((a.bH * b.bJ) + (a.bI * b.bN)) + (a.bJ * b.bR), bK: (((a.bH * b.bK) + (a.bI * b.bO)) + (a.bJ * b.bS)) + a.bK, bL: ((a.bL * b.bH) + (a.bM * b.bL)) + (a.bN * b.bP), bM: ((a.bL * b.bI) + (a.bM * b.bM)) + (a.bN * b.bQ), bN: ((a.bL * b.bJ) + (a.bM * b.bN)) + (a.bN * b.bR), bO: (((a.bL * b.bK) + (a.bM * b.bO)) + (a.bN * b.bS)) + a.bO, bP: ((a.bP * b.bH) + (a.bQ * b.bL)) + (a.bR * b.bP), bQ: ((a.bP * b.bI) + (a.bQ * b.bM)) + (a.bR * b.bQ), bR: ((a.bP * b.bJ) + (a.bQ * b.bN)) + (a.bR * b.bR), bS: (((a.bP * b.bK) + (a.bQ * b.bO)) + (a.bR * b.bS)) + a.bS, bT: 0, bU: 0, bV: 0, bW: 1};
	});
var $author$project$AltMath$Alternative$Tuple$Matrix4$mulAffine = F2(
	function (_v0, _v15) {
		var _v1 = _v0.a;
		var _v2 = _v1.a;
		var _v3 = _v2.a;
		var am11 = _v3.a;
		var am21 = _v3.b;
		var _v4 = _v2.b;
		var am31 = _v4.a;
		var am41 = _v4.b;
		var _v5 = _v1.b;
		var _v6 = _v5.a;
		var am12 = _v6.a;
		var am22 = _v6.b;
		var _v7 = _v5.b;
		var am32 = _v7.a;
		var am42 = _v7.b;
		var _v8 = _v0.b;
		var _v9 = _v8.a;
		var _v10 = _v9.a;
		var am13 = _v10.a;
		var am23 = _v10.b;
		var _v11 = _v9.b;
		var am33 = _v11.a;
		var am43 = _v11.b;
		var _v12 = _v8.b;
		var _v13 = _v12.a;
		var am14 = _v13.a;
		var am24 = _v13.b;
		var _v14 = _v12.b;
		var am34 = _v14.a;
		var am44 = _v14.b;
		var _v16 = _v15.a;
		var _v17 = _v16.a;
		var _v18 = _v17.a;
		var bm11 = _v18.a;
		var bm21 = _v18.b;
		var _v19 = _v17.b;
		var bm31 = _v19.a;
		var bm41 = _v19.b;
		var _v20 = _v16.b;
		var _v21 = _v20.a;
		var bm12 = _v21.a;
		var bm22 = _v21.b;
		var _v22 = _v20.b;
		var bm32 = _v22.a;
		var bm42 = _v22.b;
		var _v23 = _v15.b;
		var _v24 = _v23.a;
		var _v25 = _v24.a;
		var bm13 = _v25.a;
		var bm23 = _v25.b;
		var _v26 = _v24.b;
		var bm33 = _v26.a;
		var bm43 = _v26.b;
		var _v27 = _v23.b;
		var _v28 = _v27.a;
		var bm14 = _v28.a;
		var bm24 = _v28.b;
		var _v29 = _v27.b;
		var bm34 = _v29.a;
		var bm44 = _v29.b;
		return $author$project$AltMath$Alternative$Tuple$Matrix4$fromRecord(
			{bH: ((am11 * bm11) + (am12 * bm21)) + (am13 * bm31), bI: ((am11 * bm12) + (am12 * bm22)) + (am13 * bm32), bJ: ((am11 * bm13) + (am12 * bm23)) + (am13 * bm33), bK: (((am11 * bm14) + (am12 * bm24)) + (am13 * bm34)) + am14, bL: ((am21 * bm11) + (am22 * bm21)) + (am23 * bm31), bM: ((am21 * bm12) + (am22 * bm22)) + (am23 * bm32), bN: ((am21 * bm13) + (am22 * bm23)) + (am23 * bm33), bO: (((am21 * bm14) + (am22 * bm24)) + (am23 * bm34)) + am24, bP: ((am31 * bm11) + (am32 * bm21)) + (am33 * bm31), bQ: ((am31 * bm12) + (am32 * bm22)) + (am33 * bm32), bR: ((am31 * bm13) + (am32 * bm23)) + (am33 * bm33), bS: (((am31 * bm14) + (am32 * bm24)) + (am33 * bm34)) + am34, bT: 0, bU: 0, bV: 0, bW: 1});
	});
var $elm_explorations$linear_algebra$Math$Matrix4$mulAffine = _MJS_m4x4mulAffine;
var $author$project$Matrix4$mulAffine = function (_v0) {
	var glMat1 = _v0.D;
	var glMat2 = _v0.aZ;
	var adtMat1 = _v0.y;
	var adtMat2 = _v0.aW;
	var recMat1 = _v0.F;
	var recMat2 = _v0.a4;
	var tupleMat1 = _v0.G;
	var tupleMat2 = _v0.a9;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'mulAffine',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Matrix4$GLMat4(
						A2($elm_explorations$linear_algebra$Math$Matrix4$mulAffine, glMat1, glMat2));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Matrix4$AdtMat4(
						A2($author$project$AltMath$Alternative$ADT$Matrix4$mulAffine, adtMat1, adtMat2));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Matrix4$RecordMat4(
						A2($author$project$AltMath$Alternative$Record$Matrix4$mulAffine, recMat1, recMat2));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Matrix4$TupleMat4(
						A2($author$project$AltMath$Alternative$Tuple$Matrix4$mulAffine, tupleMat1, tupleMat2));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Matrix4$rotate = F3(
	function (angle, axis, _v0) {
		var axisx = axis.a;
		var axisy = axis.b;
		var axisz = axis.c;
		var m11 = _v0.a;
		var m21 = _v0.b;
		var m31 = _v0.c;
		var m41 = _v0.d;
		var m12 = _v0.e;
		var m22 = _v0.f;
		var m32 = _v0.g;
		var m42 = _v0.h;
		var m13 = _v0.i;
		var m23 = _v0.j;
		var m33 = _v0.k;
		var m43 = _v0.l;
		var m14 = _v0.m;
		var m24 = _v0.n;
		var m34 = _v0.o;
		var m44 = _v0.p;
		var s = $elm$core$Basics$sin(angle);
		var im = 1.0 / $author$project$AltMath$Alternative$ADT$Vector3$length(axis);
		var x = axisx * im;
		var xs = x * s;
		var y = axisy * im;
		var ys = y * s;
		var z = axisz * im;
		var zs = z * s;
		var c = $elm$core$Basics$cos(angle);
		var c1 = 1 - c;
		var xyc1 = (x * y) * c1;
		var t12 = xyc1 - zs;
		var t21 = xyc1 + zs;
		var xzc1 = (x * z) * c1;
		var t13 = xzc1 + ys;
		var t31 = xzc1 - ys;
		var yzc1 = (y * z) * c1;
		var t23 = yzc1 - xs;
		var t32 = yzc1 + xs;
		var t11 = ((x * x) * c1) + c;
		var t22 = ((y * y) * c1) + c;
		var t33 = ((z * z) * c1) + c;
		return $author$project$AltMath$Alternative$ADT$Matrix4$Mat4(((m11 * t11) + (m12 * t21)) + (m13 * t31))(((m21 * t11) + (m22 * t21)) + (m23 * t31))(((m31 * t11) + (m32 * t21)) + (m33 * t31))(((m41 * t11) + (m42 * t21)) + (m43 * t31))(((m11 * t12) + (m12 * t22)) + (m13 * t32))(((m21 * t12) + (m22 * t22)) + (m23 * t32))(((m31 * t12) + (m32 * t22)) + (m33 * t32))(((m41 * t12) + (m42 * t22)) + (m43 * t32))(((m11 * t13) + (m12 * t23)) + (m13 * t33))(((m21 * t13) + (m22 * t23)) + (m23 * t33))(((m31 * t13) + (m32 * t23)) + (m33 * t33))(((m41 * t13) + (m42 * t23)) + (m43 * t33))(m14)(m24)(m34)(m44);
	});
var $author$project$AltMath$Alternative$Record$Matrix4$rotate = F3(
	function (angle, axis, m) {
		var s = $elm$core$Basics$sin(angle);
		var im = 1.0 / $author$project$AltMath$Alternative$Record$Vector3$length(axis);
		var x = axis.ck * im;
		var xs = x * s;
		var y = axis.cl * im;
		var ys = y * s;
		var z = axis.cn * im;
		var zs = z * s;
		var c = $elm$core$Basics$cos(angle);
		var c1 = 1 - c;
		var xyc1 = (x * y) * c1;
		var t12 = xyc1 - zs;
		var t21 = xyc1 + zs;
		var xzc1 = (x * z) * c1;
		var t13 = xzc1 + ys;
		var t31 = xzc1 - ys;
		var yzc1 = (y * z) * c1;
		var t23 = yzc1 - xs;
		var t32 = yzc1 + xs;
		var t11 = ((x * x) * c1) + c;
		var t22 = ((y * y) * c1) + c;
		var t33 = ((z * z) * c1) + c;
		return {bH: ((m.bH * t11) + (m.bI * t21)) + (m.bJ * t31), bI: ((m.bH * t12) + (m.bI * t22)) + (m.bJ * t32), bJ: ((m.bH * t13) + (m.bI * t23)) + (m.bJ * t33), bK: m.bK, bL: ((m.bL * t11) + (m.bM * t21)) + (m.bN * t31), bM: ((m.bL * t12) + (m.bM * t22)) + (m.bN * t32), bN: ((m.bL * t13) + (m.bM * t23)) + (m.bN * t33), bO: m.bO, bP: ((m.bP * t11) + (m.bQ * t21)) + (m.bR * t31), bQ: ((m.bP * t12) + (m.bQ * t22)) + (m.bR * t32), bR: ((m.bP * t13) + (m.bQ * t23)) + (m.bR * t33), bS: m.bS, bT: ((m.bT * t11) + (m.bU * t21)) + (m.bV * t31), bU: ((m.bT * t12) + (m.bU * t22)) + (m.bV * t32), bV: ((m.bT * t13) + (m.bU * t23)) + (m.bV * t33), bW: m.bW};
	});
var $author$project$AltMath$Alternative$Tuple$Matrix4$rotate = F3(
	function (angle, axis, _v0) {
		var axisx = axis.a;
		var axisy = axis.b;
		var axisz = axis.c;
		var _v1 = _v0.a;
		var _v2 = _v1.a;
		var _v3 = _v2.a;
		var m11 = _v3.a;
		var m21 = _v3.b;
		var _v4 = _v2.b;
		var m31 = _v4.a;
		var m41 = _v4.b;
		var _v5 = _v1.b;
		var _v6 = _v5.a;
		var m12 = _v6.a;
		var m22 = _v6.b;
		var _v7 = _v5.b;
		var m32 = _v7.a;
		var m42 = _v7.b;
		var _v8 = _v0.b;
		var _v9 = _v8.a;
		var _v10 = _v9.a;
		var m13 = _v10.a;
		var m23 = _v10.b;
		var _v11 = _v9.b;
		var m33 = _v11.a;
		var m43 = _v11.b;
		var _v12 = _v8.b;
		var _v13 = _v12.a;
		var m14 = _v13.a;
		var m24 = _v13.b;
		var _v14 = _v12.b;
		var m34 = _v14.a;
		var m44 = _v14.b;
		var s = $elm$core$Basics$sin(angle);
		var im = 1.0 / $author$project$AltMath$Alternative$Tuple$Vector3$length(axis);
		var x = axisx * im;
		var xs = x * s;
		var y = axisy * im;
		var ys = y * s;
		var z = axisz * im;
		var zs = z * s;
		var c = $elm$core$Basics$cos(angle);
		var c1 = 1 - c;
		var xyc1 = (x * y) * c1;
		var t12 = xyc1 - zs;
		var t21 = xyc1 + zs;
		var xzc1 = (x * z) * c1;
		var t13 = xzc1 + ys;
		var t31 = xzc1 - ys;
		var yzc1 = (y * z) * c1;
		var t23 = yzc1 - xs;
		var t32 = yzc1 + xs;
		var t11 = ((x * x) * c1) + c;
		var t22 = ((y * y) * c1) + c;
		var t33 = ((z * z) * c1) + c;
		return $author$project$AltMath$Alternative$Tuple$Matrix4$fromRecord(
			{bH: ((m11 * t11) + (m12 * t21)) + (m13 * t31), bI: ((m11 * t12) + (m12 * t22)) + (m13 * t32), bJ: ((m11 * t13) + (m12 * t23)) + (m13 * t33), bK: m14, bL: ((m21 * t11) + (m22 * t21)) + (m23 * t31), bM: ((m21 * t12) + (m22 * t22)) + (m23 * t32), bN: ((m21 * t13) + (m22 * t23)) + (m23 * t33), bO: m24, bP: ((m31 * t11) + (m32 * t21)) + (m33 * t31), bQ: ((m31 * t12) + (m32 * t22)) + (m33 * t32), bR: ((m31 * t13) + (m32 * t23)) + (m33 * t33), bS: m34, bT: ((m41 * t11) + (m42 * t21)) + (m43 * t31), bU: ((m41 * t12) + (m42 * t22)) + (m43 * t32), bV: ((m41 * t13) + (m42 * t23)) + (m43 * t33), bW: m44});
	});
var $elm_explorations$linear_algebra$Math$Matrix4$rotate = _MJS_m4x4rotate;
var $author$project$Matrix4$rotate = F3(
	function (angle, _v0, _v1) {
		var glMat1 = _v0.D;
		var adtMat1 = _v0.y;
		var recMat1 = _v0.F;
		var tupleMat1 = _v0.G;
		var glVec1 = _v1.K;
		var adtVec1 = _v1.J;
		var recVec1 = _v1.N;
		var tupleVec1 = _v1.O;
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'rotate',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v2) {
						return $author$project$Matrix4$GLMat4(
							A3($elm_explorations$linear_algebra$Math$Matrix4$rotate, angle, glVec1, glMat1));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v3) {
						return $author$project$Matrix4$AdtMat4(
							A3($author$project$AltMath$Alternative$ADT$Matrix4$rotate, angle, adtVec1, adtMat1));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v4) {
						return $author$project$Matrix4$RecordMat4(
							A3($author$project$AltMath$Alternative$Record$Matrix4$rotate, angle, recVec1, recMat1));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v5) {
						return $author$project$Matrix4$TupleMat4(
							A3($author$project$AltMath$Alternative$Tuple$Matrix4$rotate, angle, tupleVec1, tupleMat1));
					})
				]));
	});
var $author$project$AltMath$Alternative$ADT$Matrix4$scale = F2(
	function (_v0, _v1) {
		var x = _v0.a;
		var y = _v0.b;
		var z = _v0.c;
		var m11 = _v1.a;
		var m21 = _v1.b;
		var m31 = _v1.c;
		var m41 = _v1.d;
		var m12 = _v1.e;
		var m22 = _v1.f;
		var m32 = _v1.g;
		var m42 = _v1.h;
		var m13 = _v1.i;
		var m23 = _v1.j;
		var m33 = _v1.k;
		var m43 = _v1.l;
		var m14 = _v1.m;
		var m24 = _v1.n;
		var m34 = _v1.o;
		var m44 = _v1.p;
		return $author$project$AltMath$Alternative$ADT$Matrix4$Mat4(m11 * x)(m21 * x)(m31 * x)(m41 * x)(m12 * y)(m22 * y)(m32 * y)(m42 * y)(m13 * z)(m23 * z)(m33 * z)(m43 * z)(m14)(m24)(m34)(m44);
	});
var $author$project$AltMath$Alternative$Record$Matrix4$scale = F2(
	function (_v0, m) {
		var x = _v0.ck;
		var y = _v0.cl;
		var z = _v0.cn;
		return {bH: m.bH * x, bI: m.bI * y, bJ: m.bJ * z, bK: m.bK, bL: m.bL * x, bM: m.bM * y, bN: m.bN * z, bO: m.bO, bP: m.bP * x, bQ: m.bQ * y, bR: m.bR * z, bS: m.bS, bT: m.bT * x, bU: m.bU * y, bV: m.bV * z, bW: m.bW};
	});
var $author$project$AltMath$Alternative$Tuple$Matrix4$scale = F2(
	function (_v0, _v1) {
		var x = _v0.a;
		var y = _v0.b;
		var z = _v0.c;
		var _v2 = _v1.a;
		var _v3 = _v2.a;
		var _v4 = _v3.a;
		var m11 = _v4.a;
		var m21 = _v4.b;
		var _v5 = _v3.b;
		var m31 = _v5.a;
		var m41 = _v5.b;
		var _v6 = _v2.b;
		var _v7 = _v6.a;
		var m12 = _v7.a;
		var m22 = _v7.b;
		var _v8 = _v6.b;
		var m32 = _v8.a;
		var m42 = _v8.b;
		var _v9 = _v1.b;
		var _v10 = _v9.a;
		var _v11 = _v10.a;
		var m13 = _v11.a;
		var m23 = _v11.b;
		var _v12 = _v10.b;
		var m33 = _v12.a;
		var m43 = _v12.b;
		var _v13 = _v9.b;
		var _v14 = _v13.a;
		var m14 = _v14.a;
		var m24 = _v14.b;
		var _v15 = _v13.b;
		var m34 = _v15.a;
		var m44 = _v15.b;
		return $author$project$AltMath$Alternative$Tuple$Matrix4$fromRecord(
			{bH: m11 * x, bI: m12 * y, bJ: m13 * z, bK: m14, bL: m21 * x, bM: m22 * y, bN: m23 * z, bO: m24, bP: m31 * x, bQ: m32 * y, bR: m33 * z, bS: m34, bT: m41 * x, bU: m42 * y, bV: m43 * z, bW: m44});
	});
var $elm_explorations$linear_algebra$Math$Matrix4$scale = _MJS_m4x4scale;
var $author$project$Matrix4$scale = F2(
	function (_v0, _v1) {
		var glMat1 = _v0.D;
		var adtMat1 = _v0.y;
		var recMat1 = _v0.F;
		var tupleMat1 = _v0.G;
		var glVec1 = _v1.K;
		var adtVec1 = _v1.J;
		var recVec1 = _v1.N;
		var tupleVec1 = _v1.O;
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'scale',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v2) {
						return $author$project$Matrix4$GLMat4(
							A2($elm_explorations$linear_algebra$Math$Matrix4$scale, glVec1, glMat1));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v3) {
						return $author$project$Matrix4$AdtMat4(
							A2($author$project$AltMath$Alternative$ADT$Matrix4$scale, adtVec1, adtMat1));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v4) {
						return $author$project$Matrix4$RecordMat4(
							A2($author$project$AltMath$Alternative$Record$Matrix4$scale, recVec1, recMat1));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v5) {
						return $author$project$Matrix4$TupleMat4(
							A2($author$project$AltMath$Alternative$Tuple$Matrix4$scale, tupleVec1, tupleMat1));
					})
				]));
	});
var $author$project$AltMath$Alternative$ADT$Matrix4$scale3 = F4(
	function (x, y, z, _v0) {
		var m11 = _v0.a;
		var m21 = _v0.b;
		var m31 = _v0.c;
		var m41 = _v0.d;
		var m12 = _v0.e;
		var m22 = _v0.f;
		var m32 = _v0.g;
		var m42 = _v0.h;
		var m13 = _v0.i;
		var m23 = _v0.j;
		var m33 = _v0.k;
		var m43 = _v0.l;
		var m14 = _v0.m;
		var m24 = _v0.n;
		var m34 = _v0.o;
		var m44 = _v0.p;
		return $author$project$AltMath$Alternative$ADT$Matrix4$Mat4(m11 * x)(m21 * x)(m31 * x)(m41 * x)(m12 * y)(m22 * y)(m32 * y)(m42 * y)(m13 * z)(m23 * z)(m33 * z)(m43 * z)(m14)(m24)(m34)(m44);
	});
var $author$project$AltMath$Alternative$Record$Matrix4$scale3 = F4(
	function (x, y, z, m) {
		return {bH: m.bH * x, bI: m.bI * y, bJ: m.bJ * z, bK: m.bK, bL: m.bL * x, bM: m.bM * y, bN: m.bN * z, bO: m.bO, bP: m.bP * x, bQ: m.bQ * y, bR: m.bR * z, bS: m.bS, bT: m.bT * x, bU: m.bU * y, bV: m.bV * z, bW: m.bW};
	});
var $author$project$AltMath$Alternative$Tuple$Matrix4$scale3 = F4(
	function (x, y, z, _v0) {
		var _v1 = _v0.a;
		var _v2 = _v1.a;
		var _v3 = _v2.a;
		var m11 = _v3.a;
		var m21 = _v3.b;
		var _v4 = _v2.b;
		var m31 = _v4.a;
		var m41 = _v4.b;
		var _v5 = _v1.b;
		var _v6 = _v5.a;
		var m12 = _v6.a;
		var m22 = _v6.b;
		var _v7 = _v5.b;
		var m32 = _v7.a;
		var m42 = _v7.b;
		var _v8 = _v0.b;
		var _v9 = _v8.a;
		var _v10 = _v9.a;
		var m13 = _v10.a;
		var m23 = _v10.b;
		var _v11 = _v9.b;
		var m33 = _v11.a;
		var m43 = _v11.b;
		var _v12 = _v8.b;
		var _v13 = _v12.a;
		var m14 = _v13.a;
		var m24 = _v13.b;
		var _v14 = _v12.b;
		var m34 = _v14.a;
		var m44 = _v14.b;
		return $author$project$AltMath$Alternative$Tuple$Matrix4$fromRecord(
			{bH: m11 * x, bI: m12 * y, bJ: m13 * z, bK: m14, bL: m21 * x, bM: m22 * y, bN: m23 * z, bO: m24, bP: m31 * x, bQ: m32 * y, bR: m33 * z, bS: m34, bT: m41 * x, bU: m42 * y, bV: m43 * z, bW: m44});
	});
var $elm_explorations$linear_algebra$Math$Matrix4$scale3 = _MJS_m4x4scale3;
var $author$project$Matrix4$scale3 = F4(
	function (x, y, z, _v0) {
		var glMat1 = _v0.D;
		var adtMat1 = _v0.y;
		var recMat1 = _v0.F;
		var tupleMat1 = _v0.G;
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'scale3',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v1) {
						return $author$project$Matrix4$GLMat4(
							A4($elm_explorations$linear_algebra$Math$Matrix4$scale3, x, y, z, glMat1));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v2) {
						return $author$project$Matrix4$AdtMat4(
							A4($author$project$AltMath$Alternative$ADT$Matrix4$scale3, x, y, z, adtMat1));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v3) {
						return $author$project$Matrix4$RecordMat4(
							A4($author$project$AltMath$Alternative$Record$Matrix4$scale3, x, y, z, recMat1));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v4) {
						return $author$project$Matrix4$TupleMat4(
							A4($author$project$AltMath$Alternative$Tuple$Matrix4$scale3, x, y, z, tupleMat1));
					})
				]));
	});
var $elm$core$String$toFloat = _String_toFloat;
var $author$project$Matrix4$AdtVec3 = function (a) {
	return {$: 1, a: a};
};
var $author$project$Matrix4$GLVec3 = function (a) {
	return {$: 0, a: a};
};
var $author$project$Matrix4$RecordVec3 = function (a) {
	return {$: 2, a: a};
};
var $author$project$Matrix4$TupleVec3 = function (a) {
	return {$: 3, a: a};
};
var $author$project$AltMath$Alternative$ADT$Matrix4$transform = F2(
	function (_v0, _v1) {
		var m11 = _v0.a;
		var m21 = _v0.b;
		var m31 = _v0.c;
		var m41 = _v0.d;
		var m12 = _v0.e;
		var m22 = _v0.f;
		var m32 = _v0.g;
		var m42 = _v0.h;
		var m13 = _v0.i;
		var m23 = _v0.j;
		var m33 = _v0.k;
		var m43 = _v0.l;
		var m14 = _v0.m;
		var m24 = _v0.n;
		var m34 = _v0.o;
		var m44 = _v0.p;
		var x = _v1.a;
		var y = _v1.b;
		var z = _v1.c;
		var w = (((x * m41) + (y * m42)) + (z * m43)) + m44;
		return A3($author$project$AltMath$Alternative$ADT$Vector3$Vec3, ((((m11 * x) + (m12 * y)) + (m13 * z)) + m14) / w, ((((m21 * x) + (m22 * y)) + (m23 * z)) + m24) / w, ((((m31 * x) + (m32 * y)) + (m33 * z)) + m34) / w);
	});
var $author$project$AltMath$Alternative$Record$Matrix4$transform = F2(
	function (m, _v0) {
		var x = _v0.ck;
		var y = _v0.cl;
		var z = _v0.cn;
		var w = (((x * m.bT) + (y * m.bU)) + (z * m.bV)) + m.bW;
		return {ck: ((((m.bH * x) + (m.bI * y)) + (m.bJ * z)) + m.bK) / w, cl: ((((m.bL * x) + (m.bM * y)) + (m.bN * z)) + m.bO) / w, cn: ((((m.bP * x) + (m.bQ * y)) + (m.bR * z)) + m.bS) / w};
	});
var $author$project$AltMath$Alternative$Tuple$Matrix4$transform = F2(
	function (_v0, _v15) {
		var _v1 = _v0.a;
		var _v2 = _v1.a;
		var _v3 = _v2.a;
		var m11 = _v3.a;
		var m21 = _v3.b;
		var _v4 = _v2.b;
		var m31 = _v4.a;
		var m41 = _v4.b;
		var _v5 = _v1.b;
		var _v6 = _v5.a;
		var m12 = _v6.a;
		var m22 = _v6.b;
		var _v7 = _v5.b;
		var m32 = _v7.a;
		var m42 = _v7.b;
		var _v8 = _v0.b;
		var _v9 = _v8.a;
		var _v10 = _v9.a;
		var m13 = _v10.a;
		var m23 = _v10.b;
		var _v11 = _v9.b;
		var m33 = _v11.a;
		var m43 = _v11.b;
		var _v12 = _v8.b;
		var _v13 = _v12.a;
		var m14 = _v13.a;
		var m24 = _v13.b;
		var _v14 = _v12.b;
		var m34 = _v14.a;
		var m44 = _v14.b;
		var x = _v15.a;
		var y = _v15.b;
		var z = _v15.c;
		var w = (((x * m41) + (y * m42)) + (z * m43)) + m44;
		return _Utils_Tuple3(((((m11 * x) + (m12 * y)) + (m13 * z)) + m14) / w, ((((m21 * x) + (m22 * y)) + (m23 * z)) + m24) / w, ((((m31 * x) + (m32 * y)) + (m33 * z)) + m34) / w);
	});
var $elm_explorations$linear_algebra$Math$Matrix4$transform = _MJS_v3mul4x4;
var $author$project$Matrix4$transform = F2(
	function (_v0, _v1) {
		var glMat1 = _v0.D;
		var adtMat1 = _v0.y;
		var recMat1 = _v0.F;
		var tupleMat1 = _v0.G;
		var glVec1 = _v1.K;
		var adtVec1 = _v1.J;
		var recVec1 = _v1.N;
		var tupleVec1 = _v1.O;
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'transform',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v2) {
						return $author$project$Matrix4$GLVec3(
							A2($elm_explorations$linear_algebra$Math$Matrix4$transform, glMat1, glVec1));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v3) {
						return $author$project$Matrix4$AdtVec3(
							A2($author$project$AltMath$Alternative$ADT$Matrix4$transform, adtMat1, adtVec1));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v4) {
						return $author$project$Matrix4$RecordVec3(
							A2($author$project$AltMath$Alternative$Record$Matrix4$transform, recMat1, recVec1));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v5) {
						return $author$project$Matrix4$TupleVec3(
							A2($author$project$AltMath$Alternative$Tuple$Matrix4$transform, tupleMat1, tupleVec1));
					})
				]));
	});
var $author$project$AltMath$Alternative$ADT$Matrix4$translate = F2(
	function (_v0, _v1) {
		var x = _v0.a;
		var y = _v0.b;
		var z = _v0.c;
		var m11 = _v1.a;
		var m21 = _v1.b;
		var m31 = _v1.c;
		var m41 = _v1.d;
		var m12 = _v1.e;
		var m22 = _v1.f;
		var m32 = _v1.g;
		var m42 = _v1.h;
		var m13 = _v1.i;
		var m23 = _v1.j;
		var m33 = _v1.k;
		var m43 = _v1.l;
		var m14 = _v1.m;
		var m24 = _v1.n;
		var m34 = _v1.o;
		var m44 = _v1.p;
		return $author$project$AltMath$Alternative$ADT$Matrix4$Mat4(m11)(m21)(m31)(m41)(m12)(m22)(m32)(m42)(m13)(m23)(m33)(m43)((((m11 * x) + (m12 * y)) + (m13 * z)) + m14)((((m21 * x) + (m22 * y)) + (m23 * z)) + m24)((((m31 * x) + (m32 * y)) + (m33 * z)) + m34)((((m41 * x) + (m42 * y)) + (m43 * z)) + m44);
	});
var $author$project$AltMath$Alternative$Record$Matrix4$translate = F2(
	function (_v0, m) {
		var x = _v0.ck;
		var y = _v0.cl;
		var z = _v0.cn;
		return {bH: m.bH, bI: m.bI, bJ: m.bJ, bK: (((m.bH * x) + (m.bI * y)) + (m.bJ * z)) + m.bK, bL: m.bL, bM: m.bM, bN: m.bN, bO: (((m.bL * x) + (m.bM * y)) + (m.bN * z)) + m.bO, bP: m.bP, bQ: m.bQ, bR: m.bR, bS: (((m.bP * x) + (m.bQ * y)) + (m.bR * z)) + m.bS, bT: m.bT, bU: m.bU, bV: m.bV, bW: (((m.bT * x) + (m.bU * y)) + (m.bV * z)) + m.bW};
	});
var $author$project$AltMath$Alternative$Tuple$Matrix4$translate = F2(
	function (_v0, _v1) {
		var x = _v0.a;
		var y = _v0.b;
		var z = _v0.c;
		var _v2 = _v1.a;
		var _v3 = _v2.a;
		var _v4 = _v3.a;
		var m11 = _v4.a;
		var m21 = _v4.b;
		var _v5 = _v3.b;
		var m31 = _v5.a;
		var m41 = _v5.b;
		var _v6 = _v2.b;
		var _v7 = _v6.a;
		var m12 = _v7.a;
		var m22 = _v7.b;
		var _v8 = _v6.b;
		var m32 = _v8.a;
		var m42 = _v8.b;
		var _v9 = _v1.b;
		var _v10 = _v9.a;
		var _v11 = _v10.a;
		var m13 = _v11.a;
		var m23 = _v11.b;
		var _v12 = _v10.b;
		var m33 = _v12.a;
		var m43 = _v12.b;
		var _v13 = _v9.b;
		var _v14 = _v13.a;
		var m14 = _v14.a;
		var m24 = _v14.b;
		var _v15 = _v13.b;
		var m34 = _v15.a;
		var m44 = _v15.b;
		return $author$project$AltMath$Alternative$Tuple$Matrix4$fromRecord(
			{bH: m11, bI: m12, bJ: m13, bK: (((m11 * x) + (m12 * y)) + (m13 * z)) + m14, bL: m21, bM: m22, bN: m23, bO: (((m21 * x) + (m22 * y)) + (m23 * z)) + m24, bP: m31, bQ: m32, bR: m33, bS: (((m31 * x) + (m32 * y)) + (m33 * z)) + m34, bT: m41, bU: m42, bV: m43, bW: (((m41 * x) + (m42 * y)) + (m43 * z)) + m44});
	});
var $elm_explorations$linear_algebra$Math$Matrix4$translate = _MJS_m4x4translate;
var $author$project$Matrix4$translate = F2(
	function (_v0, _v1) {
		var glMat1 = _v0.D;
		var adtMat1 = _v0.y;
		var recMat1 = _v0.F;
		var tupleMat1 = _v0.G;
		var glVec1 = _v1.K;
		var adtVec1 = _v1.J;
		var recVec1 = _v1.N;
		var tupleVec1 = _v1.O;
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'translate',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v2) {
						return $author$project$Matrix4$GLMat4(
							A2($elm_explorations$linear_algebra$Math$Matrix4$translate, glVec1, glMat1));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v3) {
						return $author$project$Matrix4$AdtMat4(
							A2($author$project$AltMath$Alternative$ADT$Matrix4$translate, adtVec1, adtMat1));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v4) {
						return $author$project$Matrix4$RecordMat4(
							A2($author$project$AltMath$Alternative$Record$Matrix4$translate, recVec1, recMat1));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v5) {
						return $author$project$Matrix4$TupleMat4(
							A2($author$project$AltMath$Alternative$Tuple$Matrix4$translate, tupleVec1, tupleMat1));
					})
				]));
	});
var $author$project$AltMath$Alternative$ADT$Matrix4$translate3 = F4(
	function (x, y, z, _v0) {
		var m11 = _v0.a;
		var m21 = _v0.b;
		var m31 = _v0.c;
		var m41 = _v0.d;
		var m12 = _v0.e;
		var m22 = _v0.f;
		var m32 = _v0.g;
		var m42 = _v0.h;
		var m13 = _v0.i;
		var m23 = _v0.j;
		var m33 = _v0.k;
		var m43 = _v0.l;
		var m14 = _v0.m;
		var m24 = _v0.n;
		var m34 = _v0.o;
		var m44 = _v0.p;
		return $author$project$AltMath$Alternative$ADT$Matrix4$Mat4(m11)(m21)(m31)(m41)(m12)(m22)(m32)(m42)(m13)(m23)(m33)(m43)((((m11 * x) + (m12 * y)) + (m13 * z)) + m14)((((m21 * x) + (m22 * y)) + (m23 * z)) + m24)((((m31 * x) + (m32 * y)) + (m33 * z)) + m34)((((m41 * x) + (m42 * y)) + (m43 * z)) + m44);
	});
var $author$project$AltMath$Alternative$Record$Matrix4$translate3 = F4(
	function (x, y, z, m) {
		return {bH: m.bH, bI: m.bI, bJ: m.bJ, bK: (((m.bH * x) + (m.bI * y)) + (m.bJ * z)) + m.bK, bL: m.bL, bM: m.bM, bN: m.bN, bO: (((m.bL * x) + (m.bM * y)) + (m.bN * z)) + m.bO, bP: m.bP, bQ: m.bQ, bR: m.bR, bS: (((m.bP * x) + (m.bQ * y)) + (m.bR * z)) + m.bS, bT: m.bT, bU: m.bU, bV: m.bV, bW: (((m.bT * x) + (m.bU * y)) + (m.bV * z)) + m.bW};
	});
var $author$project$AltMath$Alternative$Tuple$Matrix4$translate3 = F4(
	function (x, y, z, _v0) {
		var _v1 = _v0.a;
		var _v2 = _v1.a;
		var _v3 = _v2.a;
		var m11 = _v3.a;
		var m21 = _v3.b;
		var _v4 = _v2.b;
		var m31 = _v4.a;
		var m41 = _v4.b;
		var _v5 = _v1.b;
		var _v6 = _v5.a;
		var m12 = _v6.a;
		var m22 = _v6.b;
		var _v7 = _v5.b;
		var m32 = _v7.a;
		var m42 = _v7.b;
		var _v8 = _v0.b;
		var _v9 = _v8.a;
		var _v10 = _v9.a;
		var m13 = _v10.a;
		var m23 = _v10.b;
		var _v11 = _v9.b;
		var m33 = _v11.a;
		var m43 = _v11.b;
		var _v12 = _v8.b;
		var _v13 = _v12.a;
		var m14 = _v13.a;
		var m24 = _v13.b;
		var _v14 = _v12.b;
		var m34 = _v14.a;
		var m44 = _v14.b;
		return $author$project$AltMath$Alternative$Tuple$Matrix4$fromRecord(
			{bH: m11, bI: m12, bJ: m13, bK: (((m11 * x) + (m12 * y)) + (m13 * z)) + m14, bL: m21, bM: m22, bN: m23, bO: (((m21 * x) + (m22 * y)) + (m23 * z)) + m24, bP: m31, bQ: m32, bR: m33, bS: (((m31 * x) + (m32 * y)) + (m33 * z)) + m34, bT: m41, bU: m42, bV: m43, bW: (((m41 * x) + (m42 * y)) + (m43 * z)) + m44});
	});
var $elm_explorations$linear_algebra$Math$Matrix4$translate3 = _MJS_m4x4translate3;
var $author$project$Matrix4$translate3 = F4(
	function (x, y, z, _v0) {
		var glMat1 = _v0.D;
		var adtMat1 = _v0.y;
		var recMat1 = _v0.F;
		var tupleMat1 = _v0.G;
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'translate3',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v1) {
						return $author$project$Matrix4$GLMat4(
							A4($elm_explorations$linear_algebra$Math$Matrix4$translate3, x, y, z, glMat1));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v2) {
						return $author$project$Matrix4$AdtMat4(
							A4($author$project$AltMath$Alternative$ADT$Matrix4$translate3, x, y, z, adtMat1));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v3) {
						return $author$project$Matrix4$RecordMat4(
							A4($author$project$AltMath$Alternative$Record$Matrix4$translate3, x, y, z, recMat1));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v4) {
						return $author$project$Matrix4$TupleMat4(
							A4($author$project$AltMath$Alternative$Tuple$Matrix4$translate3, x, y, z, tupleMat1));
					})
				]));
	});
var $author$project$AltMath$Alternative$ADT$Matrix4$transpose = function (_v0) {
	var m11 = _v0.a;
	var m21 = _v0.b;
	var m31 = _v0.c;
	var m41 = _v0.d;
	var m12 = _v0.e;
	var m22 = _v0.f;
	var m32 = _v0.g;
	var m42 = _v0.h;
	var m13 = _v0.i;
	var m23 = _v0.j;
	var m33 = _v0.k;
	var m43 = _v0.l;
	var m14 = _v0.m;
	var m24 = _v0.n;
	var m34 = _v0.o;
	var m44 = _v0.p;
	return $author$project$AltMath$Alternative$ADT$Matrix4$Mat4(m11)(m12)(m13)(m14)(m21)(m22)(m23)(m24)(m31)(m32)(m33)(m34)(m41)(m42)(m43)(m44);
};
var $author$project$AltMath$Alternative$Record$Matrix4$transpose = function (m) {
	return {bH: m.bH, bI: m.bL, bJ: m.bP, bK: m.bT, bL: m.bI, bM: m.bM, bN: m.bQ, bO: m.bU, bP: m.bJ, bQ: m.bN, bR: m.bR, bS: m.bV, bT: m.bK, bU: m.bO, bV: m.bS, bW: m.bW};
};
var $author$project$AltMath$Alternative$Tuple$Matrix4$transpose = function (_v0) {
	var _v1 = _v0.a;
	var _v2 = _v1.a;
	var _v3 = _v2.a;
	var m11 = _v3.a;
	var m21 = _v3.b;
	var _v4 = _v2.b;
	var m31 = _v4.a;
	var m41 = _v4.b;
	var _v5 = _v1.b;
	var _v6 = _v5.a;
	var m12 = _v6.a;
	var m22 = _v6.b;
	var _v7 = _v5.b;
	var m32 = _v7.a;
	var m42 = _v7.b;
	var _v8 = _v0.b;
	var _v9 = _v8.a;
	var _v10 = _v9.a;
	var m13 = _v10.a;
	var m23 = _v10.b;
	var _v11 = _v9.b;
	var m33 = _v11.a;
	var m43 = _v11.b;
	var _v12 = _v8.b;
	var _v13 = _v12.a;
	var m14 = _v13.a;
	var m24 = _v13.b;
	var _v14 = _v12.b;
	var m34 = _v14.a;
	var m44 = _v14.b;
	return $author$project$AltMath$Alternative$Tuple$Matrix4$fromRecord(
		{bH: m11, bI: m21, bJ: m31, bK: m41, bL: m12, bM: m22, bN: m32, bO: m42, bP: m13, bQ: m23, bR: m33, bS: m43, bT: m14, bU: m24, bV: m34, bW: m44});
};
var $elm_explorations$linear_algebra$Math$Matrix4$transpose = _MJS_m4x4transpose;
var $author$project$Matrix4$transpose = function (_v0) {
	var glMat1 = _v0.D;
	var glMat2 = _v0.aZ;
	var adtMat1 = _v0.y;
	var adtMat2 = _v0.aW;
	var recMat1 = _v0.F;
	var recMat2 = _v0.a4;
	var tupleMat1 = _v0.G;
	var tupleMat2 = _v0.a9;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'transpose',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Matrix4$GLMat4(
						$elm_explorations$linear_algebra$Math$Matrix4$transpose(glMat1));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Matrix4$AdtMat4(
						$author$project$AltMath$Alternative$ADT$Matrix4$transpose(adtMat1));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Matrix4$RecordMat4(
						$author$project$AltMath$Alternative$Record$Matrix4$transpose(recMat1));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Matrix4$TupleMat4(
						$author$project$AltMath$Alternative$Tuple$Matrix4$transpose(tupleMat1));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector3$vec3 = $author$project$AltMath$Alternative$ADT$Vector3$Vec3;
var $author$project$AltMath$Alternative$Record$Vector3$vec3 = $author$project$AltMath$Alternative$Record$Vector3$Vec3;
var $author$project$AltMath$Alternative$Tuple$Vector3$vec3 = F3(
	function (x, y, z) {
		return _Utils_Tuple3(x, y, z);
	});
var $elm_explorations$linear_algebra$Math$Vector3$vec3 = _MJS_v3;
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Matrix4$all = function (_v0) {
	var m1 = _v0.a_;
	var m2 = _v0.a$;
	var dataM2 = {
		bH: A2(
			$elm$core$Maybe$withDefault,
			43,
			$elm$core$String$toFloat(m2.bH)),
		bI: A2(
			$elm$core$Maybe$withDefault,
			43,
			$elm$core$String$toFloat(m2.bI)),
		bJ: A2(
			$elm$core$Maybe$withDefault,
			43,
			$elm$core$String$toFloat(m2.bJ)),
		bK: A2(
			$elm$core$Maybe$withDefault,
			43,
			$elm$core$String$toFloat(m2.bK)),
		bL: A2(
			$elm$core$Maybe$withDefault,
			43,
			$elm$core$String$toFloat(m2.bL)),
		bM: A2(
			$elm$core$Maybe$withDefault,
			43,
			$elm$core$String$toFloat(m2.bM)),
		bN: A2(
			$elm$core$Maybe$withDefault,
			43,
			$elm$core$String$toFloat(m2.bN)),
		bO: A2(
			$elm$core$Maybe$withDefault,
			43,
			$elm$core$String$toFloat(m2.bO)),
		bP: A2(
			$elm$core$Maybe$withDefault,
			43,
			$elm$core$String$toFloat(m2.bP)),
		bQ: A2(
			$elm$core$Maybe$withDefault,
			43,
			$elm$core$String$toFloat(m2.bQ)),
		bR: A2(
			$elm$core$Maybe$withDefault,
			43,
			$elm$core$String$toFloat(m2.bR)),
		bS: A2(
			$elm$core$Maybe$withDefault,
			43,
			$elm$core$String$toFloat(m2.bS)),
		bT: A2(
			$elm$core$Maybe$withDefault,
			43,
			$elm$core$String$toFloat(m2.bT)),
		bU: A2(
			$elm$core$Maybe$withDefault,
			43,
			$elm$core$String$toFloat(m2.bU)),
		bV: A2(
			$elm$core$Maybe$withDefault,
			43,
			$elm$core$String$toFloat(m2.bV)),
		bW: A2(
			$elm$core$Maybe$withDefault,
			43,
			$elm$core$String$toFloat(m2.bW))
	};
	var dataM1 = {
		bH: A2(
			$elm$core$Maybe$withDefault,
			42,
			$elm$core$String$toFloat(m1.bH)),
		bI: A2(
			$elm$core$Maybe$withDefault,
			42,
			$elm$core$String$toFloat(m1.bI)),
		bJ: A2(
			$elm$core$Maybe$withDefault,
			42,
			$elm$core$String$toFloat(m1.bJ)),
		bK: A2(
			$elm$core$Maybe$withDefault,
			42,
			$elm$core$String$toFloat(m1.bK)),
		bL: A2(
			$elm$core$Maybe$withDefault,
			42,
			$elm$core$String$toFloat(m1.bL)),
		bM: A2(
			$elm$core$Maybe$withDefault,
			42,
			$elm$core$String$toFloat(m1.bM)),
		bN: A2(
			$elm$core$Maybe$withDefault,
			42,
			$elm$core$String$toFloat(m1.bN)),
		bO: A2(
			$elm$core$Maybe$withDefault,
			42,
			$elm$core$String$toFloat(m1.bO)),
		bP: A2(
			$elm$core$Maybe$withDefault,
			42,
			$elm$core$String$toFloat(m1.bP)),
		bQ: A2(
			$elm$core$Maybe$withDefault,
			42,
			$elm$core$String$toFloat(m1.bQ)),
		bR: A2(
			$elm$core$Maybe$withDefault,
			42,
			$elm$core$String$toFloat(m1.bR)),
		bS: A2(
			$elm$core$Maybe$withDefault,
			42,
			$elm$core$String$toFloat(m1.bS)),
		bT: A2(
			$elm$core$Maybe$withDefault,
			42,
			$elm$core$String$toFloat(m1.bT)),
		bU: A2(
			$elm$core$Maybe$withDefault,
			42,
			$elm$core$String$toFloat(m1.bU)),
		bV: A2(
			$elm$core$Maybe$withDefault,
			42,
			$elm$core$String$toFloat(m1.bV)),
		bW: A2(
			$elm$core$Maybe$withDefault,
			42,
			$elm$core$String$toFloat(m1.bW))
	};
	var data2 = {
		J: A3($author$project$AltMath$Alternative$ADT$Vector3$vec3, dataM1.bH, dataM1.bL, dataM1.bP),
		bc: A3($author$project$AltMath$Alternative$ADT$Vector3$vec3, dataM2.bH, dataM2.bL, dataM2.bP),
		bd: A3($author$project$AltMath$Alternative$ADT$Vector3$vec3, dataM2.bI, dataM2.bM, dataM2.bQ),
		K: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, dataM1.bH, dataM1.bL, dataM1.bP),
		bg: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, dataM2.bH, dataM2.bL, dataM2.bP),
		bh: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, dataM2.bI, dataM2.bM, dataM2.bQ),
		N: A3($author$project$AltMath$Alternative$Record$Vector3$vec3, dataM1.bH, dataM1.bL, dataM1.bP),
		bm: A3($author$project$AltMath$Alternative$Record$Vector3$vec3, dataM2.bH, dataM2.bL, dataM2.bP),
		bn: A3($author$project$AltMath$Alternative$Record$Vector3$vec3, dataM2.bI, dataM2.bM, dataM2.bQ),
		O: A3($author$project$AltMath$Alternative$Tuple$Vector3$vec3, dataM1.bH, dataM1.bL, dataM1.bP),
		bt: A3($author$project$AltMath$Alternative$Tuple$Vector3$vec3, dataM2.bH, dataM2.bL, dataM2.bP),
		bu: A3($author$project$AltMath$Alternative$Tuple$Vector3$vec3, dataM2.bI, dataM2.bM, dataM2.bQ)
	};
	var data1 = {
		y: $author$project$AltMath$Alternative$ADT$Matrix4$fromRecord(dataM1),
		aW: $author$project$AltMath$Alternative$ADT$Matrix4$fromRecord(dataM2),
		D: $elm_explorations$linear_algebra$Math$Matrix4$fromRecord(dataM1),
		aZ: $elm_explorations$linear_algebra$Math$Matrix4$fromRecord(dataM2),
		F: $author$project$AltMath$Alternative$Record$Matrix4$fromRecord(dataM1),
		a4: $author$project$AltMath$Alternative$Record$Matrix4$fromRecord(dataM2),
		G: $author$project$AltMath$Alternative$Tuple$Matrix4$fromRecord(dataM1),
		a9: $author$project$AltMath$Alternative$Tuple$Matrix4$fromRecord(dataM2)
	};
	return _List_fromArray(
		[
			$author$project$Matrix4$identity,
			$author$project$Matrix4$inverse(data1),
			$author$project$Matrix4$inverseOrthonormal(data1),
			$author$project$Matrix4$mul(data1),
			$author$project$Matrix4$mulAffine(data1),
			$author$project$Matrix4$transpose(data1),
			$author$project$Matrix4$makeBasis(data2),
			A2($author$project$Matrix4$transform, data1, data2),
			A6($author$project$Matrix4$makeFrustum, dataM1.bH, dataM1.bL, dataM1.bP, dataM1.bT, dataM1.bI, dataM1.bM),
			A4($author$project$Matrix4$makePerspective, dataM1.bH, dataM1.bL, dataM1.bP, dataM1.bT),
			A6($author$project$Matrix4$makeOrtho, dataM1.bH, dataM1.bL, dataM1.bP, dataM1.bT, dataM1.bI, dataM1.bM),
			A4($author$project$Matrix4$makeOrtho2D, dataM1.bH, dataM1.bL, dataM1.bP, dataM1.bT),
			$author$project$Matrix4$makeLookAt(data2),
			A3($author$project$Matrix4$rotate, dataM2.bH, data1, data2),
			A2($author$project$Matrix4$scale, data1, data2),
			A4($author$project$Matrix4$scale3, dataM1.bH, dataM1.bL, dataM1.bP, data1),
			A2($author$project$Matrix4$translate, data1, data2),
			A4($author$project$Matrix4$translate3, dataM1.bH, dataM1.bL, dataM1.bP, data1),
			A2($author$project$Matrix4$makeRotate, dataM1.bH, data2),
			$author$project$Matrix4$makeScale(data2),
			A3($author$project$Matrix4$makeScale3, dataM1.bH, dataM1.bL, dataM1.bP),
			$author$project$Matrix4$makeTranslate(data2),
			A3($author$project$Matrix4$makeTranslate3, dataM1.bH, dataM1.bL, dataM1.bP)
		]);
};
var $author$project$Vector2$AdtVec2 = function (a) {
	return {$: 1, a: a};
};
var $author$project$Vector2$GLVec2 = function (a) {
	return {$: 0, a: a};
};
var $author$project$Vector2$RecordVec2 = function (a) {
	return {$: 2, a: a};
};
var $author$project$Vector2$TupleVec2 = function (a) {
	return {$: 3, a: a};
};
var $author$project$AltMath$Alternative$ADT$Vector2$Vec2 = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$AltMath$Alternative$ADT$Vector2$add = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var bx = _v1.a;
		var by = _v1.b;
		return A2($author$project$AltMath$Alternative$ADT$Vector2$Vec2, ax + bx, ay + by);
	});
var $author$project$AltMath$Alternative$Record$Vector2$Vec2 = F2(
	function (x, y) {
		return {ck: x, cl: y};
	});
var $author$project$AltMath$Alternative$Record$Vector2$add = F2(
	function (a, b) {
		return A2($author$project$AltMath$Alternative$Record$Vector2$Vec2, a.ck + b.ck, a.cl + b.cl);
	});
var $author$project$AltMath$Alternative$Tuple$Vector2$add = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var bx = _v1.a;
		var by = _v1.b;
		return _Utils_Tuple2(ax + bx, ay + by);
	});
var $elm_explorations$linear_algebra$Math$Vector2$add = _MJS_v2add;
var $author$project$Vector2$add = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	var glVec2 = _v0.bg;
	var adtVec2 = _v0.bc;
	var recVec2 = _v0.bm;
	var tupleVec2 = _v0.bt;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'add',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Vector2$GLVec2(
						A2($elm_explorations$linear_algebra$Math$Vector2$add, glVec1, glVec2));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Vector2$AdtVec2(
						A2($author$project$AltMath$Alternative$ADT$Vector2$add, adtVec1, adtVec2));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Vector2$RecordVec2(
						A2($author$project$AltMath$Alternative$Record$Vector2$add, recVec1, recVec2));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Vector2$TupleVec2(
						A2($author$project$AltMath$Alternative$Tuple$Vector2$add, tupleVec1, tupleVec2));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector2$length = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	return $elm$core$Basics$sqrt((x * x) + (y * y));
};
var $author$project$AltMath$Alternative$ADT$Vector2$sub = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var bx = _v1.a;
		var by = _v1.b;
		return A2($author$project$AltMath$Alternative$ADT$Vector2$Vec2, ax - bx, ay - by);
	});
var $author$project$AltMath$Alternative$ADT$Vector2$direction = F2(
	function (a, b) {
		var c = A2($author$project$AltMath$Alternative$ADT$Vector2$sub, a, b);
		var x = c.a;
		var y = c.b;
		var len = $author$project$AltMath$Alternative$ADT$Vector2$length(c);
		return A2($author$project$AltMath$Alternative$ADT$Vector2$Vec2, x / len, y / len);
	});
var $author$project$AltMath$Alternative$Record$Vector2$length = function (_v0) {
	var x = _v0.ck;
	var y = _v0.cl;
	return $elm$core$Basics$sqrt((x * x) + (y * y));
};
var $author$project$AltMath$Alternative$Record$Vector2$sub = F2(
	function (a, b) {
		return A2($author$project$AltMath$Alternative$Record$Vector2$Vec2, a.ck - b.ck, a.cl - b.cl);
	});
var $author$project$AltMath$Alternative$Record$Vector2$direction = F2(
	function (a, b) {
		var c = A2($author$project$AltMath$Alternative$Record$Vector2$sub, a, b);
		var len = $author$project$AltMath$Alternative$Record$Vector2$length(c);
		return A2($author$project$AltMath$Alternative$Record$Vector2$Vec2, c.ck / len, c.cl / len);
	});
var $author$project$AltMath$Alternative$Tuple$Vector2$length = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	return $elm$core$Basics$sqrt((x * x) + (y * y));
};
var $author$project$AltMath$Alternative$Tuple$Vector2$sub = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var bx = _v1.a;
		var by = _v1.b;
		return _Utils_Tuple2(ax - bx, ay - by);
	});
var $author$project$AltMath$Alternative$Tuple$Vector2$direction = F2(
	function (a, b) {
		var c = A2($author$project$AltMath$Alternative$Tuple$Vector2$sub, a, b);
		var x = c.a;
		var y = c.b;
		var len = $author$project$AltMath$Alternative$Tuple$Vector2$length(c);
		return _Utils_Tuple2(x / len, y / len);
	});
var $elm_explorations$linear_algebra$Math$Vector2$direction = _MJS_v2direction;
var $author$project$Vector2$direction = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	var glVec2 = _v0.bg;
	var adtVec2 = _v0.bc;
	var recVec2 = _v0.bm;
	var tupleVec2 = _v0.bt;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'direction',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Vector2$GLVec2(
						A2($elm_explorations$linear_algebra$Math$Vector2$direction, glVec1, glVec2));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Vector2$AdtVec2(
						A2($author$project$AltMath$Alternative$ADT$Vector2$direction, adtVec1, adtVec2));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Vector2$RecordVec2(
						A2($author$project$AltMath$Alternative$Record$Vector2$direction, recVec1, recVec2));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Vector2$TupleVec2(
						A2($author$project$AltMath$Alternative$Tuple$Vector2$direction, tupleVec1, tupleVec2));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector2$distance = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var bx = _v1.a;
		var by = _v1.b;
		var dy = ay - by;
		var dx = ax - bx;
		return $elm$core$Basics$sqrt((dx * dx) + (dy * dy));
	});
var $author$project$AltMath$Alternative$Record$Vector2$distance = F2(
	function (a, b) {
		return $author$project$AltMath$Alternative$Record$Vector2$length(
			A2($author$project$AltMath$Alternative$Record$Vector2$sub, a, b));
	});
var $author$project$AltMath$Alternative$Tuple$Vector2$distance = F2(
	function (a, b) {
		return $author$project$AltMath$Alternative$Tuple$Vector2$length(
			A2($author$project$AltMath$Alternative$Tuple$Vector2$sub, a, b));
	});
var $elm_explorations$linear_algebra$Math$Vector2$distance = _MJS_v2distance;
var $author$project$Vector2$distance = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	var glVec2 = _v0.bg;
	var adtVec2 = _v0.bc;
	var recVec2 = _v0.bm;
	var tupleVec2 = _v0.bt;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'distance',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return A2($elm_explorations$linear_algebra$Math$Vector2$distance, glVec1, glVec2);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return A2($author$project$AltMath$Alternative$ADT$Vector2$distance, adtVec1, adtVec2);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return A2($author$project$AltMath$Alternative$Record$Vector2$distance, recVec1, recVec2);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return A2($author$project$AltMath$Alternative$Tuple$Vector2$distance, tupleVec1, tupleVec2);
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector2$distanceSquared = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var bx = _v1.a;
		var by = _v1.b;
		var dy = ay - by;
		var dx = ax - bx;
		return (dx * dx) + (dy * dy);
	});
var $author$project$AltMath$Alternative$Record$Vector2$lengthSquared = function (_v0) {
	var x = _v0.ck;
	var y = _v0.cl;
	return (x * x) + (y * y);
};
var $author$project$AltMath$Alternative$Record$Vector2$distanceSquared = F2(
	function (a, b) {
		return $author$project$AltMath$Alternative$Record$Vector2$lengthSquared(
			A2($author$project$AltMath$Alternative$Record$Vector2$sub, a, b));
	});
var $author$project$AltMath$Alternative$Tuple$Vector2$lengthSquared = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	return (x * x) + (y * y);
};
var $author$project$AltMath$Alternative$Tuple$Vector2$distanceSquared = F2(
	function (a, b) {
		return $author$project$AltMath$Alternative$Tuple$Vector2$lengthSquared(
			A2($author$project$AltMath$Alternative$Tuple$Vector2$sub, a, b));
	});
var $elm_explorations$linear_algebra$Math$Vector2$distanceSquared = _MJS_v2distanceSquared;
var $author$project$Vector2$distanceSquared = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	var glVec2 = _v0.bg;
	var adtVec2 = _v0.bc;
	var recVec2 = _v0.bm;
	var tupleVec2 = _v0.bt;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'distanceSquared',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return A2($elm_explorations$linear_algebra$Math$Vector2$distanceSquared, glVec1, glVec2);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return A2($author$project$AltMath$Alternative$ADT$Vector2$distanceSquared, adtVec1, adtVec2);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return A2($author$project$AltMath$Alternative$Record$Vector2$distanceSquared, recVec1, recVec2);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return A2($author$project$AltMath$Alternative$Tuple$Vector2$distanceSquared, tupleVec1, tupleVec2);
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector2$dot = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var bx = _v1.a;
		var by = _v1.b;
		return (ax * bx) + (ay * by);
	});
var $author$project$AltMath$Alternative$Record$Vector2$dot = F2(
	function (a, b) {
		return (a.ck * b.ck) + (a.cl * b.cl);
	});
var $author$project$AltMath$Alternative$Tuple$Vector2$dot = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var bx = _v1.a;
		var by = _v1.b;
		return (ax * bx) + (ay * by);
	});
var $elm_explorations$linear_algebra$Math$Vector2$dot = _MJS_v2dot;
var $author$project$Vector2$dot = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	var glVec2 = _v0.bg;
	var adtVec2 = _v0.bc;
	var recVec2 = _v0.bm;
	var tupleVec2 = _v0.bt;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'dot',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return A2($elm_explorations$linear_algebra$Math$Vector2$dot, glVec1, glVec2);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return A2($author$project$AltMath$Alternative$ADT$Vector2$dot, adtVec1, adtVec2);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return A2($author$project$AltMath$Alternative$Record$Vector2$dot, recVec1, recVec2);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return A2($author$project$AltMath$Alternative$Tuple$Vector2$dot, tupleVec1, tupleVec2);
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector2$fromRecord = function (_v0) {
	var x = _v0.ck;
	var y = _v0.cl;
	return A2($author$project$AltMath$Alternative$ADT$Vector2$Vec2, x, y);
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $author$project$AltMath$Alternative$Record$Vector2$fromRecord = $elm$core$Basics$identity;
var $author$project$AltMath$Alternative$Tuple$Vector2$fromRecord = function (_v0) {
	var x = _v0.ck;
	var y = _v0.cl;
	return _Utils_Tuple2(x, y);
};
var $elm_explorations$linear_algebra$Math$Vector2$fromRecord = _MJS_v2fromRecord;
var $author$project$Vector2$fromRecord = function (record) {
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'fromRecord',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v0) {
					return $author$project$Vector2$GLVec2(
						$elm_explorations$linear_algebra$Math$Vector2$fromRecord(record));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v1) {
					return $author$project$Vector2$AdtVec2(
						$author$project$AltMath$Alternative$ADT$Vector2$fromRecord(record));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v2) {
					return $author$project$Vector2$RecordVec2(
						$author$project$AltMath$Alternative$Record$Vector2$fromRecord(record));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v3) {
					return $author$project$Vector2$TupleVec2(
						$author$project$AltMath$Alternative$Tuple$Vector2$fromRecord(record));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector2$getX = function (_v0) {
	var x = _v0.a;
	return x;
};
var $author$project$AltMath$Alternative$Record$Vector2$getX = function ($) {
	return $.ck;
};
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $author$project$AltMath$Alternative$Tuple$Vector2$getX = $elm$core$Tuple$first;
var $elm_explorations$linear_algebra$Math$Vector2$getX = _MJS_v2getX;
var $author$project$Vector2$getX = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'getX',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $elm_explorations$linear_algebra$Math$Vector2$getX(glVec1);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$AltMath$Alternative$ADT$Vector2$getX(adtVec1);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$AltMath$Alternative$Record$Vector2$getX(recVec1);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$AltMath$Alternative$Tuple$Vector2$getX(tupleVec1);
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector2$getY = function (_v0) {
	var y = _v0.b;
	return y;
};
var $author$project$AltMath$Alternative$Record$Vector2$getY = function ($) {
	return $.cl;
};
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $author$project$AltMath$Alternative$Tuple$Vector2$getY = $elm$core$Tuple$second;
var $elm_explorations$linear_algebra$Math$Vector2$getY = _MJS_v2getY;
var $author$project$Vector2$getY = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'getY',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $elm_explorations$linear_algebra$Math$Vector2$getY(glVec1);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$AltMath$Alternative$ADT$Vector2$getY(adtVec1);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$AltMath$Alternative$Record$Vector2$getY(recVec1);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$AltMath$Alternative$Tuple$Vector2$getY(tupleVec1);
				})
			]));
};
var $elm_explorations$linear_algebra$Math$Vector2$length = _MJS_v2length;
var $author$project$Vector2$length = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'length',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $elm_explorations$linear_algebra$Math$Vector2$length(glVec1);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$AltMath$Alternative$ADT$Vector2$length(adtVec1);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$AltMath$Alternative$Record$Vector2$length(recVec1);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$AltMath$Alternative$Tuple$Vector2$length(tupleVec1);
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector2$lengthSquared = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	return (x * x) + (y * y);
};
var $elm_explorations$linear_algebra$Math$Vector2$lengthSquared = _MJS_v2lengthSquared;
var $author$project$Vector2$lengthSquared = function (_v0) {
	var glVec2 = _v0.bg;
	var adtVec2 = _v0.bc;
	var recVec2 = _v0.bm;
	var tupleVec2 = _v0.bt;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'lengthSquared',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $elm_explorations$linear_algebra$Math$Vector2$lengthSquared(glVec2);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$AltMath$Alternative$ADT$Vector2$lengthSquared(adtVec2);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$AltMath$Alternative$Record$Vector2$lengthSquared(recVec2);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$AltMath$Alternative$Tuple$Vector2$lengthSquared(tupleVec2);
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector2$mul = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var bx = _v1.a;
		var by = _v1.b;
		return A2($author$project$AltMath$Alternative$ADT$Vector2$Vec2, ax * bx, ay * by);
	});
var $author$project$AltMath$Alternative$Record$Vector2$mul = F2(
	function (a, b) {
		return {ck: a.ck * b.ck, cl: a.cl * b.cl};
	});
var $author$project$AltMath$Alternative$Tuple$Vector2$mul = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var bx = _v1.a;
		var by = _v1.b;
		return _Utils_Tuple2(ax * bx, ay * by);
	});
var $author$project$Vector2$mul = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	var glVec2 = _v0.bg;
	var adtVec2 = _v0.bc;
	var recVec2 = _v0.bm;
	var tupleVec2 = _v0.bt;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'mul',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'ADT',
				function (_v1) {
					return $author$project$Vector2$AdtVec2(
						A2($author$project$AltMath$Alternative$ADT$Vector2$mul, adtVec1, adtVec2));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v2) {
					return $author$project$Vector2$RecordVec2(
						A2($author$project$AltMath$Alternative$Record$Vector2$mul, recVec1, recVec2));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v3) {
					return $author$project$Vector2$TupleVec2(
						A2($author$project$AltMath$Alternative$Tuple$Vector2$mul, tupleVec1, tupleVec2));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector2$negate = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	return A2($author$project$AltMath$Alternative$ADT$Vector2$Vec2, -x, -y);
};
var $author$project$AltMath$Alternative$Record$Vector2$negate = function (a) {
	return A2($author$project$AltMath$Alternative$Record$Vector2$Vec2, -a.ck, -a.cl);
};
var $author$project$AltMath$Alternative$Tuple$Vector2$negate = function (_v0) {
	var ax = _v0.a;
	var ay = _v0.b;
	return _Utils_Tuple2(-ax, -ay);
};
var $elm_explorations$linear_algebra$Math$Vector2$negate = _MJS_v2negate;
var $author$project$Vector2$negate = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'negate',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Vector2$GLVec2(
						$elm_explorations$linear_algebra$Math$Vector2$negate(glVec1));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Vector2$AdtVec2(
						$author$project$AltMath$Alternative$ADT$Vector2$negate(adtVec1));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Vector2$RecordVec2(
						$author$project$AltMath$Alternative$Record$Vector2$negate(recVec1));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Vector2$TupleVec2(
						$author$project$AltMath$Alternative$Tuple$Vector2$negate(tupleVec1));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector2$normalize = function (v2) {
	var x = v2.a;
	var y = v2.b;
	var len = $author$project$AltMath$Alternative$ADT$Vector2$length(v2);
	return A2($author$project$AltMath$Alternative$ADT$Vector2$Vec2, x / len, y / len);
};
var $author$project$AltMath$Alternative$Record$Vector2$normalize = function (v2) {
	var len = $author$project$AltMath$Alternative$Record$Vector2$length(v2);
	return A2($author$project$AltMath$Alternative$Record$Vector2$Vec2, v2.ck / len, v2.cl / len);
};
var $author$project$AltMath$Alternative$Tuple$Vector2$normalize = function (v2) {
	var x = v2.a;
	var y = v2.b;
	var len = $author$project$AltMath$Alternative$Tuple$Vector2$length(v2);
	return _Utils_Tuple2(x / len, y / len);
};
var $elm_explorations$linear_algebra$Math$Vector2$normalize = _MJS_v2normalize;
var $author$project$Vector2$normalize = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'normalize',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Vector2$GLVec2(
						$elm_explorations$linear_algebra$Math$Vector2$normalize(glVec1));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Vector2$AdtVec2(
						$author$project$AltMath$Alternative$ADT$Vector2$normalize(adtVec1));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Vector2$RecordVec2(
						$author$project$AltMath$Alternative$Record$Vector2$normalize(recVec1));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Vector2$TupleVec2(
						$author$project$AltMath$Alternative$Tuple$Vector2$normalize(tupleVec1));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector2$scale = F2(
	function (s, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return A2($author$project$AltMath$Alternative$ADT$Vector2$Vec2, s * x, s * y);
	});
var $author$project$AltMath$Alternative$Record$Vector2$scale = F2(
	function (s, v2) {
		return A2($author$project$AltMath$Alternative$Record$Vector2$Vec2, s * v2.ck, s * v2.cl);
	});
var $author$project$AltMath$Alternative$Tuple$Vector2$scale = F2(
	function (s, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(s * x, s * y);
	});
var $elm_explorations$linear_algebra$Math$Vector2$scale = _MJS_v2scale;
var $author$project$Vector2$scale = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'scale',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Vector2$GLVec2(
						A2($elm_explorations$linear_algebra$Math$Vector2$scale, 2, glVec1));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Vector2$AdtVec2(
						A2($author$project$AltMath$Alternative$ADT$Vector2$scale, 2, adtVec1));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Vector2$RecordVec2(
						A2($author$project$AltMath$Alternative$Record$Vector2$scale, 2, recVec1));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Vector2$TupleVec2(
						A2($author$project$AltMath$Alternative$Tuple$Vector2$scale, 2, tupleVec1));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector2$setX = F2(
	function (x, _v0) {
		var y = _v0.b;
		return A2($author$project$AltMath$Alternative$ADT$Vector2$Vec2, x, y);
	});
var $author$project$AltMath$Alternative$Record$Vector2$setX = F2(
	function (x, _v0) {
		var y = _v0.cl;
		return {ck: x, cl: y};
	});
var $author$project$AltMath$Alternative$Tuple$Vector2$setX = F2(
	function (x, _v0) {
		var y = _v0.b;
		return _Utils_Tuple2(x, y);
	});
var $elm_explorations$linear_algebra$Math$Vector2$setX = _MJS_v2setX;
var $author$project$Vector2$setX = F2(
	function (_v0, x) {
		var glVec1 = _v0.K;
		var adtVec1 = _v0.J;
		var recVec1 = _v0.N;
		var tupleVec1 = _v0.O;
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'setX',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v1) {
						return $author$project$Vector2$GLVec2(
							A2($elm_explorations$linear_algebra$Math$Vector2$setX, x, glVec1));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v2) {
						return $author$project$Vector2$AdtVec2(
							A2($author$project$AltMath$Alternative$ADT$Vector2$setX, x, adtVec1));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v3) {
						return $author$project$Vector2$RecordVec2(
							A2($author$project$AltMath$Alternative$Record$Vector2$setX, x, recVec1));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v4) {
						return $author$project$Vector2$TupleVec2(
							A2($author$project$AltMath$Alternative$Tuple$Vector2$setX, x, tupleVec1));
					})
				]));
	});
var $author$project$AltMath$Alternative$ADT$Vector2$setY = F2(
	function (y, _v0) {
		var x = _v0.a;
		return A2($author$project$AltMath$Alternative$ADT$Vector2$Vec2, x, y);
	});
var $author$project$AltMath$Alternative$Record$Vector2$setY = F2(
	function (y, _v0) {
		var x = _v0.ck;
		return {ck: x, cl: y};
	});
var $author$project$AltMath$Alternative$Tuple$Vector2$setY = F2(
	function (y, _v0) {
		var x = _v0.a;
		return _Utils_Tuple2(x, y);
	});
var $elm_explorations$linear_algebra$Math$Vector2$setY = _MJS_v2setY;
var $author$project$Vector2$setY = F2(
	function (_v0, y) {
		var glVec1 = _v0.K;
		var adtVec1 = _v0.J;
		var recVec1 = _v0.N;
		var tupleVec1 = _v0.O;
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'setY',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v1) {
						return $author$project$Vector2$GLVec2(
							A2($elm_explorations$linear_algebra$Math$Vector2$setY, y, glVec1));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v2) {
						return $author$project$Vector2$AdtVec2(
							A2($author$project$AltMath$Alternative$ADT$Vector2$setY, y, adtVec1));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v3) {
						return $author$project$Vector2$RecordVec2(
							A2($author$project$AltMath$Alternative$Record$Vector2$setY, y, recVec1));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v4) {
						return $author$project$Vector2$TupleVec2(
							A2($author$project$AltMath$Alternative$Tuple$Vector2$setY, y, tupleVec1));
					})
				]));
	});
var $elm_explorations$linear_algebra$Math$Vector2$sub = _MJS_v2sub;
var $author$project$Vector2$sub = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	var glVec2 = _v0.bg;
	var adtVec2 = _v0.bc;
	var recVec2 = _v0.bm;
	var tupleVec2 = _v0.bt;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'sub',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Vector2$GLVec2(
						A2($elm_explorations$linear_algebra$Math$Vector2$sub, glVec1, glVec2));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Vector2$AdtVec2(
						A2($author$project$AltMath$Alternative$ADT$Vector2$sub, adtVec1, adtVec2));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Vector2$RecordVec2(
						A2($author$project$AltMath$Alternative$Record$Vector2$sub, recVec1, recVec2));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Vector2$TupleVec2(
						A2($author$project$AltMath$Alternative$Tuple$Vector2$sub, tupleVec1, tupleVec2));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector2$toRecord = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	return {ck: x, cl: y};
};
var $author$project$AltMath$Alternative$Record$Vector2$toRecord = $elm$core$Basics$identity;
var $author$project$AltMath$Alternative$Tuple$Vector2$toRecord = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	return {ck: x, cl: y};
};
var $elm_explorations$linear_algebra$Math$Vector2$toRecord = _MJS_v2toRecord;
var $author$project$Vector2$toRecord = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'toRecord',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $elm_explorations$linear_algebra$Math$Vector2$toRecord(glVec1);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$AltMath$Alternative$ADT$Vector2$toRecord(adtVec1);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$AltMath$Alternative$Record$Vector2$toRecord(recVec1);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$AltMath$Alternative$Tuple$Vector2$toRecord(tupleVec1);
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector2$vec2 = $author$project$AltMath$Alternative$ADT$Vector2$Vec2;
var $author$project$AltMath$Alternative$Record$Vector2$vec2 = $author$project$AltMath$Alternative$Record$Vector2$Vec2;
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $author$project$AltMath$Alternative$Tuple$Vector2$vec2 = $elm$core$Tuple$pair;
var $elm_explorations$linear_algebra$Math$Vector2$vec2 = _MJS_v2;
var $author$project$Vector2$vec2 = F2(
	function (x, y) {
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'vec2',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v0) {
						return $author$project$Vector2$GLVec2(
							A2($elm_explorations$linear_algebra$Math$Vector2$vec2, x, y));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v1) {
						return $author$project$Vector2$AdtVec2(
							A2($author$project$AltMath$Alternative$ADT$Vector2$vec2, x, y));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v2) {
						return $author$project$Vector2$RecordVec2(
							A2($author$project$AltMath$Alternative$Record$Vector2$vec2, x, y));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v3) {
						return $author$project$Vector2$TupleVec2(
							A2($author$project$AltMath$Alternative$Tuple$Vector2$vec2, x, y));
					})
				]));
	});
var $author$project$Vector2$all = function (_v0) {
	var x1 = _v0.aa;
	var x2 = _v0.ab;
	var y1 = _v0.ac;
	var y2 = _v0.ad;
	var y2_ = A2(
		$elm$core$Maybe$withDefault,
		10,
		$elm$core$String$toFloat(y2));
	var y1_ = A2(
		$elm$core$Maybe$withDefault,
		1,
		$elm$core$String$toFloat(y1));
	var x2_ = A2(
		$elm$core$Maybe$withDefault,
		10,
		$elm$core$String$toFloat(x2));
	var x1_ = A2(
		$elm$core$Maybe$withDefault,
		1,
		$elm$core$String$toFloat(x1));
	var record = {ck: x1_, cl: y1_};
	var data = {
		J: A2($author$project$AltMath$Alternative$ADT$Vector2$vec2, x1_, y1_),
		bc: A2($author$project$AltMath$Alternative$ADT$Vector2$vec2, x2_, y2_),
		K: A2($elm_explorations$linear_algebra$Math$Vector2$vec2, x1_, y1_),
		bg: A2($elm_explorations$linear_algebra$Math$Vector2$vec2, x2_, y2_),
		N: A2($author$project$AltMath$Alternative$Record$Vector2$vec2, x1_, y1_),
		bm: A2($author$project$AltMath$Alternative$Record$Vector2$vec2, x2_, y2_),
		O: A2($author$project$AltMath$Alternative$Tuple$Vector2$vec2, x1_, y1_),
		bt: A2($author$project$AltMath$Alternative$Tuple$Vector2$vec2, x2_, y2_)
	};
	return _List_fromArray(
		[
			A2($author$project$Vector2$vec2, x1_, y1_),
			$author$project$Vector2$add(data),
			$author$project$Vector2$direction(data),
			$author$project$Vector2$distance(data),
			$author$project$Vector2$distanceSquared(data),
			$author$project$Vector2$dot(data),
			$author$project$Vector2$fromRecord(record),
			$author$project$Vector2$getX(data),
			$author$project$Vector2$getY(data),
			$author$project$Vector2$length(data),
			$author$project$Vector2$lengthSquared(data),
			$author$project$Vector2$mul(data),
			$author$project$Vector2$negate(data),
			$author$project$Vector2$normalize(data),
			$author$project$Vector2$scale(data),
			A2($author$project$Vector2$setX, data, x2_),
			A2($author$project$Vector2$setY, data, y2_),
			$author$project$Vector2$sub(data),
			$author$project$Vector2$toRecord(data)
		]);
};
var $author$project$Vector3$AdtVec3 = function (a) {
	return {$: 1, a: a};
};
var $author$project$Vector3$GLVec3 = function (a) {
	return {$: 0, a: a};
};
var $author$project$Vector3$RecordVec3 = function (a) {
	return {$: 2, a: a};
};
var $author$project$Vector3$TupleVec3 = function (a) {
	return {$: 3, a: a};
};
var $author$project$AltMath$Alternative$ADT$Vector3$add = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var az = _v0.c;
		var bx = _v1.a;
		var by = _v1.b;
		var bz = _v1.c;
		return A3($author$project$AltMath$Alternative$ADT$Vector3$Vec3, ax + bx, ay + by, az + bz);
	});
var $author$project$AltMath$Alternative$Record$Vector3$add = F2(
	function (a, b) {
		return A3($author$project$AltMath$Alternative$Record$Vector3$Vec3, a.ck + b.ck, a.cl + b.cl, a.cn + b.cn);
	});
var $author$project$AltMath$Alternative$Tuple$Vector3$add = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var az = _v0.c;
		var bx = _v1.a;
		var by = _v1.b;
		var bz = _v1.c;
		return _Utils_Tuple3(ax + bx, ay + by, az + bz);
	});
var $elm_explorations$linear_algebra$Math$Vector3$add = _MJS_v3add;
var $author$project$Vector3$add = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	var glVec2 = _v0.bg;
	var adtVec2 = _v0.bc;
	var recVec2 = _v0.bm;
	var tupleVec2 = _v0.bt;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'add',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Vector3$GLVec3(
						A2($elm_explorations$linear_algebra$Math$Vector3$add, glVec1, glVec2));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Vector3$AdtVec3(
						A2($author$project$AltMath$Alternative$ADT$Vector3$add, adtVec1, adtVec2));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Vector3$RecordVec3(
						A2($author$project$AltMath$Alternative$Record$Vector3$add, recVec1, recVec2));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Vector3$TupleVec3(
						A2($author$project$AltMath$Alternative$Tuple$Vector3$add, tupleVec1, tupleVec2));
				})
			]));
};
var $elm_explorations$linear_algebra$Math$Vector3$cross = _MJS_v3cross;
var $author$project$Vector3$cross = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	var glVec2 = _v0.bg;
	var adtVec2 = _v0.bc;
	var recVec2 = _v0.bm;
	var tupleVec2 = _v0.bt;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'cross',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Vector3$GLVec3(
						A2($elm_explorations$linear_algebra$Math$Vector3$cross, glVec1, glVec2));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Vector3$AdtVec3(
						A2($author$project$AltMath$Alternative$ADT$Vector3$cross, adtVec1, adtVec2));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Vector3$RecordVec3(
						A2($author$project$AltMath$Alternative$Record$Vector3$cross, recVec1, recVec2));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Vector3$TupleVec3(
						A2($author$project$AltMath$Alternative$Tuple$Vector3$cross, tupleVec1, tupleVec2));
				})
			]));
};
var $elm_explorations$linear_algebra$Math$Vector3$direction = _MJS_v3direction;
var $author$project$Vector3$direction = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	var glVec2 = _v0.bg;
	var adtVec2 = _v0.bc;
	var recVec2 = _v0.bm;
	var tupleVec2 = _v0.bt;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'direction',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Vector3$GLVec3(
						A2($elm_explorations$linear_algebra$Math$Vector3$direction, glVec1, glVec2));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Vector3$AdtVec3(
						A2($author$project$AltMath$Alternative$ADT$Vector3$direction, adtVec1, adtVec2));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Vector3$RecordVec3(
						A2($author$project$AltMath$Alternative$Record$Vector3$direction, recVec1, recVec2));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Vector3$TupleVec3(
						A2($author$project$AltMath$Alternative$Tuple$Vector3$direction, tupleVec1, tupleVec2));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector3$distance = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var az = _v0.c;
		var bx = _v1.a;
		var by = _v1.b;
		var bz = _v1.c;
		var z = az - bz;
		var y = ay - by;
		var x = ax - bx;
		return $elm$core$Basics$sqrt(((x * x) + (y * y)) + (z * z));
	});
var $author$project$AltMath$Alternative$Record$Vector3$distance = F2(
	function (a, b) {
		var z = a.cn - b.cn;
		var y = a.cl - b.cl;
		var x = a.ck - b.ck;
		return $elm$core$Basics$sqrt(((x * x) + (y * y)) + (z * z));
	});
var $author$project$AltMath$Alternative$Tuple$Vector3$distance = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var az = _v0.c;
		var bx = _v1.a;
		var by = _v1.b;
		var bz = _v1.c;
		var z = az - bz;
		var y = ay - by;
		var x = ax - bx;
		return $elm$core$Basics$sqrt(((x * x) + (y * y)) + (z * z));
	});
var $elm_explorations$linear_algebra$Math$Vector3$distance = _MJS_v3distance;
var $author$project$Vector3$distance = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	var glVec2 = _v0.bg;
	var adtVec2 = _v0.bc;
	var recVec2 = _v0.bm;
	var tupleVec2 = _v0.bt;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'distance',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return A2($elm_explorations$linear_algebra$Math$Vector3$distance, glVec1, glVec2);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return A2($author$project$AltMath$Alternative$ADT$Vector3$distance, adtVec1, adtVec2);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return A2($author$project$AltMath$Alternative$Record$Vector3$distance, recVec1, recVec2);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return A2($author$project$AltMath$Alternative$Tuple$Vector3$distance, tupleVec1, tupleVec2);
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector3$distanceSquared = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var az = _v0.c;
		var bx = _v1.a;
		var by = _v1.b;
		var bz = _v1.c;
		var z = az - bz;
		var y = ay - by;
		var x = ax - bx;
		return ((x * x) + (y * y)) + (z * z);
	});
var $author$project$AltMath$Alternative$Record$Vector3$distanceSquared = F2(
	function (a, b) {
		var z = a.cn - b.cn;
		var y = a.cl - b.cl;
		var x = a.ck - b.ck;
		return ((x * x) + (y * y)) + (z * z);
	});
var $author$project$AltMath$Alternative$Tuple$Vector3$distanceSquared = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var az = _v0.c;
		var bx = _v1.a;
		var by = _v1.b;
		var bz = _v1.c;
		var z = az - bz;
		var y = ay - by;
		var x = ax - bx;
		return ((x * x) + (y * y)) + (z * z);
	});
var $elm_explorations$linear_algebra$Math$Vector3$distanceSquared = _MJS_v3distanceSquared;
var $author$project$Vector3$distanceSquared = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	var glVec2 = _v0.bg;
	var adtVec2 = _v0.bc;
	var recVec2 = _v0.bm;
	var tupleVec2 = _v0.bt;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'distanceSquared',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return A2($elm_explorations$linear_algebra$Math$Vector3$distanceSquared, glVec1, glVec2);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return A2($author$project$AltMath$Alternative$ADT$Vector3$distanceSquared, adtVec1, adtVec2);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return A2($author$project$AltMath$Alternative$Record$Vector3$distanceSquared, recVec1, recVec2);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return A2($author$project$AltMath$Alternative$Tuple$Vector3$distanceSquared, tupleVec1, tupleVec2);
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector3$dot = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var az = _v0.c;
		var bx = _v1.a;
		var by = _v1.b;
		var bz = _v1.c;
		return ((ax * bx) + (ay * by)) + (az * bz);
	});
var $author$project$AltMath$Alternative$Record$Vector3$dot = F2(
	function (a, b) {
		return ((a.ck * b.ck) + (a.cl * b.cl)) + (a.cn * b.cn);
	});
var $author$project$AltMath$Alternative$Tuple$Vector3$dot = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var az = _v0.c;
		var bx = _v1.a;
		var by = _v1.b;
		var bz = _v1.c;
		return ((ax * bx) + (ay * by)) + (az * bz);
	});
var $elm_explorations$linear_algebra$Math$Vector3$dot = _MJS_v3dot;
var $author$project$Vector3$dot = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	var glVec2 = _v0.bg;
	var adtVec2 = _v0.bc;
	var recVec2 = _v0.bm;
	var tupleVec2 = _v0.bt;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'dot',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return A2($elm_explorations$linear_algebra$Math$Vector3$dot, glVec1, glVec2);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return A2($author$project$AltMath$Alternative$ADT$Vector3$dot, adtVec1, adtVec2);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return A2($author$project$AltMath$Alternative$Record$Vector3$dot, recVec1, recVec2);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return A2($author$project$AltMath$Alternative$Tuple$Vector3$dot, tupleVec1, tupleVec2);
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector3$fromRecord = function (_v0) {
	var x = _v0.ck;
	var y = _v0.cl;
	var z = _v0.cn;
	return A3($author$project$AltMath$Alternative$ADT$Vector3$Vec3, x, y, z);
};
var $author$project$AltMath$Alternative$Record$Vector3$fromRecord = $elm$core$Basics$identity;
var $author$project$AltMath$Alternative$Tuple$Vector3$fromRecord = function (_v0) {
	var x = _v0.ck;
	var y = _v0.cl;
	var z = _v0.cn;
	return _Utils_Tuple3(x, y, z);
};
var $elm_explorations$linear_algebra$Math$Vector3$fromRecord = _MJS_v3fromRecord;
var $author$project$Vector3$fromRecord = function (record) {
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'fromRecord',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v0) {
					return $author$project$Vector3$GLVec3(
						$elm_explorations$linear_algebra$Math$Vector3$fromRecord(record));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v1) {
					return $author$project$Vector3$AdtVec3(
						$author$project$AltMath$Alternative$ADT$Vector3$fromRecord(record));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v2) {
					return $author$project$Vector3$RecordVec3(
						$author$project$AltMath$Alternative$Record$Vector3$fromRecord(record));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v3) {
					return $author$project$Vector3$TupleVec3(
						$author$project$AltMath$Alternative$Tuple$Vector3$fromRecord(record));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector3$getX = function (_v0) {
	var x = _v0.a;
	return x;
};
var $author$project$AltMath$Alternative$Record$Vector3$getX = function ($) {
	return $.ck;
};
var $author$project$AltMath$Alternative$Tuple$Vector3$getX = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	var z = _v0.c;
	return x;
};
var $elm_explorations$linear_algebra$Math$Vector3$getX = _MJS_v3getX;
var $author$project$Vector3$getX = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'getX',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $elm_explorations$linear_algebra$Math$Vector3$getX(glVec1);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$AltMath$Alternative$ADT$Vector3$getX(adtVec1);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$AltMath$Alternative$Record$Vector3$getX(recVec1);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$AltMath$Alternative$Tuple$Vector3$getX(tupleVec1);
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector3$getY = function (_v0) {
	var y = _v0.b;
	return y;
};
var $author$project$AltMath$Alternative$Record$Vector3$getY = function ($) {
	return $.cl;
};
var $author$project$AltMath$Alternative$Tuple$Vector3$getY = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	var z = _v0.c;
	return y;
};
var $elm_explorations$linear_algebra$Math$Vector3$getY = _MJS_v3getY;
var $author$project$Vector3$getY = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'getY',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $elm_explorations$linear_algebra$Math$Vector3$getY(glVec1);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$AltMath$Alternative$ADT$Vector3$getY(adtVec1);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$AltMath$Alternative$Record$Vector3$getY(recVec1);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$AltMath$Alternative$Tuple$Vector3$getY(tupleVec1);
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector3$getZ = function (_v0) {
	var z = _v0.c;
	return z;
};
var $author$project$AltMath$Alternative$Record$Vector3$getZ = function ($) {
	return $.cn;
};
var $author$project$AltMath$Alternative$Tuple$Vector3$getZ = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	var z = _v0.c;
	return z;
};
var $elm_explorations$linear_algebra$Math$Vector3$getZ = _MJS_v3getZ;
var $author$project$Vector3$getZ = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'getZ',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $elm_explorations$linear_algebra$Math$Vector3$getZ(glVec1);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$AltMath$Alternative$ADT$Vector3$getZ(adtVec1);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$AltMath$Alternative$Record$Vector3$getZ(recVec1);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$AltMath$Alternative$Tuple$Vector3$getZ(tupleVec1);
				})
			]));
};
var $elm_explorations$linear_algebra$Math$Vector3$length = _MJS_v3length;
var $author$project$Vector3$length = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'length',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $elm_explorations$linear_algebra$Math$Vector3$length(glVec1);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$AltMath$Alternative$ADT$Vector3$length(adtVec1);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$AltMath$Alternative$Record$Vector3$length(recVec1);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$AltMath$Alternative$Tuple$Vector3$length(tupleVec1);
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector3$lengthSquared = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	var z = _v0.c;
	return ((x * x) + (y * y)) + (z * z);
};
var $author$project$AltMath$Alternative$Record$Vector3$lengthSquared = function (_v0) {
	var x = _v0.ck;
	var y = _v0.cl;
	var z = _v0.cn;
	return ((x * x) + (y * y)) + (z * z);
};
var $author$project$AltMath$Alternative$Tuple$Vector3$lengthSquared = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	var z = _v0.c;
	return ((x * x) + (y * y)) + (z * z);
};
var $elm_explorations$linear_algebra$Math$Vector3$lengthSquared = _MJS_v3lengthSquared;
var $author$project$Vector3$lengthSquared = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'lengthSquared',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $elm_explorations$linear_algebra$Math$Vector3$lengthSquared(glVec1);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$AltMath$Alternative$ADT$Vector3$lengthSquared(adtVec1);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$AltMath$Alternative$Record$Vector3$lengthSquared(recVec1);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$AltMath$Alternative$Tuple$Vector3$lengthSquared(tupleVec1);
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector3$negate = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	var z = _v0.c;
	return A3($author$project$AltMath$Alternative$ADT$Vector3$Vec3, -x, -y, -z);
};
var $author$project$AltMath$Alternative$Record$Vector3$negate = function (v3) {
	return A3($author$project$AltMath$Alternative$Record$Vector3$Vec3, -v3.ck, -v3.cl, -v3.cn);
};
var $author$project$AltMath$Alternative$Tuple$Vector3$negate = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	var z = _v0.c;
	return _Utils_Tuple3(-x, -y, -z);
};
var $elm_explorations$linear_algebra$Math$Vector3$negate = _MJS_v3negate;
var $author$project$Vector3$negate = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'negate',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Vector3$GLVec3(
						$elm_explorations$linear_algebra$Math$Vector3$negate(glVec1));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Vector3$AdtVec3(
						$author$project$AltMath$Alternative$ADT$Vector3$negate(adtVec1));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Vector3$RecordVec3(
						$author$project$AltMath$Alternative$Record$Vector3$negate(recVec1));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Vector3$TupleVec3(
						$author$project$AltMath$Alternative$Tuple$Vector3$negate(tupleVec1));
				})
			]));
};
var $elm_explorations$linear_algebra$Math$Vector3$normalize = _MJS_v3normalize;
var $author$project$Vector3$normalize = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'normalize',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Vector3$GLVec3(
						$elm_explorations$linear_algebra$Math$Vector3$normalize(glVec1));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Vector3$AdtVec3(
						$author$project$AltMath$Alternative$ADT$Vector3$normalize(adtVec1));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Vector3$RecordVec3(
						$author$project$AltMath$Alternative$Record$Vector3$normalize(recVec1));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Vector3$TupleVec3(
						$author$project$AltMath$Alternative$Tuple$Vector3$normalize(tupleVec1));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector3$scale = F2(
	function (s, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		var z = _v0.c;
		return A3($author$project$AltMath$Alternative$ADT$Vector3$Vec3, s * x, s * y, s * z);
	});
var $author$project$AltMath$Alternative$Record$Vector3$scale = F2(
	function (s, v3) {
		return A3($author$project$AltMath$Alternative$Record$Vector3$Vec3, s * v3.ck, s * v3.cl, s * v3.cn);
	});
var $author$project$AltMath$Alternative$Tuple$Vector3$scale = F2(
	function (s, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		var z = _v0.c;
		return _Utils_Tuple3(s * x, s * y, s * z);
	});
var $elm_explorations$linear_algebra$Math$Vector3$scale = _MJS_v3scale;
var $author$project$Vector3$scale = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'scale',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Vector3$GLVec3(
						A2($elm_explorations$linear_algebra$Math$Vector3$scale, 2, glVec1));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Vector3$AdtVec3(
						A2($author$project$AltMath$Alternative$ADT$Vector3$scale, 2, adtVec1));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Vector3$RecordVec3(
						A2($author$project$AltMath$Alternative$Record$Vector3$scale, 2, recVec1));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Vector3$TupleVec3(
						A2($author$project$AltMath$Alternative$Tuple$Vector3$scale, 2, tupleVec1));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector3$setX = F2(
	function (x, _v0) {
		var y = _v0.b;
		var z = _v0.c;
		return A3($author$project$AltMath$Alternative$ADT$Vector3$Vec3, x, y, z);
	});
var $author$project$AltMath$Alternative$Record$Vector3$setX = F2(
	function (x, _v0) {
		var y = _v0.cl;
		var z = _v0.cn;
		return {ck: x, cl: y, cn: z};
	});
var $author$project$AltMath$Alternative$Tuple$Vector3$setX = F2(
	function (x, _v0) {
		var y = _v0.b;
		var z = _v0.c;
		return _Utils_Tuple3(x, y, z);
	});
var $elm_explorations$linear_algebra$Math$Vector3$setX = _MJS_v3setX;
var $author$project$Vector3$setX = F2(
	function (_v0, a) {
		var glVec1 = _v0.K;
		var adtVec1 = _v0.J;
		var recVec1 = _v0.N;
		var tupleVec1 = _v0.O;
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'setX',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v1) {
						return $author$project$Vector3$GLVec3(
							A2($elm_explorations$linear_algebra$Math$Vector3$setX, a, glVec1));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v2) {
						return $author$project$Vector3$AdtVec3(
							A2($author$project$AltMath$Alternative$ADT$Vector3$setX, a, adtVec1));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v3) {
						return $author$project$Vector3$RecordVec3(
							A2($author$project$AltMath$Alternative$Record$Vector3$setX, a, recVec1));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v4) {
						return $author$project$Vector3$TupleVec3(
							A2($author$project$AltMath$Alternative$Tuple$Vector3$setX, a, tupleVec1));
					})
				]));
	});
var $author$project$AltMath$Alternative$ADT$Vector3$setY = F2(
	function (y, _v0) {
		var x = _v0.a;
		var z = _v0.c;
		return A3($author$project$AltMath$Alternative$ADT$Vector3$Vec3, x, y, z);
	});
var $author$project$AltMath$Alternative$Record$Vector3$setY = F2(
	function (y, _v0) {
		var x = _v0.ck;
		var z = _v0.cn;
		return {ck: x, cl: y, cn: z};
	});
var $author$project$AltMath$Alternative$Tuple$Vector3$setY = F2(
	function (y, _v0) {
		var x = _v0.a;
		var z = _v0.c;
		return _Utils_Tuple3(x, y, z);
	});
var $elm_explorations$linear_algebra$Math$Vector3$setY = _MJS_v3setY;
var $author$project$Vector3$setY = F2(
	function (_v0, a) {
		var glVec1 = _v0.K;
		var adtVec1 = _v0.J;
		var recVec1 = _v0.N;
		var tupleVec1 = _v0.O;
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'setY',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v1) {
						return $author$project$Vector3$GLVec3(
							A2($elm_explorations$linear_algebra$Math$Vector3$setY, a, glVec1));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v2) {
						return $author$project$Vector3$AdtVec3(
							A2($author$project$AltMath$Alternative$ADT$Vector3$setY, a, adtVec1));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v3) {
						return $author$project$Vector3$RecordVec3(
							A2($author$project$AltMath$Alternative$Record$Vector3$setY, a, recVec1));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v4) {
						return $author$project$Vector3$TupleVec3(
							A2($author$project$AltMath$Alternative$Tuple$Vector3$setY, a, tupleVec1));
					})
				]));
	});
var $author$project$AltMath$Alternative$ADT$Vector3$setZ = F2(
	function (z, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return A3($author$project$AltMath$Alternative$ADT$Vector3$Vec3, x, y, z);
	});
var $author$project$AltMath$Alternative$Record$Vector3$setZ = F2(
	function (z, _v0) {
		var x = _v0.ck;
		var y = _v0.cl;
		return {ck: x, cl: y, cn: z};
	});
var $author$project$AltMath$Alternative$Tuple$Vector3$setZ = F2(
	function (z, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple3(x, y, z);
	});
var $elm_explorations$linear_algebra$Math$Vector3$setZ = _MJS_v3setZ;
var $author$project$Vector3$setZ = F2(
	function (_v0, a) {
		var glVec1 = _v0.K;
		var adtVec1 = _v0.J;
		var recVec1 = _v0.N;
		var tupleVec1 = _v0.O;
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'setZ',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v1) {
						return $author$project$Vector3$GLVec3(
							A2($elm_explorations$linear_algebra$Math$Vector3$setZ, a, glVec1));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v2) {
						return $author$project$Vector3$AdtVec3(
							A2($author$project$AltMath$Alternative$ADT$Vector3$setZ, a, adtVec1));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v3) {
						return $author$project$Vector3$RecordVec3(
							A2($author$project$AltMath$Alternative$Record$Vector3$setZ, a, recVec1));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v4) {
						return $author$project$Vector3$TupleVec3(
							A2($author$project$AltMath$Alternative$Tuple$Vector3$setZ, a, tupleVec1));
					})
				]));
	});
var $elm_explorations$linear_algebra$Math$Vector3$sub = _MJS_v3sub;
var $author$project$Vector3$sub = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	var glVec2 = _v0.bg;
	var adtVec2 = _v0.bc;
	var recVec2 = _v0.bm;
	var tupleVec2 = _v0.bt;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'sub',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Vector3$GLVec3(
						A2($elm_explorations$linear_algebra$Math$Vector3$sub, glVec1, glVec2));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Vector3$AdtVec3(
						A2($author$project$AltMath$Alternative$ADT$Vector3$sub, adtVec1, adtVec2));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Vector3$RecordVec3(
						A2($author$project$AltMath$Alternative$Record$Vector3$sub, recVec1, recVec2));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Vector3$TupleVec3(
						A2($author$project$AltMath$Alternative$Tuple$Vector3$sub, tupleVec1, tupleVec2));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector3$toRecord = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	var z = _v0.c;
	return {ck: x, cl: y, cn: z};
};
var $author$project$AltMath$Alternative$Record$Vector3$toRecord = $elm$core$Basics$identity;
var $author$project$AltMath$Alternative$Tuple$Vector3$toRecord = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	var z = _v0.c;
	return {ck: x, cl: y, cn: z};
};
var $elm_explorations$linear_algebra$Math$Vector3$toRecord = _MJS_v3toRecord;
var $author$project$Vector3$toRecord = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'toRecord',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $elm_explorations$linear_algebra$Math$Vector3$toRecord(glVec1);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$AltMath$Alternative$ADT$Vector3$toRecord(adtVec1);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$AltMath$Alternative$Record$Vector3$toRecord(recVec1);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$AltMath$Alternative$Tuple$Vector3$toRecord(tupleVec1);
				})
			]));
};
var $author$project$Vector3$vec3 = F3(
	function (x, y, z) {
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'vec3',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v0) {
						return $author$project$Vector3$GLVec3(
							A3($elm_explorations$linear_algebra$Math$Vector3$vec3, x, y, z));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v1) {
						return $author$project$Vector3$AdtVec3(
							A3($author$project$AltMath$Alternative$ADT$Vector3$vec3, x, y, z));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v2) {
						return $author$project$Vector3$RecordVec3(
							A3($author$project$AltMath$Alternative$Record$Vector3$vec3, x, y, z));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v3) {
						return $author$project$Vector3$TupleVec3(
							A3($author$project$AltMath$Alternative$Tuple$Vector3$vec3, x, y, z));
					})
				]));
	});
var $author$project$Vector3$all = function (_v0) {
	var x1 = _v0.aa;
	var x2 = _v0.ab;
	var y1 = _v0.ac;
	var y2 = _v0.ad;
	var z1 = _v0.aH;
	var z2 = _v0.aI;
	var z2_ = A2(
		$elm$core$Maybe$withDefault,
		10,
		$elm$core$String$toFloat(z2));
	var z1_ = A2(
		$elm$core$Maybe$withDefault,
		1,
		$elm$core$String$toFloat(z1));
	var y2_ = A2(
		$elm$core$Maybe$withDefault,
		10,
		$elm$core$String$toFloat(y2));
	var y1_ = A2(
		$elm$core$Maybe$withDefault,
		1,
		$elm$core$String$toFloat(y1));
	var x2_ = A2(
		$elm$core$Maybe$withDefault,
		10,
		$elm$core$String$toFloat(x2));
	var x1_ = A2(
		$elm$core$Maybe$withDefault,
		1,
		$elm$core$String$toFloat(x1));
	var record = {ck: x1_, cl: y1_, cn: z1_};
	var data = {
		J: A3($author$project$AltMath$Alternative$ADT$Vector3$vec3, x1_, y1_, z1_),
		bc: A3($author$project$AltMath$Alternative$ADT$Vector3$vec3, x2_, y2_, z2_),
		K: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, x1_, y1_, z1_),
		bg: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, x2_, y2_, z2_),
		N: A3($author$project$AltMath$Alternative$Record$Vector3$vec3, x1_, y1_, z1_),
		bm: A3($author$project$AltMath$Alternative$Record$Vector3$vec3, x2_, y2_, z2_),
		O: A3($author$project$AltMath$Alternative$Tuple$Vector3$vec3, x1_, y1_, z1_),
		bt: A3($author$project$AltMath$Alternative$Tuple$Vector3$vec3, x2_, y2_, z2_)
	};
	return _List_fromArray(
		[
			A3($author$project$Vector3$vec3, x1_, y1_, z1_),
			$author$project$Vector3$add(data),
			$author$project$Vector3$cross(data),
			$author$project$Vector3$direction(data),
			$author$project$Vector3$distance(data),
			$author$project$Vector3$distanceSquared(data),
			$author$project$Vector3$dot(data),
			$author$project$Vector3$fromRecord(record),
			$author$project$Vector3$getX(data),
			$author$project$Vector3$getY(data),
			$author$project$Vector3$getZ(data),
			$author$project$Vector3$length(data),
			$author$project$Vector3$lengthSquared(data),
			$author$project$Vector3$negate(data),
			$author$project$Vector3$normalize(data),
			$author$project$Vector3$scale(data),
			A2($author$project$Vector3$setX, data, x1_),
			A2($author$project$Vector3$setY, data, y1_),
			A2($author$project$Vector3$setZ, data, z1_),
			$author$project$Vector3$sub(data),
			$author$project$Vector3$toRecord(data)
		]);
};
var $author$project$Vector4$AdtVec4 = function (a) {
	return {$: 1, a: a};
};
var $author$project$Vector4$GLVec4 = function (a) {
	return {$: 0, a: a};
};
var $author$project$Vector4$RecordVec4 = function (a) {
	return {$: 2, a: a};
};
var $author$project$Vector4$TupleVec4 = function (a) {
	return {$: 3, a: a};
};
var $author$project$AltMath$Alternative$ADT$Vector4$Vec4 = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $author$project$AltMath$Alternative$ADT$Vector4$add = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var az = _v0.c;
		var aw = _v0.d;
		var bx = _v1.a;
		var by = _v1.b;
		var bz = _v1.c;
		var bw = _v1.d;
		return A4($author$project$AltMath$Alternative$ADT$Vector4$Vec4, ax + bx, ay + by, az + bz, aw + bw);
	});
var $author$project$AltMath$Alternative$Record$Vector4$Vec4 = F4(
	function (x, y, z, w) {
		return {ch: w, ck: x, cl: y, cn: z};
	});
var $author$project$AltMath$Alternative$Record$Vector4$add = F2(
	function (a, b) {
		return A4($author$project$AltMath$Alternative$Record$Vector4$Vec4, a.ck + b.ck, a.cl + b.cl, a.cn + b.cn, a.ch + b.ch);
	});
var $author$project$AltMath$Alternative$Tuple$Vector4$add = F2(
	function (_v0, _v3) {
		var _v1 = _v0.a;
		var ax = _v1.a;
		var ay = _v1.b;
		var _v2 = _v0.b;
		var az = _v2.a;
		var aw = _v2.b;
		var _v4 = _v3.a;
		var bx = _v4.a;
		var by = _v4.b;
		var _v5 = _v3.b;
		var bz = _v5.a;
		var bw = _v5.b;
		return _Utils_Tuple2(
			_Utils_Tuple2(ax + bx, ay + by),
			_Utils_Tuple2(az + bz, aw + bw));
	});
var $elm_explorations$linear_algebra$Math$Vector4$add = _MJS_v4add;
var $author$project$Vector4$add = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	var glVec2 = _v0.bg;
	var adtVec2 = _v0.bc;
	var recVec2 = _v0.bm;
	var tupleVec2 = _v0.bt;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'add',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Vector4$GLVec4(
						A2($elm_explorations$linear_algebra$Math$Vector4$add, glVec1, glVec2));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Vector4$AdtVec4(
						A2($author$project$AltMath$Alternative$ADT$Vector4$add, adtVec1, adtVec2));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Vector4$RecordVec4(
						A2($author$project$AltMath$Alternative$Record$Vector4$add, recVec1, recVec2));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Vector4$TupleVec4(
						A2($author$project$AltMath$Alternative$Tuple$Vector4$add, tupleVec1, tupleVec2));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector4$length = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	var z = _v0.c;
	var w = _v0.d;
	return $elm$core$Basics$sqrt((((x * x) + (y * y)) + (z * z)) + (w * w));
};
var $author$project$AltMath$Alternative$ADT$Vector4$sub = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var az = _v0.c;
		var aw = _v0.d;
		var bx = _v1.a;
		var by = _v1.b;
		var bz = _v1.c;
		var bw = _v1.d;
		return A4($author$project$AltMath$Alternative$ADT$Vector4$Vec4, ax - bx, ay - by, az - bz, aw - bw);
	});
var $author$project$AltMath$Alternative$ADT$Vector4$direction = F2(
	function (a, b) {
		var c = A2($author$project$AltMath$Alternative$ADT$Vector4$sub, a, b);
		var x = c.a;
		var y = c.b;
		var z = c.c;
		var w = c.d;
		var len = $author$project$AltMath$Alternative$ADT$Vector4$length(c);
		return A4($author$project$AltMath$Alternative$ADT$Vector4$Vec4, x / len, y / len, z / len, w / len);
	});
var $author$project$AltMath$Alternative$Record$Vector4$length = function (_v0) {
	var x = _v0.ck;
	var y = _v0.cl;
	var z = _v0.cn;
	var w = _v0.ch;
	return $elm$core$Basics$sqrt((((x * x) + (y * y)) + (z * z)) + (w * w));
};
var $author$project$AltMath$Alternative$Record$Vector4$sub = F2(
	function (a, b) {
		return A4($author$project$AltMath$Alternative$Record$Vector4$Vec4, a.ck - b.ck, a.cl - b.cl, a.cn - b.cn, a.ch - b.ch);
	});
var $author$project$AltMath$Alternative$Record$Vector4$direction = F2(
	function (a, b) {
		var c = A2($author$project$AltMath$Alternative$Record$Vector4$sub, a, b);
		var len = $author$project$AltMath$Alternative$Record$Vector4$length(c);
		return A4($author$project$AltMath$Alternative$Record$Vector4$Vec4, c.ck / len, c.cl / len, c.cn / len, c.ch / len);
	});
var $author$project$AltMath$Alternative$Tuple$Vector4$length = function (_v0) {
	var _v1 = _v0.a;
	var x = _v1.a;
	var y = _v1.b;
	var _v2 = _v0.b;
	var z = _v2.a;
	var w = _v2.b;
	return $elm$core$Basics$sqrt((((x * x) + (y * y)) + (z * z)) + (w * w));
};
var $author$project$AltMath$Alternative$Tuple$Vector4$sub = F2(
	function (_v0, _v3) {
		var _v1 = _v0.a;
		var ax = _v1.a;
		var ay = _v1.b;
		var _v2 = _v0.b;
		var az = _v2.a;
		var aw = _v2.b;
		var _v4 = _v3.a;
		var bx = _v4.a;
		var by = _v4.b;
		var _v5 = _v3.b;
		var bz = _v5.a;
		var bw = _v5.b;
		return _Utils_Tuple2(
			_Utils_Tuple2(ax - bx, ay - by),
			_Utils_Tuple2(az - bz, aw - bw));
	});
var $author$project$AltMath$Alternative$Tuple$Vector4$direction = F2(
	function (a, b) {
		var c = A2($author$project$AltMath$Alternative$Tuple$Vector4$sub, a, b);
		var _v0 = c.a;
		var x = _v0.a;
		var y = _v0.b;
		var _v1 = c.b;
		var z = _v1.a;
		var w = _v1.b;
		var len = $author$project$AltMath$Alternative$Tuple$Vector4$length(c);
		return _Utils_Tuple2(
			_Utils_Tuple2(x / len, y / len),
			_Utils_Tuple2(z / len, w / len));
	});
var $elm_explorations$linear_algebra$Math$Vector4$direction = _MJS_v4direction;
var $author$project$Vector4$direction = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	var glVec2 = _v0.bg;
	var adtVec2 = _v0.bc;
	var recVec2 = _v0.bm;
	var tupleVec2 = _v0.bt;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'direction',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Vector4$GLVec4(
						A2($elm_explorations$linear_algebra$Math$Vector4$direction, glVec1, glVec2));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Vector4$AdtVec4(
						A2($author$project$AltMath$Alternative$ADT$Vector4$direction, adtVec1, adtVec2));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Vector4$RecordVec4(
						A2($author$project$AltMath$Alternative$Record$Vector4$direction, recVec1, recVec2));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Vector4$TupleVec4(
						A2($author$project$AltMath$Alternative$Tuple$Vector4$direction, tupleVec1, tupleVec2));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector4$distance = F2(
	function (a, b) {
		return $author$project$AltMath$Alternative$ADT$Vector4$length(
			A2($author$project$AltMath$Alternative$ADT$Vector4$sub, a, b));
	});
var $author$project$AltMath$Alternative$Record$Vector4$distance = F2(
	function (a, b) {
		return $author$project$AltMath$Alternative$Record$Vector4$length(
			A2($author$project$AltMath$Alternative$Record$Vector4$sub, a, b));
	});
var $author$project$AltMath$Alternative$Tuple$Vector4$distance = F2(
	function (a, b) {
		return $author$project$AltMath$Alternative$Tuple$Vector4$length(
			A2($author$project$AltMath$Alternative$Tuple$Vector4$sub, a, b));
	});
var $elm_explorations$linear_algebra$Math$Vector4$distance = _MJS_v4distance;
var $author$project$Vector4$distance = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	var glVec2 = _v0.bg;
	var adtVec2 = _v0.bc;
	var recVec2 = _v0.bm;
	var tupleVec2 = _v0.bt;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'distance',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return A2($elm_explorations$linear_algebra$Math$Vector4$distance, glVec1, glVec2);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return A2($author$project$AltMath$Alternative$ADT$Vector4$distance, adtVec1, adtVec2);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return A2($author$project$AltMath$Alternative$Record$Vector4$distance, recVec1, recVec2);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return A2($author$project$AltMath$Alternative$Tuple$Vector4$distance, tupleVec1, tupleVec2);
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector4$lengthSquared = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	var z = _v0.c;
	var w = _v0.d;
	return (((x * x) + (y * y)) + (z * z)) + (w * w);
};
var $author$project$AltMath$Alternative$ADT$Vector4$distanceSquared = F2(
	function (a, b) {
		return $author$project$AltMath$Alternative$ADT$Vector4$lengthSquared(
			A2($author$project$AltMath$Alternative$ADT$Vector4$sub, a, b));
	});
var $author$project$AltMath$Alternative$Record$Vector4$lengthSquared = function (_v0) {
	var x = _v0.ck;
	var y = _v0.cl;
	var z = _v0.cn;
	var w = _v0.ch;
	return (((x * x) + (y * y)) + (z * z)) + (w * w);
};
var $author$project$AltMath$Alternative$Record$Vector4$distanceSquared = F2(
	function (a, b) {
		return $author$project$AltMath$Alternative$Record$Vector4$lengthSquared(
			A2($author$project$AltMath$Alternative$Record$Vector4$sub, a, b));
	});
var $author$project$AltMath$Alternative$Tuple$Vector4$lengthSquared = function (_v0) {
	var _v1 = _v0.a;
	var x = _v1.a;
	var y = _v1.b;
	var _v2 = _v0.b;
	var z = _v2.a;
	var w = _v2.b;
	return (((x * x) + (y * y)) + (z * z)) + (w * w);
};
var $author$project$AltMath$Alternative$Tuple$Vector4$distanceSquared = F2(
	function (a, b) {
		return $author$project$AltMath$Alternative$Tuple$Vector4$lengthSquared(
			A2($author$project$AltMath$Alternative$Tuple$Vector4$sub, a, b));
	});
var $elm_explorations$linear_algebra$Math$Vector4$distanceSquared = _MJS_v4distanceSquared;
var $author$project$Vector4$distanceSquared = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	var glVec2 = _v0.bg;
	var adtVec2 = _v0.bc;
	var recVec2 = _v0.bm;
	var tupleVec2 = _v0.bt;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'distanceSquared',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return A2($elm_explorations$linear_algebra$Math$Vector4$distanceSquared, glVec1, glVec2);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return A2($author$project$AltMath$Alternative$ADT$Vector4$distanceSquared, adtVec1, adtVec2);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return A2($author$project$AltMath$Alternative$Record$Vector4$distanceSquared, recVec1, recVec2);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return A2($author$project$AltMath$Alternative$Tuple$Vector4$distanceSquared, tupleVec1, tupleVec2);
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector4$dot = F2(
	function (_v0, _v1) {
		var ax = _v0.a;
		var ay = _v0.b;
		var az = _v0.c;
		var aw = _v0.d;
		var bx = _v1.a;
		var by = _v1.b;
		var bz = _v1.c;
		var bw = _v1.d;
		return (((ax * bx) + (ay * by)) + (az * bz)) + (aw * bw);
	});
var $author$project$AltMath$Alternative$Record$Vector4$dot = F2(
	function (a, b) {
		return (((a.ck * b.ck) + (a.cl * b.cl)) + (a.cn * b.cn)) + (a.ch * b.ch);
	});
var $author$project$AltMath$Alternative$Tuple$Vector4$dot = F2(
	function (_v0, _v3) {
		var _v1 = _v0.a;
		var ax = _v1.a;
		var ay = _v1.b;
		var _v2 = _v0.b;
		var az = _v2.a;
		var aw = _v2.b;
		var _v4 = _v3.a;
		var bx = _v4.a;
		var by = _v4.b;
		var _v5 = _v3.b;
		var bz = _v5.a;
		var bw = _v5.b;
		return (((ax * bx) + (ay * by)) + (az * bz)) + (aw * bw);
	});
var $elm_explorations$linear_algebra$Math$Vector4$dot = _MJS_v4dot;
var $author$project$Vector4$dot = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	var glVec2 = _v0.bg;
	var adtVec2 = _v0.bc;
	var recVec2 = _v0.bm;
	var tupleVec2 = _v0.bt;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'dot',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return A2($elm_explorations$linear_algebra$Math$Vector4$dot, glVec1, glVec2);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return A2($author$project$AltMath$Alternative$ADT$Vector4$dot, adtVec1, adtVec2);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return A2($author$project$AltMath$Alternative$Record$Vector4$dot, recVec1, recVec2);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return A2($author$project$AltMath$Alternative$Tuple$Vector4$dot, tupleVec1, tupleVec2);
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector4$fromRecord = function (_v0) {
	var x = _v0.ck;
	var y = _v0.cl;
	var z = _v0.cn;
	var w = _v0.ch;
	return A4($author$project$AltMath$Alternative$ADT$Vector4$Vec4, x, y, z, w);
};
var $author$project$AltMath$Alternative$Record$Vector4$fromRecord = $elm$core$Basics$identity;
var $author$project$AltMath$Alternative$Tuple$Vector4$fromRecord = function (_v0) {
	var x = _v0.ck;
	var y = _v0.cl;
	var z = _v0.cn;
	var w = _v0.ch;
	return _Utils_Tuple2(
		_Utils_Tuple2(x, y),
		_Utils_Tuple2(z, w));
};
var $elm_explorations$linear_algebra$Math$Vector4$fromRecord = _MJS_v4fromRecord;
var $author$project$Vector4$fromRecord = function (record) {
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'fromRecord',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v0) {
					return $author$project$Vector4$GLVec4(
						$elm_explorations$linear_algebra$Math$Vector4$fromRecord(record));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v1) {
					return $author$project$Vector4$AdtVec4(
						$author$project$AltMath$Alternative$ADT$Vector4$fromRecord(record));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v2) {
					return $author$project$Vector4$RecordVec4(
						$author$project$AltMath$Alternative$Record$Vector4$fromRecord(record));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v3) {
					return $author$project$Vector4$TupleVec4(
						$author$project$AltMath$Alternative$Tuple$Vector4$fromRecord(record));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector4$getW = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	var z = _v0.c;
	var w = _v0.d;
	return w;
};
var $author$project$AltMath$Alternative$Record$Vector4$getW = function ($) {
	return $.ch;
};
var $author$project$AltMath$Alternative$Tuple$Vector4$getW = function (_v0) {
	var _v1 = _v0.a;
	var x = _v1.a;
	var y = _v1.b;
	var _v2 = _v0.b;
	var z = _v2.a;
	var w = _v2.b;
	return w;
};
var $elm_explorations$linear_algebra$Math$Vector4$getW = _MJS_v4getW;
var $author$project$Vector4$getW = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'getW',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $elm_explorations$linear_algebra$Math$Vector4$getW(glVec1);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$AltMath$Alternative$ADT$Vector4$getW(adtVec1);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$AltMath$Alternative$Record$Vector4$getW(recVec1);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$AltMath$Alternative$Tuple$Vector4$getW(tupleVec1);
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector4$getX = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	var z = _v0.c;
	var w = _v0.d;
	return x;
};
var $author$project$AltMath$Alternative$Record$Vector4$getX = function ($) {
	return $.ck;
};
var $author$project$AltMath$Alternative$Tuple$Vector4$getX = function (_v0) {
	var _v1 = _v0.a;
	var x = _v1.a;
	var y = _v1.b;
	var _v2 = _v0.b;
	var z = _v2.a;
	var w = _v2.b;
	return x;
};
var $elm_explorations$linear_algebra$Math$Vector4$getX = _MJS_v4getX;
var $author$project$Vector4$getX = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'getX',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $elm_explorations$linear_algebra$Math$Vector4$getX(glVec1);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$AltMath$Alternative$ADT$Vector4$getX(adtVec1);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$AltMath$Alternative$Record$Vector4$getX(recVec1);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$AltMath$Alternative$Tuple$Vector4$getX(tupleVec1);
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector4$getY = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	var z = _v0.c;
	var w = _v0.d;
	return y;
};
var $author$project$AltMath$Alternative$Record$Vector4$getY = function ($) {
	return $.cl;
};
var $author$project$AltMath$Alternative$Tuple$Vector4$getY = function (_v0) {
	var _v1 = _v0.a;
	var x = _v1.a;
	var y = _v1.b;
	var _v2 = _v0.b;
	var z = _v2.a;
	var w = _v2.b;
	return y;
};
var $elm_explorations$linear_algebra$Math$Vector4$getY = _MJS_v4getY;
var $author$project$Vector4$getY = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'getY',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $elm_explorations$linear_algebra$Math$Vector4$getY(glVec1);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$AltMath$Alternative$ADT$Vector4$getY(adtVec1);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$AltMath$Alternative$Record$Vector4$getY(recVec1);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$AltMath$Alternative$Tuple$Vector4$getY(tupleVec1);
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector4$getZ = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	var z = _v0.c;
	var w = _v0.d;
	return z;
};
var $author$project$AltMath$Alternative$Record$Vector4$getZ = function ($) {
	return $.cn;
};
var $author$project$AltMath$Alternative$Tuple$Vector4$getZ = function (_v0) {
	var _v1 = _v0.a;
	var x = _v1.a;
	var y = _v1.b;
	var _v2 = _v0.b;
	var z = _v2.a;
	var w = _v2.b;
	return z;
};
var $elm_explorations$linear_algebra$Math$Vector4$getZ = _MJS_v4getZ;
var $author$project$Vector4$getZ = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'getZ',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $elm_explorations$linear_algebra$Math$Vector4$getZ(glVec1);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$AltMath$Alternative$ADT$Vector4$getZ(adtVec1);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$AltMath$Alternative$Record$Vector4$getZ(recVec1);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$AltMath$Alternative$Tuple$Vector4$getZ(tupleVec1);
				})
			]));
};
var $elm_explorations$linear_algebra$Math$Vector4$length = _MJS_v4length;
var $author$project$Vector4$length = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'length',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $elm_explorations$linear_algebra$Math$Vector4$length(glVec1);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$AltMath$Alternative$ADT$Vector4$length(adtVec1);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$AltMath$Alternative$Record$Vector4$length(recVec1);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$AltMath$Alternative$Tuple$Vector4$length(tupleVec1);
				})
			]));
};
var $elm_explorations$linear_algebra$Math$Vector4$lengthSquared = _MJS_v4lengthSquared;
var $author$project$Vector4$lengthSquared = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'lengthSquared',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $elm_explorations$linear_algebra$Math$Vector4$lengthSquared(glVec1);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$AltMath$Alternative$ADT$Vector4$lengthSquared(adtVec1);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$AltMath$Alternative$Record$Vector4$lengthSquared(recVec1);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$AltMath$Alternative$Tuple$Vector4$lengthSquared(tupleVec1);
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector4$negate = function (_v0) {
	var ax = _v0.a;
	var ay = _v0.b;
	var az = _v0.c;
	var aw = _v0.d;
	return A4($author$project$AltMath$Alternative$ADT$Vector4$Vec4, -ax, -ay, -az, -aw);
};
var $author$project$AltMath$Alternative$Record$Vector4$negate = function (a) {
	return A4($author$project$AltMath$Alternative$Record$Vector4$Vec4, -a.ck, -a.cl, -a.cn, -a.ch);
};
var $author$project$AltMath$Alternative$Tuple$Vector4$negate = function (_v0) {
	var _v1 = _v0.a;
	var x = _v1.a;
	var y = _v1.b;
	var _v2 = _v0.b;
	var z = _v2.a;
	var w = _v2.b;
	return _Utils_Tuple2(
		_Utils_Tuple2(-x, -y),
		_Utils_Tuple2(-z, -w));
};
var $elm_explorations$linear_algebra$Math$Vector4$negate = _MJS_v4negate;
var $author$project$Vector4$negate = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'negate',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Vector4$GLVec4(
						$elm_explorations$linear_algebra$Math$Vector4$negate(glVec1));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Vector4$AdtVec4(
						$author$project$AltMath$Alternative$ADT$Vector4$negate(adtVec1));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Vector4$RecordVec4(
						$author$project$AltMath$Alternative$Record$Vector4$negate(recVec1));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Vector4$TupleVec4(
						$author$project$AltMath$Alternative$Tuple$Vector4$negate(tupleVec1));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector4$normalize = function (v4) {
	var x = v4.a;
	var y = v4.b;
	var z = v4.c;
	var w = v4.d;
	var len = $author$project$AltMath$Alternative$ADT$Vector4$length(v4);
	return A4($author$project$AltMath$Alternative$ADT$Vector4$Vec4, x / len, y / len, z / len, w / len);
};
var $author$project$AltMath$Alternative$Record$Vector4$normalize = function (v4) {
	var len = $author$project$AltMath$Alternative$Record$Vector4$length(v4);
	return A4($author$project$AltMath$Alternative$Record$Vector4$Vec4, v4.ck / len, v4.cl / len, v4.cn / len, v4.ch / len);
};
var $author$project$AltMath$Alternative$Tuple$Vector4$normalize = function (v4) {
	var _v0 = v4.a;
	var x = _v0.a;
	var y = _v0.b;
	var _v1 = v4.b;
	var z = _v1.a;
	var w = _v1.b;
	var len = $author$project$AltMath$Alternative$Tuple$Vector4$length(v4);
	return _Utils_Tuple2(
		_Utils_Tuple2(x / len, y / len),
		_Utils_Tuple2(z / len, w / len));
};
var $elm_explorations$linear_algebra$Math$Vector4$normalize = _MJS_v4normalize;
var $author$project$Vector4$normalize = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'normalize',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Vector4$GLVec4(
						$elm_explorations$linear_algebra$Math$Vector4$normalize(glVec1));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Vector4$AdtVec4(
						$author$project$AltMath$Alternative$ADT$Vector4$normalize(adtVec1));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Vector4$RecordVec4(
						$author$project$AltMath$Alternative$Record$Vector4$normalize(recVec1));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Vector4$TupleVec4(
						$author$project$AltMath$Alternative$Tuple$Vector4$normalize(tupleVec1));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector4$scale = F2(
	function (s, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		var z = _v0.c;
		var w = _v0.d;
		return A4($author$project$AltMath$Alternative$ADT$Vector4$Vec4, s * x, s * y, s * z, s * w);
	});
var $author$project$AltMath$Alternative$Record$Vector4$scale = F2(
	function (s, v) {
		return A4($author$project$AltMath$Alternative$Record$Vector4$Vec4, s * v.ck, s * v.cl, s * v.cn, s * v.ch);
	});
var $author$project$AltMath$Alternative$Tuple$Vector4$scale = F2(
	function (s, _v0) {
		var _v1 = _v0.a;
		var x = _v1.a;
		var y = _v1.b;
		var _v2 = _v0.b;
		var z = _v2.a;
		var w = _v2.b;
		return _Utils_Tuple2(
			_Utils_Tuple2(s * x, s * y),
			_Utils_Tuple2(s * z, s * w));
	});
var $elm_explorations$linear_algebra$Math$Vector4$scale = _MJS_v4scale;
var $author$project$Vector4$scale = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'scale',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Vector4$GLVec4(
						A2($elm_explorations$linear_algebra$Math$Vector4$scale, 2, glVec1));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Vector4$AdtVec4(
						A2($author$project$AltMath$Alternative$ADT$Vector4$scale, 2, adtVec1));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Vector4$RecordVec4(
						A2($author$project$AltMath$Alternative$Record$Vector4$scale, 2, recVec1));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Vector4$TupleVec4(
						A2($author$project$AltMath$Alternative$Tuple$Vector4$scale, 2, tupleVec1));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector4$setW = F2(
	function (w, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		var z = _v0.c;
		return A4($author$project$AltMath$Alternative$ADT$Vector4$Vec4, x, y, z, w);
	});
var $author$project$AltMath$Alternative$Record$Vector4$setW = F2(
	function (w, _v0) {
		var x = _v0.ck;
		var y = _v0.cl;
		var z = _v0.cn;
		return {ch: w, ck: x, cl: y, cn: z};
	});
var $author$project$AltMath$Alternative$Tuple$Vector4$setW = F2(
	function (w, _v0) {
		var _v1 = _v0.a;
		var x = _v1.a;
		var y = _v1.b;
		var _v2 = _v0.b;
		var z = _v2.a;
		return _Utils_Tuple2(
			_Utils_Tuple2(x, y),
			_Utils_Tuple2(z, w));
	});
var $elm_explorations$linear_algebra$Math$Vector4$setW = _MJS_v4setW;
var $author$project$Vector4$setW = F2(
	function (_v0, a) {
		var glVec1 = _v0.K;
		var adtVec1 = _v0.J;
		var recVec1 = _v0.N;
		var tupleVec1 = _v0.O;
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'setW',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v1) {
						return $author$project$Vector4$GLVec4(
							A2($elm_explorations$linear_algebra$Math$Vector4$setW, a, glVec1));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v2) {
						return $author$project$Vector4$AdtVec4(
							A2($author$project$AltMath$Alternative$ADT$Vector4$setW, a, adtVec1));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v3) {
						return $author$project$Vector4$RecordVec4(
							A2($author$project$AltMath$Alternative$Record$Vector4$setW, a, recVec1));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v4) {
						return $author$project$Vector4$TupleVec4(
							A2($author$project$AltMath$Alternative$Tuple$Vector4$setW, a, tupleVec1));
					})
				]));
	});
var $author$project$AltMath$Alternative$ADT$Vector4$setX = F2(
	function (x, _v0) {
		var y = _v0.b;
		var z = _v0.c;
		var w = _v0.d;
		return A4($author$project$AltMath$Alternative$ADT$Vector4$Vec4, x, y, z, w);
	});
var $author$project$AltMath$Alternative$Record$Vector4$setX = F2(
	function (x, _v0) {
		var y = _v0.cl;
		var z = _v0.cn;
		var w = _v0.ch;
		return {ch: w, ck: x, cl: y, cn: z};
	});
var $author$project$AltMath$Alternative$Tuple$Vector4$setX = F2(
	function (x, _v0) {
		var _v1 = _v0.a;
		var y = _v1.b;
		var _v2 = _v0.b;
		var z = _v2.a;
		var w = _v2.b;
		return _Utils_Tuple2(
			_Utils_Tuple2(x, y),
			_Utils_Tuple2(z, w));
	});
var $elm_explorations$linear_algebra$Math$Vector4$setX = _MJS_v4setX;
var $author$project$Vector4$setX = F2(
	function (_v0, a) {
		var glVec1 = _v0.K;
		var adtVec1 = _v0.J;
		var recVec1 = _v0.N;
		var tupleVec1 = _v0.O;
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'setX',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v1) {
						return $author$project$Vector4$GLVec4(
							A2($elm_explorations$linear_algebra$Math$Vector4$setX, a, glVec1));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v2) {
						return $author$project$Vector4$AdtVec4(
							A2($author$project$AltMath$Alternative$ADT$Vector4$setX, a, adtVec1));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v3) {
						return $author$project$Vector4$RecordVec4(
							A2($author$project$AltMath$Alternative$Record$Vector4$setX, a, recVec1));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v4) {
						return $author$project$Vector4$TupleVec4(
							A2($author$project$AltMath$Alternative$Tuple$Vector4$setX, a, tupleVec1));
					})
				]));
	});
var $author$project$AltMath$Alternative$ADT$Vector4$setY = F2(
	function (y, _v0) {
		var x = _v0.a;
		var z = _v0.c;
		var w = _v0.d;
		return A4($author$project$AltMath$Alternative$ADT$Vector4$Vec4, x, y, z, w);
	});
var $author$project$AltMath$Alternative$Record$Vector4$setY = F2(
	function (y, _v0) {
		var x = _v0.ck;
		var z = _v0.cn;
		var w = _v0.ch;
		return {ch: w, ck: x, cl: y, cn: z};
	});
var $author$project$AltMath$Alternative$Tuple$Vector4$setY = F2(
	function (y, _v0) {
		var _v1 = _v0.a;
		var x = _v1.a;
		var _v2 = _v0.b;
		var z = _v2.a;
		var w = _v2.b;
		return _Utils_Tuple2(
			_Utils_Tuple2(x, y),
			_Utils_Tuple2(z, w));
	});
var $elm_explorations$linear_algebra$Math$Vector4$setY = _MJS_v4setY;
var $author$project$Vector4$setY = F2(
	function (_v0, a) {
		var glVec1 = _v0.K;
		var adtVec1 = _v0.J;
		var recVec1 = _v0.N;
		var tupleVec1 = _v0.O;
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'setY',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v1) {
						return $author$project$Vector4$GLVec4(
							A2($elm_explorations$linear_algebra$Math$Vector4$setY, a, glVec1));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v2) {
						return $author$project$Vector4$AdtVec4(
							A2($author$project$AltMath$Alternative$ADT$Vector4$setY, a, adtVec1));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v3) {
						return $author$project$Vector4$RecordVec4(
							A2($author$project$AltMath$Alternative$Record$Vector4$setY, a, recVec1));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v4) {
						return $author$project$Vector4$TupleVec4(
							A2($author$project$AltMath$Alternative$Tuple$Vector4$setY, a, tupleVec1));
					})
				]));
	});
var $author$project$AltMath$Alternative$ADT$Vector4$setZ = F2(
	function (z, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		var w = _v0.d;
		return A4($author$project$AltMath$Alternative$ADT$Vector4$Vec4, x, y, z, w);
	});
var $author$project$AltMath$Alternative$Record$Vector4$setZ = F2(
	function (z, _v0) {
		var x = _v0.ck;
		var y = _v0.cl;
		var w = _v0.ch;
		return {ch: w, ck: x, cl: y, cn: z};
	});
var $author$project$AltMath$Alternative$Tuple$Vector4$setZ = F2(
	function (z, _v0) {
		var _v1 = _v0.a;
		var x = _v1.a;
		var y = _v1.b;
		var _v2 = _v0.b;
		var w = _v2.b;
		return _Utils_Tuple2(
			_Utils_Tuple2(x, y),
			_Utils_Tuple2(z, w));
	});
var $elm_explorations$linear_algebra$Math$Vector4$setZ = _MJS_v4setZ;
var $author$project$Vector4$setZ = F2(
	function (_v0, a) {
		var glVec1 = _v0.K;
		var adtVec1 = _v0.J;
		var recVec1 = _v0.N;
		var tupleVec1 = _v0.O;
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'setZ',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v1) {
						return $author$project$Vector4$GLVec4(
							A2($elm_explorations$linear_algebra$Math$Vector4$setZ, a, glVec1));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v2) {
						return $author$project$Vector4$AdtVec4(
							A2($author$project$AltMath$Alternative$ADT$Vector4$setZ, a, adtVec1));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v3) {
						return $author$project$Vector4$RecordVec4(
							A2($author$project$AltMath$Alternative$Record$Vector4$setZ, a, recVec1));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v4) {
						return $author$project$Vector4$TupleVec4(
							A2($author$project$AltMath$Alternative$Tuple$Vector4$setZ, a, tupleVec1));
					})
				]));
	});
var $elm_explorations$linear_algebra$Math$Vector4$sub = _MJS_v4sub;
var $author$project$Vector4$sub = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	var glVec2 = _v0.bg;
	var adtVec2 = _v0.bc;
	var recVec2 = _v0.bm;
	var tupleVec2 = _v0.bt;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'sub',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $author$project$Vector4$GLVec4(
						A2($elm_explorations$linear_algebra$Math$Vector4$sub, glVec1, glVec2));
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$Vector4$AdtVec4(
						A2($author$project$AltMath$Alternative$ADT$Vector4$sub, adtVec1, adtVec2));
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$Vector4$RecordVec4(
						A2($author$project$AltMath$Alternative$Record$Vector4$sub, recVec1, recVec2));
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$Vector4$TupleVec4(
						A2($author$project$AltMath$Alternative$Tuple$Vector4$sub, tupleVec1, tupleVec2));
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector4$toRecord = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	var z = _v0.c;
	var w = _v0.d;
	return {ch: w, ck: x, cl: y, cn: z};
};
var $author$project$AltMath$Alternative$Record$Vector4$toRecord = $elm$core$Basics$identity;
var $author$project$AltMath$Alternative$Tuple$Vector4$toRecord = function (_v0) {
	var _v1 = _v0.a;
	var x = _v1.a;
	var y = _v1.b;
	var _v2 = _v0.b;
	var z = _v2.a;
	var w = _v2.b;
	return {ch: w, ck: x, cl: y, cn: z};
};
var $elm_explorations$linear_algebra$Math$Vector4$toRecord = _MJS_v4toRecord;
var $author$project$Vector4$toRecord = function (_v0) {
	var glVec1 = _v0.K;
	var adtVec1 = _v0.J;
	var recVec1 = _v0.N;
	var tupleVec1 = _v0.O;
	return A2(
		$elm_explorations$benchmark$Benchmark$scale,
		'toRecord',
		_List_fromArray(
			[
				_Utils_Tuple2(
				'Linear Algebra',
				function (_v1) {
					return $elm_explorations$linear_algebra$Math$Vector4$toRecord(glVec1);
				}),
				_Utils_Tuple2(
				'ADT',
				function (_v2) {
					return $author$project$AltMath$Alternative$ADT$Vector4$toRecord(adtVec1);
				}),
				_Utils_Tuple2(
				'Record',
				function (_v3) {
					return $author$project$AltMath$Alternative$Record$Vector4$toRecord(recVec1);
				}),
				_Utils_Tuple2(
				'Tuple',
				function (_v4) {
					return $author$project$AltMath$Alternative$Tuple$Vector4$toRecord(tupleVec1);
				})
			]));
};
var $author$project$AltMath$Alternative$ADT$Vector4$vec4 = $author$project$AltMath$Alternative$ADT$Vector4$Vec4;
var $author$project$AltMath$Alternative$Record$Vector4$vec4 = $author$project$AltMath$Alternative$Record$Vector4$Vec4;
var $author$project$AltMath$Alternative$Tuple$Vector4$vec4 = F4(
	function (x, y, z, w) {
		return _Utils_Tuple2(
			_Utils_Tuple2(x, y),
			_Utils_Tuple2(z, w));
	});
var $elm_explorations$linear_algebra$Math$Vector4$vec4 = _MJS_v4;
var $author$project$Vector4$vec4 = F4(
	function (x, y, z, w) {
		return A2(
			$elm_explorations$benchmark$Benchmark$scale,
			'vec4',
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Linear Algebra',
					function (_v0) {
						return $author$project$Vector4$GLVec4(
							A4($elm_explorations$linear_algebra$Math$Vector4$vec4, x, y, z, w));
					}),
					_Utils_Tuple2(
					'ADT',
					function (_v1) {
						return $author$project$Vector4$AdtVec4(
							A4($author$project$AltMath$Alternative$ADT$Vector4$vec4, x, y, z, w));
					}),
					_Utils_Tuple2(
					'Record',
					function (_v2) {
						return $author$project$Vector4$RecordVec4(
							A4($author$project$AltMath$Alternative$Record$Vector4$vec4, x, y, z, w));
					}),
					_Utils_Tuple2(
					'Tuple',
					function (_v3) {
						return $author$project$Vector4$TupleVec4(
							A4($author$project$AltMath$Alternative$Tuple$Vector4$vec4, x, y, z, w));
					})
				]));
	});
var $author$project$Vector4$all = function (_v0) {
	var x1 = _v0.aa;
	var x2 = _v0.ab;
	var y1 = _v0.ac;
	var y2 = _v0.ad;
	var z1 = _v0.aH;
	var z2 = _v0.aI;
	var w1 = _v0.ba;
	var w2 = _v0.bb;
	var z2_ = A2(
		$elm$core$Maybe$withDefault,
		10,
		$elm$core$String$toFloat(z2));
	var z1_ = A2(
		$elm$core$Maybe$withDefault,
		1,
		$elm$core$String$toFloat(z1));
	var y2_ = A2(
		$elm$core$Maybe$withDefault,
		10,
		$elm$core$String$toFloat(y2));
	var y1_ = A2(
		$elm$core$Maybe$withDefault,
		1,
		$elm$core$String$toFloat(y1));
	var x2_ = A2(
		$elm$core$Maybe$withDefault,
		10,
		$elm$core$String$toFloat(x2));
	var x1_ = A2(
		$elm$core$Maybe$withDefault,
		1,
		$elm$core$String$toFloat(x1));
	var w2_ = A2(
		$elm$core$Maybe$withDefault,
		10,
		$elm$core$String$toFloat(w2));
	var w1_ = A2(
		$elm$core$Maybe$withDefault,
		1,
		$elm$core$String$toFloat(w1));
	var record = {ch: w1_, ck: x1_, cl: y1_, cn: z1_};
	var data = {
		J: A4($author$project$AltMath$Alternative$ADT$Vector4$vec4, x1_, y1_, z1_, w1_),
		bc: A4($author$project$AltMath$Alternative$ADT$Vector4$vec4, x2_, y2_, z2_, w2_),
		K: A4($elm_explorations$linear_algebra$Math$Vector4$vec4, x1_, y1_, z1_, w1_),
		bg: A4($elm_explorations$linear_algebra$Math$Vector4$vec4, x2_, y2_, z2_, w2_),
		N: A4($author$project$AltMath$Alternative$Record$Vector4$vec4, x1_, y1_, z1_, w1_),
		bm: A4($author$project$AltMath$Alternative$Record$Vector4$vec4, x2_, y2_, z2_, w2_),
		O: A4($author$project$AltMath$Alternative$Tuple$Vector4$vec4, x1_, y1_, z1_, w1_),
		bt: A4($author$project$AltMath$Alternative$Tuple$Vector4$vec4, x2_, y2_, z2_, w2_)
	};
	return _List_fromArray(
		[
			A4($author$project$Vector4$vec4, x1_, y1_, z1_, w1_),
			$author$project$Vector4$getX(data),
			$author$project$Vector4$getY(data),
			$author$project$Vector4$getZ(data),
			$author$project$Vector4$getW(data),
			A2($author$project$Vector4$setX, data, x2_),
			A2($author$project$Vector4$setY, data, y2_),
			A2($author$project$Vector4$setZ, data, z2_),
			A2($author$project$Vector4$setW, data, w2_),
			$author$project$Vector4$add(data),
			$author$project$Vector4$sub(data),
			$author$project$Vector4$negate(data),
			$author$project$Vector4$scale(data),
			$author$project$Vector4$dot(data),
			$author$project$Vector4$normalize(data),
			$author$project$Vector4$direction(data),
			$author$project$Vector4$length(data),
			$author$project$Vector4$lengthSquared(data),
			$author$project$Vector4$distance(data),
			$author$project$Vector4$distanceSquared(data),
			$author$project$Vector4$toRecord(data),
			$author$project$Vector4$fromRecord(record)
		]);
};
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.n) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.p),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.p);
		} else {
			var treeLen = builder.n * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.q) : builder.q;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.n);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.p) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.p);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{q: nodeList, n: (len / $elm$core$Array$branchFactor) | 0, p: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {bC: fragment, bE: host, b_: path, b1: port_, b6: protocol, b7: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$document = _Browser_document;
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $author$project$Main$initModel = {
	ag: $elm$core$Dict$empty,
	E: {
		av: {
			a_: {bH: '1', bI: '1', bJ: '1', bK: '1', bL: '14', bM: '1', bN: '1', bO: '1', bP: '1', bQ: '1', bR: '1', bS: '1', bT: '1', bU: '1', bV: '1', bW: '1'},
			a$: {bH: '1', bI: '1', bJ: '1', bK: '1', bL: '1', bM: '1', bN: '1', bO: '1', bP: '1', bQ: '1', bR: '1', bS: '1', bT: '7', bU: '1', bV: '1', bW: '1'}
		},
		aD: {aa: '1', ab: '2', ac: '10', ad: '20'},
		aE: {aa: '1', ab: '2', ac: '10', ad: '20', aH: '3', aI: '4'},
		aF: {ba: '30', bb: '40', aa: '1', ab: '2', ac: '10', ad: '20', aH: '3', aI: '4'}
	},
	R: false
};
var $author$project$Main$Input = function (a) {
	return {$: 1, a: a};
};
var $elm$html$Html$br = _VirtualDom_node('br');
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $author$project$Main$cursor = F2(
	function (spec2, spec1) {
		return {
			h: A2($elm$core$Basics$composeR, spec2.h, spec1.h),
			a: F2(
				function (comp, world) {
					return A2(
						spec2.a,
						A2(
							spec1.a,
							comp,
							spec2.h(world)),
						world);
				})
		};
	});
var $elm$html$Html$a = _VirtualDom_node('a');
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$p = _VirtualDom_node('p');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$Main$dataInput = F3(
	function (attr, name, v) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('tile is-11 field has-addons')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$p,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('control ')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$a,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('button is-static is-small')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(name)
								]))
						])),
					A2(
					$elm$html$Html$p,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('control is-expanded')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$input,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('input is-small'),
									$elm$html$Html$Attributes$value(v),
									attr
								]),
							_List_Nil)
						]))
				]));
	});
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$json$Json$Decode$string = _Json_decodeString;
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $author$project$Main$specMat4 = {
	h: function ($) {
		return $.av;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{av: comps});
		})
};
var $author$project$Main$specM1 = A2(
	$author$project$Main$cursor,
	$author$project$Main$specMat4,
	{
		h: function ($) {
			return $.a_;
		},
		a: F2(
			function (comps, world) {
				return _Utils_update(
					world,
					{a_: comps});
			})
	});
var $author$project$Main$specM11 = {
	h: function ($) {
		return $.bH;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{bH: comps});
		})
};
var $author$project$Main$specM12 = {
	h: function ($) {
		return $.bI;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{bI: comps});
		})
};
var $author$project$Main$specM13 = {
	h: function ($) {
		return $.bJ;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{bJ: comps});
		})
};
var $author$project$Main$specM14 = {
	h: function ($) {
		return $.bK;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{bK: comps});
		})
};
var $author$project$Main$specM2 = A2(
	$author$project$Main$cursor,
	$author$project$Main$specMat4,
	{
		h: function ($) {
			return $.a$;
		},
		a: F2(
			function (comps, world) {
				return _Utils_update(
					world,
					{a$: comps});
			})
	});
var $author$project$Main$specM21 = {
	h: function ($) {
		return $.bL;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{bL: comps});
		})
};
var $author$project$Main$specM22 = {
	h: function ($) {
		return $.bM;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{bM: comps});
		})
};
var $author$project$Main$specM23 = {
	h: function ($) {
		return $.bN;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{bN: comps});
		})
};
var $author$project$Main$specM24 = {
	h: function ($) {
		return $.bO;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{bO: comps});
		})
};
var $author$project$Main$specM31 = {
	h: function ($) {
		return $.bP;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{bP: comps});
		})
};
var $author$project$Main$specM32 = {
	h: function ($) {
		return $.bQ;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{bQ: comps});
		})
};
var $author$project$Main$specM33 = {
	h: function ($) {
		return $.bR;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{bR: comps});
		})
};
var $author$project$Main$specM34 = {
	h: function ($) {
		return $.bS;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{bS: comps});
		})
};
var $author$project$Main$specM41 = {
	h: function ($) {
		return $.bT;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{bT: comps});
		})
};
var $author$project$Main$specM42 = {
	h: function ($) {
		return $.bU;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{bU: comps});
		})
};
var $author$project$Main$specM43 = {
	h: function ($) {
		return $.bV;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{bV: comps});
		})
};
var $author$project$Main$specM44 = {
	h: function ($) {
		return $.bW;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{bW: comps});
		})
};
var $author$project$Main$mat4Inputs = F2(
	function (block, _v0) {
		var m1 = _v0.a_;
		var m2 = _v0.a$;
		var attr = function (set) {
			return block ? $elm$html$Html$Attributes$disabled(true) : $elm$html$Html$Events$onInput(
				A2($elm$core$Basics$composeR, set, $author$project$Main$Input));
		};
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('tile box is-vertical')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('tile')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('tile is-child')
										]),
									_List_fromArray(
										[
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM1, $author$project$Main$specM11).a),
											'm11',
											m1.bH),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM1, $author$project$Main$specM21).a),
											'm21',
											m1.bL),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM1, $author$project$Main$specM31).a),
											'm31',
											m1.bP),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM1, $author$project$Main$specM41).a),
											'm41',
											m1.bT)
										])),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('tile is-child')
										]),
									_List_fromArray(
										[
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM1, $author$project$Main$specM12).a),
											'm12',
											m1.bI),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM1, $author$project$Main$specM22).a),
											'm22',
											m1.bM),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM1, $author$project$Main$specM32).a),
											'm32',
											m1.bQ),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM1, $author$project$Main$specM42).a),
											'm42',
											m1.bU)
										])),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('tile is-child')
										]),
									_List_fromArray(
										[
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM1, $author$project$Main$specM13).a),
											'm13',
											m1.bJ),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM1, $author$project$Main$specM23).a),
											'm23',
											m1.bN),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM1, $author$project$Main$specM33).a),
											'm33',
											m1.bR),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM1, $author$project$Main$specM43).a),
											'm43',
											m1.bV)
										])),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('tile is-child')
										]),
									_List_fromArray(
										[
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM1, $author$project$Main$specM14).a),
											'm14',
											m1.bK),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM1, $author$project$Main$specM24).a),
											'm24',
											m1.bO),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM1, $author$project$Main$specM34).a),
											'm34',
											m1.bS),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM1, $author$project$Main$specM44).a),
											'm44',
											m1.bW)
										]))
								])),
							A2($elm$html$Html$br, _List_Nil, _List_Nil),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('tile')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('tile is-child')
										]),
									_List_fromArray(
										[
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM2, $author$project$Main$specM11).a),
											'm11',
											m2.bH),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM2, $author$project$Main$specM21).a),
											'm21',
											m2.bL),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM2, $author$project$Main$specM31).a),
											'm31',
											m2.bP),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM2, $author$project$Main$specM41).a),
											'm41',
											m2.bT)
										])),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('tile is-child')
										]),
									_List_fromArray(
										[
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM2, $author$project$Main$specM12).a),
											'm12',
											m2.bI),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM2, $author$project$Main$specM22).a),
											'm22',
											m2.bM),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM2, $author$project$Main$specM32).a),
											'm32',
											m2.bQ),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM2, $author$project$Main$specM42).a),
											'm42',
											m2.bU)
										])),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('tile is-child')
										]),
									_List_fromArray(
										[
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM2, $author$project$Main$specM13).a),
											'm13',
											m2.bJ),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM2, $author$project$Main$specM23).a),
											'm23',
											m2.bN),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM2, $author$project$Main$specM33).a),
											'm33',
											m2.bR),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM2, $author$project$Main$specM43).a),
											'm43',
											m2.bV)
										])),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('tile is-child')
										]),
									_List_fromArray(
										[
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM2, $author$project$Main$specM14).a),
											'm14',
											m2.bK),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM2, $author$project$Main$specM24).a),
											'm24',
											m2.bO),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM2, $author$project$Main$specM34).a),
											'm34',
											m2.bS),
											A3(
											$author$project$Main$dataInput,
											attr(
												A2($author$project$Main$cursor, $author$project$Main$specM2, $author$project$Main$specM44).a),
											'm44',
											m2.bW)
										]))
								]))
						]))
				]));
	});
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $elm$html$Html$Attributes$rel = _VirtualDom_attribute('rel');
var $elm$html$Html$aside = _VirtualDom_node('aside');
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm_explorations$benchmark$Benchmark$Reporting$Group = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $elm_explorations$benchmark$Benchmark$Reporting$Series = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm_explorations$benchmark$Benchmark$Reporting$Single = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm_explorations$benchmark$Benchmark$Reporting$fromBenchmark = function (internal) {
	switch (internal.$) {
		case 0:
			var name = internal.a;
			var status = internal.c;
			return A2($elm_explorations$benchmark$Benchmark$Reporting$Single, name, status);
		case 1:
			var name = internal.a;
			var benchmarks = internal.b;
			return A2(
				$elm_explorations$benchmark$Benchmark$Reporting$Series,
				name,
				A2(
					$elm$core$List$map,
					function (_v1) {
						var childName = _v1.a;
						var status = _v1.c;
						return _Utils_Tuple2(childName, status);
					},
					benchmarks));
		default:
			var name = internal.a;
			var benchmarks = internal.b;
			return A2(
				$elm_explorations$benchmark$Benchmark$Reporting$Group,
				name,
				A2($elm$core$List$map, $elm_explorations$benchmark$Benchmark$Reporting$fromBenchmark, benchmarks));
	}
};
var $author$project$Main$getName = function (bench) {
	var _v0 = $elm_explorations$benchmark$Benchmark$Reporting$fromBenchmark(bench);
	switch (_v0.$) {
		case 0:
			var name = _v0.a;
			return name;
		case 1:
			var name = _v0.a;
			var l = _v0.b;
			return name;
		default:
			var name = _v0.a;
			var l = _v0.b;
			return name;
	}
};
var $elm$html$Html$h2 = _VirtualDom_node('h2');
var $author$project$Main$Step = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$core$String$fromFloat = _String_fromNumber;
var $BrianHicks$elm_trend$Trend$Math$NeedMoreValues = function (a) {
	return {$: 0, a: a};
};
var $elm$core$List$sum = function (numbers) {
	return A3($elm$core$List$foldl, $elm$core$Basics$add, 0, numbers);
};
var $BrianHicks$elm_trend$Trend$Math$mean = function (numbers) {
	if (!numbers.b) {
		return $elm$core$Result$Err(
			$BrianHicks$elm_trend$Trend$Math$NeedMoreValues(1));
	} else {
		return $elm$core$Result$Ok(
			$elm$core$List$sum(numbers) / $elm$core$List$length(numbers));
	}
};
var $elm$core$Basics$pow = _Basics_pow;
var $BrianHicks$elm_trend$Trend$Linear$predictY = F2(
	function (_v0, x) {
		var slope = _v0.bp;
		var intercept = _v0.bi;
		return (slope * x) + intercept;
	});
var $elm$core$List$unzip = function (pairs) {
	var step = F2(
		function (_v0, _v1) {
			var x = _v0.a;
			var y = _v0.b;
			var xs = _v1.a;
			var ys = _v1.b;
			return _Utils_Tuple2(
				A2($elm$core$List$cons, x, xs),
				A2($elm$core$List$cons, y, ys));
		});
	return A3(
		$elm$core$List$foldr,
		step,
		_Utils_Tuple2(_List_Nil, _List_Nil),
		pairs);
};
var $elm$core$Result$withDefault = F2(
	function (def, result) {
		if (!result.$) {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var $BrianHicks$elm_trend$Trend$Linear$goodnessOfFit = function (_v0) {
	var fit = _v0.a;
	var values = _v0.b;
	var _v1 = $elm$core$List$unzip(values);
	var xs = _v1.a;
	var ys = _v1.b;
	var predictions = A2(
		$elm$core$List$map,
		$BrianHicks$elm_trend$Trend$Linear$predictY(fit),
		xs);
	var meanY = A2(
		$elm$core$Result$withDefault,
		0,
		$BrianHicks$elm_trend$Trend$Math$mean(ys));
	var sumSquareResiduals = $elm$core$List$sum(
		A3(
			$elm$core$List$map2,
			F2(
				function (actual, prediction) {
					return A2($elm$core$Basics$pow, actual - prediction, 2);
				}),
			ys,
			predictions));
	var sumSquareTotal = $elm$core$List$sum(
		A2(
			$elm$core$List$map,
			function (y) {
				return A2($elm$core$Basics$pow, y - meanY, 2);
			},
			ys));
	return 1 - (sumSquareResiduals / sumSquareTotal);
};
var $elm$core$Basics$round = _Basics_round;
var $author$project$CustomRunner$Humanize$percent = A2(
	$elm$core$Basics$composeR,
	$elm$core$Basics$mul(10000),
	A2(
		$elm$core$Basics$composeR,
		$elm$core$Basics$round,
		A2(
			$elm$core$Basics$composeR,
			$elm$core$Basics$toFloat,
			A2(
				$elm$core$Basics$composeR,
				function (a) {
					return a / 100;
				},
				A2(
					$elm$core$Basics$composeR,
					$elm$core$String$fromFloat,
					function (a) {
						return a + '%';
					})))));
var $author$project$Main$goodnessOfFit = A2($elm$core$Basics$composeR, $BrianHicks$elm_trend$Trend$Linear$goodnessOfFit, $author$project$CustomRunner$Humanize$percent);
var $elm$html$Html$Attributes$max = $elm$html$Html$Attributes$stringProperty('max');
var $elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === -2) {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm_explorations$benchmark$Benchmark$Samples$count = function (_v0) {
	var samples = _v0;
	return A3(
		$elm$core$Dict$foldl,
		F3(
			function (_v1, times, acc) {
				return $elm$core$List$length(times) + acc;
			}),
		0,
		samples);
};
var $elm_explorations$benchmark$Benchmark$Status$numBuckets = 25;
var $elm_explorations$benchmark$Benchmark$Status$samplesPerBucket = 5;
var $elm_explorations$benchmark$Benchmark$Status$progress = function (status) {
	switch (status.$) {
		case 0:
			return 0;
		case 1:
			return 0;
		case 2:
			var samples = status.b;
			return A3(
				$elm$core$Basics$clamp,
				0,
				1,
				$elm_explorations$benchmark$Benchmark$Samples$count(samples) / ($elm_explorations$benchmark$Benchmark$Status$numBuckets * $elm_explorations$benchmark$Benchmark$Status$samplesPerBucket));
		case 3:
			return 1;
		default:
			return 1;
	}
};
var $elm$html$Html$progress = _VirtualDom_node('progress');
var $elm$core$String$fromList = _String_fromList;
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $author$project$CustomRunner$Humanize$groupsOf = F2(
	function (howMany, items) {
		var _v0 = A2($elm$core$List$take, howMany, items);
		if (!_v0.b) {
			return _List_Nil;
		} else {
			var xs = _v0;
			return A2(
				$elm$core$List$cons,
				xs,
				A2(
					$author$project$CustomRunner$Humanize$groupsOf,
					howMany,
					A2($elm$core$List$drop, howMany, items)));
		}
	});
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $author$project$CustomRunner$Humanize$int = A2(
	$elm$core$Basics$composeR,
	$elm$core$String$fromInt,
	A2(
		$elm$core$Basics$composeR,
		$elm$core$String$toList,
		A2(
			$elm$core$Basics$composeR,
			$elm$core$List$reverse,
			A2(
				$elm$core$Basics$composeR,
				$author$project$CustomRunner$Humanize$groupsOf(3),
				A2(
					$elm$core$Basics$composeR,
					$elm$core$List$reverse,
					A2(
						$elm$core$Basics$composeR,
						$elm$core$List$map(
							A2($elm$core$Basics$composeR, $elm$core$List$reverse, $elm$core$String$fromList)),
						$elm$core$String$join(',')))))));
var $BrianHicks$elm_trend$Trend$Linear$line = function (_v0) {
	var precalculated = _v0.a;
	return precalculated;
};
var $BrianHicks$elm_trend$Trend$Linear$predictX = F2(
	function (_v0, y) {
		var slope = _v0.bp;
		var intercept = _v0.bi;
		return (y - intercept) / slope;
	});
var $author$project$Main$runsPerSecond = A2(
	$elm$core$Basics$composeR,
	$BrianHicks$elm_trend$Trend$Linear$line,
	A2(
		$elm$core$Basics$composeR,
		function (a) {
			return A2($BrianHicks$elm_trend$Trend$Linear$predictX, a, 1000);
		},
		A2($elm$core$Basics$composeR, $elm$core$Basics$floor, $author$project$CustomRunner$Humanize$int)));
var $elm$html$Html$span = _VirtualDom_node('span');
var $author$project$Main$contents = function (status) {
	switch (status.$) {
		case 4:
			var trend = status.b;
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('level-left')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('level-item has-text-centered')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$p,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('title is-size-4')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(
												$author$project$Main$runsPerSecond(trend))
											])),
										A2(
										$elm$html$Html$p,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('heading')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('runs / second')
											]))
									]))
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('level-item has-text-centered ')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$p,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('title is-size-4')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(
												$author$project$Main$goodnessOfFit(trend))
											])),
										A2(
										$elm$html$Html$p,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('heading')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('goodness of fit')
											]))
									]))
							]))
					]));
		case 0:
			return A2(
				$elm$html$Html$span,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(' Cold')
					]));
		default:
			var percent = $elm_explorations$benchmark$Benchmark$Status$progress(status);
			return A2(
				$elm$html$Html$progress,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('progress is-primary'),
						$elm$html$Html$Attributes$value(
						$elm$core$String$fromFloat(percent)),
						$elm$html$Html$Attributes$max('1')
					]),
				_List_Nil);
	}
};
var $elm$html$Html$li = _VirtualDom_node('li');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $author$project$Main$report = F3(
	function (block, topName, bench) {
		var _v0 = $elm_explorations$benchmark$Benchmark$Reporting$fromBenchmark(bench);
		switch (_v0.$) {
			case 0:
				var name = _v0.a;
				return A2(
					$elm$html$Html$li,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Single::' + name)
						]));
			case 1:
				var name = _v0.a;
				var l = _v0.b;
				return A2(
					$elm$html$Html$li,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$p,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('level-left')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('level-item has-text-weight-bold')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(name)
										])),
									A2(
									$elm$html$Html$button,
									A2(
										$elm$core$List$cons,
										block ? $elm$html$Html$Attributes$disabled(true) : $elm$html$Html$Events$onClick(
											A2($author$project$Main$Step, topName + ('::' + name), bench)),
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('level-item button is-primary is-small is-rounded')
											])),
									_List_fromArray(
										[
											$elm$html$Html$text('Start')
										]))
								])),
							A2(
							$elm$html$Html$ul,
							_List_Nil,
							A2(
								$elm$core$List$map,
								function (_v1) {
									var nn = _v1.a;
									var s = _v1.b;
									return A2(
										$elm$html$Html$li,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												$elm$html$Html$span,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('has-text-weight-semibold')
													]),
												_List_fromArray(
													[
														$elm$html$Html$text(nn)
													])),
												$author$project$Main$contents(s)
											]));
								},
								l))
						]));
			default:
				var name = _v0.a;
				var l = _v0.b;
				return A2(
					$elm$html$Html$li,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Group::' + name)
						]));
		}
	});
var $author$project$Main$reportTop = F4(
	function (inputs, model, name, l) {
		return A2(
			$elm$html$Html$aside,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('tile is-parent is-vertical')
				]),
			_List_fromArray(
				[
					inputs,
					A2(
					$elm$html$Html$h2,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('has-text-weight-bold is-size-3')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(name)
						])),
					A2(
					$elm$html$Html$ul,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('menu-list')
						]),
					A2(
						$elm$core$List$map,
						function (bench) {
							return A3(
								$author$project$Main$report,
								model.R,
								name,
								A2(
									$elm$core$Maybe$withDefault,
									bench,
									A2(
										$elm$core$Dict$get,
										_Utils_ap(
											name + '::',
											$author$project$Main$getName(bench)),
										model.ag)));
						},
						l))
				]));
	});
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $author$project$GitHubCorner$d = $elm$html$Html$Attributes$attribute('d');
var $author$project$GitHubCorner$fill = $elm$html$Html$Attributes$attribute('fill');
var $elm$html$Html$Attributes$height = function (n) {
	return A2(
		_VirtualDom_attribute,
		'height',
		$elm$core$String$fromInt(n));
};
var $elm$virtual_dom$VirtualDom$nodeNS = function (tag) {
	return _VirtualDom_nodeNS(
		_VirtualDom_noScript(tag));
};
var $author$project$GitHubCorner$path = A2($elm$virtual_dom$VirtualDom$nodeNS, 'http://www.w3.org/2000/svg', 'path');
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $author$project$GitHubCorner$svg = A2($elm$virtual_dom$VirtualDom$nodeNS, 'http://www.w3.org/2000/svg', 'svg');
var $author$project$GitHubCorner$viewBox = $elm$html$Html$Attributes$attribute('viewBox');
var $elm$html$Html$Attributes$width = function (n) {
	return A2(
		_VirtualDom_attribute,
		'width',
		$elm$core$String$fromInt(n));
};
var $author$project$GitHubCorner$topRight = function (url) {
	return A2(
		$elm$html$Html$a,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$href(url),
				$elm$html$Html$Attributes$class('github-corner')
			]),
		_List_fromArray(
			[
				A2(
				$author$project$GitHubCorner$svg,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$width(80),
						$elm$html$Html$Attributes$height(80),
						$author$project$GitHubCorner$viewBox('0 0 250 250'),
						A2($elm$html$Html$Attributes$attribute, 'style', 'fill:#151513; color:#fff; position: absolute; top: 0; border: 0; right: 0;')
					]),
				_List_fromArray(
					[
						A2(
						$author$project$GitHubCorner$path,
						_List_fromArray(
							[
								$author$project$GitHubCorner$d('M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z')
							]),
						_List_Nil),
						A2(
						$author$project$GitHubCorner$path,
						_List_fromArray(
							[
								$author$project$GitHubCorner$fill('currentColor'),
								A2($elm$html$Html$Attributes$attribute, 'class', 'octo-arm'),
								A2($elm$html$Html$Attributes$style, 'transform-origin', '130px 106px'),
								$author$project$GitHubCorner$d('M128.3,109 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2')
							]),
						_List_Nil),
						A2(
						$author$project$GitHubCorner$path,
						_List_fromArray(
							[
								$author$project$GitHubCorner$fill('currentColor'),
								A2($elm$html$Html$Attributes$attribute, 'class', 'octo-body'),
								$author$project$GitHubCorner$d('M115,115 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z')
							]),
						_List_Nil)
					])),
				A3(
				$elm$html$Html$node,
				'style',
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('.github-corner:hover .octo-arm{animation:octocat-wave 560ms ease-in-out}@keyframes octocat-wave{0%,100%{transform:rotate(0)}20%,60%{transform:rotate(-25deg)}40%,80%{transform:rotate(10deg)}}@media (max-width:500px){.github-corner:hover .octo-arm{animation:none}.github-corner .octo-arm{animation:octocat-wave 560ms ease-in-out}}')
					]))
			]));
};
var $elm$core$Process$sleep = _Process_sleep;
var $author$project$Main$breakForRender = function (task) {
	return A2(
		$elm$core$Task$andThen,
		function (_v0) {
			return task;
		},
		$elm$core$Process$sleep(0));
};
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$core$Basics$not = _Basics_not;
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $elm_explorations$benchmark$Benchmark$done = function (benchmark_) {
	switch (benchmark_.$) {
		case 0:
			var status = benchmark_.c;
			return $elm_explorations$benchmark$Benchmark$Status$progress(status) === 1;
		case 1:
			var benchmarks = benchmark_.b;
			return A2(
				$elm$core$List$all,
				$elm$core$Basics$eq(1),
				A2(
					$elm$core$List$map,
					$elm_explorations$benchmark$Benchmark$Status$progress,
					A2(
						$elm$core$List$map,
						function (_v1) {
							var status = _v1.c;
							return status;
						},
						benchmarks)));
		default:
			var benchmarks = benchmark_.b;
			return A2($elm$core$List$all, $elm_explorations$benchmark$Benchmark$done, benchmarks);
	}
};
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm_explorations$benchmark$Benchmark$Benchmark$Group = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $elm_explorations$benchmark$Benchmark$Benchmark$Single = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $elm_explorations$benchmark$Benchmark$Status$Failure = function (a) {
	return {$: 3, a: a};
};
var $elm_explorations$benchmark$Benchmark$Status$MeasurementError = function (a) {
	return {$: 0, a: a};
};
var $elm_explorations$benchmark$Benchmark$Status$Pending = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $elm_explorations$benchmark$Benchmark$Status$Unsized = {$: 1};
var $elm_explorations$benchmark$Benchmark$Status$bucketSpacingRatio = 2;
var $elm_explorations$benchmark$Benchmark$Samples$Samples = $elm$core$Basics$identity;
var $elm_explorations$benchmark$Benchmark$Samples$empty = $elm$core$Dict$empty;
var $elm_explorations$benchmark$Benchmark$Status$AnalysisError = function (a) {
	return {$: 1, a: a};
};
var $elm_explorations$benchmark$Benchmark$Status$Success = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $elm$core$Dict$map = F2(
	function (func, dict) {
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				A2(func, key, value),
				A2($elm$core$Dict$map, func, left),
				A2($elm$core$Dict$map, func, right));
		}
	});
var $elm$core$Result$map = F2(
	function (func, ra) {
		if (!ra.$) {
			var a = ra.a;
			return $elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return $elm$core$Result$Err(e);
		}
	});
var $elm$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _v0) {
				var trues = _v0.a;
				var falses = _v0.b;
				return pred(x) ? _Utils_Tuple2(
					A2($elm$core$List$cons, x, trues),
					falses) : _Utils_Tuple2(
					trues,
					A2($elm$core$List$cons, x, falses));
			});
		return A3(
			$elm$core$List$foldr,
			step,
			_Utils_Tuple2(_List_Nil, _List_Nil),
			list);
	});
var $elm_explorations$benchmark$Benchmark$Samples$pointify = function (samples) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (sampleSize, values, acc) {
				return _Utils_ap(
					A2(
						$elm$core$List$map,
						function (b) {
							return _Utils_Tuple2(sampleSize, b);
						},
						values),
					acc);
			}),
		_List_Nil,
		samples);
};
var $BrianHicks$elm_trend$Trend$Math$AllZeros = {$: 1};
var $BrianHicks$elm_trend$Trend$Linear$Robust = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $BrianHicks$elm_trend$Trend$Linear$Trend = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$Result$fromMaybe = F2(
	function (err, maybe) {
		if (!maybe.$) {
			var v = maybe.a;
			return $elm$core$Result$Ok(v);
		} else {
			return $elm$core$Result$Err(err);
		}
	});
var $elm$core$Basics$isInfinite = _Basics_isInfinite;
var $elm$core$Basics$isNaN = _Basics_isNaN;
var $elm$core$Maybe$map3 = F4(
	function (func, ma, mb, mc) {
		if (ma.$ === 1) {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 1) {
				return $elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				if (mc.$ === 1) {
					return $elm$core$Maybe$Nothing;
				} else {
					var c = mc.a;
					return $elm$core$Maybe$Just(
						A3(func, a, b, c));
				}
			}
		}
	});
var $elm$core$List$sortBy = _List_sortBy;
var $elm$core$List$sort = function (xs) {
	return A2($elm$core$List$sortBy, $elm$core$Basics$identity, xs);
};
var $BrianHicks$elm_trend$Trend$Linear$Line = F2(
	function (slope, intercept) {
		return {bi: intercept, bp: slope};
	});
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (!maybeValue.$) {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		if (ma.$ === 1) {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 1) {
				return $elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				return $elm$core$Maybe$Just(
					A2(func, a, b));
			}
		}
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Result$toMaybe = function (result) {
	if (!result.$) {
		var v = result.a;
		return $elm$core$Maybe$Just(v);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $BrianHicks$elm_trend$Trend$Linear$percentile = F2(
	function (k, xs) {
		var index = $elm$core$List$length(xs) * k;
		return (!(index - $elm$core$Basics$floor(index))) ? $elm$core$List$head(
			A2(
				$elm$core$List$drop,
				$elm$core$Basics$ceiling(index) - 1,
				xs)) : $elm$core$Result$toMaybe(
			$BrianHicks$elm_trend$Trend$Math$mean(
				A2(
					$elm$core$List$take,
					2,
					A2(
						$elm$core$List$drop,
						$elm$core$Basics$floor(index) - 1,
						xs))));
	});
var $BrianHicks$elm_trend$Trend$Linear$theilSenLine = F3(
	function (pct, slopes, points) {
		var slope = A2($BrianHicks$elm_trend$Trend$Linear$percentile, pct, slopes);
		var intercept = A2(
			$elm$core$Maybe$andThen,
			$BrianHicks$elm_trend$Trend$Linear$percentile(pct),
			A2(
				$elm$core$Maybe$map,
				$elm$core$List$sort,
				A2(
					$elm$core$Maybe$map,
					function (m) {
						return A2(
							$elm$core$List$map,
							function (_v0) {
								var x = _v0.a;
								var y = _v0.b;
								return y - (m * x);
							},
							points);
					},
					slope)));
		return A3($elm$core$Maybe$map2, $BrianHicks$elm_trend$Trend$Linear$Line, slope, intercept);
	});
var $BrianHicks$elm_trend$Trend$Linear$robust = function (values) {
	if (!values.b) {
		return $elm$core$Result$Err(
			$BrianHicks$elm_trend$Trend$Math$NeedMoreValues(2));
	} else {
		if (!values.b.b) {
			return $elm$core$Result$Err(
				$BrianHicks$elm_trend$Trend$Math$NeedMoreValues(2));
		} else {
			var slopes = $elm$core$List$sort(
				A3(
					$elm$core$List$foldl,
					F2(
						function (_v1, acc1) {
							var x = _v1.a;
							var y = _v1.b;
							return A3(
								$elm$core$List$foldl,
								F2(
									function (_v2, acc2) {
										var x1 = _v2.a;
										var y1 = _v2.b;
										var res = (y - y1) / (x - x1);
										return $elm$core$Basics$isNaN(res) ? acc2 : A2($elm$core$List$cons, res, acc2);
									}),
								acc1,
								values);
						}),
					_List_Nil,
					values));
			var finiteSlopes = A2(
				$elm$core$List$filter,
				A2($elm$core$Basics$composeL, $elm$core$Basics$not, $elm$core$Basics$isInfinite),
				slopes);
			return A2(
				$elm$core$Result$fromMaybe,
				$BrianHicks$elm_trend$Trend$Math$AllZeros,
				A4(
					$elm$core$Maybe$map3,
					F3(
						function (trendLine, lower, upper) {
							return A2(
								$BrianHicks$elm_trend$Trend$Linear$Trend,
								trendLine,
								A2($BrianHicks$elm_trend$Trend$Linear$Robust, lower, upper));
						}),
					A3($BrianHicks$elm_trend$Trend$Linear$theilSenLine, 0.5, finiteSlopes, values),
					A3($BrianHicks$elm_trend$Trend$Linear$theilSenLine, 0.975, slopes, values),
					A3($BrianHicks$elm_trend$Trend$Linear$theilSenLine, 0.025, slopes, values)));
		}
	}
};
var $elm_explorations$benchmark$Benchmark$Samples$groups = function (_v0) {
	var samples = _v0;
	return A2(
		$elm$core$Result$withDefault,
		_Utils_Tuple2(samples, $elm$core$Dict$empty),
		A2(
			$elm$core$Result$map,
			A2(
				$elm$core$Dict$foldl,
				F3(
					function (key, _v1, _v2) {
						var good = _v1.a;
						var outliers = _v1.b;
						var accGood = _v2.a;
						var accOutliers = _v2.b;
						return _Utils_Tuple2(
							A3($elm$core$Dict$insert, key, good, accGood),
							A3($elm$core$Dict$insert, key, outliers, accOutliers));
					}),
				_Utils_Tuple2($elm$core$Dict$empty, $elm$core$Dict$empty)),
			A2(
				$elm$core$Result$map,
				function (line) {
					return A2(
						$elm$core$Dict$map,
						F2(
							function (sampleSize, values) {
								var predicted = A2($BrianHicks$elm_trend$Trend$Linear$predictY, line, sampleSize);
								var upperBound = predicted * 1.1;
								var lowerBound = predicted / 1.1;
								return A2(
									$elm$core$List$partition,
									function (v) {
										return (_Utils_cmp(lowerBound, v) < 0) && (_Utils_cmp(v, upperBound) < 0);
									},
									values);
							}),
						samples);
				},
				A2(
					$elm$core$Result$map,
					$BrianHicks$elm_trend$Trend$Linear$line,
					$BrianHicks$elm_trend$Trend$Linear$robust(
						$elm_explorations$benchmark$Benchmark$Samples$pointify(samples))))));
};
var $elm$core$Tuple$mapFirst = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var $elm$core$Tuple$mapSecond = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			x,
			func(y));
	});
var $elm_explorations$benchmark$Benchmark$Samples$points = function (samples) {
	return A2(
		$elm$core$Tuple$mapSecond,
		$elm_explorations$benchmark$Benchmark$Samples$pointify,
		A2(
			$elm$core$Tuple$mapFirst,
			$elm_explorations$benchmark$Benchmark$Samples$pointify,
			$elm_explorations$benchmark$Benchmark$Samples$groups(samples)));
};
var $BrianHicks$elm_trend$Trend$Linear$Quick = $elm$core$Basics$identity;
var $elm$core$Result$andThen = F2(
	function (callback, result) {
		if (!result.$) {
			var value = result.a;
			return callback(value);
		} else {
			var msg = result.a;
			return $elm$core$Result$Err(msg);
		}
	});
var $elm$core$Result$map2 = F3(
	function (func, ra, rb) {
		if (ra.$ === 1) {
			var x = ra.a;
			return $elm$core$Result$Err(x);
		} else {
			var a = ra.a;
			if (rb.$ === 1) {
				var x = rb.a;
				return $elm$core$Result$Err(x);
			} else {
				var b = rb.a;
				return $elm$core$Result$Ok(
					A2(func, a, b));
			}
		}
	});
var $BrianHicks$elm_trend$Trend$Math$stddev = function (numbers) {
	var helper = function (seriesMean) {
		return A2(
			$elm$core$Result$map,
			$elm$core$Basics$sqrt,
			$BrianHicks$elm_trend$Trend$Math$mean(
				A2(
					$elm$core$List$map,
					function (n) {
						return A2($elm$core$Basics$pow, n - seriesMean, 2);
					},
					numbers)));
	};
	return A2(
		$elm$core$Result$andThen,
		helper,
		$BrianHicks$elm_trend$Trend$Math$mean(numbers));
};
var $BrianHicks$elm_trend$Trend$Math$correlation = function (values) {
	if (!values.b) {
		return $elm$core$Result$Err(
			$BrianHicks$elm_trend$Trend$Math$NeedMoreValues(2));
	} else {
		if (!values.b.b) {
			return $elm$core$Result$Err(
				$BrianHicks$elm_trend$Trend$Math$NeedMoreValues(2));
		} else {
			var standardize = F3(
				function (meanResult, stddevResult, series) {
					return A3(
						$elm$core$Result$map2,
						F2(
							function (meanValue, stddevValue) {
								return A2(
									$elm$core$List$map,
									function (point) {
										return (point - meanValue) / stddevValue;
									},
									series);
							}),
						meanResult,
						stddevResult);
				});
			var _v1 = $elm$core$List$unzip(values);
			var xs = _v1.a;
			var ys = _v1.b;
			var summedProduct = A2(
				$elm$core$Result$map,
				$elm$core$List$sum,
				A3(
					$elm$core$Result$map2,
					F2(
						function (stdX, stdY) {
							return A3($elm$core$List$map2, $elm$core$Basics$mul, stdX, stdY);
						}),
					A3(
						standardize,
						$BrianHicks$elm_trend$Trend$Math$mean(xs),
						$BrianHicks$elm_trend$Trend$Math$stddev(xs),
						xs),
					A3(
						standardize,
						$BrianHicks$elm_trend$Trend$Math$mean(ys),
						$BrianHicks$elm_trend$Trend$Math$stddev(ys),
						ys)));
			return A2(
				$elm$core$Result$andThen,
				function (val) {
					return $elm$core$Basics$isNaN(val) ? $elm$core$Result$Err($BrianHicks$elm_trend$Trend$Math$AllZeros) : $elm$core$Result$Ok(val);
				},
				A2(
					$elm$core$Result$map,
					function (sum) {
						return sum / $elm$core$List$length(values);
					},
					summedProduct));
		}
	}
};
var $elm$core$Result$map3 = F4(
	function (func, ra, rb, rc) {
		if (ra.$ === 1) {
			var x = ra.a;
			return $elm$core$Result$Err(x);
		} else {
			var a = ra.a;
			if (rb.$ === 1) {
				var x = rb.a;
				return $elm$core$Result$Err(x);
			} else {
				var b = rb.a;
				if (rc.$ === 1) {
					var x = rc.a;
					return $elm$core$Result$Err(x);
				} else {
					var c = rc.a;
					return $elm$core$Result$Ok(
						A3(func, a, b, c));
				}
			}
		}
	});
var $BrianHicks$elm_trend$Trend$Linear$quick = function (values) {
	if (!values.b) {
		return $elm$core$Result$Err(
			$BrianHicks$elm_trend$Trend$Math$NeedMoreValues(2));
	} else {
		if (!values.b.b) {
			return $elm$core$Result$Err(
				$BrianHicks$elm_trend$Trend$Math$NeedMoreValues(2));
		} else {
			var _v1 = $elm$core$List$unzip(values);
			var xs = _v1.a;
			var ys = _v1.b;
			var slopeResult = A4(
				$elm$core$Result$map3,
				F3(
					function (correl, stddevY, stddevX) {
						return (correl * stddevY) / stddevX;
					}),
				$BrianHicks$elm_trend$Trend$Math$correlation(values),
				$BrianHicks$elm_trend$Trend$Math$stddev(ys),
				$BrianHicks$elm_trend$Trend$Math$stddev(xs));
			var intercept = A4(
				$elm$core$Result$map3,
				F3(
					function (meanY, slope, meanX) {
						return meanY - (slope * meanX);
					}),
				$BrianHicks$elm_trend$Trend$Math$mean(ys),
				slopeResult,
				$BrianHicks$elm_trend$Trend$Math$mean(xs));
			return A2(
				$elm$core$Result$map,
				function (trendLine) {
					return A2($BrianHicks$elm_trend$Trend$Linear$Trend, trendLine, values);
				},
				A3($elm$core$Result$map2, $BrianHicks$elm_trend$Trend$Linear$Line, slopeResult, intercept));
		}
	}
};
var $elm_explorations$benchmark$Benchmark$Samples$trend = function (samples) {
	return $BrianHicks$elm_trend$Trend$Linear$quick(
		$elm_explorations$benchmark$Benchmark$Samples$points(samples).a);
};
var $elm_explorations$benchmark$Benchmark$finalize = function (samples) {
	var _v0 = $elm_explorations$benchmark$Benchmark$Samples$trend(samples);
	if (!_v0.$) {
		var trend = _v0.a;
		return A2($elm_explorations$benchmark$Benchmark$Status$Success, samples, trend);
	} else {
		var err = _v0.a;
		return $elm_explorations$benchmark$Benchmark$Status$Failure(
			$elm_explorations$benchmark$Benchmark$Status$AnalysisError(err));
	}
};
var $elm_explorations$benchmark$Benchmark$LowLevel$defaultMinimum = 1;
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $elm$core$List$minimum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$min, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2($elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var $elm$core$List$repeat = F2(
	function (n, value) {
		return A3($elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var $elm_explorations$benchmark$Benchmark$LowLevel$sample = F2(
	function (n, operation_) {
		return A2(_Benchmark_sample, n, operation_);
	});
var $elm_explorations$benchmark$Benchmark$LowLevel$standardizeSampleSize = function (sampleSize) {
	var helper = F2(
		function (rough, magnitude) {
			helper:
			while (true) {
				if (rough > 10) {
					var $temp$rough = $elm$core$Basics$round(rough / 10),
						$temp$magnitude = magnitude * 10;
					rough = $temp$rough;
					magnitude = $temp$magnitude;
					continue helper;
				} else {
					return rough * magnitude;
				}
			}
		});
	return A2(helper, sampleSize, 1);
};
var $elm_explorations$benchmark$Benchmark$LowLevel$findSampleSizeWithMinimum = F2(
	function (minimumRuntime, operation_) {
		var sampleSize = function (i) {
			return i * 10;
		};
		var resample = F2(
			function (iteration, total) {
				return (_Utils_cmp(total, minimumRuntime) < 0) ? A2(
					$elm$core$Task$andThen,
					resample(iteration + 1),
					A2(
						$elm$core$Task$map,
						A2(
							$elm$core$Basics$composeR,
							$elm$core$List$minimum,
							$elm$core$Maybe$withDefault(0)),
						$elm$core$Task$sequence(
							A2(
								$elm$core$List$repeat,
								3,
								A2(
									$elm_explorations$benchmark$Benchmark$LowLevel$sample,
									sampleSize(iteration),
									operation_))))) : $elm$core$Task$succeed(
					sampleSize(iteration));
			});
		return A2(
			$elm$core$Task$map,
			$elm_explorations$benchmark$Benchmark$LowLevel$standardizeSampleSize,
			A2(resample, 1, 0));
	});
var $elm_explorations$benchmark$Benchmark$LowLevel$findSampleSize = $elm_explorations$benchmark$Benchmark$LowLevel$findSampleSizeWithMinimum($elm_explorations$benchmark$Benchmark$LowLevel$defaultMinimum);
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$Basics$modBy = _Basics_modBy;
var $elm$core$Task$onError = _Scheduler_onError;
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === -1) && (dict.d.$ === -1)) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.e.d.$ === -1) && (!dict.e.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.d.d.$ === -1) && (!dict.d.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === -1) && (!left.a)) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === -1) && (right.a === 1)) {
					if (right.d.$ === -1) {
						if (right.d.a === 1) {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === -1) && (dict.d.$ === -1)) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor === 1) {
			if ((lLeft.$ === -1) && (!lLeft.a)) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === -1) {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === -1) && (left.a === 1)) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === -1) && (!lLeft.a)) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === -1) {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === -1) {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === -1) {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (!_v0.$) {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm_explorations$benchmark$Benchmark$Samples$record = F3(
	function (sampleSize, sample, _v0) {
		var samplesDict = _v0;
		return A3(
			$elm$core$Dict$update,
			sampleSize,
			function (value) {
				if (value.$ === 1) {
					return $elm$core$Maybe$Just(
						_List_fromArray(
							[sample]));
				} else {
					var samples_ = value.a;
					return $elm$core$Maybe$Just(
						A2($elm$core$List$cons, sample, samples_));
				}
			},
			samplesDict);
	});
var $elm_explorations$benchmark$Benchmark$LowLevel$warmup = function (operation_) {
	var toCollect = 1000;
	var sampleSize = 10000;
	var helper = function (soFar) {
		return (_Utils_cmp(soFar, toCollect) > -1) ? $elm$core$Task$succeed(0) : A2(
			$elm$core$Task$andThen,
			helper,
			A2(
				$elm$core$Task$map,
				$elm$core$Basics$add(soFar),
				A2($elm_explorations$benchmark$Benchmark$LowLevel$sample, sampleSize, operation_)));
	};
	return helper(0);
};
var $elm_explorations$benchmark$Benchmark$stepLowLevel = F2(
	function (operation, status) {
		switch (status.$) {
			case 0:
				return A2(
					$elm$core$Task$onError,
					A2(
						$elm$core$Basics$composeL,
						A2($elm$core$Basics$composeL, $elm$core$Task$succeed, $elm_explorations$benchmark$Benchmark$Status$Failure),
						$elm_explorations$benchmark$Benchmark$Status$MeasurementError),
					A2(
						$elm$core$Task$map,
						function (_v1) {
							return $elm_explorations$benchmark$Benchmark$Status$Unsized;
						},
						$elm_explorations$benchmark$Benchmark$LowLevel$warmup(operation)));
			case 1:
				return A2(
					$elm$core$Task$onError,
					A2(
						$elm$core$Basics$composeL,
						A2($elm$core$Basics$composeL, $elm$core$Task$succeed, $elm_explorations$benchmark$Benchmark$Status$Failure),
						$elm_explorations$benchmark$Benchmark$Status$MeasurementError),
					A2(
						$elm$core$Task$map,
						function (sampleSize) {
							return A2($elm_explorations$benchmark$Benchmark$Status$Pending, sampleSize, $elm_explorations$benchmark$Benchmark$Samples$empty);
						},
						$elm_explorations$benchmark$Benchmark$LowLevel$findSampleSize(operation)));
			case 2:
				var baseSampleSize = status.a;
				var samples = status.b;
				var sampleSize = baseSampleSize * (($elm_explorations$benchmark$Benchmark$Status$bucketSpacingRatio * A2(
					$elm$core$Basics$modBy,
					$elm_explorations$benchmark$Benchmark$Status$numBuckets,
					$elm_explorations$benchmark$Benchmark$Samples$count(samples))) + 1);
				return A2(
					$elm$core$Task$onError,
					A2(
						$elm$core$Basics$composeL,
						A2($elm$core$Basics$composeL, $elm$core$Task$succeed, $elm_explorations$benchmark$Benchmark$Status$Failure),
						$elm_explorations$benchmark$Benchmark$Status$MeasurementError),
					A2(
						$elm$core$Task$map,
						function (newSample) {
							var newSamples = A3($elm_explorations$benchmark$Benchmark$Samples$record, sampleSize, newSample, samples);
							return (_Utils_cmp(
								$elm_explorations$benchmark$Benchmark$Samples$count(newSamples),
								$elm_explorations$benchmark$Benchmark$Status$numBuckets * $elm_explorations$benchmark$Benchmark$Status$samplesPerBucket) > -1) ? $elm_explorations$benchmark$Benchmark$finalize(newSamples) : A2($elm_explorations$benchmark$Benchmark$Status$Pending, baseSampleSize, newSamples);
						},
						A2($elm_explorations$benchmark$Benchmark$LowLevel$sample, sampleSize, operation)));
			default:
				return $elm$core$Task$succeed(status);
		}
	});
var $elm_explorations$benchmark$Benchmark$step = function (benchmark_) {
	switch (benchmark_.$) {
		case 0:
			var name = benchmark_.a;
			var inner = benchmark_.b;
			var status = benchmark_.c;
			return A2(
				$elm$core$Task$map,
				A2($elm_explorations$benchmark$Benchmark$Benchmark$Single, name, inner),
				A2($elm_explorations$benchmark$Benchmark$stepLowLevel, inner, status));
		case 1:
			var name = benchmark_.a;
			var benchmarks = benchmark_.b;
			return A2(
				$elm$core$Task$map,
				$elm_explorations$benchmark$Benchmark$Benchmark$Series(name),
				$elm$core$Task$sequence(
					A2(
						$elm$core$List$map,
						function (_v1) {
							var name_ = _v1.a;
							var inner = _v1.b;
							var status = _v1.c;
							return A2(
								$elm$core$Task$map,
								function (status_) {
									return _Utils_Tuple3(name_, inner, status_);
								},
								A2($elm_explorations$benchmark$Benchmark$stepLowLevel, inner, status));
						},
						benchmarks)));
		default:
			var name = benchmark_.a;
			var benchmarks = benchmark_.b;
			return A2(
				$elm$core$Task$map,
				$elm_explorations$benchmark$Benchmark$Benchmark$Group(name),
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm_explorations$benchmark$Benchmark$step, benchmarks)));
	}
};
var $author$project$Main$update = F2(
	function (msg, model) {
		if (!msg.$) {
			var s = msg.a;
			var benchmark = msg.b;
			return $elm_explorations$benchmark$Benchmark$done(benchmark) ? _Utils_Tuple2(
				_Utils_update(
					model,
					{
						ag: A3($elm$core$Dict$insert, s, benchmark, model.ag),
						R: false
					}),
				$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
				_Utils_update(
					model,
					{
						ag: A3($elm$core$Dict$insert, s, benchmark, model.ag),
						R: true
					}),
				A2(
					$elm$core$Task$perform,
					$author$project$Main$Step(s),
					$author$project$Main$breakForRender(
						$elm_explorations$benchmark$Benchmark$step(benchmark))));
		} else {
			var fn = msg.a;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						E: fn(model.E)
					}),
				$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Main$specVec2 = {
	h: function ($) {
		return $.aD;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{aD: comps});
		})
};
var $author$project$Main$specX1 = {
	h: function ($) {
		return $.aa;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{aa: comps});
		})
};
var $author$project$Main$specX2 = {
	h: function ($) {
		return $.ab;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{ab: comps});
		})
};
var $author$project$Main$specY1 = {
	h: function ($) {
		return $.ac;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{ac: comps});
		})
};
var $author$project$Main$specY2 = {
	h: function ($) {
		return $.ad;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{ad: comps});
		})
};
var $author$project$Main$vec2Inputs = F2(
	function (block, _v0) {
		var x1 = _v0.aa;
		var x2 = _v0.ab;
		var y1 = _v0.ac;
		var y2 = _v0.ad;
		var attr = function (set) {
			return block ? $elm$html$Html$Attributes$disabled(true) : $elm$html$Html$Events$onInput(
				A2($elm$core$Basics$composeR, set, $author$project$Main$Input));
		};
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('tile box')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('tile is-child')
								]),
							_List_fromArray(
								[
									A3(
									$author$project$Main$dataInput,
									attr(
										A2($author$project$Main$cursor, $author$project$Main$specVec2, $author$project$Main$specX1).a),
									'x1',
									x1),
									A3(
									$author$project$Main$dataInput,
									attr(
										A2($author$project$Main$cursor, $author$project$Main$specVec2, $author$project$Main$specX2).a),
									'x2',
									x2)
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('tile is-child')
								]),
							_List_fromArray(
								[
									A3(
									$author$project$Main$dataInput,
									attr(
										A2($author$project$Main$cursor, $author$project$Main$specVec2, $author$project$Main$specY1).a),
									'y1',
									y1),
									A3(
									$author$project$Main$dataInput,
									attr(
										A2($author$project$Main$cursor, $author$project$Main$specVec2, $author$project$Main$specY2).a),
									'y2',
									y2)
								]))
						]))
				]));
	});
var $author$project$Main$specVec3 = {
	h: function ($) {
		return $.aE;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{aE: comps});
		})
};
var $author$project$Main$specZ1 = {
	h: function ($) {
		return $.aH;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{aH: comps});
		})
};
var $author$project$Main$specZ2 = {
	h: function ($) {
		return $.aI;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{aI: comps});
		})
};
var $author$project$Main$vec3Inputs = F2(
	function (block, _v0) {
		var x1 = _v0.aa;
		var x2 = _v0.ab;
		var y1 = _v0.ac;
		var y2 = _v0.ad;
		var z1 = _v0.aH;
		var z2 = _v0.aI;
		var attr = function (set) {
			return block ? $elm$html$Html$Attributes$disabled(true) : $elm$html$Html$Events$onInput(
				A2($elm$core$Basics$composeR, set, $author$project$Main$Input));
		};
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('tile box')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('tile is-child')
								]),
							_List_fromArray(
								[
									A3(
									$author$project$Main$dataInput,
									attr(
										A2($author$project$Main$cursor, $author$project$Main$specVec3, $author$project$Main$specX1).a),
									'x1',
									x1),
									A3(
									$author$project$Main$dataInput,
									attr(
										A2($author$project$Main$cursor, $author$project$Main$specVec3, $author$project$Main$specX2).a),
									'x2',
									x2)
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('tile is-child')
								]),
							_List_fromArray(
								[
									A3(
									$author$project$Main$dataInput,
									attr(
										A2($author$project$Main$cursor, $author$project$Main$specVec3, $author$project$Main$specY1).a),
									'y1',
									y1),
									A3(
									$author$project$Main$dataInput,
									attr(
										A2($author$project$Main$cursor, $author$project$Main$specVec3, $author$project$Main$specY2).a),
									'y2',
									y2)
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('tile is-child')
								]),
							_List_fromArray(
								[
									A3(
									$author$project$Main$dataInput,
									attr(
										A2($author$project$Main$cursor, $author$project$Main$specVec3, $author$project$Main$specZ1).a),
									'z1',
									z1),
									A3(
									$author$project$Main$dataInput,
									attr(
										A2($author$project$Main$cursor, $author$project$Main$specVec3, $author$project$Main$specZ2).a),
									'z2',
									z2)
								]))
						]))
				]));
	});
var $author$project$Main$specVec4 = {
	h: function ($) {
		return $.aF;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{aF: comps});
		})
};
var $author$project$Main$specW1 = {
	h: function ($) {
		return $.ba;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{ba: comps});
		})
};
var $author$project$Main$specW2 = {
	h: function ($) {
		return $.bb;
	},
	a: F2(
		function (comps, world) {
			return _Utils_update(
				world,
				{bb: comps});
		})
};
var $author$project$Main$vec4Inputs = F2(
	function (block, _v0) {
		var x1 = _v0.aa;
		var x2 = _v0.ab;
		var y1 = _v0.ac;
		var y2 = _v0.ad;
		var z1 = _v0.aH;
		var z2 = _v0.aI;
		var w1 = _v0.ba;
		var w2 = _v0.bb;
		var attr = function (set) {
			return block ? $elm$html$Html$Attributes$disabled(true) : $elm$html$Html$Events$onInput(
				A2($elm$core$Basics$composeR, set, $author$project$Main$Input));
		};
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('tile box')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('tile is-child')
								]),
							_List_fromArray(
								[
									A3(
									$author$project$Main$dataInput,
									attr(
										A2($author$project$Main$cursor, $author$project$Main$specVec4, $author$project$Main$specX1).a),
									'x1',
									x1),
									A3(
									$author$project$Main$dataInput,
									attr(
										A2($author$project$Main$cursor, $author$project$Main$specVec4, $author$project$Main$specX2).a),
									'x2',
									x2)
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('tile is-child')
								]),
							_List_fromArray(
								[
									A3(
									$author$project$Main$dataInput,
									attr(
										A2($author$project$Main$cursor, $author$project$Main$specVec4, $author$project$Main$specY1).a),
									'y1',
									y1),
									A3(
									$author$project$Main$dataInput,
									attr(
										A2($author$project$Main$cursor, $author$project$Main$specVec4, $author$project$Main$specY2).a),
									'y2',
									y2)
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('tile is-child')
								]),
							_List_fromArray(
								[
									A3(
									$author$project$Main$dataInput,
									attr(
										A2($author$project$Main$cursor, $author$project$Main$specVec4, $author$project$Main$specZ1).a),
									'z1',
									z1),
									A3(
									$author$project$Main$dataInput,
									attr(
										A2($author$project$Main$cursor, $author$project$Main$specVec4, $author$project$Main$specZ2).a),
									'z2',
									z2)
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('tile is-child')
								]),
							_List_fromArray(
								[
									A3(
									$author$project$Main$dataInput,
									attr(
										A2($author$project$Main$cursor, $author$project$Main$specVec4, $author$project$Main$specW1).a),
									'w1',
									w1),
									A3(
									$author$project$Main$dataInput,
									attr(
										A2($author$project$Main$cursor, $author$project$Main$specVec4, $author$project$Main$specW2).a),
									'w2',
									w2)
								]))
						]))
				]));
	});
var $author$project$Main$main = $elm$browser$Browser$document(
	{
		cI: function (_v0) {
			return _Utils_Tuple2($author$project$Main$initModel, $elm$core$Platform$Cmd$none);
		},
		cW: function (model) {
			return $elm$core$Platform$Sub$none;
		},
		cY: $author$project$Main$update,
		c_: function (model) {
			return {
				cq: _List_fromArray(
					[
						A3(
						$elm$html$Html$node,
						'link',
						_List_fromArray(
							[
								$elm$html$Html$Attributes$rel('stylesheet'),
								$elm$html$Html$Attributes$href('https://cdn.jsdelivr.net/npm/bulma@0.8.0/css/bulma.min.css')
							]),
						_List_Nil),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('tile')
							]),
						_List_fromArray(
							[
								A4(
								$author$project$Main$reportTop,
								A2($author$project$Main$vec2Inputs, model.R, model.E.aD),
								model,
								'Vector2',
								$author$project$Vector2$all(model.E.aD)),
								A4(
								$author$project$Main$reportTop,
								A2($author$project$Main$vec3Inputs, model.R, model.E.aE),
								model,
								'Vector3',
								$author$project$Vector3$all(model.E.aE)),
								A4(
								$author$project$Main$reportTop,
								A2($author$project$Main$vec4Inputs, model.R, model.E.aF),
								model,
								'Vector4',
								$author$project$Vector4$all(model.E.aF)),
								A4(
								$author$project$Main$reportTop,
								A2($author$project$Main$mat4Inputs, model.R, model.E.av),
								model,
								'Matrix4',
								$author$project$Matrix4$all(model.E.av))
							])),
						$author$project$GitHubCorner$topRight('https://github.com/justgook/alt-linear-algebra')
					]),
				cX: 'Math Benchmark'
			};
		}
	});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(0))(0)}});}(this));