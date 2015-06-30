# sjsp - Simple JavaScript Profiler
## Why?
This is a JavaScript profiler, injecting a profiling codes into your JavaScript files.

Applications written in JavaScript are getting larger these days and more complicated.
There are many JavaScript Frameworks and they sometimes make us difficult to profile
our applications. The default profilers of the Web Browsers get useless as our
applications become huge and the frameworks do tricky things. The profiler sometimes
lists many many functions of the frameworks.

Let's get back to what we really need.

#### We want to profile the code we write.

In a really simple way.
```js
function test() {
  var start_time = +new Date(); // grab the current time at the top

  // our code
  
  log_profile("test", +new Date() - start_time); // grab the current time again and log the time the function consumed.
}
```

Do we have to write the profiling codes in all the functions by hand?

Of course not.

So, here comes `sjsp`, a tool for injecting profiling codes into JavaScript files.

## Installation
```
 $ git clone https://github.com/itchyny/sjsp
 $ cd sjsp
 $ sudo cabal install
```

## Usage
```
 $ sjsp test.js            # generates test.sjsp.js
```
1. Use `sjsp` command on the JavaScript files you want to profile.
   The command `sjsp` does not break your test.js, but creates a new file.
2. Use the generated test.sjsp.js instead of test.js
3. Open the page with your favorite browser and look into the JavaScript console.
   The profiling result will be reported every 10 seconds.

The profiling result will look like the following.
```
========== SORT BY TIME ==========
time:  15.13sec    count:      71    test.js test7 (line: 37, col: 18)   function test7() {
time:  13.47sec    count:      61    test.js test5 (line: 25, col: 18)   function test5() {
time:   9.35sec    count:      68    test.js test2 (line: 7, col: 18)   function test2() {
time:   6.44sec    count:      42    test.js test6 (line: 31, col: 18)   function test6() {
time:   5.13sec    count:      46    test.js test4 (line: 19, col: 18)   function test4() {
time:   3.69sec    count:      31    test.js test3 (line: 13, col: 18)   function test3() {
time:   3.04sec    count:      24    test.js test1 (line: 1, col: 18)   function test1() {
========== SORT BY COUNT ==========
time:  15.13sec    count:      71    test.js test7 (line: 37, col: 18)   function test7() {
time:   9.35sec    count:      68    test.js test2 (line: 7, col: 18)   function test2() {
time:  13.47sec    count:      61    test.js test5 (line: 25, col: 18)   function test5() {
time:   5.13sec    count:      46    test.js test4 (line: 19, col: 18)   function test4() {
time:   6.44sec    count:      42    test.js test6 (line: 31, col: 18)   function test6() {
time:   3.69sec    count:      31    test.js test3 (line: 13, col: 18)   function test3() {
time:   3.04sec    count:      24    test.js test1 (line: 1, col: 18)   function test1() {
```
The result is easy to read and shows the functions you have to improve the performance of.

## How it works
Suppose `test.js` looks like the following.
```js
function test() {
  console.log('test');
}
```
The `sjsp` command generates `test.sjsp.js`.
```js
/* some dirty codes of sjsp */ function test() { var sjsp___state = sjsp___start("test.js",1,17,"test","function test() {");
  console.log('test');; sjsp___end(sjsp___state);
}
```
It simply inserts `sjsp___start` and `sjsp___end` function calls at the top and
the end of the functions. The local variable `sjsp___state` holds the current
time. It also saves the name, line number and column number of the function and
the whole line. When the `sjsp___end` function is called, the profiling result
is stored.

It just inserts the two statements for each functions.
However, remember that functions can be aborted with `return` statements.

How does it handle `return` statements?
Suppose the expression which is returned by the function is heavy.
```js
function test() {  
  return someHeavyFunction();
}
```
Firstly consider the following code.
```js
function test() { var sjsp___state = sjsp___start("test.js",1,17,"test","function test() {  ");  
  return someHeavyFunction(); sjsp___end(sjsp___state);
}
```
Unfortunately, the `sjsp___end` function will never be called. Then what about
placing the function before the `return` statement?
```js
function test() { var sjsp___state = sjsp___start("test.js",1,17,"test","function test() {  ");  
  sjsp___end(sjsp___state); return someHeavyFunction();
}
```
The function will surely be called but the profiling result is not be correct.
Now, let's see how `sjsp` handles `return` statements.
```js
function test() { var sjsp___state = sjsp___start("test.js",1,17,"test","function test() {  ");  
  return (function(){ var sjsp___return = someHeavyFunction(); sjsp___end(sjsp___state); return sjsp___return; } ).call(this);; sjsp___end(sjsp___state);
}
```
It creates an anonymous function, captures the result and calls the function instantly.
This way does not break the logic and the profiling result is correct.

## Author
itchyny (https://github.com/itchyny)

## License
This software is released under the MIT License, see LICENSE.
