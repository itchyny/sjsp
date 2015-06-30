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
  start_profile(); // grab the time at the top

  // our code
  
  end_profile(); // grab the time again and log the time the function consumed.
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
1. Use `sjsp` command on the JavaScript files you want to profile.
```
 $ sjsp test.js            # this command generates test.sjsp.js
```
  The command `sjsp` does not break your test.js, but creates a new file.
2. Use the generated test.sjsp.js instead of test.js
3. Open the page with your favorite browser and look into the JavaScript console.
   It reports the profiling information every 10 seconds.

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
the end of the functions.


But how does it handle `return` statements? Suppose the expression which is
returned by the function is heavy.
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
