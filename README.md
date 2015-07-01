# sjsp - Simple JavaScript Profiler
## Why?
This is a JavaScript profiler, injecting profiling codes into your JavaScript files.

Applications written in JavaScript are getting larger these days and more complicated.
There are many JavaScript Frameworks and they sometimes make us difficult to profile our applications.
The default profilers of the Web Browsers get useless as our applications become huge and the frameworks do tricky things.
The profiler sometimes lists many many functions of the frameworks.

Let's get back to what we really need.

#### We want to profile the code we write.

In a really simple way.
```js
function test() {
  var start_time = Date.now(); // grab the current time at the top

  // our code

  log_profile("test", Date.now() - start_time); // grab the current time again and log the time the function consumed.
}
```

Do we have to write the profiling codes in all the functions by hand?

Of course not.

So, here comes `sjsp`, a tool for injecting profiling codes into JavaScript files.

## Installation

There are two ways to build `sjsp`; cabal and stack. If you do not have Haskell environment yet, stack is a simpler way.

### cabal

```
 $ git clone https://github.com/itchyny/sjsp
 $ cd sjsp
 $ sudo cabal install
```

When you failed to install with the error `The program happy version >=x.xx is required but it could not be found.`, add the path to the environment variable.
```
export PATH=$PATH:$HOME/.cabal/bin
```

### stack

Prepare `stack` command to download it from [here](https://github.com/commercialhaskell/stack/releases) and then run the following script:

```
 $ git clone https://github.com/itchyny/sjsp
 $ cd sjsp
 $ stack install
```

```
export PATH=$PATH:$HOME/.local/bin
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
time:   30.20sec   count:      71         test.js          test6   (line:  31, col: 18)   function test6() {
time:   16.47sec   count:      41         test.js          test7   (line:  37, col: 18)   function test7() {
time:   15.49sec   count:     133         test.js          test4   (line:  19, col: 18)   function test4() {
time:    5.98sec   count:     216         test.js          test1   (line:   1, col: 18)   function test1() {
time:    4.37sec   count:      18         test.js          test5   (line:  25, col: 18)   function test5() {
time:    3.24sec   count:     512         test.js          test3   (line:  13, col: 18)   function test3() {
time:    0.87sec   count:      67         test.js      anonymous   (line:  49, col: 24)   setInterval(function() {
time:    0.80sec   count:       2         test.js          test2   (line:   7, col: 18)   function test2() {
time:    0.44sec   count:       2         test.js      anonymous   (line:  43, col: 23)   setTimeout(function() {
========== SORT BY COUNT ==========
time:    3.24sec   count:     512         test.js          test3   (line:  13, col: 18)   function test3() {
time:    5.98sec   count:     216         test.js          test1   (line:   1, col: 18)   function test1() {
time:   15.49sec   count:     133         test.js          test4   (line:  19, col: 18)   function test4() {
time:   30.20sec   count:      71         test.js          test6   (line:  31, col: 18)   function test6() {
time:    0.87sec   count:      67         test.js      anonymous   (line:  49, col: 24)   setInterval(function() {
time:   16.47sec   count:      41         test.js          test7   (line:  37, col: 18)   function test7() {
time:    4.37sec   count:      18         test.js          test5   (line:  25, col: 18)   function test5() {
time:    0.80sec   count:       2         test.js          test2   (line:   7, col: 18)   function test2() {
time:    0.44sec   count:       2         test.js      anonymous   (line:  43, col: 23)   setTimeout(function() {
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
/* some dirty codes of sjsp */ function test() { var sjsp__state = sjsp__start("test.js",1,17,"test","function test() {");
  console.log('test');; sjsp__end(sjsp__state);
}
```
It simply inserts `sjsp__start` and `sjsp__end` function calls at the top and
the end of the functions. The local variable `sjsp__state` holds the starting
time. It also saves the name, line number and column number of the function and
the whole line. When the `sjsp__end` function is called, the profiling result
is stored.

It just inserts the two statements for each functions.
However, remember that functions can be aborted with `return` statements.
How does it handle `return` statements?

Suppose the expression which is returned by the function is heavy.
```js
function test() {  
  return someHeavyExpression;
}
```
Firstly consider the following code.
```js
function test() { var sjsp__state = sjsp__start("test.js",1,17,"test","function test() {  ");  
  return someHeavyExpression; sjsp__end(sjsp__state);
}
```
Unfortunately, the `sjsp__end` function will never be called. Then what about
placing the function before the `return` statement?
```js
function test() { var sjsp__state = sjsp__start("test.js",1,17,"test","function test() {  ");  
  sjsp__end(sjsp__state); return someHeavyExpression;
}
```
The function will surely be called but the profiling result is not correct.
Now, let's see how `sjsp` handles `return` statements.
```js
function test() { var sjsp__state = sjsp__start("test.js",1,17,"test","function test() {  ");  
  return (function(){ var sjsp__return = someHeavyExpression; sjsp__end(sjsp__state); return sjsp__return; } ).call(this);; sjsp__end(sjsp__state);
}
```
It creates an anonymous function, captures the result and calls the function instantly.
This way does not break the logic and the profiling result is correct.

## Author
itchyny (https://github.com/itchyny)

## License
This software is released under the MIT License, see LICENSE.
