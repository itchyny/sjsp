# sjsp - Simple JavaScript Profiler
## Why?
This is a JavaScript profiler, injecting a profiling codes into your JavaScript files.

Applications written in JavaScript are getting larger these days and more complicated.
There are many JavaScript Frameworks and they sometimes make us difficult to profile
our applications. The default profilers of the Web Browsers get useless as our
applications become huge and the frameworks do tricky things. The profiler sometimes
lists many many functions of the frameworks.

Let's get back to what we really need.

#### We want profile the code we write.

In a really simple way.
```js
function test() {
  start_profile(); // grab the current date

  // our code
  
  end_profile(); // grab the date again and log the time the function cost
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
 $ sjsp test.js # this command generates test.sjsp.js
 # (sjsp is safe, does not break your test.js)
 # import the generated test.sjsp.js instead of test.js
```

## Author
itchyny (https://github.com/itchyny)

## License
This software is released under the MIT License, see LICENSE.
