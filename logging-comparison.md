---
layout: default
title: Comparison of Common Lisp Logging Libraries
---
- [Comparison of Common Lisp Logging Libraries](#org4f9eb78)
- [Comparing a-cl-logger, cl-syslog, com.ravenbrook.common-lisp-log, hu.dwim.logger, log4cl, log5, verbose, and vom](#org050713d)
- [Libraries](#org20786e0)
- [Quick Summary](#org4855ee6)
  - [Best](#orgf6c6740)
  - [Not So Best](#org804cda7)
  - [Overall Preference](#orgc797479)
- [Problem Space](#orgc092479)
  - [A comment about timestamps.](#orgead86d8)
  - [Note to self about the logging messages](#orgf6334a1)
  - [Comparative Samples](#orgd46c174)
- [Glossary:](#orgdaf0980)
  - [Loggers/ Log Managers](#orgc4a43e3)
  - [Messengers/Senders/Appenders/Faucets](#org6a67dee)
  - [Levels and Categories](#org383520f)
- [Wish list](#orge863103)
- [Functionality Comparisons](#org3a574d7)
- [Default Available Log Levels](#org150bf85)
- [Benchmarking](#org36a7daf)
  - [Benchmarking with logging](#org3995554)
  - [Benchmarking without triggering logging](#org5726596)
- [a-cl-logger](#orgfe1b50c)
  - [Summary](#org081d1a2)
  - [Setup](#orgdcae309)
  - [Built-in Log Levels](#orgaa3393f)
  - [Logging Call with sample message and Change Log Levels](#org05f5f44)
  - [Logging to File](#org0e9c7af)
  - [Muting Logging to Repl](#org8d77e4a)
  - [Additional Features](#org4a6a6d3)
- [cl-log aka com.ravenbrook.common-lisp-log](#org765b029)
  - [Summary](#org8c60e78)
  - [Setup](#orgdb78a62)
  - [Log Levels](#org1e4b4c7)
  - [Logging Call with sample message](#orgf32f81e)
  - [Logging to Ring](#org4097b65)
  - [Muting Logging to Repl](#orgc003710)
  - [Log Rotation](#org0bdda41)
  - [Timestamp access](#org4cf61fd)
  - [Loggers aka Log-managers](#orgcd427a4)
  - [Messages](#orgefb426c)
  - [Finding a messenger](#org417fd98)
- [cl-syslog](#org1d7d00c)
  - [Summary](#orgf206334)
  - [Setup](#org052ab0e)
  - [Built-in Log Levels](#org469373b)
  - [Logging Call with sample message](#org00a6d73)
  - [Changing Log Levels](#org50fcb4f)
  - [Logging to Remote System](#orga7596c9)
  - [Log Rotation](#orga349daa)
- [hu.dwim.logger](#org37145b1)
  - [Summary](#orgf817bc9)
  - [Setup](#orgf030070)
  - [Built-in Log Levels](#orge4e63fa)
  - [Logging Call with sample message](#orgeef9776)
  - [Logging to File](#orgb28aab4)
  - [Muting Logging to Repl](#org508a553)
  - [Log Rotation](#org5d3d8b4)
  - [Additional Features](#orgd8c2584)
  - [Misc](#org293bed5)
- [log4cl](#org5ac6999)
  - [Summary](#org54b92f0)
  - [Setup](#org2873223)
  - [Built-in Log Levels](#orgbd5f230)
  - [Logging Call with sample message](#org81248d6)
  - [Changing Log Levels](#org8c6e16e)
  - [Logging to File](#org18c2ba0)
  - [Muting Logging to Repl](#org2edd997)
  - [Log Rotation](#org14d981f)
  - [Additional Features](#orgb45192d)
  - [Problems](#orgf1c15f8)
- [log5](#org5f7082e)
  - [Summary](#org7024aee)
  - [Setup](#org2060e29)
  - [Built-in Log Levels](#org9392a43)
  - [Built-in Categories](#orga44ad64)
  - [Logging Call with sample message](#org7be81b9)
  - [Changing Log Levels](#org497905e)
  - [Logging to File](#org46f8481)
  - [Muting Logging to Repl](#org7b03105)
  - [Unmuting Logging to Repl](#org1f85f29)
  - [Log Rotation](#org770a321)
  - [Config File](#org48dc728)
- [verbose](#org4c5e457)
  - [Summary](#orgce313fc)
  - [Setup](#org0fa4464)
  - [Built-in Log Levels](#org59385b2)
  - [Logging Call with sample message](#org5154aa9)
  - [Changing Log Levels](#orgd4d5648)
  - [Categories](#org418f5fe)
  - [Sharing across threads](#org688158a)
  - [Log to File](#org648e88c)
  - [Log to Rotating Log File](#org4b9fd3b)
  - [Muting Logging to Repl](#orgb667ff2)
  - [Timestamp formatting](#orge7c0e37)
  - [Restarting the Global Controller](#orgd09fa6f)
- [vom](#orgffa1206)
  - [Summary](#org819e464)
  - [Setup](#orgcfe8a1b)
  - [Built-in Log Levels](#org50fa663)
  - [Logging Call with sample message](#org8a586f4)
  - [Changing Log Levels](#orgf01681b)
  - [Logging to File](#org2974d95)
  - [Setting Multiple Streams](#org4c2df11)
  - [Muting Logging to Repl](#org360edce)
  - [Additional Features](#org6f44347)
- [Function Mapping](#org2e5b9b0)
- [To Do](#org2ae08e4)


<a id="org4f9eb78"></a>

# Comparison of Common Lisp Logging Libraries

<a id="org9155e6a"></a>


<a id="org050713d"></a>

# Comparing a-cl-logger, cl-syslog, com.ravenbrook.common-lisp-log, hu.dwim.logger, log4cl, log5, verbose, and vom

Common Lisp has several logging libraries to choose from (<http://cliki.net/Development>). Of course, many of us find ourselves throwing together an ad-hoc logging library in the middle of development. I decided I wanted to make an informed choice, leading up to this comparison article. I do want to mention that I have left out at least two. First, [logv](https://github.com/nallen05/logv), which I could not get to compile. Second, [hu.dwim.logger](http://dwim.hu/darcsweb/darcsweb.cgi?r=HEAD%20hu.dwim.logger;a=summary) because I just cannot get my head around the idiosyncratic syntax used. I will, however, add them (or others) and invite anyone who is willing to fill in the blanks to email me at sabra.crolleton@gmail.com.


<a id="org20786e0"></a>

# Libraries

| Library                                                  | Author                                            | License       | Website                                                                  | Most Recent Quicklisp Version |
|-------------------------------------------------------- |------------------------------------------------- |------------- |------------------------------------------------------------------------ |----------------------------- |
| [a-cl-logger](#org6999e97)  AKA a-log                    | Russ Tyndall,  Nathan Bird, Ryan Davis            | BSD           | <https://github.com/AccelerationNet/a-cl-logger>                         | April 6 2015 (1)              |
| [cl-syslog](#org193a076)                                 | Erik Enge, Mike Maul                              | MIT           | <https://github.com/mmaul/cl-syslog>                                     | cl-syslog-20140713            |
| [cl-log](#org273cad2) AKA com.ravenbrook.common-lisp-log | Nick Levine                                       | Public Domain | <http://www.nicklevine.org/cl-log/>                                      | 2012-05-15                    |
| [hu.dwim.logger](#org879e2c2)                            | Tamás Borbély,  Attila Lendvai,  Levente Mészáros | BSD           | <http://dwim.hu/darcsweb/darcsweb.cgi?r=HEAD%20hu.dwim.logger;a=summary> | hu.dwim.logger-20151218       |
| [log4cl](#org87c97e5)                                    | Max Mikhanosa                                     | Apache 2.0    | <https://github.com/7max/log4cl>                                         | log4cl-20141217               |
| [log5](#org23e5aea)                                      | Gary Warren King                                  | MIT           | <https://github.com/gwkkwg/log5>                                         | log5-20110619                 |
| [verbose](#orgbd4e2e1)                                   | Nicolas Hafner                                    | Artistic      | <https://github.com/Shinmera/verbose>                                    | verbose-20160531              |
| [vom](#orgd3cb374)                                       | Andrew Danger Lyon                                | MIT           | <https://github.com/orthecreedence/vom>                                  | vom-20160318                  |

(1) Most recent version on github is 6 April 2015. A-cl-logger is not in quicklisp.

[top](#org9155e6a) <a id="orgc14aa43"></a>


<a id="org4855ee6"></a>

# Quick Summary

For terminology purposes, think of "loggers" as the first entry point in determining whether a log message embedded in the source code is logged and "senders" as the second stage in determining where to log the message. Loggers and senders may or may not have their own sets of filters to trigger their various actions. In general, more "loggers" and "senders" result in more flexibility for having many logging messages embedded in code, but only turning them on when desired and, potentially, turning on different priority levels in different portions of an application. It also enables flexibility with respect to how many messages are turned on during development (and where they are sent) compared to what is logged while an application runs unattended.

| Library                            | [a-cl-logger](#org6999e97) | [cl-syslog](#org193a076) | [cl-log](#org273cad2) | hu.dwim.logger | [log4cl](#org87c97e5) | [log5](#org23e5aea) | [verbose](#orgbd4e2e1) | [vom](#orgd3cb374) |
|---------------------------------- |-------------------------- |------------------------ |--------------------- |-------------- |--------------------- |------------------- |---------------------- |------------------ |
| Can Turn Logging Off               | YES                        | NO                       | YES                   |                | YES                   | YES                 | YES                    | YES                |
| Multiple Loggers                   | YES                        | NO                       | YES (1)               |                | YES                   | NO                  | NO                     | NO                 |
| Loggers Have Filters               | YES                        | NO                       | YES                   |                | YES                   | NO                  | NO                     | NO                 |
| Multiple Senders per Logger        | YES                        | NO                       | YES                   |                | YES                   | YES                 | YES                    | YES                |
| Senders Have Filters               | PRIORITY                   | NO                       | YES                   |                | YES                   | YES                 | YES                    | (5)                |
| Multiple Priority Levels           | YES                        | (2)                      | YES                   |                | YES                   | YES                 | YES                    | (5)                |
| Levels and Categories are distinct | NO                         | NO                       | NO (3)                |                | YES                   | NO                  | YES                    | NO                 |
| Logger Inheritance Hierarchies (6) | YES                        | NO                       | NO                    |                | YES                   | NO                  | NO                     | NO                 |
| Category Hierarchies               | NO                         | NO                       | NO (4)                |                | YES                   | NO (4)              | YES                    | NO                 |

(1) If you want to call a logger which is not the global logger, you need to use a slightly different logging function 'log-manager-message' rather than 'log-message'. See [cl-log-multiple-logger-setup](#orgf72bf30)

(2) Cl-syslog has multiple priority levels, but that is just a flag in the log message and is not filterable.

(3) Categories and levels are the same types of flags in cl-log.

(4) You can accomplish a similar result to hierarchies with appropriate use of 'or' and 'and' in the filters.

(5) You can set the log level by package, but not by any finer granularity

(6) Taking a-cl-logger as an example, there is a **root-logger** and then you can create instance of other loggers which will be children of the root logger. If the **root-logger** has appenders and the logging level of the **root-logger** and the child loggers are equal to or lower in priority than that specified in the log message, then both **root-logger** and child loggers will log the message.

Aside from flexibility, each library seems to have one or two features that make it standout. That means that which one you choose comes down to whether there is a killer feature that you need which only happens to be in one library or which library just feels more comfortable to you.


<a id="orgf6c6740"></a>

## Best

-   Simplest: [vom](#orgd3cb374), [cl-syslog](#org193a076)

-   Best User Defined Category Handling: [log5](#org23e5aea) and [verbose](#orgbd4e2e1)

-   Ease in Changing Global Logging Levels: [verbose](#orgbd4e2e1), [vom](#orgd3cb374),

-   Most Flexible (Separating different packages, etc): [log4cl](#org87c97e5), [cl-log](#org273cad2), [verbose](#orgbd4e2e1)

-   Integration with system logs: [cl-syslog](#org193a076)

-   Ring logging: [cl-log](#org273cad2)

-   Log File Rotation: [verbose](#orgbd4e2e1), [log4cl](#org87c97e5)

-   Remote Logging: [cl-syslog](#org193a076)

-   Integration with slime: [log4cl](#org87c97e5). Log4cl has the best integration with slime if you happen to be looking more for logging as an interactive debugger, but it is also throwing its own error in some functions.

-   Integration with logstash: a-cl-logger

-   Speed: [log5](#org23e5aea)

-   Best Automatic Context Generation: [log4cl](#org87c97e5) (automatically logs package and function names and time)

-   Most Recent Development: [verbose](#orgbd4e2e1) and [vom](#orgd3cb374)


<a id="org804cda7"></a>

## Not So Best

-   Slowest: [a-cl-logger](#org6999e97). a-cl-logger was markedly slower than all the other libraries. The maintainer agrees but may not have the time to fix the performance issue.

-   Unable to turn off logging: [cl-syslog](#org193a076)

-   Changing logging levels: tie between [cl-log](#org273cad2) and [log5](#org23e5aea)

-   Most Confusing: [log4cl](#org87c97e5) due to multiple namespaces


<a id="orgc797479"></a>

## Overall Preference

I have two general use cases - development and logging on remotely running long lived programs. Based on those use cases, I have a very slight preference for the category and level functionality of verbose and the ability to set file rotation at pretty much any time interval that seems to work best for that application. [top](#org9155e6a)


<a id="orgc092479"></a>

# Problem Space

<a id="org06b0493"></a> Logging is used for troubleshooting, auditing, profiling, alerts to impending danger and statistics. In a way, it is like documentation at runtime. As matthieum commented on reddit.com “Logs are like tests, you don't know if you have enough logs, you only know that you don't have enough when you get stumped.” Logging is complementary to testing. Testing is exercising functions under controlled conditions. Logging allows access to information in uncontrolled conditions. Logs help identify maintenance and perform and show whether new additions are creating more or less problems.

At its absolute simplest, it is like putting printf statements in your code and being able to turn them on and off at will. Thus, a logging framework should provide functionality to generate a text message for display, processing or to save the message somewhere for future review. Logging frameworks can set different levels which will be triggered, displayed, processed or stored in different locations. For example, an error-level message may be treated differently than a debug-level message. Typically a logging level is set such that only certain levels of errors (or higher) are triggered and logged. Thus you often have logging statements scattered through your code but only high level problem statements are actually logged unless you reset the trigger level to log more information. For that reason people may be interested in benchmarks showing a function without logging, the function with logging statements but below the trigger level and the function with logging statemente where the logging is actually triggered.

Some people believe that a logging library should always output to the repl and its secondary function should be logging to a file. That assumes that you always have repl available as the program is running. That is not always true with respect to applications running remotely (in the cloud or wherever) which may be running unattended. As a result, I have a preference for being able to turn the repl logging on and off conveniently.

This being common lisp, with the source being open and the licenses permissive, you can add functionality to any of these libraries. However, for purposes of this comparison, I am only comparing out of the box features with a caveat. cl-log does require that you define your categories whereas the other libraries have default categories that you can use.


<a id="orgead86d8"></a>

## A comment about timestamps.

Timestamps are good things. Timestamps with timezones (or using universal time) is almost required if you anticipate that your application is going to be crossing timezones. If timing is critical, debugging occasionally requires that you determine if the user's timezone output is properly set.


<a id="orgf6334a1"></a>

## Note to self about the logging messages

This is a reminder to myself as much as anything. Good logging output often requires context and variable state. Remember who your audience is for each type of statement. The users can be very different. Consider the different needs of an application user, a help desk person, a system administrator or a developer. You may need very different error messages for the different audiences. This implies an advantage for the libraries providing different loggers for different purposes that you can turn on and off.

You might also consider whether you need a log that is in a machine parseable format (json?) or human readable. Either way, consistency in logging message format will be extremely helpful. For machine paraseable formatted messages, right now you are probably going to have to write your own message formatting function that you pass to the logging statement.

If you are internationalizing/localizing, you probably need centralized error codes.

Be really careful about conditionals in logging messages. I have been known to insert conditionals that themselves trigger errors in the program - resulting in the embarrassing situation that the code was correct but the logging message crashed the program.

Finally, and this is important - Never log sensitive data – e.g. social security numbers, credit cards, passwords etc.

The following are some links to what some people consider best practices in logging. <https://www.owasp.org/index.php/Logging_Cheat_Sheet> , <https://logentries.com/doc/best-practices-logs/>, <http://dev.splunk.com/view/logging-best-practices/SP-CAAADP6>, <http://stackify.com/smarter-errors-logs-putting-data-work-2/>

[top](#org9155e6a) <a id="orgc84a461"></a>


<a id="orgd46c174"></a>

## Comparative Samples

1.  Sample setup for the different libraries

    The following are some sample calls to set up the different libraries. These specifically were intended to turn off any logging to the repl because I only wanted to log to a file.

    ```lisp
    (defun setup-a-cl-logger ()
      "setup a-cl-logger so that it logs to file but only with warning or higher categories. Drop the appenders in the root logger.
    Does the fact that we are using a child of the root logger slow down a-cl-logger? Answer: Not appreciably."
      (pop (a-cl-logger:appenders a-cl-logger:*root-logger*)) ;get rid of
                                            ;repl logger that is set up by
                                            ;default in the root-logger
      (a-cl-logger:define-logger filelog2 ()
                               :appenders
                               (make-instance 'a-cl-logger:file-log-appender
                                              :log-file "/home/sabra/test/a-cl-logger-test.log"
                                              :buffer-p nil))
      (setf (a-cl-logger:log-level (a-cl-logger:get-logger 'filelog2)) a-cl-logger:+warn+))

    (defun setup-cl-syslog ()
      "No setup required")

    (defun setup-cl-log ()
      "Setting up cl-log categories, creating a log-manager and starting a messenger that only logs warning or higher"
      (cl-log:defcategory :critical)
      (cl-log:defcategory :error   (or :error :critical))
      (cl-log:defcategory :warning (or :warning :error))
      (cl-log:defcategory :notice  (or :notice :warning))
      (cl-log:defcategory :info    (or :info :notice))
      (cl-log:defcategory :debug   (or :debug :info))
      (cl-log:defcategory :trace   (or :trace :debug))
      (setf (cl-log:log-manager) (make-instance 'cl-log:log-manager
                                                :message-class 'cl-log:formatted-message))
      (cl-log:start-messenger 'cl-log:text-file-messenger :filename
                              "/home/sabra/test/cl-log-test.log"
                              :filter '(and :warning)))

    (defun setup-log4cl ()
      "Set up log4cl so that it only logs to file and logs warning or higher"
      (log4cl:remove-all-appenders log4cl:*root-logger*)
      (log:config :daily "/home/sabra/test/log4cl-test1.log" :backup nil)
      (setf (log4cl:logger-log-level Log4cl:*root-logger*) log4cl:+log-level-warn+))

    (defun setup-log5 ()
      "Starting a sender that will only log warn, error or fatal"
      (log5:start-sender 'debug  (log5:stream-sender
                                  :location "/home/sabra/test/log5.log")
                         :output-spec '(time log5:category log5:message)
                         :category-spec '(or log5:WARN log5:ERROR
                                          log5:FATAL)))

    (defun setup-verbose ()
      "Alternative 1 for setting up verbose: Redirecting the repl logging"
      (setf (v:repl-level) :warn)
      (setf (v:shared-instance '*standard-output*) (open #p"~/test/verbose.log" :direction :output :if-exists :append)))

    (defun setup-verbose-1 ()
      "Alternative 1I for setting up verbose: Actually getting rid of the repl logging and only setting up a file-faucet"
      (when (> (length (v::pipeline v:*global-controller*)) 0)
        (piping:remove-segment v:*global-controller* '(0)))
      (v:add-pipe (make-instance 'piping:predicate-filter
                                 :predicate #'(lambda (message) (verbose:message-visible message :WARN)))
                  (make-instance 'v:file-faucet :file #p"~/test/verbose-1.log")))

    (defun setup-vom ()
      "No setup required")

    ```

2.  Sample logging calls embedded in your code with resulting log entries

    These assume you have already done any required setup.

    ```lisp
    (a-cl-2.dribble "a-cl-logger enter sub-step-1 ~a" x) ;where a-cl-2 was
                                            ;a logger you created with define-logger

    (a-cl-logger:do-log a-cl-logger:*root-logger* a-cl-logger:+error+ "log something here ~a" 10)
    2016-05-25T21:35:49.238951-07:00  ROOT-LOGGER ERROR   log something here 10

    (cl-log:log-message :critical "fun fun ~a" "fun")
    3673226771.075 CRITICAL Fun fun fun

    (cl-syslog:log "myprog" :syslog :debug
                   (format nil "cl-syslog enter sub-step-1 ~a" x))

    (log4cl:log-info  "log4cl enter sub-step-1 ~a" 10)
     <INFO> [21:43:55] library-comparison () - log4cl enter sub-step-1 10

    (log5:log-for log5:error+ "Log5 enter sub-step-1 ~a" 10)
    3673226566 ERROR+ Log5 enter sub-step-1 10

    (v:info '(:foo.library-comparison.function1) "fun fun ~a" "fun")
    LOG: 2016-05-25 21:50:04 [INFO ] <FOO.LIBRARY-COMPARISON.FUNCTION1>: fun fun fun

    (vom:emerg "Vom enter sub-step-1 ~a" 10)
     <EMERG> [21:51:18] library-comparison - Vom enter sub-step-1 10
    ```

3.  Logging messages with more complex categories

    1.  a-cl-logger

        [a-cl-logger](#org6999e97) does not really have categories. You can throw a lot of things into the log message. E.g.

        ```lisp
        (a-cl-logger:do-log 'testlog a-cl-logger:+debug+
                            :a-plist-key :a-plist-value
                            :some-key "some value")
        2016-05-08T19:26:02.896705-07:00      TESTLOG DEBUG    A-PLIST-KEY::A-PLIST-VALUE, SOME-KEY:some value
        ```

    2.  cl-log

        In [cl-log](#org273cad2), categories can be passed as an atom or list in the log-message or log-manager message. Both log-managers and messengers have filters that accept everything, only a single specified item or a list starting with AND, OR and NOT. The filter cannot be changed.

    3.  cl-syslog

    4.  log4cl

    5.  log5

        Start by defining categories which can be as complicated as you like. Then start a sender which picks up at least some of these new categories. Then your log-for calls are sprinkled across your source code.

        ```lisp
        (log5:defcategory file-system)
        (log5:defcategory memory)
        (log5:defcategory process)
        (log5:defcategory application)
        (log5:defcategory operating-system
             (and (or file-system memory process
                  (not application))))

        (log5:start-sender 'repl-ops  (log5:stream-sender
                                        :location *standard-output*)
                           :output-spec '(time log5:category log5:message)
                           :category-spec '(or operating-system log5:ERROR))

        (log5:log-for (and operating-system memory) "something ~a" "warm")
        >> 3671731698 (AND OPERATING-SYSTEM MEMORY) something warm
        ```

    6.  verbose

        The following provides an example of the category tree filter for verbose. First we add a hierarchical category that has a wildcard. Since our category definition required "foo.bar.<something>", if we try a message that does not have the full required hierarchy, it does not get logged.

        ```lisp
        (v:add-repl-category :foo.bar.*)
        (:FOO.BAR.*)

        (v:info :foo "Foo")
        NIL

        (v:info :foo.bar.1 "Foo")
        NIL
        LOG: 2016-05-08 18:36:04 [INFO ] <FOO.BAR.1>: Foo

        (v:info :foo.bar.2 "Foo")
        NIL
        LOG: 2016-05-08 18:36:10 [INFO ] <FOO.BAR.2>: Foo
        ```

    7.  vom

        [top](#org9155e6a)


<a id="orgdaf0980"></a>

# Glossary:

<a id="orga77eb25"></a> The simplest libraries (e.g. vom) just have you configure what level to log and where to send the message. The more complex libraries add more flexibility. You may have different loggers for different packages or purposes, multiple messengers/appenders for each logger with each messenger or appender sending the messages to different places (e.g. the console, a file, a remote system) and each messenger or appender may respond to different categories, levels or filters.


<a id="orgc4a43e3"></a>

## Loggers/ Log Managers

<a id="org0ee0781"></a> These may be called loggers (a-cl-logger, log4cl) or log-managers (cl-log, log5). [Log4cl](#org87c97e5) actually calls them categories which I think confuses things even more. Essentially these are class instances which allow logging. They may be set up to only log particular packages, source files, or whatever. There may be only a single logger or there may be multiple loggers subclassed off a root-logger which may or may not be tied into a hierarchy. Each logger will have one or more messengers/appenders attached to it.


<a id="org6a67dee"></a>

## Messengers/Senders/Appenders/Faucets

<a id="org5a84b0c"></a> The terminology varies from library to library - messengers (cl-log). senders (log5), appenders (a-cl-logger, log4cl) or faucets (verbose). In any case, one or more are typically owned by a logger and these classes define where a message will be logged (e.g. a file, a stream, a ring, a system-log, etc). They will have filters that accept certain levels/categories of messages and then will write those messages plus potentially other information to various streams, files, rings, or whatever. Generally any one messenger or appender has a single destination that it sends a message.

For consistency, for the rest of this article I will refer to these as messengers.


<a id="org383520f"></a>

## Levels and Categories

<a id="org40ee55b"></a> Some libraries distinguish between log levels and categories, some treat them the same. I personally think they are two entirely different concepts, but YMMV. The libraries that treat them differently use the log level to determine whether the message is should be logged and will allow you to set the log level as a priority filter at any time. Those libraries use categories to allow additional levels of filtering.

1.  Log Levels

    <a id="orga61e9d5"></a> Log levels denote the level of importance or priority. Typically log levels are set at a lower level during development and testing (getting more information) and higher levels in production. Typical log levels are:

    -   Fatal – Should only occur with something that should force the program to terminate.

    -   Critical – About to go fatal if something doesn’t happen to help.

    -   Error – There is a real error which needs to be logged and investigated, but it does not rise to the panic levels of critical and fatal. You really should log every error condition.

    -   Warn – Log all events that could potentially become an error. For example, you are about to run out of resources. It could also include use of deprecated APIs or other almost errors.

    -   Notice – All significant events which are not considered errors

    -   Info – User driven or system specific actions. Administrators and advanced users should be able to understand the message.

    -   Debug, Trace – different levels during troubleshooting. These should only be logged to a file and typically are not logged to a console. Some people argue that even production systems should have this running if it doesn’t clog the system so as to help debug remote applications without having to walk the user through changing logging levels.

2.  Setting all libraries' logging level to :warn

    The following are sample calls to set all the libaries' global logging level to :warn. (This does assume that you have written the function suggested by cl-log to help that library and you have done any required setup.) Note that some libraries allow multiple loggers or senders which may allow you to set different levels operating simultaneously.

    ```lisp
    (setf (v:repl-level) :error) ; this is verbose

    (vom:config t :warn)

    (setf (a-cl-logger:level (a-cl-logger:get-logger 'a-cl-2))
          a-cl-logger:+warn+)

    (log:config log4cl:+LOG-LEVEL-WARN+) ; this is log4cl

    (setf (log-level) :warn) ; setting log-level for cl-log. This requires a
                             ; self written function and only applies to the root logger.

    (log5:stop-sender 'repl-warn)
    (log5:start-sender 'repl-warn (log5:stream-sender
                                   :location *standard-output*)
                       :output-spec '(time log5:category log5:message)
                       :category-spec '(or log5:INFO log5:WARN log5:ERROR
                                           log5:FATAL))
    ```

3.  Getting the current logging levels

    Getting the current level for when messages will actually be logged is obviously going to be easier if you are running only a single logger and only one messenger. The more filters you have running, the more complicated it might be to determine why something was or was not logged.

    ```lisp
    (a-cl-logger:log-level-name-of ; This looks ugly
      (a-cl-logger:log-level
        (a-cl-logger:get-logger 'a-cl-2)))

    (get-cl-log-level-set)

    (log4cl:log-level-to-string
      (log4cl:logger-log-level log4cl:*root-logger*))

    (log5:category-spec
      (first (log5:senders)))

    (v:repl-level)

    vom::*config*
    ```

4.  Categories

    <a id="orge060ef0"></a> If the library distinguishes between log levels and categories, categories are user defined and provide further information and often the filters in messengers will look at categories to decide whether to log the message. Categories may be individual items (e.g. system-error) or they might be trees (e.g. database.table.row).

    <a id="org0da9518"></a> [top](#org9155e6a)


<a id="orge863103"></a>

# Wish list

<a id="org3e42417"></a> No, we do not need another logging library. If I was going to extend a library, some of the features I would add might be: json output for machine parseable logs, the ability to compress and/or encrypt the log files and ensuring that timestamps had timezone or stick to universal time which would be easier for machine parsing. Depending on the application, I might want support for localization.

Maybe something like the following from tech.grammarly.com/blog/posts/Running-Lisp-in-Production.html

```lisp
(defun graylog (message &key level backtrace file line-no)
  (let ((msg (salza2:compress-data
              (babel:string-to-octets
               (json:encode-json-to-string
                #{:version "1.0"
                :facility "lisp"
                :host *hostname*
                :|short_message| message
                :|full_message| backtrace
                :timestamp (local-time:timestamp-to-unix (local-time:now))
                :level level))
               :encoding :utf-8)
              'salza2:zlib-compressor)))
    (usocket:socket-send
     (usocket:socket-connect
      *graylog-host* *graylog-port*
      :protocol :datagram :element-type '(unsigned-byte 8))
     msg (length msg))))
```

<a id="org40b9cd6"></a> [top](#org9155e6a)


<a id="org3a574d7"></a>

# Functionality Comparisons

<a id="org2819aba"></a>

| Library                            | [a-cl-logger](#org6999e97) | [cl-syslog](#org193a076) | [cl-log](#org273cad2) | hu.dwim.logger | [log4cl](#org87c97e5)   | [log5](#org23e5aea)           | [verbose](#orgbd4e2e1)   | [vom](#orgd3cb374) |
|---------------------------------- |-------------------------- |------------------------ |--------------------- |-------------- |----------------------- |----------------------------- |------------------------ |------------------ |
| Can Turn Logging Off               | YES                        | NO                       | YES                   |                | YES                     | YES                           | YES                      | YES                |
| Multiple Loggers                   | YES                        | NO                       | YES (1)               |                | YES                     | NO                            | NO                       | NO                 |
| Loggers Have Filters               | YES                        | NO                       | YES                   |                | YES                     | NO                            | NO                       | NO                 |
| Multiple Senders per Logger        | YES                        | NO                       | YES                   |                | YES                     | YES                           | YES                      | YES                |
| Senders Have Filters               | PRIORITY                   | NO                       | YES                   |                | YES                     | YES                           | YES                      | (5)                |
| Multiple Priority Levels           | YES                        | (2)                      | YES                   |                | YES                     | YES                           | YES                      | (5)                |
| Levels and Categories are distinct | NO                         | NO                       | NO (3)                |                | YES                     | NO                            | YES                      | NO                 |
| Logger Hierarchies                 | YES                        | NO                       | NO                    |                | YES                     | NO                            | NO                       | NO                 |
| Category Hierarchies               | NO                         | NO                       | NO (4)                |                | YES                     | YES (4)                       | YES                      | NO                 |
|                                    |                            |                          |                       |                |                         |                               |                          |                    |
| Documentation?                     | YES                        | YES                      | YES                   | NO             | Very good               | YES    (look in source files) | YES                      | YES                |
| User Defined Categories?           | NO                         | NO                       | YES                   |                | YES                     | YES                           | YES                      | NO                 |
| Optional Config File Use?          | NO                         | NO                       | NO                    |                | YES                     | YES                           | NO                       | NO                 |
| Slime Integration?                 | NO                         | NO                       | NO                    |                | YES                     | NO                            | NO                       | NO                 |
| Set trigger level globally         | YES                        | NO                       | YES                   |                | YES                     | YES                           | YES                      | YES                |
| Set trigger level by package       |                            | NO                       | YES                   |                | YES (logs package name) |                               |                          | YES                |
| Set trigger level by logger        | YES                        |                          |                       |                |                         |                               |                          |                    |
| Set trigger level by function      |                            | NO                       | NO                    |                | YES                     | NO                            | NO                       | NO                 |
| Category Filters?                  | YES                        | NO                       | YES                   |                | YES                     | YES                           | YES                      | NO                 |
| Textfile Logs?                     | YES                        | YES                      | YES                   | YES            | YES                     | YES                           | YES                      | YES                |
| Ring Logs?                         | NO                         | NO                       | YES                   |                | NO                      | NO                            | NO                       | NO                 |
| Stream Logs?                       | YES                        | NO                       | YES                   |                | YES                     | YES                           | YES                      | YES                |
| Socket Logging?                    | YES                        | YES                      | NO                    |                | NO                      | NO                            | NO                       | NO                 |
| SMTP or Email Logging?             | NO                         | NO                       | NO                    |                | NO                      | NO                            | NO                       | NO                 |
| SysLog Logging?                    | NO                         | YES                      | NO                    |                | NO                      | NO                            | NO                       | NO                 |
| Remote Logging                     |                            | YES                      | NO                    |                |                         | NO                            |                          | NO                 |
| Binary Logs                        |                            | NO                       | YES (6)               |                |                         |                               |                          | NO                 |
| Logstash loggins                   | YES                        | NO                       | NO                    |                | NO                      | NO                            | NO                       | NO                 |
| Output as json?                    | (7)                        | NO                       | NO                    |                | NO                      | NO                            | NO                       | NO                 |
| Rotate Logs?                       | NO                         | Relies on System         | NO                    |                | Daily (8)               | NO                            | YES As often as you want | NO                 |
| Compress Log Files?                | NO                         | Relies on system         | NO                    |                | NO  (8)                 | NO                            | NO                       | NO                 |
| Recover from i/o failures?         | YES                        | NO                       | NO                    |                | YES                     | Can be set to enter debugging | ?                        | NO                 |
| Different logging by user?         | NO                         | NO                       | NO                    |                | NO                      | NO                            | NO                       | NO                 |
| Multiple Loggers?                  | YES                        | 2 if remote logging      | YES                   |                | YES                     | NO                            | NO                       | NO                 |
| Multiple Streams?                  | YES                        | 2 if remote-logging      | YES                   |                | YES                     | YES                           | YES                      | YES                |
| Mute Individual Streams?           | YES                        | NO                       | YES                   |                |                         | YES                           | YES                      | NO                 |
| Hierarchical Categories            |                            |                          |                       |                |                         |                               | YES                      |                    |
| Variables in Messages              | YES                        | YES                      | YES                   |                | YES                     | YES                           | YES                      | YES                |
| Configurable Messages              | YES                        | NO                       | YES                   |                | YES                     | YES                           | Somewhat                 | NO                 |
| Can take function as messages      | YES                        | YES                      | YES                   |                | YES                     | YES                           | YES                      | YES                |
| Node-Logstash Integration          | YES                        | NO                       | NO                    |                | NO                      | NO                            | NO                       | NO                 |
| Thread handling?                   | NO                         | YES                      | YES                   |                | YES                     | NO                            | YES                      | NO                 |
| Thread safe                        | NO                         | YES                      | ?                     |                | YES                     | NO                            | YES                      | NO                 |
| Any Localization Support?          | NO                         | NO                       | NO                    |                | NO                      | NO                            | NO                       | NO                 |
| Multiple categories in entry       | NO                         | NO                       | NO                    |                | NO                      | NO                            | YES                      | NO                 |
| Conditions,Signals and Restarts    | YES                        | NO                       | NO                    |                | YES                     | NO                            | YES                      | NO                 |
| Message Interception               | YES                        |                          |                       |                |                         |                               |                          |                    |
| Default Timestamps                 | YES with timezone          | month-day hh:mm:ss       | Universal time        |                | YES                     | Universal time                | year-month-day hh:mm:ss  | hh:mm:ss           |

(1) If you want to call a logger which is not the global logger, you need to use a slightly different logging function 'log-manager-message' rather than 'log-message'. See [cl-log-multiple-logger-setup](#orgf72bf30)

(2) Cl-syslog has multiple priority levels, but that is just a flag in the log message and is not filterable.

(3) Categories and levels are the same types of flags in cl-log.

(4) You can accomplish a similar result to hierarchies with appropriate use of 'or' and 'and' in the filters.

(5) You can set the log level by package, but not by any finer granularity

(6) See <http://nklein.com/2011/04/binary-logging-with-cl-log/> for an explanation of binary usage.

(7) The ability to output json seems to have begun but never finished.

(8) You can add methods to the generic function backup-log-file that could compress or encrypt the backup file or change the log rotation from daily to weekly or whatever. [top](#org9155e6a)


<a id="org150bf85"></a>

# Default Available Log Levels

<a id="orga38cbd6"></a>

| Library                    | Default Levels                                                                                                                                                                                                                                                                                                 |
|-------------------------- |-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [a-cl-logger](#org6999e97) | fatal error warn info debug dribble                                                                                                                                                                                                                                                                            |
| cl-log                     | NO DEFAULT LEVELS                                                                                                                                                                                                                                                                                              |
| [cl-syslog](#org193a076)   | :emerg :alert :crit :err :warning :notice :info :debug (these are just priority indicators passed in a message.  [cl-syslog](#org193a076) does not have the ability to turn the logging levels on or off)                                                                                                      |
| hu.dwim.logger             | \\+FATAL+ \\+ERROR+ \\+WARN+ \\+INFO \\+DEBUG+ \\+DRIBBLE+                                                                                                                                                                                                                                                     |
| log4cl                     | \\+log-level-fatal+    \\+log-level-error+ \\+log-level-warn+ \\+log-level-info+  \\+log-level-debug+ \\+log-level-debu1+ \\+log-level-debu2+ \\+log-level-debu3+ \\+log-level-debu4+ \\+log-level-trace+  \\+log-level-debu5+ \\+log-level-debu6+ \\+log-level-debu7+ \\+log-level-debu8+ \\+log-level-debu9+ |
| log5    (9)                | fatal error warn info trace dribble in-development                                                                                                                                                                                                                                                             |
| verbose                    | :FATAL :SEVERE :ERROR :WARN :INFO :DEBUG :TRACE                                                                                                                                                                                                                                                                |
| vom                        | :emerg :alert :crit :error :warn :notice :info :debug :debug1 :debug2 :debug3 :debug4                                                                                                                                                                                                                          |

(9) These are categories, not levels. [top](#org9155e6a)


<a id="org36a7daf"></a>

# Benchmarking

<a id="org576dd09"></a> These tests are run on a linux box running a 4.4.5 kernel on an intel i5-4590 processor with 16gig of ram using sbcl 1.3.4. The functions being logged just log a simple message with a single variable. Any logging which has to do evaluations to insert into the message are going to slow down the results. The intent is merely to give a comparison between (a) the packages with logging installed and triggered (b) the same function without any logging installed and (c) logging installed but not triggered.

Only verbose and vom seemed to take advantage of multiple cores during the test in the sense that the real time is substantially less than the run time.


<a id="org3995554"></a>

## Benchmarking with logging

| Description      | Base       | [a-cl-logger](#org6999e97) (1) | [cl-syslog](#org193a076) | [cl-log](#org273cad2) | hu | [log4cl](#org87c97e5) | [log5](#org23e5aea) | [verbose](#orgbd4e2e1) | [vom](#orgd3cb374) |
| Real time        | 0.015      | 1.881                          | 0.753                    | 0.418                 |    | 0.666                 | 0.290               | 0.398                  | 0.656              |
| Run time         | 0.0133     | 1.8866                         | 0.609999                 | 0.416667              |    | 0.673334              | 0.293334            | 0.719999               | 1.25000            |
| Processor Cycles | 48,977,628 | 6,206,034,839                  | 2,482,452,207            | 1,379,735,775         |    | 2,224,277,943         | 959,444,772         | 1,314,644,001          | 2,162,339,446      |
| Bytes Consed     | 0          | 1,014,445,296                  | 59,227,586               | 153,615,328           |    | 230,406,384           | 129,586,720         | 288,066,784            | 622,654,464        |


<a id="org5726596"></a>

## Benchmarking without triggering logging

| Description      | Base       | [a-cl-logger](#org6999e97) | [cl-syslog](#org193a076) | [cl-log](#org273cad2) | hu | [log4cl](#org87c97e5) | [log5](#org23e5aea) | [verbose](#orgbd4e2e1) | [vom](#orgd3cb374) |
| Run time         | 0.0133     | 0.02333                    | (2)                      | 0.02666               |    | 0.013                 | 0.0200              | 0.669                  | 0.063              |
| Processor Cycles | 48,977,628 | 40,028,946                 | (2)                      | 45,593,217            |    | 20,304,396            | 34,488,513          | 1,143,962,355          | 204,091,635        |
| Bytes Consed     | 0          | 15,510,496                 | (2)                      | 6,736,640             |    | 3,041,280             | 8,266,384           | 285,694,224            | 3,211,264          |

(1) I ran a-cl-logger both with the root-logger and with an inherited logger. There was no appreciable difference in the testing. The setup discussion shows setting it up with an inherited logger.

(2) It makes no sense to test cl-syslog here because it has no way to choose not to log.

[top](#org9155e6a) <a id="org6999e97"></a>


<a id="orgfe1b50c"></a>

# a-cl-logger


<a id="org081d1a2"></a>

## Summary

As you can tell from the benchmarking just above, a-cl-logger is much slower than the other libraries. In part this is because it is intended to do a lot with contexts and signals to augment the log message with information about the running environment. Russ Tyndall has tracked down some of the problem functions but may not have the time to make it more performant. It is also not currently in quicklisp, but may make it there in the future.


<a id="orgdcae309"></a>

## Setup

The following will set up a new logger which will be a child of the \\\*root-logger\* and will have a single messenger of its own which will log to the specified log-file.

```lisp
(a-cl-logger:define-logger filelog2 ()
  :appenders (make-instance 'a-cl-logger:file-log-appender :log-file "/home/sabra/test/a-cl-logger-test.log" :buffer-p nil))
#<A-CL-LOGGER:LOGGER FILELOG2 1 {1007D90A53}>
```

You can get the new logger by:

```lisp
(a-cl-logger:get-logger 'filelog2)
```

You can add appenders by pushing them into the appenders slot of the desired logger. Below we validate that there are no appenders in the testlog logger, then add a new console appender, validate that it is now there, and trigger a simple message.

```lisp
(a-cl-logger:appenders (a-cl-logger:get-logger 'testlog))
NIL

(push (make-instance 'a-cl-logger:stderr-log-appender)
      (a-cl-logger:appenders (a-cl-logger:get-logger 'testlog)))
(#<A-CL-LOGGER:STDERR-LOG-APPENDER {1005F4DBE3}>)

(a-cl-logger:appenders (a-cl-logger:get-logger 'testlog))
(#<A-CL-LOGGER:STDERR-LOG-APPENDER {1005F4DBE3}>)

(testlog.debug "help")
2016-04-29T21:50:09.498509-07:00      TESTLOG DEBUG   help
#<A-CL-LOGGER:MESSAGE {10064A0D13}>
#<A-CL-LOGGER:LOGGER TESTLOG 1 {1002A781F3}>
```

If something goes wrong, you can use the setup-logger function to ensure that log-level and standard debug-io-appender / file appenders are in place for the specified logger. In the course of testing I even used this to reset the **root-logger** after getting too creative.


<a id="orgaa3393f"></a>

## Built-in Log Levels

The built in log-levels are: fatal error warn info debug dribble. These are actually numbers and the variable is prepended and postpended with '+' symbols.

You can get the log-level name from the number with (log-level-name-of). E.g.

```lisp
a-cl-logger:+warn+
3
(a-cl-logger:log-level-name-of a-cl-logger:+warn+)
WARN
```

So assuming you were using a logger with the name filelog2, you could find out the current logging level by name as follows:

```lisp
(a-cl-logger:log-level-name-of (a-cl-logger:log-level (a-cl-logger:get-logger 'filelog2)))
WARN
```


<a id="org05f5f44"></a>

## Logging Call with sample message and Change Log Levels

First, check the log level for the logger named filelog2, call it at info level, call it at dribble level, noting that it does not log at dribble. Reset the log-level for the filelog2 logger to dribble, demonstrate that it will now trigger, reset the log-level to warn, demonstrate that it will not trigger at the dribble level anymore. Then change the log level at the root logger and demonstrate that the lower tier logger filelog2 will again trigger.

```lisp
(a-cl-logger:log-level (a-cl-logger:get-logger 'filelog2))
1

(filelog2.info "did something to ~a again" "George")
2016-04-23T22:56:27.180490-07:00     FILELOG1 INFO    did something to George again
#<A-CL-LOGGER:MESSAGE {10090EF7B3}>
#<A-CL-LOGGER:LOGGER FILELOG1 1 {1007D90A53}>

(setf (a-cl-logger:log-level (a-cl-logger:get-logger 'filelog2)) a-cl-logger:+dribble+)
0

(filelog2.dribble "did something")
#<A-CL-LOGGER:MESSAGE {1009BC8743}>
#<A-CL-LOGGER:LOGGER FILELOG2 0 {1003271033}>

(setf (a-cl-logger:log-level (a-cl-logger:get-logger 'filelog2)) a-cl-logger:+warn+)
3

(filelog2.dribble "did something")
NIL

(setf (a-cl-logger:log-level a-cl-logger:*root-logger*) a-cl-logger:+dribble+)
0

(filelog2.dribble "did something")
#<A-CL-LOGGER:MESSAGE {10047135B3}>
#<A-CL-LOGGER:LOGGER FILELOG2 0 {1003271033}>
```

You can also set the level as a filter at the appender level as well as the logger level.


<a id="org0e9c7af"></a>

## Logging to File

Provide an appender to a logger which specifies the relevant filename.


<a id="org8d77e4a"></a>

## Muting Logging to Repl

Remove any logger appenders which output to the repl. There is a mute level, but it is not exported for some reason.


<a id="org4a6a6d3"></a>

## Additional Features

From the documentation:

-   \`a-log:add-signal-handler\`: given a logger, a condition type and a function, this will allow signals to be handled.

-   \`get-log-fn\`: given a logger and an optional level create a function of &rest args that logs to the given logger. Useful for interacting with libraries providing functional logging hooks

-   \`with-appender\`: create a dynamic scope inside which messages to logger will additionally be appended to this appender

-   \`when-log-message-generated/logged/appended\`: These are macros that establish a dynamic context inside of which log messages will be intercepted at key points in their life cycle. The first body is the message handler and the second is the scope.

[top](#org9155e6a)


<a id="org765b029"></a>

# cl-log aka com.ravenbrook.common-lisp-log

<a id="org273cad2"></a> <a id="org7d29803"></a>


<a id="org8c60e78"></a>

## Summary

1.  Terminology

    Cl-log uses the term log-manager rather than logger and uses the term messenger rather than appender for what you attach to a log-manager. It has no logging levels that you can set and reset out of the box but the documentation does suggest a function that you can write to create this functionality). Everything is a user-defined category. Categories can be set up as a single keyword or they can be set up as including other categories. E.g.

    ```lisp
    (defcategory :warning (or :warning :error))
    ```

    You can set filters at both the log-manager and messenger level. Messengers will accept and respond to any messages unless you give them a filter and once instantiated, you cannot change the filter. As a result, you need to define categories prior to create messengers that might use them.

2.  What I liked

    I really liked the ring messenger. Cl-log has the ability to do binary logs, which some may find handy or even necessary.

3.  What I didn't like

    I found it clumsy to validate what categories are set when I had several log-managers running.

    I found it cumbersome to change logging levels compared to other libraries. Each appender/messenger has a filter which is specified when it is instantiated. They cannot be changed. As a result, to change what gets logged, you either have to redefine the actual category or use the defcategory-fn macro to reset the filters. The documentation suggests that you might set the filter in managers to be :log-level, then define a new function as follows:

    ```lisp
    (defun (setf log-level) (new-value)
        (cl-log:defcategory-fn :log-level new-value))
    ```

    Then you can (setf (log-level) :warning) to have all messengers with :log-level to respond to warnings. You should be able to create similar functions with different category names, enabling messengers with different priority levels or categories. In testing I found it just as easy to manually delete messengers in a log-manager and creating new messengers.


<a id="orgdb78a62"></a>

## Setup

As noted above, there are no built-in log levels and cl-log does not have a separate concept of log-level - everything is a category and you can define them however you choose. They must all be defined by the user prior to creating a appender. The following sets up categories, creates a log-manager that accepts formatted messages and starts a messenger that will log to a specific file any log messages with the category of warning or higher.

```lisp

(cl-log:defcategory :critical)
(cl-log:defcategory :error   (or :error :critical))
(cl-log:defcategory :warning (or :warning :error))
(cl-log:defcategory :notice  (or :notice :warning))
(cl-log:defcategory :info    (or :info :notice))
(cl-log:defcategory :debug   (or :debug :info))
(cl-log:defcategory :trace   (or :debug :debug))

(setf (cl-log:log-manager) (make-instance 'cl-log:log-manager
      :message-class 'cl-log:formatted-message))

(cl-log:start-messenger 'cl-log:text-file-messenger :filename
        "/home/sabra/test/cl-log-test.log"
        :filter '(and :warning))
```

If you do not provide a filter, it will respond to any message.

<a id="orgf72bf30"></a>

1.  Multiple Logger Setup

    Assume we have two packages (mypkg-1 and mypkg-2) in the application and we want to log them separately and we want to also log different categories as tags, with the ability to turn those on and off.

    ```lisp
    (defvar *mypkg-1-categories* (make-instance 'cl-log:category-set))
    (cl-log:defcategory :mypkg-1-critical  ()  *mypkg-1-categories*)
    (cl-log:defcategory :mkpkg-1-error (or :mkpkg-1-error :mkpkg-1-critical)
      *mypkg-1-categories*)
    (cl-log:defcategory :mkpkg-1-warning (or :mkpkg-1-warning :mkpkg-1-error)
      *mypkg-1-categories*)
    (cl-log:defcategory :mypkg-1-notice (or :mypkg-1-notice :mkpkg-1-warning)
      *mypkg-1-categories*)
    (cl-log:defcategory :mypkg-1-info (or :mypkg-1-info :mkpkg-1-warning)
      *mypkg-1-categories*)
    (cl-log:defcategory :mypkg-1-debug (or :mypkg-1-debug :mkpkg-1-info)
      *mypkg-1-categories*)
    (cl-log:defcategory :mypkg-1-trace (or :mypkg-1-trace :mkpkg-1-debug)
      *mypkg-1-categories*)

    (defparameter *mypkg-1-log-manager*
      (make-instance 'cl-log:log-manager :message-class 'cl-log:formatted-message
                                         :categories *mypkg-1-categories*))

    (defvar *mypkg-2-categories* (make-instance 'cl-log:category-set))
    (cl-log:defcategory :mypkg-2-critical  ()  *mypkg-2-categories*)
    (cl-log:defcategory :mkpkg-2-error (or :mkpkg-2-error :mkpkg-2-critical)
      *mypkg-2-categories*)
    (cl-log:defcategory :mkpkg-2-warning (or :mkpkg-2-warning :mkpkg-2-error)
      *mypkg-2-categories*)
    (cl-log:defcategory :mypkg-2-notice (or :mypkg-2-notice :mkpkg-2-warning)
      *mypkg-2-categories*)
    (cl-log:defcategory :mypkg-2-info (or :mypkg-2-info :mkpkg-2-warning)
      *mypkg-2-categories*)
    (cl-log:defcategory :mypkg-2-debug (or :mypkg-2-debug :mkpkg-2-info)
      *mypkg-2-categories*)
    (cl-log:defcategory :mypkg-2-trace (or :mypkg-2-trace :mkpkg-2-debug)
      *mypkg-2-categories*)

    (defparameter *mypkg-2-log-manager*
          (make-instance 'cl-log:log-manager :message-class 'cl-log:formatted-message
                                             :categories *mypkg-2-categories*))

    (cl-log:start-messenger 'cl-log:text-file-messenger
                            :name "mypkg-1-file-messenger"
                            :manager *mypkg-1-log-manager*
                            :filename "/home/sabra/test/cl-log-test1.log"
                            :filter '(or :mypkg-1-trace :mypkg-1-debug :mypkg-1-info :mypkg-1-notice
                                         :mypkg-1-warning :mypkg-1-eror :mypkg-1-critical))

    (cl-log:start-messenger 'cl-log:text-file-messenger
                            :name "mypkg-2-file-messenger"
                            :manager *mypkg-2-log-manager*
                            :filename "/home/sabra/test/cl-log-test2.log"
                            :filter '(or :critical :mypkg-2-trace :mypkg-2-debug :mypkg-2-info :mypkg-2-notice
                                         :mypkg-2-warning :mypkg-2-eror :mypkg-2-critical))
    ```

    Note: I had to list all the allowable permutations, not just rely on the category definition including the higher priority categories.

    You can then call messages such as:

    ```lisp
    (cl-log:log-manager-message *mypkg-2-log-manager* :mypkg-2-warning "George Paul Mason")
    ```

    Messages such as

    ```lisp
    (cl-log:log-manager-message *mypkg-2-log-manager* :warning "George Paul Mason")
    ```

    will now fall through and not be logged.


<a id="org1e4b4c7"></a>

## Log Levels

There are no built-in log levels - cl-log treats log levels as categories. The best way to handle changing log levels in cl-log is just kill the relevant messengers and create new ones.


<a id="orgf32f81e"></a>

## Logging Call with sample message

```lisp
(cl-log:log-message :warning "Did I really understand what I was doing?")

3670985284.923 WARNING Did I really understand what I was doing?
```


<a id="org4097b65"></a>

## Logging to Ring

```lisp
(cl-log:start-messenger 'cl-log:ring-messenger
                     :length 10
                     :filter '(and :info (not :error)))

(cl-log:log-message :warning "Logging system poorly understood")
NIL
(cl-log:log-message :warning "Logging system almost understood")
NIL
(cl-log:log-message :warning "Logging system now understood")
NIL
(cl-log:log-message :warning "Logging system now forgotten")
NIL
(loop for message in (cl-log:ring-messenger-messages
                          (car (cl-log:log-manager-messengers (cl-log:log-manager))))
          when message collect (cl-log:message-description message))
("Logging system poorly understood" "Logging system almost understood"
 "Logging system now understood" "Logging system now forgotten")
```

Note that ring logging by default does not have timestamps.


<a id="orgc003710"></a>

## Muting Logging to Repl

Remove any messengers that write to the **standard-output**


<a id="org0bdda41"></a>

## Log Rotation

None


<a id="org4cf61fd"></a>

## Timestamp access

To get universal time out of the message:

```lisp
(cl-log:timestamp-universal-time
        (cl-log:message-timestamp <cl-log-message-here>))
```


<a id="orgcd427a4"></a>

## Loggers aka Log-managers

There is a global log manager: (cl-log:log-manager) or you can create multiple log-managers as described above.


<a id="orgefb426c"></a>

## Messages

Every message has a timestamp, category, description, arguments.

Messages can be read by the individual function calls:

```lispmessage-timestamp
message-category
message-description
message-arguments

or formatted to text with
format-message
```

<a id="org193a076"></a>


<a id="org417fd98"></a>

## Finding a messenger

```lisp
(cl-log:find-messenger 'cl-log-everything)
#<COM.RAVENBROOK.COMMON-LISP-LOG:TEXT-FILE-MESSENGER CL-LOG-EVERYTHING {10054773B3}>
```

[top](#org9155e6a)


<a id="org1d7d00c"></a>

# cl-syslog


<a id="orgf206334"></a>

## Summary

Cl-syslog will append log messages to the designated system log file. There really is no way to turn off the logging. Using the udp-logger, cl-syslog allows one to log to a remote ip address.


<a id="org052ab0e"></a>

## Setup

No setup is required.


<a id="org469373b"></a>

## Built-in Log Levels

:emerg :alert :crit :err :warning :notice :info :debug

If you create a log message that has an invalid log level, that will trigger an invalid priority error passed to the repl.


<a id="org00a6d73"></a>

## Logging Call with sample message

```lisp
(syslog:log "myprog" :user :info "message 1deesy7")
```

This will log:

```lisp
Apr 25 19:39:14 scruppy myprog[29111]: message 1deesy7
```

into a system log file named user.log. On this particular linux box, that is found in /var/log/user.log.

The second parameter is referred to as the facility. Possible facilities can be found at the unexported variable cl-syslog::\*facilities\* On my particular setup, the default facilities were: :kern :user :mail :daemon :auth :syslog :lpr :news :uucp :cron :authpriv :ftp :local0 :local1 :local2 :local3 :local4 :local5 :local6 :local7


<a id="org50fcb4f"></a>

## Changing Log Levels

Not possible.


<a id="orga7596c9"></a>

## Logging to Remote System

To log to a remote system, create a udp-logger with the ip address and port and then you can create your log messages. Example:

```lisp
(syslog.udp:udp-logger "127.0.0.1" 514)
(syslog.udp:log "myapp" :syslog :info "Message from a bottle")
```


<a id="orga349daa"></a>

## Log Rotation

Assuming your system logs rotate, then cl-syslog will be able to take advantage of that.

<a id="org879e2c2"></a>

[top](#org9155e6a)


<a id="org37145b1"></a>

# hu.dwim.logger


<a id="orgf817bc9"></a>

## Summary


<a id="orgf030070"></a>

## Setup


<a id="orge4e63fa"></a>

## Built-in Log Levels


<a id="orgeef9776"></a>

## Logging Call with sample message


<a id="orgb28aab4"></a>

## Logging to File


<a id="org508a553"></a>

## Muting Logging to Repl


<a id="org5d3d8b4"></a>

## Log Rotation


<a id="orgd8c2584"></a>

## Additional Features


<a id="org293bed5"></a>

## Misc

[top](#org9155e6a)


<a id="org5ac6999"></a>

# log4cl

<a id="org87c97e5"></a>


<a id="org54b92f0"></a>

## Summary

Log4cl is the all singing, all dancing logging library. It integrates with slime, it has the most flexible built-in logging information. You can spend a week just learning all the functionality of the library. I, at least, spent the first day confused. log4cl re-exports a very small subset of external functions to 'log'. It essentially treats category interchangeably with logger. Log levels have several different names, some exported, some not.

1.  What I liked

    The amount of information possible to put into a log message is impressive as is the ability to change log level by function.

2.  What I didn't like

    I do find log4cl a bit confusing about whether I need to use the package prefix log4cl or just log. You can spend a week just learning all the functionality of the library. I, at least, spent the first day confused. log4cl re-exports a very small subset of external functions to 'log'. It essentially treats category interchangeably with logger. Log levels have several different names, some exported, some not.


<a id="org2873223"></a>

## Setup

The following is an example will clear all current appenders, log to a file which will be rotated daily and will only log messages with a level of warn or higher.

```lisp
(log4cl:remove-all-appenders log4cl:*root-logger*)
(log:config :daily "/home/sabra/test/log4cl-test1.log" :backup t)
(setf (log4cl:logger-log-level Log4cl:*root-logger*) log4cl:+log-level-warn+)
```


<a id="orgbd5f230"></a>

## Built-in Log Levels

You can think of the log levels for log4cl as :off :fatal error warn info debug debu1 debu2 debu3 debu4 debu5 debu6 debu7 debu8 debu9

However, I find log4cl confusing in this area. There is no exported way to list the log-levels. If you call the internal variable \\+log-level-symbols+, you get

```lisp
log4cl::+log-level-symbols+
(LOG4CL-IMPL::OFF LOG4CL-IMPL::FATAL ERROR WARN LOG4CL-IMPL::INFO DEBUG
 LOG4CL-IMPL::DEBU1 LOG4CL-IMPL::DEBU2 LOG4CL-IMPL::DEBU3 LOG4CL-IMPL::DEBU4
 TRACE LOG4CL-IMPL::DEBU5 LOG4CL-IMPL::DEBU6 LOG4CL-IMPL::DEBU7
 LOG4CL-IMPL::DEBU8 LOG4CL-IMPL::DEBU9 LOG4CL-IMPL::UNSET)
```

If you look for exported symbols, you discover:

log4cl:+log-level-debu1+ log4cl:+log-level-debu2+ log4cl:+log-level-debu3+ log4cl:+log-level-debu4+ log4cl:+log-level-debu5+ log4cl:+log-level-debu6+ log4cl:+log-level-debu7+ log4cl:+log-level-debu8+ log4cl:+log-level-debu9+ log4cl:+log-level-debug+ log4cl:+log-level-error+ log4cl:+log-level-fatal+ log4cl:+log-level-info+ log4cl:+log-level-off+ log4cl:+log-level-trace+ log4cl:+log-level-unset+ log4cl:+log-level-warn+

If you want to check on the current log level of the root-logger, it will respond with an integer. So then you go looking for the string name. You can then setf the level for that logger calling a keyword.

```lisp
(log4cl:logger-log-level log4cl:*root-logger*)
4

(log4cl:log-level-to-string (log4cl:logger-log-level log4cl:*root-logger*))
"INFO"

(setf (log4cl:logger-log-level log4cl:*root-logger*) :warn)
3

(log4cl:log-level-to-string (log4cl:logger-log-level log4cl:*root-logger*))
"WARN"
```


<a id="org81248d6"></a>

## Logging Call with sample message

The log messages can vary substantially, depending on how much you want to put into the layout-pattern. A simple example, with a default layout might look like this:

```lisp
(defun hello ()
  (log:info "I just ate a ~5f, feeling tired" pi)
  (when (log:debug)
    (dotimes (sheep 3)
      (log:debug sheep "zzz")))
  (log:warn "doh fell asleep for" (random 10) "minutes"))

(hello)
INFO - I just ate a 3.142, feeling tired
DEBUG - SHEEP: 0 zzz
DEBUG - SHEEP: 1 zzz
DEBUG - SHEEP: 2 zzz
WARN - doh fell asleep for (RANDOM 10): 4 minutes
```

or you could make the layout more complicated. For example:

```lisp
(defun sabra-test-pattern (pattern)
           (with-package-log-hierarchy
             (clear-logging-configuration)
             (let ((level 6)
                   (logger (make-logger '(one two three))))
               (with-output-to-string
                   (s)
                 (add-appender *root-logger*
                         (make-instance
                          'fixed-stream-appender
                          :stream s
                          :layout
                          (make-instance 'pattern-layout
                                         :conversion-pattern pattern)))
           (setf (logger-log-level *root-logger*) level)
           (log-fatal :logger logger "test message")))))
=begin
 (sabra-test-pattern "%-5p [%c] - %h %t %x %i %m%n %d{%Y-%m-%d %H:%M:%S%}")
=bend

"FATAL [ONE.TWO.THREE] - scruppy new-repl-thread  11895 test message
 2016-05-01 15:12:25"
```

In the above example, the layout pattern has:

-   %-5p : the log level

-   [%c] : the category name for the logger

-   %h : hostname

-   %t : current thread-name

-   %i : process id of the the lisp process

-   %m : message with a newline

-   %d : the data and time in UTC with its own format code.

Now check the log level for the logger named **root-level**, call it at warn level, call it at info level, noting that it does not log at info. Reset the log-level for the blah logger to info, demonstrate that it will now trigger, reset the log-level to warn, demonstrate that it will not trigger at the info level anymore.

```lisp
(log4cl:logger-log-level log4cl:*root-logger*)
3

(log:warn "stuff")
WARN - stuff

(log:info "stuff")

(setf (log4cl:logger-log-level log4cl:*root-logger*) log4cl:+log-level-info+)
4

(log:info "stuff")
INFO - stuff

(setf (log4cl:logger-log-level log4cl:*root-logger*) log4cl:+log-level-warn+)
3

(log:info "stuff")

```


<a id="org8c6e16e"></a>

## Changing Log Levels

To change log levels, setf the logger-log-level for the logger you are dealing with. For example, the following would set the log level to debug:

```lisp
(setf (log4cl:logger-log-level Log4cl:*root-logger*) log4cl:+log-level-debug+)
```


<a id="org18c2ba0"></a>

## Logging to File

The following example logs the appropriately leveled messages to a file, but does not rotate the file because :backup is set to nil.

```lisp
(log:config :daily "/home/sabra/test/log4cl-test1.log" :backup nil)
```


<a id="org2edd997"></a>

## Muting Logging to Repl

I had some difficulty muting the logging to the repl. I ended up calling

```lisp
(log4cl:remove-all-appenders log4cl:*root-logger*)
```

and then adding the appender I wanted by calling

```lisp
(log:config :daily "/home/sabra/test/log4cl-test1.log" :backup nil)
```

Note the different package prefixes required.


<a id="org14d981f"></a>

## Log Rotation

The above call to log:config with the keyword :daily generates daily log rotation. Just set :backup to T or leave it out completely as the default is T.


<a id="orgb45192d"></a>

## Additional Features

log4cl with its companion package log4slime integrate quite well into slime and make logging much more like a debugger. Please see the documentation at <https://github.com/7max/log4cl>


<a id="orgf1c15f8"></a>

## Problems

There are some issues listed in the github location that are occassionally resolved by users. It does not appear that the library is under active development. [top](#org9155e6a) <a id="org23e5aea"></a>


<a id="org5f7082e"></a>

# log5


<a id="org7024aee"></a>

## Summary

1.  Terminology

    Log5 uses the term log-manager rather than logger and has a single log-manager. It also uses the term sender rather than messenger or appender for what you attach to a log-manager. It has no logging levels that you can set and reset out of the box but uses categories as tags, some of which are supplied as defaults. Categories can be set up as a single keyword or they can be set up as including other categories. E.g.

    ```lisp
    (defcategory :warning (or :warning :error))
    ```

    Messengers will accept and respond to any messages unless you give them a filter and once instantiated, you cannot change the filter. As a result, you need to define categories prior to create messengers that might use them.

2.  What I liked

    It is the fastest; it is easy to define categories and I also liked the built-in ring logging.

3.  What I didn't like

    I would prefer to have the ability to just change sender logging levels rather than killing the senders.

4.  Overview

    The following is from the overview description in the source files:

    the bird's eye view looks like this: You define **categories** for your application. These might look like

    (defcategory motion)

    (defcategory energy)

    (defcategory physics (or energy motion))

    (defcategory planner)

    and so forth. Categories are sort of like Lisp \`\*features\*\` with names. They can be simple (like \`motion\`) or boolean combinations (like \`physics\`). When you write a typical log **message**, you use a combination of categories to describe it:

    ```lisp
    (log-for (and physics (not file-system) trace)
           "starting widget simulation")
    ```

    or

    (log-for (or planner motion) "Planning path for agent ~a" (name agent))

    You start a **sender** using \`start-sender\` (surprise!). You specify what kind of sender it is (e.g., a stream sender or a database sender or an HTML sender or whatever) and pass along whatever arguments are needed to create it. You also specify the categories and the **outputs** the sender will send. Categories were discussed above; a sender's outputs are a list of named properties defined with defoutput. For example:

    ```lisp
    (defoutput time (get-universal-time))

    (defoutput virtual-memory (os-get-virtual-memory))

    (defoutput current-database (name *db*)))
    ```

    Outputs can compute anything that makes sense for your program (though they ought to compute it quickly if you don't want logging to kill performance!). Some outputs are special and predefined. For example, the output \`message\` refers to the string created by the log message statement (e.g., the \`log-for\` examples above). The output \`context\` refers to the current **context** (the last of our five players).

    The context is a carry-all you can use to specify whatever important is happening in the global environment. If you're writing a web-application; the context might track the current session ID; A planner might track the current agent and so forth. Information from the context is added to the end of each log message sent and so functions as a variable portion in contrast to the fixed structure of the sender's output.

    \### Debugging with log5

    You can also use log5 in debugging. The [log-manager][] includes a debug console to which log messages can be sent. Use [debugging][] and [undebugging][] to tell log5 which categories you want to see. Let's use this code for to explain:

    ```lisp
    (log5:defcategory dribble)
    (log5:defcategory trace)
    (log5:defcategory info)
    (log5:defcategory warn)

    (defun sub-step-1 ()
      (log5:log-for dribble "enter sub-step-1")
      (log5:log-for dribble "exit sub-step-1"))

    (defun step-1 ()
          (log5:log-for trace "enter step-1")
          (sub-step-1)
          (log5:log-for trace "exit step-1"))

    (defun log5-run-program ()
          (log5:log-for info "enter run")
          (step-1)
          (log5:log-for info "exit run"))

    (log5-run-program)
    ```

    If I just evaluate \`(log5-run-program)\`, then I'll see no output. Suppose that I decide to debug at the lowest level of detail: \`dribble\`:

    ```lisp
    > (debugging 'dribble+)
    (or dribble+)

    > (run-program)
    "enter run"
    "enter step-1"
    "enter sub-step-1"
    "exit sub-step-1"
    "exit step-1"
    "exit run"
    ```

    If I just want to see the high-level structure, I could debug at the \`info\` level. I can also change the \`output-spec\` used by the console. For example, I might want to see the time when each event occurs:

    ```lisp
    > (debugging 'info+ :reset? t :output-spec 'time)
    (or info+)

    > (run-program)
    3452689957 "enter run"
    3452689957 "exit run"
    ```


<a id="org2060e29"></a>

## Setup

Log5 requires that you be fairly specific with respect to what you want to do. For example, the following will create a sender that logs messages to a file and another sender that sends log messages to the repl. Note that you need to specify what output you want, what categories, etc. The category spec is the filter which determines what categories will be logged by that sender. You can write a output function using (defoutput) to perform as much computation as you want to insert into the log message.

```lisp
(log5:start-sender 'debug  (log5:stream-sender
                            :location "/home/sabra/test/log5.log")
                   :output-spec '(time log5:category log5:message)
                   :category-spec '(or log5:INFO log5:WARN log5:ERROR
                                       log5:FATAL))

(log5:start-sender 'repl-warn  (log5:stream-sender
                                :location *standard-output*)
                   :output-spec '(time log5:category log5:message)
                   :category-spec '(or log5:INFO log5:WARN log5:ERROR
                                       log5:FATAL))
#<LOG5:STREAM-SENDER REPL-WARN {1004A79553}>
```


<a id="org9392a43"></a>

## Built-in Log Levels

None


<a id="orga44ad64"></a>

## Built-in Categories

Per the documentation, "categories are just a way to organize log messages; you can make them hierarchical if that works for you or you can make them more flexible. Note that log5 comes with the standard set of category levels:"

```lisp
(log5:category-specs)
(#<category 0: FATAL> #<category 1: ERROR>
 #<category 2: ERROR+ -> (OR ERROR FATAL)> #<category 3: WARN>
 #<category 4: WARN+ -> (OR WARN ERROR+)> #<category 5: INFO>
 #<category 6: INFO+ -> (OR INFO WARN+)> #<category 7: TRACE>
 #<category 8: TRACE+ -> (OR TRACE INFO+)> #<category 9: DRIBBLE>
 #<category 10: DRIBBLE+ -> (OR DRIBBLE TRACE+)> #<category 11: IN-DEVELOPMENT>
 #<category 12: (OR ERROR) -> (ERROR)> #<category 13: (OR WARN) -> (WARN)>)
```

When creating a sender, you specify the categories being captured for logging by the :category-spec slot (see above).


<a id="org7be81b9"></a>

## Logging Call with sample message

```lisp
(log5:log-for (log5:warn) "Anything that can be destroyed by the truth ~a." "should be")
3670967360 (WARN) Anything that can be destroyed by the truth should be.
```

Note the use of the log5:warn category rather than just 'warn'. In the resulting log message, the first part is the universal time, the second is the category, then the formatted message, just as specified in the :output-spec when we started the repl-warn sender.


<a id="org497905e"></a>

## Changing Log Levels

The log levels in log5 are kept in the category-spec slot in the sender. Because this slot only has a reader, you may consider it easier to delete the sender and create a new sender than to go into the externals tyring to change the log level.


<a id="org46f8481"></a>

## Logging to File

```lisp
(log5:start-sender 'debug  (log5:stream-sender
                            :location "/home/sabra/test/log5.log")
                   :output-spec '(time log5:category log5:message)
                   :category-spec '(or log5:INFO log5:WARN log5:ERROR
                                       log5:FATAL))
#<LOG5:STREAM-SENDER DEBUG {1005C23243}>
```


<a id="org7b03105"></a>

## Muting Logging to Repl

If you have a sender which is logging to the repl, you can use the stop-sender function so long as you know the name of the sender. This actually deletes the sender rather than muting it.


<a id="org1f85f29"></a>

## Unmuting Logging to Repl

If you have deleted a sender, you need to actually recreate it from scratch.


<a id="org770a321"></a>

## Log Rotation

There are no functions in the library to automatically rotate logs.


<a id="org48dc728"></a>

## Config File

You can create a config file for log5. It will look initially in the directory specified by **default-pathname-defaults** for a file named .

```lisp
(start-sender 'warnings-and-worse (stream-sender :location
              *error-output*)
              :category-spec '(warn+) :output-spec '(time message context))

(log5:category-specs )
(#<category 0: FATAL> #<category 1: ERROR> #<category 2: ERROR+ -> (OR ERROR FATAL)> #<category 3: WARN> #<category 4: WARN+ -> (OR WARN ERROR+)>
 #<category 5: INFO> #<category 6: INFO+ -> (OR INFO WARN+)> #<category 7: TRACE> #<category 8: TRACE+ -> (OR TRACE INFO+)> #<category 9: DRIBBLE>
 #<category 10: DRIBBLE+ -> (OR DRIBBLE TRACE+)> #<category 11: IN-DEVELOPMENT>)

```

[top](#org9155e6a)


<a id="org4c5e457"></a>

# verbose

<a id="orgbd4e2e1"></a>


<a id="orgce313fc"></a>

## Summary

Verbose uses a slightly different terminology, with messages going to pipelines (which have filters) and messages which successfully go through the filtering process are handed to faucets, which direct the output to specified locations. Pipelines can be strung together for multiple filtering processes. In effect, verbose has a single root logger, like log5 with multiple faucets (aka senders/messengers/appenders). It has a preference to always output messages to the repl even if they are also routed elsewhere, so it is a little more work to mute the repl logging for unattended applications.


<a id="org0fa4464"></a>

## Setup

Out of the box, verbose starts a separate thread with a global controller assigned to the variable **global-controller**. Immediately, you can make calls without having to set up a messenger or appender.


<a id="org59385b2"></a>

## Built-in Log Levels

The default log levels are: :FATAL :SEVERE :ERROR :WARN :INFO :DEBUG :TRACE

We can check to see what logging level is set:

```lisp
(v:repl-level)
:INFO
```

To prove that it only logs to the repl-level

```lisp
  (defparameter *v-levels* '(v:fatal v:severe v:error v:warn v:info
  v:debug v:trace))

  (loop for x in *v-levels* do (funcall x :test "Hello World ~a" "Ringo"))
NIL
LOG: 2016-04-26 20:21:55 [FATAL] <TEST>: Hello World Ringo
LOG: 2016-04-26 20:21:55 [SEVERE] <TEST>: Hello World Ringo
LOG: 2016-04-26 20:21:55 [ERROR] <TEST>: Hello World Ringo
LOG: 2016-04-26 20:21:55 [WARN ] <TEST>: Hello World Ringo
LOG: 2016-04-26 20:21:55 [INFO ] <TEST>: Hello World Ringo
```

Because "debug" and "trace" were below the repl-level, they did not trigger a log message. Now if we setf repl-level to trace, they should all show.

```lisp
(setf (v:repl-level) :trace)
:TRACE
LIBRARY-COMPARISON> (loop for x in *v-levels* do (funcall x :test "Hello World"))
NIL
LOG: 2016-04-26 20:15:39 [FATAL] <TEST>: Hello World
LOG: 2016-04-26 20:15:39 [SEVERE] <TEST>: Hello World
LOG: 2016-04-26 20:15:39 [ERROR] <TEST>: Hello World
LOG: 2016-04-26 20:15:39 [WARN ] <TEST>: Hello World
LOG: 2016-04-26 20:15:39 [INFO ] <TEST>: Hello World
LOG: 2016-04-26 20:15:39 [DEBUG] <TEST>: Hello World
LOG: 2016-04-26 20:15:39 [TRACE] <TEST>: Hello World
```


<a id="org5154aa9"></a>

## Logging Call with sample message

```lisp
(v:log :info :test "message here format variable : ~a" "insert here")
NIL
LOG: 2016-04-29 18:08:10 [INFO ] <TEST>: message here format variable : insert here
```

You can actually short cut this by treating the logging level itself as a function. By default, the levels are :fatal :severe :error :warn :info :debug :trace with the default level set at start at info. So, e.g.:

```lisp
(v:warn :test "Hello World ~a" "Ringo")
NIL
LOG: 2016-04-26 20:06:22 [WARN ] <TEST>: Hello World Ringo
```

Looking at the call itself, we have the level, the category and a datum followed by datum-args. The data can be a string, symbol or function. So the simplest and most typical logging would be a string with format ~a parameters embedded as above, followed by the values to insert into the strng. But as noted, you could pass a function or merely a symbol as well. Looking at the logging statement result, we have date, time, the level, the category and the message. You can p


<a id="orgd4d5648"></a>

## Changing Log Levels

You can change the global log level easily if you are looking at logging to the repl.

```lisp
(setf (v:repl-level) :DEBUG)
```

You can use this method if you are redirecting the repl output to a file stream

Logging to other faucets such as a file-faucet requires that you change the filter that is in place in the pipeline prior to the faucet. When you do this, you may or may not see a slowdown in logging performance. I noticed it inconsistently in this comparison and have been unable to reproduce it consistently.


<a id="org418f5fe"></a>

## Categories

What are categories in verbose? It may be implicit above, but categories are required to be keywords. Without further setup, there are no set categories and no filters for the categories. You can pass multiple categories in the same statement. As the documentation states, this means that categories are more like tags. Thus:

```lisp
(v:info '(:system :server) "Starting up!")
NIL
LOG: 2016-04-26 20:31:53 [INFO ] <SYSTEM><SERVER>: Starting up!
```

Verbose starts to add power when you start specifying categories because that gives the filters something to work with. Look at the categories that the libraries thinks it uses without any setup:

```lisp
LIBRARY-COMPARISON> (v:repl-categories)
T
```

At this point, it will accept anything you throw at it as a category. Now consider the following where we add a hierarchical tree category (notice the wild card in the final position) and see the results:

```lisp
(v:add-repl-category :foo.bar.*)
(:FOO.BAR.*)

(v:info :foo "foo stuff here")
 NIL

(v:info :foo.bar "foo stuff here")
 NIL

(v:info :foo.bar.part-three "foo stuff here")
 NIL
 LOG: 2016-04-26 20:37:57 [INFO ] <FOO.BAR.PART-THREE>: foo stuff here

  (v:info '(:system :server) "Starting up!")
  NIL
```

What happened in that last call? Once we explicitly added a category, we can only use categories that have been added. Checking on what categories the system will recognize, we now get the following:

```lisp
(v:repl-categories)
(:FOO.BAR.*)
```

As previously noted, we can still pass multiple categories in the same statement.

```lisp
(v:info '(:foo.bar.part-three :foo.bar.part-two) "foo stuff here")
NIL
LOG: 2016-04-26 20:46:43 [INFO ] <FOO.BAR.PART-THREE><FOO.BAR.PART-TWO>: foo stuff here
```

Add our simple category :test to the repl-category

```lisp
(v:add-repl-category :test)
(:TEST :FOO.BAR.*)
```

Now we are going to try to muffle some categories in the logging call and see what comes out in the statement.

```lisp
(v:with-muffled-logging (:test)
    (v:info :foo.bar.part-three "A")
    (v:info :test "B")
    (v:info :something.or.other "C"))
NIL
LOG: 2016-04-26 21:03:29 [INFO ] <FOO.BAR.PART-THREE>: A
```

As we might expect, the explicitly muffled :test category was not logged, nor was the :something.or.other category which had not been added to the repl-category, and the unmuffled :foo.bar.part-three was allowed through. But remember that :foo.bar.\* was a tree with three segments. What happens if we put partials as parameters into the with-muffled-logging call?

```lisp
(v:with-muffled-logging (:foo.bar)
    (v:info '(:foo :foo.bar.part-three :foo.bar :foo)
            "Did we manage to hide some things and keep others?"))
NIL
```

What happened? The parameter passed to (with-muffled-logging) muffled everything lower in the tree.

```lisp
(v:with-muffled-logging (:foo.bar.part-two)
    (v:info '(:foo.bar :foo :foo.bar.part-two :foo.bar.part-three)
            "Did we manage to hide some parts and keep others?"))
NIL
```

You can remove a category with (remove-repl-category). Thus:

```lisp
(v:repl-categories)
(:FOO.BAR.* :TEST)

(v:remove-repl-category :test)
(:FOO.BAR.*)
```


<a id="org688158a"></a>

## Sharing across threads

As noted in the documentation, "Log message passing through the pipeline happens in a separate thread. If you create new pipe segments for your logging pipeline that need to access some form of shared variable, you can use share, which is SETFable. One share that is most likely of interest to everyone is saved under the symbol **standard-output**. Setting this anew is useful if you start a new REPL session and need to redirect logging to it."

```lisp
(setf (v:shared-instance '*standard-output*) *standard-output*)
```


<a id="org648e88c"></a>

## Log to File

There are a couple of ways to do this. First, you can redirect the normal repl output to a file:

```lisp
(setf (v:shared-instance '*standard-output*) (open #p"~/verbose.log" :direction :output :if-exists :append))
```

Second, you could use a single file, adding a pipe with a file-faucet:

```lisp
(v:add-pipe (make-instance 'v:file-faucet
                           :file #p "~/test/verbose-single.log"))
```

Note that this was added in April, 2016.

Third, you could using a rotating log faucet, see the next section.


<a id="org4b9fd3b"></a>

## Log to Rotating Log File

Verbose is one of two libraries that provide the default ability to rotate log files (the other being log4cl) and the only library that allows you to set the rotation interval for anything other than daily.

```lisp
  (v:add-pipe (make-instance 'v:rotating-log-faucet
                             :file #p"~/verbose.log"
                             :interval (v:make-cron-interval "* * * * *")))
#(>>ROTATE(#<CRON-SCHEDULE {1005C829E3}>))
LIBRARY-COMPARISON> (let ((error "george")) (v:error :test "hello world ~a" error))
NIL
LOG: 2016-04-23 16:26:35 [ERROR] <TEST>: hello world george
```

will log to a file "2016-04-23 16:26:28-verbose.log" in my home directory.


<a id="orgb667ff2"></a>

## Muting Logging to Repl

You could set the repl-categories to nil:

```lisp
(setf (v:repl-categories) nil)
```

Obviously unmuting is as simple as:

```lisp
(setf (v:repl-categories) t)
```

or you could completely remove the segment that sends to the repl:

```lisp
(piping:remove-segment v:*global-controller* '(0))
```


<a id="orge7c0e37"></a>

## Timestamp formatting

The timestamp format pattern is in the variable **repl-faucet-timestamp**, so you should be able to modify the timestamp pattern. By default the pattern looks like:

```lisp
((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2) #\  (:HOUR 2) #\: (:MIN 2)
    #\: (:SEC 2))
```


<a id="orgd09fa6f"></a>

## Restarting the Global Controller

```lisp
(v:restart-global-controller)
#<VERBOSE:CONTROLLER :threaded :running :queue-size 0>
```

[top](#org9155e6a)


<a id="orgffa1206"></a>

# vom

<a id="orgd3cb374"></a>


<a id="org819e464"></a>

## Summary

Vom is small and simple. You can turn logging on and off by package.


<a id="orgcfe8a1b"></a>

## Setup

You can use a single stream (the default being **log-stream** ) or multiple streams. Vom can set the logging level either on a global basis:

```lisp
(vom:config t :error)
```

or per package

```lisp
(vom:config :my-package :notice)
```

As a default, config is set to warn, so any events with a level less significant than warn will not be logged.


<a id="org50fa663"></a>

## Built-in Log Levels

:emerg :alert :crit :error :warn :notice :info :debug :debug1 :debug2 :debug3 :debug4


<a id="org8a586f4"></a>

## Logging Call with sample message

```lisp
Sample use:
(let ((error "some-error"))
     (vom:error "there was a problem in setting up the datbase ~a" error))
 <ERROR> [15:49:31] library-comparison - there was a problem in setting up the datbase some-error
```


<a id="orgf01681b"></a>

## Changing Log Levels

Use the config function to change logging levels.


<a id="org2974d95"></a>

## Logging to File

Logging to a file does require you to explicitly open a file for vom. E.g.:

```lisp
(with-open-file
 (vom-s "/home/sabra/test/vom-logging.log"
        :direction :output :if-exists :append :if-does-not-exist :create)
 (setf vom:*log-stream* vom-s)
 (vom:error "there was a problem in setting up the database ~a" "some additional error note"))
```


<a id="org4c2df11"></a>

## Setting Multiple Streams

To trigger multiple streams, set **log-hook**

```lisp
;; example: this hook logs the request to multiple streams if we're getting a
;; log entry from the "particle-accelerator" package
(setf vom:*log-hook*
  (lambda (level package package-level)
    (declare (ignore level package-level))
    (if (eq package :particle-accelerator)
        (values t *my-file-log-stream* *another-stream*)
        t)))
```


<a id="org360edce"></a>

## Muting Logging to Repl

By setting vom:\*log-stream\* to an open file without using the multiple stream vom:\*log-hook\* method above, you will be redirecting the logging which would have otherwise gone to the repl to the open file.


<a id="org6f44347"></a>

## Additional Features

As noted above, vom can be set on a per-package basis.

[top](#org9155e6a)


<a id="org2e5b9b0"></a>

# Function Mapping

<a id="orgdc6cfdc"></a>

| Library                                                   | [a-cl-logger](#org6999e97)                                                 | [cl-syslog](#org193a076)           | [cl-log](#org273cad2)             | hu | [log4cl](#org87c97e5)                                                              | [log5](#org23e5aea)        | [verbose](#orgbd4e2e1)                   | [vom](#orgd3cb374)                        |
|--------------------------------------------------------- |-------------------------------------------------------------------------- |---------------------------------- |--------------------------------- |--- |---------------------------------------------------------------------------------- |-------------------------- |---------------------------------------- |----------------------------------------- |
| Logging function for your source code                     | (do-log) (do-logging) or (<name-of-logger>.<error-level>                   | (log)                              | (log-message :error text-message) |    | (log:<error-level>                                                                 | (log5:log-for)             | (log), (log-message) v:<level>)          | (error)                                   |
| Default log level                                         | :debug                                                                     |                                    |                                   |    | :info                                                                              |                            | :info                                    | :warn                                     |
| List available levels                                     | \\\*log-level-names\*                                                      |                                    |                                   |    |                                                                                    |                            | v:\*levels\*                             |                                           |
| Root or global logger/log manager                         | \\\*root-logger\*                                                          |                                    | (log-manager)                     |    | \\\*root-logger\*                                                                  | (log-manager)              | \\\*global-controller\*                  |                                           |
| Set log level at global level                             |                                                                            |                                    |                                   |    | (log:config <error-level>)                                                         | (debugging), (undebugging) | (setf (v:repl-level) desired-level)      | (config t :error)                         |
| Set log level at package level                            |                                                                            |                                    |                                   |    | [use log4slime]                                                                    |                            |                                          | (config :my-package :error)               |
| Set log level at the function level                       |                                                                            |                                    |                                   |    | [use log4slime]                                                                    |                            |                                          |                                           |
| Set log level at log manager                              | (setf (level <logger-name>) desired-level), (setf log-level)               |                                    |                                   |    | (set-log-level)                                                                    |                            |                                          |                                           |
| Get logger's log level                                    | (log-level) (10)                                                           |                                    |                                   |    | (logger-log-level) (10)                                                            |                            |                                          |                                           |
| Get log level as string                                   | (log-level-name-of)                                                        |                                    |                                   |    | (log-level-to-string)                                                              |                            |                                          |                                           |
| List levels handled by a log manager                      | (level <logger-name>) (10)                                                 |                                    |                                   |    |                                                                                    |                            | (repl-level)                             |                                           |
| Get logger's effective log level (inheriting parent's)    |                                                                            |                                    |                                   |    | (effective-log-level)                                                              |                            |                                          |                                           |
| Add level                                                 |                                                                            |                                    |                                   |    | (make-log-level)                                                                   |                            |                                          |                                           |
|                                                           |                                                                            |                                    |                                   |    |                                                                                    |                            |                                          |                                           |
| Creating a new logger                                     | (setup-logger) or (define-logger)                                          |                                    |                                   |    | (make-logger)(%get-logger)                                                         | (start-sender)             | (make-standard-global-controller)        |                                           |
| list loggers                                              | (children **root-logger**)                                                 |                                    |                                   |    | (logger-children),(logger-descendants)                                             |                            |                                          |                                           |
| Get a log manager by name                                 | (get-logger name) or (get-log-fn)                                          |                                    |                                   |    |                                                                                    |                            |                                          |                                           |
| Get name of a log manager                                 | (name <logger>)                                                            |                                    |                                   |    | (logger-name)                                                                      | (name)                     |                                          |                                           |
| Inherited parents of a logger                             | (parents)                                                                  |                                    |                                   |    | (logger-parent)(logger-ancestors)                                                  |                            |                                          |                                           |
| Stop logger                                               |                                                                            |                                    |                                   |    |                                                                                    |                            | (stop-controller)                        |                                           |
| Delete global logger                                      |                                                                            |                                    |                                   |    |                                                                                    |                            | (remove-global-controller)               |                                           |
| Restarting the global logger                              |                                                                            |                                    |                                   |    |                                                                                    |                            | (restart-global-controller)              |                                           |
|                                                           |                                                                            |                                    |                                   |    |                                                                                    |                            |                                          |                                           |
| List all categories                                       | \\\*log-level-names\*                                                      |                                    |                                   |    |                                                                                    | (category-specs)           | (repl-categories)                        |                                           |
| Change categories handled by a log manager                |                                                                            |                                    |                                   |    |                                                                                    |                            |                                          |                                           |
| Change categories handled by a log messenger              |                                                                            |                                    |                                   |    |                                                                                    |                            |                                          |                                           |
| Add category                                              |                                                                            |                                    | (defcategory)                     |    |                                                                                    | (defcategory)              | (add-repl-category)                      |                                           |
| Defining a new category                                   |                                                                            |                                    | (defcategory), (defcategory-fn)   |    |                                                                                    | (defcategory)              |                                          |                                           |
| Remove a category                                         |                                                                            |                                    | (undefcategory-fn)                |    |                                                                                    |                            | (remove-repl-category)                   |                                           |
| Clearing all categories                                   |                                                                            |                                    | (clear-categories)                |    | (clear-logging-configuration)                                                      | (log5::reset-categories)   |                                          |                                           |
| category id to category instance                          |                                                                            |                                    |                                   |    |                                                                                    | (id->category)             |                                          |                                           |
|                                                           |                                                                            |                                    |                                   |    |                                                                                    |                            |                                          |                                           |
| Set global stream for logging destination                 |                                                                            |                                    |                                   |    |                                                                                    |                            |                                          | (setf vom:\*log-stream\* <stream name>    |
| Set multiple streams for logging destination              |                                                                            |                                    |                                   |    |                                                                                    |                            |                                          | (setf vom:\*log-hook\* &#x2026;see below) |
| Redirect output                                           |                                                                            |                                    |                                   |    |                                                                                    |                            | (output-here)                            |                                           |
|                                                           |                                                                            |                                    |                                   |    |                                                                                    |                            |                                          |                                           |
| Suppress logging (all or a level)                         |                                                                            |                                    |                                   |    |                                                                                    |                            | (with-muffled-logging)                   |                                           |
| Disable logging                                           | (remove-appender), (abort)                                                 |                                    | (logging-disabled)                |    |                                                                                    | (stop-all-senders)         |                                          |                                           |
| Restart logging                                           |                                                                            |                                    | (logging-disabled)                |    |                                                                                    |                            |                                          |                                           |
|                                                           |                                                                            |                                    |                                   |    |                                                                                    |                            |                                          |                                           |
| List all messengers                                       |                                                                            |                                    |                                   |    |                                                                                    | (senders)                  |                                          |                                           |
| List all messengers for a log manager                     | (appenders logger)                                                         |                                    | (log-manager-messengers)          |    | (all-appenders)                                                                    |                            |                                          |                                           |
| Get the message class handled by a particular log manager |                                                                            |                                    | (log-manager-message-class)       |    |                                                                                    |                            |                                          |                                           |
| Start a messenger                                         |                                                                            |                                    | (start-messenger)                 |    |                                                                                    | (start-sender)             |                                          |                                           |
| Stop a particular messenger                               |                                                                            |                                    | (stop-messenger)                  |    |                                                                                    | (stop-sender)              |                                          |                                           |
| Add a messenger                                           |                                                                            |                                    |                                   |    | (add-appender)                                                                     |                            | (add-pipe)                               |                                           |
| Temporarily add a messenger                               | (with-appender)                                                            |                                    |                                   |    |                                                                                    |                            |                                          |                                           |
| Delete a messenger                                        |                                                                            |                                    |                                   |    | (remove-appender)                                                                  |                            |                                          |                                           |
| Delete all messengers                                     |                                                                            |                                    |                                   |    | (remove-all-appenders)                                                             |                            |                                          |                                           |
| Flush a messenger                                         |                                                                            |                                    |                                   |    | (flush-appender)                                                                   |                            |                                          |                                           |
| Flush all messengers                                      |                                                                            |                                    |                                   |    | (flush-all-appenders)                                                              |                            |                                          |                                           |
| Find a named messenger                                    | (find-appender)                                                            |                                    | (find-messenger)                  |    |                                                                                    |                            |                                          |                                           |
| Get the log-manager for a particular messenger            |                                                                            |                                    | (messenger-manager)               |    |                                                                                    |                            |                                          |                                           |
| Get the name for a particular messenger                   |                                                                            |                                    | (messenger-name)                  |    |                                                                                    |                            |                                          |                                           |
| List the categories handled by a particular messenger     |                                                                            |                                    | (messenger-category)              |    |                                                                                    | (category-spec)            |                                          |                                           |
| List the filter for a particular messenger                |                                                                            |                                    | (messenger-filter)                |    |                                                                                    |                            |                                          |                                           |
| Get text file name for a particular messenger             |                                                                            |                                    | (text-file-messenger-file)        |    | (appender-filename)                                                                |                            |                                          |                                           |
| Get stream name for a particular messenger                |                                                                            |                                    | (text-stream-messenger-stream)    |    | (appender-stream)                                                                  |                            |                                          |                                           |
| Get logger for a particular messenger                     |                                                                            |                                    | (messenger-manager)               |    |                                                                                    |                            |                                          |                                           |
| Get Destination for a particular messenger                |                                                                            |                                    |                                   |    |                                                                                    | (location)                 |                                          |                                           |
|                                                           |                                                                            |                                    |                                   |    |                                                                                    |                            |                                          |                                           |
| Put a message somewhere                                   | (append-message)                                                           |                                    |                                   |    |                                                                                    |                            |                                          |                                           |
| Get ring messages                                         |                                                                            |                                    | (ring-messenger-messages)         |    |                                                                                    |                            |                                          |                                           |
| Formatted message                                         | (format-message)                                                           |                                    | (format-message)                  |    |                                                                                    |                            |                                          |                                           |
| Unformatted message                                       |                                                                            |                                    | (message-description)             |    |                                                                                    |                            |                                          |                                           |
| Get message timestamp                                     |                                                                            |                                    | (message-timestamp)               |    |                                                                                    |                            | (message-time)                           |                                           |
| Get message category                                      |                                                                            |                                    | (message-category)                |    |                                                                                    |                            | (message-category), (message-categories) |                                           |
| Get message level                                         |                                                                            |                                    |                                   |    |                                                                                    |                            | (message-level)                          |                                           |
| Get message text                                          |                                                                            |                                    | (message-text)                    |    |                                                                                    |                            | (message-content)                        |                                           |
| Get message thread                                        |                                                                            |                                    |                                   |    |                                                                                    |                            | (message-thread)                         |                                           |
| Add signal handler                                        | (add-signal-handler)                                                       |                                    |                                   |    |                                                                                    |                            |                                          |                                           |
| Message with conditions                                   | (appending-message), (generating-message), (logging-message)               |                                    |                                   |    |                                                                                    | (log-if)                   | (log-condition),(message-condition)      |                                           |
| Conditions                                                | (log-serious-conditions)(log-errors)                                       | invalid-facility, invalid-priority |                                   |    | (log4cl-error)(pattern-layout-error)(property-parser-error)(handle-appender-error) | error                      |                                          |                                           |
| Starting message condition                                |                                                                            |                                    |                                   |    |                                                                                    |                            |                                          |                                           |
| Dynamic message helpers                                   | (when-log-message-appended)(when-log-message-generated)(push-into-message) |                                    |                                   |    |                                                                                    |                            |                                          |                                           |
| Logging hook helpers                                      | (get-log-fn)                                                               |                                    |                                   |    |                                                                                    |                            |                                          |                                           |
|                                                           |                                                                            |                                    |                                   |    |                                                                                    |                            |                                          |                                           |
| access context                                            |                                                                            |                                    |                                   |    |                                                                                    | (context)                  |                                          |                                           |
| add context                                               |                                                                            |                                    |                                   |    |                                                                                    | (push-context)             |                                          |                                           |
| subtract context                                          |                                                                            |                                    |                                   |    |                                                                                    | (pop-context)              |                                          |                                           |
| other context management                                  |                                                                            |                                    |                                   |    |                                                                                    | (with-context)             |                                          |                                           |
|                                                           |                                                                            |                                    |                                   |    |                                                                                    |                            |                                          |                                           |
| Define output                                             |                                                                            |                                    |                                   |    |                                                                                    | (defoutput)                |                                          |                                           |
| Layout and format functions                               |                                                                            |                                    |                                   |    |                                                                                    | (output-spec)              |                                          |                                           |
|                                                           |                                                                            |                                    |                                   |    |                                                                                    |                            |                                          |                                           |
| Rotate Log                                                |                                                                            |                                    |                                   |    | (backup-log-file)                                                                  |                            | (rotate-log)                             |                                           |
| Stop Rotation                                             |                                                                            |                                    |                                   |    |                                                                                    |                            | (stop-rotation)                          |                                           |
| Change Rotation Intervals                                 |                                                                            |                                    |                                   |    |                                                                                    |                            | (update-interval)                        |                                           |
| Parse Cron Intervals                                      |                                                                            |                                    |                                   |    |                                                                                    |                            | (make-cron-interval)                     |                                           |
| Save Configuration                                        |                                                                            |                                    |                                   |    | (save)                                                                             |                            |                                          |                                           |
| Restore Configuration to a previous configuration         |                                                                            |                                    |                                   |    | (restore)                                                                          |                            |                                          |                                           |
| Reset Configuration to default                            |                                                                            |                                    |                                   |    | (reset-logging-configuration)                                                      |                            | (restart-global-controller)              |                                           |
| Clear logging configuration                               |                                                                            |                                    |                                   |    | (clear-logging-configuration)                                                      |                            |                                          |                                           |
| Configure from file                                       |                                                                            |                                    |                                   |    |                                                                                    | (configure-from-file)      |                                          |                                           |
| Ignore errors or go to debugging                          |                                                                            |                                    |                                   |    |                                                                                    | (ignore-errors-p)          |                                          |                                           |
| Json functions                                            | (alist-as-json)(plist-as-json) (11)                                        |                                    |                                   |    |                                                                                    |                            |                                          |                                           |

(10) Returns an integer (11) Personally I would have used one of the specialy json libraries

<a id="orgeef00b0"></a> [top](#org9155e6a)


<a id="org2ae08e4"></a>

# To Do

More discussion on filters E.g. log4cl automatically gives you function, package. Does that preclude the ability to filter on those? Demo how that would work in each package.

What happens when you generate an executable?

How do you track separate threads? Does the library provide a built-in call or do you have to write your own?
