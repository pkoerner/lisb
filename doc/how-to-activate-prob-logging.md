# How to activate ProB logging (with custom ProB):

I think two steps can be sensible:

## Use ProB from custom source:

Add the following injections to your `project.clj`:

```
  :injections [(.. System (setProperty "prob.home" "/home/philipp/repos/prob_prolog/"))
               (.. System (setProperty "PROB_HOME" "/home/philipp/repos/prob_prolog/"))
               ;; whatever you had here before
               ]
```

Of course, adjust the pathes of `prob.home` and `PROB_HOME`.

## Enable logging:

Add the following injection in your `project.clj`:

```
  :injections [(.. System (setProperty "logback.configurationFile" "de/prob/logging/fulltrace.xml"))
               ;; whatever you had here before
               ]
```

Also add to your dependencies:

```
   :dependencies [[ch.qos.logback/logback-classic "1.5.11" :scope "provided"]
                  ;; whatever you had here before
                  ]
```
