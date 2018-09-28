# viseq

> poor man's VIdeo SEQuencer

Using OpenCV's Common Lisp bindings at [byulparan/common-cv](https://github.com/byulparan/common-cv)

## Usage

Don't.

You send some (push-cvideo) commands to make one(or more) video(s) appear and call (push-cvideo) again to update some of the state of the video. Like add an effect or skip to time in seconds. Finally you have (delete-cvideo) to delete the video (which doesn't actually delete anything just stops rendering the video but the underlaying capture resource is still open to avoid locking).


```
> (ql:quickload :viseq)
> (in-package :viseq)
> (show-videos)
> (push-cvideo :bit "/home/sendai/clips/bitcoin.mp4")
> (push-cvideo :bit "/home/sendai/clips/bitcoin.mp4" :scale 2f0)
> (delete-cvideo :bit)
```

## Dev note

There is god object that has flags/states that are put on a queue, that queue is transversed each frame and the properties are evaluated depending of flags or properties of each object.

You can recompile the (render) function to test things out there in real-time.

## See also

* https://gitlab.com/Theemacsshibe/cl-vep/

## TODO's

- Add continuable
- Effects (? Face recognition(?
- Multiple windows support
- OSC (?
- Might be write a DSL to define different orders of when an effect is applied (?
- Frame manipulations (those are hard as there is no numpy for Lisp and need to be written on lisp through cffi calls)
