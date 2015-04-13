# Summary #

Optimization greatly increased performance for points. Cairo is generally the main cause of slow performance. For 100k points, OVT accounts for roughly 0.2s. Cairo code takes anywhere from 1.7s for points to 3.1s for lines. Until OVT supports a different renderer, further optimization is not worthwhile.

Results below which show paths without frames to be much faster than those within frames are erroneous; I was not rendering the paths properly outside frames.

## Notes from [issue 16](https://code.google.com/p/ovt/issues/detail?id=16) ##

First test results:
```
4.376273s render 100k points
4.792301s render 100k points in frame
1.156071s render 100k path
3.396213s render 100k path in frame
```

I'm puzzled that frame rendering slows paths much more than points. My first guess
was that transformations have something to do with it. If I change
transform\_scene\_node to return the node without transforming it, the path falls back
in line:
```
4.096256s render 100k points
4.320270s render 100k points in frame
1.132072s render 100k path
1.192074s render 100k path in frame
```

Nulling out cairo drawing for paths drops the time to near zero:
```
0.000000s render 100k path
0.064004s render 100k path in frame
```

Without making cairo faster, it looks like all I can do for paths is improve
transformation time.

Disabling antialiasing makes paths about .3s faster.

Cutting out cairo drawing for points drops the time significantly:
```
0.224014s render 100k points
0.432027s render 100k points in frame
```

Drawing the point shapes might be significantly slower than drawing paths, but since I represent points individually while the entire path is a single primitive, this could also be the cause. I'll change path to render point shapes to test it out:
```
4.084254s render 100k points
4.308270s render 100k points in frame
3.856241s render 100k path
3.924245s render 100k path in frame
```

Drawing all 100k points to a path before filling caused cairo to run out of memory die, so this represents a compromise where I make paths for 1000 points before filling and continuing on. Looks like this is just slow. Since I'm duplicating the same shape, is there a way to render once and copy to each location?

Possibilities
  * Copy the path with cairo\_copy\_path and render it back with cairo\_append\_path. This might be good if cairo\_arc is the problem, and bad if filling is still slow.
  * Draw the shape to a group, pop the group as a source, and paint it back in different places.

Making the path is fast, but drawing operations are slow. With a bit of fiddling, the second idea works quite well. Instead of popping the group as source, I need to pop it and re-set it after each translation to the new point location, but it's fast:
```
1.396088s render 100k path
1.844115s render 100k path in frame
```

## Notes from applying optimization ##

I changed the point primitive to hold a list instead of a single point:
```
3.792237s render 100k points
4.184262s render 100k points in frame
1.164072s render 100k path
3.420214s render 100k path in frame
```

Now to apply the optimization and avoid redrawing the shape for each instance:
```
1.344084s render 100k points
1.912119s render 100k points in frame
1.128070s render 100k path
3.324208s render 100k path in frame
```

Success! Now if I just figure out why paths are so slow in the frame, then I'll be done.

Mysterious. If I remove all cairo calls, then it appears the frame scaling only takes .2 seconds:
```
0.000000s render 100k path
0.208013s render 100k path in frame
```

If I create a path from these points, the frame time jumps dramatically:
```
0.028002s render 100k path
1.092068s render 100k path in frame
```

I thought maybe it had something to do with the actual values used, so I changed the range of the random floats, but this had no effect. If it has something to do with the transformations, then changing the frame to use fixed scaling should drop the time. Ok, changing to fixed has no effect. Could this be a GC issue? Let's flip the order in which tests are conducted. Nothing. Even with the fixed scaling, the frame is still applying transformations, and I do have those earlier results which say transformations account for the difference. I'll try to reproduce those.

Making Primitive.transform act as identity for paths drops the time by .2s, consistent with the observation above that shows .2s for scaling. Making SceneGraph.transform\_scene\_node act as identity does nothing further.

Inspecting the output from the tests shows that the path by itself isn't rendering anything. If everything is clipped, this would explain the difference. Ok, fixed this and now both times are in at around 3s. I would have preferred that both dropped to less than 1s, but alas.

Conclusion is this: ovt code on 100k points takes about 0.2s. cairo code on 100k points takes 1.7s for points, 3.1s for lines. Optimization done.

## Testing with Cairo ##

Always ensure cairo rendering tests are truly rendering the same things. The difference between paths in and out of frames probably had several causes, but scaling was at the root of them. When all points had coordinates in [0, 1] and the path was rendered outside a frame, cairo could render very quickly. Scaling the points to fill the 100x100 surface slowed cairo considerably, and not because of the scaling operation but because of cairo.