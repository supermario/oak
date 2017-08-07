## Oak FAQs

#### Why not just run Elm on the server-side using nodejs?

This has [already been tried](https://github.com/eeue56/take-home), and these takeaways match some of my thoughts/motiviations for writing Oak:

> As an Elm programmer, I like to write Elm! As a server-side Elm programmer, I hate writing yet another integration library that wraps around a Node library that uses mutable objects and callbacks in a weird way. There are a lot of battles that you have to face everyday writing libraries that work with Node. Sometimes there just isn't a way to make Node libraries play nicely with Elm. This does not make for a stable runtime, nor a stable platform.

> The tl;dr here is that Node is not the ideal platform for server-side Elm. An alternate platform to base itself on would be great, but is unlikely to happen "soon". Please take away some of the ideas here and think about them! But if you value your sanity, your stability and your users, don't use this proof of concept for anything more than interest!

Oak is one way of doing some thinking about this problem, in particular around the possibility of a subset of Haskell being a possible Elm-like platform for _today_.

Perhaps in future there will be a more ideal / purpose-built server side platform that Elm targets directly – that would be really awesome! Hopefully some of the exploration in Oak in the meantime helps towards that longer term goal.


#### Why not just use GHCJS and have no Elm dependency?

Haskell is great, but it wasn't built specifically with the browser in mind. GHCJS, while an impressive feat, doesn't change this.

Elm has been very carefully crafted from the ground up specifically for the browser and web development, and as a result I think it is _excellent_ in its domain*. Conversely, Elm wasn't created with being run outside of the browser in mind.

Oak aims to explore some of the ideas of Elm in Haskell, and seek answers to the question of "what might a good static, type-inferred and pure server-side web development focused language look like?"


<sub>\* If you don't agree, perhaps you have different use cases or value different things in a front-end language. That's ok. In any case, there are many alternatives with different focuses available! Pick the one you think suits your needs best.</sub>
