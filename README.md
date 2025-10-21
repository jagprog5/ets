## Entity Trait System

This is a proof of concept (aka very unpolished) entity trait system which
_could_ compete with
[archetypal](https://csherratt.github.io/blog/posts/specs-and-legion/) entity
component systems. It's a bold claim and I'd love for someone to point out the
issues and tradeoffs!

```bash
$ clear; cargo test -- --nocapture
```

Creating a video soon with more details.

Requires the rust nightly compiler due to the basic but necessary use of the
[specialization](https://doc.rust-lang.org/beta/unstable-book/language-features/specialization.html)
feature. I'd love to get rid of this requirement, but since rust doesn't have
C++ style SFINAE I don't see another way.

Help wanted! Feel free to create PRs.
