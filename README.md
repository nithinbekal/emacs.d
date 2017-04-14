
# Emacs configuration with evil-mode

Emacs configuration, which is mostly a port of my [vimrc](https://github.com/nithinbekal/dotfiles/blob/master/vimrc).

The init.el file contains all of my emacs configuration.
I use the excellent
[`use-package`](https://github.com/jwiegley/use-package)
package for managing package configuration.

## Open Jekyll

My setup for opening my Jekyll blog in emacs
and running `jekyll serve` in a shell buffer
can be found in jekyll.el.
The Jekyll blog can be run by:

{% highlight bash %}
emacs -nw --load path/to/jekyll.el
{% endhighlight %}

## Links

Here's a few awesome emacs configurations that helped me get this set up.

- [Spacemacs](https://github.com/syl20bnr/spacemacs) is an excellent starter kit for people coming in from vim.
- [Sacha Chua's Emacs configuration](http://pages.sachachua.com/.emacs.d/Sacha.html) - written in literate programming style.
- [Howard Abraham's .emacs](https://github.com/howardabrams/dot-files/blob/master/emacs.org) - another literate programming style emacs configuration.
- [jcf/emacs.d](https://github.com/jcf/emacs.d)

