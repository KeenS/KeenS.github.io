HUGO?=hugo
GIT?=git
THEME=liquorice-k

THEME_PATH=themes/$(THEME)
BLOG_PATH=public

SOURCE_ORIGIN=git@github.com:KeenS/KeenS.github.io.git
BLOG_ORIGIN=$(SOURCE_ORIGIN)
THEME_ORIGIN=git@github.com:KeenS/liquorice-k.git

default: preview

preview:
	$(HUGO) server -Dtrue -vw -t $(THEME)

build:
	$(HUGO) -v -t $(THEME)

deploy:
	$(HUGO) -v -t $(THEME)
	$(GIT) add public
	$(GIT) commit -m "Site updated at `date '+%Y-%m-%d %H:%M:%S UTC'`"
	$(GIT) subtree push -P $(BLOG_PATH) $(BLOG_ORIGIN) master

push_theme:
	$(GIT) subtree push -P $(THEME_PATH) $(THEME_ORIGIN) master

.PHONY: preview build deploy dry push_theme
