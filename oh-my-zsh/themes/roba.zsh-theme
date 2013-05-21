if [ $UID -eq 0 ]; then NCOLOR="red"; else NCOLOR="green"; fi

#╭─
#╰─
# $(git_prompt_info)\
# $(hg_prompt_info) \

PROMPT='%{$fg[$NCOLOR]%}%n%{$reset_color%}@%{$fg[cyan]%}%m\
%{$reset_color%}:%{$fg[magenta]%}%~\
%{$fg[red]%}%{$reset_color%} %(!.#.») '
PROMPT1='%{$fg_bold[red]%}\ %{$reset_color%}'

MODE_INDICATOR="%{$fg[red]%}--INSERT--%{$reset_color%}"

ZSH_THEME_HG_PROMPT_PREFIX=" %{$fg[yellow]%}("
ZSH_THEME_HG_PROMPT_CLEAN="" #"%{$fg[green]%}○%{$reset_color%}"
ZSH_THEME_HG_PROMPT_DIRTY="%{$fg[red]%}⚡%{$reset_color%}"
ZSH_THEME_HG_PROMPT_SUFFIX="%{$fg[yellow]%})%{$reset_color%}"

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[yellow]%}("
ZSH_THEME_GIT_PROMPT_CLEAN="" #"%{$fg[green]%}○%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}⚡%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$fg[yellow]%})%{$reset_color%}"
