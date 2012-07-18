# Checks if working tree is dirty
parse_hg_dirty() {
  #dirty=$(hg status --no-color 2> /dev/null | grep "^[MAR\?\!] ")
  #if $dirty ; then
  if [[ -n $(hg status) ]]; then
    echo "$ZSH_THEME_HG_PROMPT_DIRTY"
  else
    echo "$ZSH_THEME_HG_PROMPT_CLEAN"
  fi
}

function hg_prompt_info() {
  branch=$(hg id -b 2> /dev/null) || return
  tag=$(hg id -t 2> /dev/null) || return
  #rev_number=$(hg id -n 2> /dev/null) || return
  #rev_id=$(hg id -i 2> /dev/null) || return
  echo "${ZSH_THEME_HG_PROMPT_PREFIX}${branch}@${tag}$(parse_hg_dirty)${ZSH_THEME_HG_PROMPT_SUFFIX}"
}
